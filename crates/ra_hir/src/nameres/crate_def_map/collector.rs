use std::sync::Arc;

use rustc_hash::FxHashMap;
use ra_db::FileId;
use ra_syntax::{AstNode, ast::{self, ModuleItemOwner}};

use crate::{
    Function, Module, Struct, Enum, Const, Static, Trait, TypeAlias,
    Crate, PersistentHirDatabase, HirFileId, Name, PathKind, Path,
    KnownName,
    nameres::{Resolution, PerNs, ModuleDef},
    ids::{AstItemDef, LocationCtx, MacroCallLoc, SourceItemId},
};

use super::{CrateDefMap, ModuleId, ModuleData, raw};

pub(crate) fn crate_def_map_query(
    db: &impl PersistentHirDatabase,
    krate: Crate,
) -> Arc<CrateDefMap> {
    let mut collector = DefCollector {
        db,
        krate,
        def_map: CrateDefMap::default(),
        unresolved_imports: Vec::new(),
        unexpanded_macros: Vec::new(),
        global_macro_scope: FxHashMap::default(),
    };
    collector.collect();
    let def_map = collector.finish();
    Arc::new(def_map)
}

struct DefCollector<DB> {
    db: DB,
    krate: Crate,
    def_map: CrateDefMap,
    unresolved_imports: Vec<(ModuleId, raw::Import)>,
    unexpanded_macros: Vec<(ModuleId, raw::Macro)>,
    global_macro_scope: FxHashMap<Name, mbe::MacroRules>,
}

struct ModCollector<'a, D> {
    def_collector: D,
    module_id: ModuleId,
    file_id: HirFileId,
    raw_items: &'a raw::RawItems,
}

impl<'a, DB> DefCollector<&'a DB>
where
    DB: PersistentHirDatabase,
{
    fn collect(&mut self) {
        let crate_graph = self.db.crate_graph();
        let file_id = crate_graph.crate_root(self.krate.crate_id());
        let raw_items = raw::RawItems::raw_items_query(self.db, file_id);
        let module_id = self.alloc_module();
        ModCollector {
            def_collector: &mut *self,
            module_id,
            file_id: file_id.into(),
            raw_items: &raw_items,
        }
        .collect(&raw_items.items);
    }

    fn define_macro(&mut self, name: Name, tt: &tt::Subtree) {
        if let Ok(rules) = mbe::MacroRules::parse(tt) {
            self.global_macro_scope.insert(name, rules);
        }
    }

    fn alloc_module(&mut self) -> ModuleId {
        self.def_map.modules.alloc(ModuleData::default())
    }

    fn finish(self) -> CrateDefMap {
        self.def_map
    }
}

impl<DB> ModCollector<'_, &'_ mut DefCollector<&'_ DB>>
where
    DB: PersistentHirDatabase,
{
    fn collect(&mut self, items: &[raw::RawItem]) {
        for item in items {
            match *item {
                raw::RawItem::Module(m) => match &self.raw_items[m] {
                    raw::ModuleData::Definition { name, items } => {
                        let module_id = self.define_child_module(name.clone());
                        ModCollector {
                            def_collector: &mut *self.def_collector,
                            module_id,
                            file_id: self.file_id,
                            raw_items: self.raw_items,
                        }
                        .collect(&*items);
                    }
                    raw::ModuleData::Declaration { name } => {
                        let module_id = self.define_child_module(name.clone());
                        let file_id = unimplemented!("self.resolve_module(name)");
                        let raw_items =
                            raw::RawItems::raw_items_query(self.def_collector.db, file_id);
                        ModCollector {
                            def_collector: &mut *self.def_collector,
                            module_id,
                            file_id: file_id.into(),
                            raw_items: &raw_items,
                        }
                        .collect(&*raw_items.items)
                    }
                },
                raw::RawItem::Import(import) => {
                    self.def_collector.unresolved_imports.push((self.module_id, import))
                }
                raw::RawItem::Def(def) => self.define_def(&self.raw_items[def]),
                raw::RawItem::Macro(mac) => self.collect_macro(mac, &self.raw_items[mac]),
            }
        }
    }

    fn define_child_module(&mut self, name: Name) -> ModuleId {
        let res = self.def_collector.alloc_module();
        self.def_collector.def_map.modules[res].parent = Some(self.module_id);
        self.def_collector.def_map.modules[self.module_id].children.insert(name, res);
        res
    }

    fn module(&self) -> Module {
        Module { krate: self.def_collector.krate, module_id: self.module_id }
    }

    fn define_def(&mut self, def: &raw::DefData) {
        let ctx = LocationCtx::new(self.def_collector.db, self.module(), self.file_id.into());
        macro_rules! id {
            () => {
                AstItemDef::from_source_item_id_unchecked(ctx, def.source_item_id)
            };
        }
        let name = def.name.clone();
        let def: PerNs<ModuleDef> = match def.kind {
            raw::DefKind::Function => PerNs::values(Function { id: id!() }.into()),
            raw::DefKind::Struct => {
                let s = Struct { id: id!() }.into();
                PerNs::both(s, s)
            }
            raw::DefKind::Enum => PerNs::types(Enum { id: id!() }.into()),
            raw::DefKind::Const => PerNs::values(Const { id: id!() }.into()),
            raw::DefKind::Static => PerNs::values(Static { id: id!() }.into()),
            raw::DefKind::Trait => PerNs::types(Trait { id: id!() }.into()),
            raw::DefKind::TypeAlias => PerNs::types(TypeAlias { id: id!() }.into()),
        };
        let resolution = Resolution { def, import: None };
        self.def_collector.def_map.modules[self.module_id].scope.items.insert(name, resolution);
    }

    fn collect_macro(&mut self, mac_id: raw::Macro, mac: &raw::MacroData) {
        // Case 1: macro rules, define a macro in crate-global mutable scope
        if is_macro_rules(&mac.path) {
            if let Some(name) = &mac.name {
                self.def_collector.define_macro(name.clone(), &mac.arg)
            }
            return;
        }
        // Case 2: try to expand macro_rules from this crate, triggering
        // recursive item collection.
        if let Some(rules) =
            mac.path.as_ident().and_then(|name| self.def_collector.global_macro_scope.get(name))
        {
            if let Ok(tt) = rules.expand(&mac.arg) {
                // XXX: this **does not** go through a database, because we
                // can't identify macro_call without adding the whole state of
                // name resolution as a parameter to the query.
                //
                // So, we run the queries "manually" and we should maintain the
                // invariant that, when phases after the name resolution execute
                // queries to get parse tree for macro expansion, thouse phases
                // get the equivalent results.
                //
                // To put in other way, do to salsa model limitation (no cyclic
                // queries), we expand each macro twice: first, when doing name
                // resolution, and than, later, when we want to inspect it's
                // body closer.
                let source_item_id =
                    SourceItemId { file_id: self.file_id.into(), item_id: mac.source_item_id };
                let macro_call_id = MacroCallLoc { module: self.module(), source_item_id }
                    .id(self.def_collector.db);
                let file_id: HirFileId = macro_call_id.into();
                let source_file = mbe::token_tree_to_ast_item_list(&tt);
                let raw_items = raw::RawItems::from_source_file(&source_file, file_id);
                ModCollector {
                    def_collector: &mut *self.def_collector,
                    file_id,
                    module_id: self.module_id,
                    raw_items: &raw_items,
                }
                .collect(&raw_items.items)
            }
            return;
        }

        // Case 3: path to a macro from another crate, expand during name resolution
        self.def_collector.unexpanded_macros.push((self.module_id, mac_id))
    }
}

fn is_macro_rules(path: &Path) -> bool {
    path.as_ident().and_then(Name::as_known_name) == Some(KnownName::MacroRules)
}
