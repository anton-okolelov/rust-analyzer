use std::sync::Arc;

use rustc_hash::FxHashMap;
use ra_db::FileId;
use ra_arena::{Arena, ArenaId, impl_arena_id, RawId};
use ra_syntax::{AstNode, ast::{self, ModuleItemOwner}};

use crate::{
    Function, Module, Struct, Enum, Const, Static, Trait, TypeAlias,
    Crate, PersistentHirDatabase, HirFileId, Name,
    nameres::{Resolution, PerNs, ModuleDef},
    ids::{AstItemDef, LocationCtx},
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
}

struct ModCollector<'a, D> {
    def_collector: D,
    module_id: ModuleId,
    file_id: FileId,
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
        ModCollector { def_collector: &mut *self, module_id, file_id, raw_items: &raw_items }
            .collect(&raw_items.items);
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
                            file_id,
                            raw_items: &raw_items,
                        }
                        .collect(&*raw_items.items)
                    }
                },
                raw::RawItem::Import(import) => {
                    self.def_collector.unresolved_imports.push((self.module_id, import))
                }
                raw::RawItem::Macro(mac) => {
                    self.def_collector.unexpanded_macros.push((self.module_id, mac))
                }
                raw::RawItem::Def(def) => self.define_def(&self.raw_items[def]),
            }
        }
    }

    fn define_child_module(&mut self, name: Name) -> ModuleId {
        let res = self.def_collector.alloc_module();
        self.def_collector.def_map.modules[res].parent = Some(self.module_id);
        self.def_collector.def_map.modules[self.module_id].children.insert(name, res);
        res
    }

    fn define_def(&mut self, def: &raw::DefData) {
        let ctx = LocationCtx::new(
            self.def_collector.db,
            Module { krate: self.def_collector.krate, module_id: self.module_id },
            self.file_id.into(),
        );
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
}
