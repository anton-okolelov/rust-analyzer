use std::{
    sync::Arc,
    ops::Index,
};

use ra_db::FileId;
use ra_arena::{Arena, impl_arena_id, RawId, map::ArenaMap};
use ra_syntax::{
    AstPtr, AstNode,
    ast::{self, ModuleItemOwner, NameOwner, AttrsOwner},
};

use crate::{
    PersistentHirDatabase, Name, AsName, Path,
    ids::{SourceFileItemId, SourceFileItems},
};

#[derive(Default, PartialEq, Eq)]
pub(crate) struct RawItems {
    modules: Arena<Module, ModuleData>,
    imports: Arena<Import, ImportData>,
    defs: Arena<Def, DefData>,
    macros: Arena<Macro, MacroData>,

    pub(crate) items: Vec<RawItem>,
}

impl RawItems {
    pub(crate) fn raw_items_query(db: &impl PersistentHirDatabase, file_id: FileId) -> RawItems {
        let mut collector = RawItemsCollector {
            raw_items: RawItems::default(),
            source_file_items: db.file_items(file_id.into()),
        };
        let source_file = db.parse(file_id);
        collector.process_module(None, &*source_file);
        collector.raw_items
    }
}

impl Index<Module> for RawItems {
    type Output = ModuleData;
    fn index(&self, idx: Module) -> &ModuleData {
        &self.modules[idx]
    }
}

impl Index<Import> for RawItems {
    type Output = ImportData;
    fn index(&self, idx: Import) -> &ImportData {
        &self.imports[idx]
    }
}

impl Index<Def> for RawItems {
    type Output = DefData;
    fn index(&self, idx: Def) -> &DefData {
        &self.defs[idx]
    }
}

impl Index<Macro> for RawItems {
    type Output = MacroData;
    fn index(&self, idx: Macro) -> &MacroData {
        &self.macros[idx]
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct RawItemsSourceMap {
    imports: ArenaMap<Import, AstPtr<ast::PathSegment>>,
}

#[derive(PartialEq, Eq)]
pub(crate) enum RawItem {
    Module(Module),
    Import(Import),
    Def(Def),
    Macro(Macro),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Module(RawId);
impl_arena_id!(Module);

#[derive(PartialEq, Eq)]
pub(crate) enum ModuleData {
    Declaration { name: Name },
    Definition { name: Name, items: Vec<RawItem> },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Import(RawId);
impl_arena_id!(Import);

#[derive(PartialEq, Eq)]
pub(crate) struct ImportData {
    path: Path,
    alias: Option<Name>,
    is_glob: bool,
    is_prelude: bool,
    is_extern_crate: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Def(RawId);
impl_arena_id!(Def);

#[derive(PartialEq, Eq)]
pub(crate) struct DefData {
    pub(crate) name: Name,
    pub(crate) source_item_id: SourceFileItemId,
    pub(crate) kind: DefKind,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub(crate) enum DefKind {
    Function,
    Struct,
    Enum,
    Const,
    Static,
    Trait,
    TypeAlias,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct Macro(RawId);
impl_arena_id!(Macro);

#[derive(PartialEq, Eq)]
pub(crate) struct MacroData {
    path: Path,
    arg: tt::Subtree,
}

struct RawItemsCollector {
    raw_items: RawItems,
    source_file_items: Arc<SourceFileItems>,
}

impl RawItemsCollector {
    fn process_module(&mut self, current_module: Option<Module>, body: &impl ast::ModuleItemOwner) {
        for item_or_macro in body.items_with_macros() {
            match item_or_macro {
                ast::ItemOrMacro::Macro(m) => self.add_macro(current_module, m),
                ast::ItemOrMacro::Item(item) => self.add_item(current_module, item),
            }
        }
    }

    fn add_item(&mut self, current_module: Option<Module>, item: &ast::ModuleItem) {
        let (kind, name) = match item.kind() {
            ast::ModuleItemKind::Module(module) => {
                self.add_module(current_module, module);
                return;
            }
            ast::ModuleItemKind::UseItem(use_item) => {
                self.add_use_item(current_module, use_item);
                return;
            }
            ast::ModuleItemKind::ExternCrateItem(_) => {
                // FIXME: desugar to use
                return;
            }
            ast::ModuleItemKind::ImplBlock(_) => {
                // impls don't participate in name resolution
                return;
            }
            ast::ModuleItemKind::StructDef(it) => (DefKind::Struct, it.name()),
            ast::ModuleItemKind::EnumDef(it) => (DefKind::Enum, it.name()),
            ast::ModuleItemKind::FnDef(it) => (DefKind::Function, it.name()),
            ast::ModuleItemKind::TraitDef(it) => (DefKind::Trait, it.name()),
            ast::ModuleItemKind::TypeAliasDef(it) => (DefKind::TypeAlias, it.name()),
            ast::ModuleItemKind::ConstDef(it) => (DefKind::Const, it.name()),
            ast::ModuleItemKind::StaticDef(it) => (DefKind::Static, it.name()),
        };
        if let Some(name) = name {
            let name = name.as_name();
            let source_item_id = self.source_file_items.id_of_unchecked(item.syntax());
            let def = self.raw_items.defs.alloc(DefData { name, kind, source_item_id });
            self.push_item(current_module, RawItem::Def(def))
        }
    }

    fn add_module(&mut self, current_module: Option<Module>, module: &ast::Module) {
        let name = match module.name() {
            Some(it) => it.as_name(),
            None => return,
        };
        let item = if module.has_semi() {
            self.raw_items.modules.alloc(ModuleData::Declaration { name })
        } else if let Some(item_list) = module.item_list() {
            let item =
                self.raw_items.modules.alloc(ModuleData::Definition { name, items: Vec::new() });
            self.process_module(Some(item), item_list);
            item
        } else {
            return;
        };
        self.push_item(current_module, RawItem::Module(item));
    }

    fn add_use_item(&mut self, current_module: Option<Module>, use_item: &ast::UseItem) {
        let is_prelude = use_item
            .attrs()
            .any(|attr| attr.as_atom().map(|s| s == "prelude_import").unwrap_or(false));

        Path::expand_use_item(use_item, |path, segment, alias| {
            let import = self.raw_items.imports.alloc(ImportData {
                path,
                alias,
                is_glob: segment.is_none(),
                is_prelude,
                is_extern_crate: false,
            });
        })
    }

    fn add_macro(&mut self, current_module: Option<Module>, m: &ast::MacroCall) {
        if let Some((path, arg)) = (|| {
            let path = m.path().and_then(Path::from_ast)?;
            let tt = m.token_tree()?;
            let arg = mbe::ast_to_token_tree(tt)?.0;
            Some((path, arg))
        })() {
            let m = self.raw_items.macros.alloc(MacroData { path, arg });
            self.push_item(current_module, RawItem::Macro(m));
        }
    }

    fn push_item(&mut self, current_module: Option<Module>, item: RawItem) {
        match current_module {
            Some(module) => match &mut self.raw_items.modules[module] {
                ModuleData::Definition { items, .. } => items,
                ModuleData::Declaration { .. } => unreachable!(),
            },
            None => &mut self.raw_items.items,
        }
        .push(item)
    }
}
