use ra_db::FileId;
use ra_arena::{Arena, ArenaId, impl_arena_id, RawId};
use ra_syntax::{
    ast::{self, ModuleItemOwner, NameOwner},
};

use crate::{PersistentHirDatabase, Name, AsName, Path};

#[derive(Default, PartialEq, Eq)]
struct RawItems {
    modules: Arena<Module, ModuleData>,
    imports: Arena<Import, ImportData>,
    defs: Arena<Def, DefData>,
    macros: Arena<Macro, MacroData>,

    items: Vec<RawItem>,
}

#[derive(PartialEq, Eq)]
enum RawItem {
    Module(Module),
    Import(Import),
    Def(Def),
    Macro(Macro),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Module(RawId);
impl_arena_id!(Module);

#[derive(PartialEq, Eq)]
enum ModuleData {
    Declaration { name: Name },
    Definition { name: Name, items: Vec<RawItem> },
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Import(RawId);
impl_arena_id!(Import);

#[derive(PartialEq, Eq)]
struct ImportData {
    path: Path,
    alias: Option<Name>,
    is_glob: bool,
    is_prelude: bool,
    is_extern_crate: bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Def(RawId);
impl_arena_id!(Def);

#[derive(PartialEq, Eq)]
struct DefData {
    name: Name,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct Macro(RawId);
impl_arena_id!(Macro);

#[derive(PartialEq, Eq)]
struct MacroData {
    path: Path,
    arg: tt::Subtree,
}

fn raw_items_query(db: &impl PersistentHirDatabase, file_id: FileId) -> RawItems {
    let mut res = RawItems::default();
    let source_file = db.parse(file_id);
    res.process_module(None, &*source_file);
    res
}

impl RawItems {
    fn process_module(&mut self, module: Option<Module>, body: &impl ast::ModuleItemOwner) {
        for item_or_macro in body.items_with_macros() {
            match item_or_macro {
                ast::ItemOrMacro::Item(item) => {
                    if let Some(raw_item) = self.alloc_raw_item(item) {
                        self.push_item(module, raw_item)
                    }
                }
                ast::ItemOrMacro::Macro(m) => {
                    if let Some(m) = self.alloc_macro(m) {
                        self.push_item(module, RawItem::Macro(m))
                    }
                }
            }
        }
    }

    fn push_item(&mut self, module: Option<Module>, item: RawItem) {
        match module {
            Some(module) => match &mut self.modules[module] {
                ModuleData::Definition { items, .. } => items,
                ModuleData::Declaration { .. } => unreachable!(),
            },
            None => &mut self.items,
        }
        .push(item)
    }

    fn alloc_raw_item(&mut self, ast: &ast::ModuleItem) -> Option<RawItem> {
        match ast.kind() {
            ast::ModuleItemKind::Module(m) => {
                let name: Name = m.name()?.as_name();
                let item = if m.has_semi() {
                    self.modules.alloc(ModuleData::Declaration { name })
                } else {
                    let item =
                        self.modules.alloc(ModuleData::Definition { name, items: Vec::new() });

                    item
                };
                return Some(RawItem::Module(item));
            }
            ast::ModuleItemKind::UseItem(_) => (),
            ast::ModuleItemKind::StructDef(_) => (),
            ast::ModuleItemKind::EnumDef(_) => (),
            ast::ModuleItemKind::FnDef(_) => (),
            ast::ModuleItemKind::TraitDef(_) => (),
            ast::ModuleItemKind::TypeAliasDef(_) => (),
            ast::ModuleItemKind::ImplBlock(_) => (),
            ast::ModuleItemKind::ExternCrateItem(_) => (),
            ast::ModuleItemKind::ConstDef(_) => (),
            ast::ModuleItemKind::StaticDef(_) => (),
        }
        None
    }

    fn alloc_macro(&mut self, m: &ast::MacroCall) -> Option<Macro> {
        let path = m.path().and_then(Path::from_ast)?;
        let tt = m.token_tree()?;
        let arg = mbe::ast_to_token_tree(tt)?.0;
        let res = self.macros.alloc(MacroData { path, arg });
        Some(res)
    }
}
