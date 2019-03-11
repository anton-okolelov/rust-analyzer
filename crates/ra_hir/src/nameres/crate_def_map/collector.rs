use std::sync::Arc;

use rustc_hash::FxHashMap;
use ra_db::FileId;
use ra_arena::{Arena, ArenaId, impl_arena_id, RawId};
use ra_syntax::{AstNode, ast::{self, ModuleItemOwner}};

use crate::{Crate, PersistentHirDatabase, HirFileId, Name};

use super::{CrateDefMap, ModuleIndex, ModuleData};

pub(crate) fn crate_def_map_query(
    db: &impl PersistentHirDatabase,
    krate: Crate,
) -> Arc<CrateDefMap> {
    let mut collector = DefCollector { db, krate, def_map: CrateDefMap::default() };
    collector.collect();
    let def_map = collector.finish();
    Arc::new(def_map)
}

struct DefCollector<DB> {
    db: DB,
    krate: Crate,
    def_map: CrateDefMap,
}

impl<'a, DB> DefCollector<&'a DB>
where
    DB: PersistentHirDatabase,
{
    fn collect(&mut self) {
        let crate_graph = self.db.crate_graph();
        let file_id = crate_graph.crate_root(self.krate.crate_id());
        let raw_items = raw_items_query(self.db, file_id);
        let module_id = self.alloc_module();
        self.collect_module(module_id, raw_items);
    }

    fn collect_module<'s>(&mut self, module_id: ModuleIndex, raw_items: RawItems) {
        for item in raw_items {
            match item {
                RawItem::Module(m) => match m {
                    RawModule::Definition { name, raw_items } => {
                        let module_id = self.child_module(module_id, name);
                        self.collect_module(module_id, raw_items)
                    }
                    RawModule::Declaration { name } => {
                        let module_id = self.child_module(module_id, name);
                        let file_id = unimplemented!("self.resolve_module(name)");
                        let raw_items = raw_items_query(self.db, file_id);
                        self.collect_module(module_id, raw_items)
                    }
                },
                RawItem::Import(_im) => {}
                RawItem::Def(_def) => {}
                RawItem::Macro(_mac) => {}
            }
        }
    }

    fn child_module(&mut self, parent: ModuleIndex, name: Name) -> ModuleIndex {
        let res = self.alloc_module();
        self.def_map.modules[parent].children.insert(name, res);
        res
    }

    fn alloc_module(&mut self) -> ModuleIndex {
        self.def_map.modules.alloc(ModuleData::default())
    }

    fn finish(self) -> CrateDefMap {
        self.def_map
    }
}

type RawItems = Vec<RawItem>;

enum RawItem {
    Module(RawModule),
    Import(RawImport),
    Def(RawImport),
    Macro(RawImport),
}

enum RawModule {
    Definition { name: Name, raw_items: RawItems },
    Declaration { name: Name },
}
struct RawImport;
struct RawDef;
struct RawMacro;

fn raw_items_query(db: &impl PersistentHirDatabase, file_id: FileId) -> RawItems {
    unimplemented!()
}
