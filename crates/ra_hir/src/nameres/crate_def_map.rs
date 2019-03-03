use std::sync::Arc;

use ra_arena::{Arena, ArenaId, impl_arena_id, RawId};
use ra_syntax::{AstNode, ast::{self, ModuleItemOwner}};

use crate::{Crate, PersistentHirDatabase};

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct ModuleId(RawId);
impl_arena_id!(ModuleId);

#[derive(Default)]
struct ModuleData {}

/// Contans all top-level defs from a macro-expanded crate
#[derive(Default)]
pub(crate) struct CrateDefMap {
    modules: Arena<ModuleId, ModuleData>,
}

impl CrateDefMap {
    pub(crate) fn crate_def_map_query(
        db: &impl PersistentHirDatabase,
        krate: Crate,
    ) -> Arc<CrateDefMap> {
        let mut collector = DefCollector { db, krate, def_map: CrateDefMap::default() };
        collector.collect();
        let def_map = collector.finish();
        Arc::new(def_map)
    }
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
        let source_file = self.db.parse(file_id);
        let module_id = self.def_map.modules.alloc(ModuleData::default());
        self.collect_module(module_id, source_file.items());
    }

    fn collect_module<'s>(
        &mut self,
        module_id: ModuleId,
        items: impl Iterator<Item = &'s ast::ModuleItem>,
    ) {

    }

    fn finish(self) -> CrateDefMap {
        self.def_map
    }
}
