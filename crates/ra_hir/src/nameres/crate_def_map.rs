use std::sync::Arc;

use crate::{Crate, PersistentHirDatabase};

/// Contans all top-level defs from a macro-expanded crate
#[derive(Default)]
pub(crate) struct CrateDefMap {}

impl CrateDefMap {
    pub(crate) fn crate_def_map_query(
        db: &impl PersistentHirDatabase,
        krate: Crate,
    ) -> Arc<CrateDefMap> {
        Arc::new(CrateDefMap::default())
    }
}
