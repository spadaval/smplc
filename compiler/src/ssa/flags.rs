/**
 * When enabled, constant instructions will be computed at compile-time
 */
pub static CONST_COMPTIME: bool = true;
/**
 * When enabled, instructions will be eliminated if dominating sinstructions are found
 */
pub static ELIMINATE_DOMINANCE: bool = true;
/**
 * Should the SSA value of stores be considered a valid value for load eliminations?
 */
pub static STORE_RESIDUAL: bool = true;
