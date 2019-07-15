-- The maximum amount of memory the cullbranches rendering approach is allowed
-- to use.  Increase to be able to draw more points.  Decrease to avoid crashing
-- the program because of out-of-memory errors.  The best value depends both on
-- the size of your GPU memory and on Futhark's general memory usage.
let cullbranches_bytes = 512i64 * 1024**2

-- The default number of iterations for fractals with 2, 3, and 4 branches when
-- using the scalarloop rendering approach.
let iterations2 = 22i32
let iterations3 = 14i32
let iterations4 = 11i32
