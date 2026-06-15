# R style guide — research scripts for teaching

Best-practice R for academic research, optimized for **pedagogy and reproducibility**.
Companion to `SLIDE_STYLE.md`. The worked reference is `2026-06-15.R`.

## 0. Guiding principle
- A research/teaching script is **read far more than it is run.** Optimize for a reader
  (a student, a co-author, future-you) reconstructing the *reasoning*, not for cleverness
  or brevity. When transparency and concision conflict, choose **transparency**.
- Every script must run top-to-bottom from a clean session via `Rscript file.R` and
  reproduce the same numbers and figures.

## 1. Naming -- snake_case
- **snake_case** for everything: variables, functions, files. Lowercase words joined by
  `_`: `t_obs`, `all_z`, `milk_first_sets`, `fisher_test_stat()`.
- **No dots in names** (`my.var`) -- dots collide with S3 method dispatch and read as
  structure that isn't there. No camelCase. Be consistent.
- Variables are **nouns** (`null_df`, `obs_stat`); functions are **verbs / verb-phrases**
  (`compute_stat()`, `make_assignment()`).
- Names describe the *thing*, not its type: `z_obs`, not `z_vector`. Use whole words;
  reserve single letters for genuine math indices in a tight scope (`i`, `z`, `y`).
- Bind problem parameters to **named constants**, never hardcode: `n_cups <- 8`.
- `TRUE` / `FALSE`, never `T` / `F` (which are reassignable variables).

## 2. Name your arguments -- the default, with principled exceptions
Default: **name the arguments in a function call.** Named arguments document intent, teach
the function's interface, and prevent silent position errors.

**ALWAYS name:**
- logical flags and behaviour-changing arguments: `na.rm = TRUE`, `simplify = TRUE`,
  `byrow = TRUE`, `replace = FALSE`, `drop = FALSE`;
- arguments whose order is non-obvious or easy to transpose:
  `apply(X = m, MARGIN = 2, FUN = sum)`, `combn(x = 1:8, m = 4)`,
  `matrix(data = v, nrow = 8, ncol = 1)`, `seq(from = 1, to = 10, by = 2)`,
  `rep(x = 0, times = 8)`, `sample(x = s, size = n, replace = FALSE)`,
  `ggsave(filename = "f.png", plot = p, width = 8, height = 4, units = "in", dpi = 300)`;
- **every** argument past the first in a function you wrote.

**Do NOT name an argument when the name is noise, not meaning** -- omit it for:
1. the single, self-evident **primary input** of a one-argument function:
   `sqrt(x)`, `length(z)`, `nrow(df)`, `sort(v)`, `summary(fit)`, `sum(z * y)`,
   `mean(all_t >= t_obs)`;
2. items passed through **`...`** (they have no parameter name): `c(1, 0, 1)`,
   `paste0("fig_", i)`, `rbind(a, b)`, `data.frame(...)` value columns;
3. **operators / comparisons**: `z * y`, `a + b`, `x %in% s`, `i == 1`;
4. the value flowing through a **pipe**: `df |> filter(x > 0)` -- never restate it;
5. **idiom-is-the-convention** calls where a name reads as clutter: `1:8`,
   `seq_len(n)`, `head(x)`, and anonymous bodies `function(z) sum(z * y)`.

Rule of thumb: **name it when the name teaches the reader something or guards against a
transposition; drop it when the argument is obviously "the data," an operator, a `...`
item, or a piped value.**

## 3. Layout and spacing
- `<-` for assignment (never `=`); reserve `=` for named arguments.
- Spaces around binary operators (`a + b`, `x <- 1`) and after commas; no space before a
  call's `(`.
- Indent **2 spaces**, no tabs. Keep lines `<= ~80` characters.
- One statement per line; never `;`-chain. No right-assign (`->`).
- Long calls: one argument per line, the closing `)` on its own line.

## 4. Comments and literate structure
- Comment the **why**, and for teaching the **what-it-means**, not the obvious how:
  `# columns of all_z = the 70 possible assignments` beats `# apply over columns`.
- Banner comments mark the script's acts and mirror the analysis steps.
- One lead comment per conceptual block; reserve end-of-line comments for short value
  checks (`t_obs <- sum(z * y)   # = 4`).
- Keep comments in sync with code -- delete stale ones.

## 5. Reproducibility (research-critical)
- `library()` calls at the **top**; load only what you use. Prefer `pkg::fun()` for a
  one-off call from a heavy package.
- Set a seed before any randomness: `set.seed(seed = 12345)`.
- **Relative paths only** (`images/fig.png`); never `setwd()` to an absolute path.
- No `rm(list = ls())` and no `rstudioapi` / interactive-only calls in a sourced script --
  they break `Rscript` and clobber a caller's session.
- Print the few key results so a reader can verify them (`nrow(all_256)  # 256`).

## 6. Functions
- Write a function when logic repeats or names a concept. Keep it short and **pure**
  (no side effects beyond the return value).
- Roxygen-style header for non-trivial functions: one-line purpose, `@param`, `@return`.
- Name all parameters in the definition; give options sensible defaults; in teaching code
  return **explicitly** with `return()` so the reader need not hunt for the last value.

## 7. Data and control flow
- Vectorize. Never grow a vector/data frame in a loop (`x <- c(x, new)`); preallocate or
  use the `*apply` / `purrr` family.
- `seq_len(n)` / `seq_along(x)`, never `1:length(x)` (wrong when length is 0).
- Prefer an explicit `apply(X =, MARGIN =, FUN =)` / `vapply()` over an opaque one-liner
  in teaching code -- clarity over cleverness.

## 8. Plots (figures for papers and slides)
- Build the plot object, then `ggsave()` it with **explicit, named** `width`, `height`,
  `units`, `dpi` -- never rely on the device default; figures must be reproducible at a
  fixed size.
- Save to a relative `images/` (or `figures/`) folder; descriptive snake_case file names
  (`fisher_null_dist.png`).
- Label axes in full; use `expression()` for math so the figure matches the
  manuscript/slide notation (e.g. bold `bold(z)^T * bold(y)`).
- One figure = one clearly-commented block.

## 9. Avoid
- `attach()`, `setwd("/abs/path")`, `rm(list = ls())` in sourced scripts, `T`/`F`,
  `1:length(x)`, right-assign `->`, `;`-chains, dot-names, and over-compressed one-liners
  a student cannot unpack.
