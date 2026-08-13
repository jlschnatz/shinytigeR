# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

---

## What this project is

**shinytigeR** (Training mit individuell generierten Erfolgsrückmeldungen in R) is a Shiny web app for Goethe University psychology students to practice statistics and R. Students log in, filter and select multiple-choice practice items, answer them, receive immediate automated feedback, and track their progress via an IRT-based competency dashboard.

The app is structured as an **R package** (`shinytigeR`) with the Shiny app living in `inst/app/`. It is deployed via Docker on ShinyProxy.

---

## Commands

**For full local setup (installing R/`rv`/`libsodium`, or the Docker-based alternative), see `SETUP.md` — it's the authoritative onboarding doc.** The commands below assume that setup is already done.

```bash
# Run locally (from project root) — uses the rv-managed library, loads the
# package via pkgload::load_all(), starts at http://localhost:7331
rv run dev/run.R
```

```r

# or install the package first and call:
shinytigeR::run_app()

# Document (regenerate NAMESPACE and man/) — needs devtools or roxygen2,
# neither of which is in rproject.toml (see Testing below for why); install
# one separately (e.g. in a personal, non-rv-managed library) to run this
devtools::document()

# Check the package — same caveat as above
devtools::check(vignettes = FALSE)

# Build tarball + Dockerfile for deployment (run from project root)
Rscript deploy/build.R
# Then:
docker build --platform linux/amd64 -t shinytiger deploy/
```

Note `docker/Dockerfile.dev` (used by `SETUP.md`'s Docker option) is dev-only (live source + package library, no production tarball) — distinct from `deploy/Dockerfile`, which builds the ShinyProxy production image described under Deployment below.

### Manually testing numeric items

```bash
rv run dev/add_numeric_samples.R   # idempotent — adds 3 numeric sample items (ids 90001-90003) to your local db_item.sqlite
rv run dev/run.R
```

Log in and either select the matching learning area (Deskriptivstatistik/Wahrscheinlichkeit/Regression) in the selector, or use "Spezifische Aufgabe auswählen" to jump straight to `90001`/`90002`/`90003` — the direct-ID lookup goes to `mod_inspect.R`, not the practice queue, so to exercise the practice flow (typing an answer, matching/unmatched/skip, the check button) use the selector. `dev/db_item_sample.sqlite` (what a fresh clone gets via `dev/seed_db.R`) already has these three items baked in too.

`dev/add_numeric_samples.R` is dev/local-only (it inserts fake practice items) — **never** run it against a production DB. For just the schema migration (`answer_mode` column + backfill, no sample items — safe against production), use `dev/migrate_answer_mode.R` directly; see the Deployment section below for when that has to run.

### Dependency management (rv)

Dependencies are managed with [`rv`](https://github.com/A2-ai/rv), a Rust-based R package manager. Config is in `rproject.toml`; the resolved lock is in `rv.lock`.

```bash
rv sync        # install/update packages to match rv.lock
rv add <pkg>   # add a package and update rv.lock
rv remove <pkg>
rv run dev/run.R                # run with the rv-managed library on PATH
```

Do **not** edit `rv.lock` by hand. The `rv/` directory is the local package library (arm64/macOS dev); it is gitignored and not part of the built package.

### Testing

`testthat`, `withr`, and `pkgload` are in `rproject.toml` (all three are already declared in `DESCRIPTION`'s `Suggests:`, and `pkgload` is what `dev/run.R` uses to load the package). `devtools` deliberately is **not** — a dry run showed it pulls in ~25 extra packages (`roxygen2`, `rmarkdown`, `tinytex`, `usethis`, `rcmdcheck`, `stringi`, `ragg`/`systemfonts`, the `gert`/`credentials` git stack, …) and would even force a different `rlang` version, which risks shifting versions the app itself depends on. `testthat` + `withr` + `pkgload` alone add only 14 light packages with no new system dependencies (`fs` is the only sysreqs hit, and it's already pulled in by `shiny`/`bslib`).

Run the suite via `rv run`, which uses the rv-managed library regardless of what (if anything) is in a personal R library:

```bash
rv run -e 'pkgload::load_all(quiet = TRUE); testthat::test_dir("tests/testthat")'
```

If you have `devtools` available separately (e.g. in a personal, non-rv-managed library — not through `rv run`), `devtools::test()` / `devtools::test_active_file()` work the same way and are more convenient for iterating on a single file.

`tests/testthat/` (1800+ lines) covers `irt.R`, `db.R`, `utils.R`, the dashboard helpers, registration (`db_register_user`/`db_username_exists`/`REG_*` constants), and module reactivity for `mod_selector`, `mod_practice` (both `mc` and `num` answer modes), `mod_inspect` (both answer modes), and `mod_dashboard` via `shiny::testServer()`.

**`tests/testthat/helper-modules.R`** has reusable fixtures for `testServer()`-based module tests — check here before writing ad hoc test setup:

- `make_data_item()` — minimal 6-row `mc` item `data.frame` (2 areas × content/coding/content, IRT params set, `answer_mode = "mc"`)
- `make_numeric_item(id_item, correct_value, distractors)` — single-row `num` item `data.frame`, distractor values in reused `answeroption_0X` columns
- `fake_credentials(user)` — a logged-in `credentials()` reactive
- `selector_cell_inputs(area_vals, type_vals, n_items, only_new)` — builds the `cell_i_j` input list `mod_selector_server` expects
- `make_user_db(user, item_ids, correct, areas)` — writes a temp SQLite user DB pre-populated with response rows, returns its path

> **Testing `mod_numeric_answer`'s skip button in `testServer()`:** call `session$flushReact()` once *before* the test's first `session$setInputs()`. Reason: `observeEvent(..., ignoreInit = TRUE)` swallows whatever the *first* reactive flush's action would have been — in a real browser session that first flush always happens before a user could possibly click anything, but in `testServer()`'s synchronous world, if the test's first `setInputs()` call *is* the click itself, `ignoreInit` swallows that legitimate first click too. See the `"practice: numeric item — explicit skip button..."` test in `test-modules.R` for the pattern.

---

## Project structure

```
app_v3/
├── DESCRIPTION            # package metadata; Imports lists all R dependencies
├── NAMESPACE              # auto-generated by devtools::document() — do not edit
├── .Rbuildignore          # excludes rv/, deploy/, app.R, *.sqlite from tarball
├── rproject.toml          # rv dependency config (repositories + top-level deps)
├── rv.lock                # full resolved dependency tree (auto-generated by rv)
│
├── R/                     # Package R source — all functions exported or internal
│   ├── run_app.R          # run_app() — the intended entry point (NAMESPACE also exports app_server/app_ui, mainly for testing/advanced use)
│   ├── shinytigeR-package.R  # package-level imports + globalVariables()
│   ├── constants.R        # app-wide constants (see below)
│   ├── db.R               # all SQLite access functions
│   ├── irt.R              # IRT model (2PL) functions
│   ├── utils.R            # markdown/math rendering + answer helpers
│   ├── ui.R               # app_ui() — top-level page_navbar layout
│   ├── server.R           # app_server() — wires auth + all modules
│   ├── mod_home.R         # module: home/landing panel (post-login)
│   ├── mod_register.R     # module: self-registration (pre-login, semester-code gated)
│   ├── mod_selector.R     # module: item filter/selection UI
│   ├── mod_practice.R     # module: item display + answer checking; dispatches to mod_mc_answer/mod_numeric_answer
│   ├── mod_mc_answer.R    # module: MC answer widget (radios) — child of mod_practice.R
│   ├── mod_numeric_answer.R  # module: numeric answer widget (typed value, tolerance match) — child of mod_practice.R
│   ├── mod_inspect.R      # module: read-only item overview (direct ID lookup)
│   └── mod_dashboard.R    # module: progress dashboard
│
├── dev/
│   ├── run.R                    # local dev entrypoint — rv run dev/run.R
│   ├── seed_db.R                 # creates db_credentials.sqlite + empty db_user.sqlite; copies db_item_sample.sqlite in if db_item.sqlite is missing
│   ├── make_sample_items.R       # regenerates dev/db_item_sample.sqlite from the full local db_item.sqlite (maintainers only)
│   ├── migrate_answer_mode.R     # production-safe: idempotently adds answer_mode column + backfills to 'mc'. No sample items — run this before deploying
│   ├── add_numeric_samples.R     # dev-only: sources migrate_answer_mode.R, then adds 3 numeric sample items — never run against production
│   └── db_item_sample.sqlite     # committed sample pool (17 items: 14 mc + 3 num)
│
├── inst/app/
│   ├── app.R              # loaded by runApp(); skips library() if already loaded via load_all()
│   └── www/
│       ├── css/app.css    # all custom CSS
│       ├── img_app/       # app branding: logo files (tigeR_hex.png, tiger_logo_white.png, favicon.png)
│       └── img_item/      # item content images — path baked into db_item.sqlite; do not rename without migrating
│
├── deploy/
│   ├── build.R            # generates Dockerfile + builds tarball (run this, not the Dockerfile directly)
│   ├── Dockerfile         # auto-generated by build.R — do not edit by hand; production image for ShinyProxy
│   ├── rv.lock            # copied here by build.R for the Docker build context
│   ├── rproject.toml      # copied here by build.R for the Docker build context
│   └── shinytigeR_*.tar.gz  # built by build.R
│
├── docker/                # dev-only Docker environment (not for deployment — see SETUP.md)
│   ├── Dockerfile.dev     # R 4.6 + rv + libsodium-dev; source is bind-mounted, not copied
│   ├── docker-compose.yml # bind-mounts project root; rv sync runs on container start
│   └── run_dev.R          # container entrypoint — like dev/run.R but binds 0.0.0.0, no browser launch
│
├── SETUP.md               # local setup guide: native (rig + rv) or Docker
│
├── tests/testthat/        # testthat suite — see "Testing" above
│
├── docs/                  # gitignored — forward-looking research memos, not shipped code docs
│                           # (dashboard/IRT redesign literature reviews, model comparisons)
│
├── db_item.sqlite         # item pool (gitignored in production, present locally)
├── db_user.sqlite         # per-user response log (gitignored)
└── db_credentials.sqlite  # hashed passwords (gitignored)
```

---

## Architecture

### Module flow

The app has five Shiny modules wired together in `server.R`:

```mermaid
%%{init: {'theme': 'base', 'themeVariables': {'primaryColor': '#e8f0f7', 'primaryBorderColor': '#285f8a', 'primaryTextColor': '#1a1a1a', 'lineColor': '#285f8a', 'edgeLabelBackground': '#ffffff', 'clusterBkg': '#f8f9fa', 'clusterBorder': '#dee2e6'}}}%%
flowchart LR
    auth([Login]) --> home
    reg[mod_register\nself-registration] -.->|session reload| auth

    subgraph server.R
        home[mod_home\nlanding]
        sel[mod_selector\nfilter / pick]
        prac[mod_practice\ndisplay + check]
        insp[mod_inspect\nread-only overview]
        dash[mod_dashboard\nprogress]
    end

    home -->|go_train| sel
    sel  -->|practice_ids| prac
    sel  -->|inspect_id| insp
    prac -->|write_trigger| dash
    prac -->|practice_ids = NULL| sel
    insp -->|inspect_id = NULL| sel
```

- **`go_train`** is a plain callback function passed to `mod_home`. When the "Jetzt üben" button is clicked it calls `bslib::nav_select()` using the app-level session (captured in `server.R` via closure) to navigate to the train panel.
- **`practice_ids`** (`reactiveVal(NULL)`) drives the selector ↔ practice toggle. `NULL` means show the selector; an integer vector of item IDs means show the practice module. It lives in `server.R` and is passed by reference to both `mod_selector` and `mod_practice`.
- **`inspect_id`** (`reactiveVal(NULL)`) is set when a student looks up a single item by ID in the selector (see `R/mod_selector.R` below). It routes to `mod_inspect` — a read-only overview — instead of the practice queue, and takes priority over `practice_ids` in the train-view switch. It never triggers a DB write.
- **`write_trigger`** (`reactiveVal(0L)`) is incremented by `mod_practice` after every confirmed DB write. `mod_dashboard` uses it to invalidate its user-data cache without being directly coupled to the practice module. `mod_inspect` never touches it, since it never writes.

### Authentication pattern

All modules are registered **inside** an `observeEvent(credentials()$user_auth, {...})` block in `server.R`. This means modules only activate after a successful login. Navbar tabs for protected panels are hidden via CSS on startup and revealed with `shinyjs::show()` post-login.

`mod_register_server` is the one exception — it's registered **before** the auth gate (self-registration has to work for users who don't have credentials yet). Its UI (`mod_register_ui`) sits in the login panel alongside the login form; `input$show_register`/`input$show_login` toggle between them via `shinyjs::runjs()` in `server.R` (`toggle_login_register()`), not `bslib::nav_select()` — this is a same-panel show/hide, not a tab switch. See `R/mod_register.R` below.

### Train panel view switching

The train panel renders the selector, practice, or inspect UI dynamically. `inspect_id` takes priority — if a student has looked up an item by ID, that overview shows even if a practice queue also happens to be set:

```r
output$train_view <- renderUI({
  if (!is.null(inspect_id()))  mod_inspect_ui("inspect_1")
  else if (is.null(practice_ids())) mod_selector_ui("selector_1", data_item)
  else                               mod_practice_ui("practice_1")
})
```

All three module **servers** are registered once at login time and remain active; only the UI toggles.

### Answer-mode dispatch inside `mod_practice.R`

Items are answered one of two ways, controlled by `db_item.sqlite`'s `answer_mode` column (`"mc"` or `"num"`): a multiple-choice radio group, or a typed numeric value matched against distractor values with a tolerance. `mod_practice.R` doesn't render either directly — it composes two child modules, **`mod_mc_answer.R`** and **`mod_numeric_answer.R`**, following the same "register once, toggle visibility" pattern as the top-level modules above (see `R/mod_practice.R` below for why visibility, not remounting, and why the very first item of a session needed special handling to avoid a race).

---

## Key files in detail

### `R/constants.R`

Single source of truth for:

| Constant | Type | Purpose |
|---|---|---|
| `LEARNING_AREA_LEVELS` | `character` vector | Canonical ordering of the 7 topic areas; used as factor levels throughout |
| `LEARNING_AREA_LABELS` | named `character` | Short display names → full DB values (e.g. `"Inferenz" = "Grundlagen der Inferenzstatistik"`) |
| `ITEM_TYPE_LABELS` | named `character` | `"Inhaltlich" = "content"`, `"R-Code" = "coding"` — these must match `type_item` values in `db_item.sqlite` |
| `PRIMARY_COLOR` | `character` | `"#285f8a"` (Goethe blue) — used in theme and CSS variables |
| `ANSWER_COLORS` | named list | Hex colors for correct/incorrect/skip states |
| `DB_ITEMS()` / `DB_USERS()` / `DB_CREDS()` | functions | Return full paths to the three SQLite files; read `TIGER_DB_DIR` env var (default `"."`) |
| `CONTACT_EMAIL` | `character` | Shown in error messages (e.g. registration unavailable) and on the home panel |
| `REG_USERNAME_PATTERN` / `REG_PW_MIN_LENGTH` / `REG_MAX_ATTEMPTS` / `REG_ATTEMPT_DELAY_S` | validation constants | Used by `mod_register.R` — see that section below |
| `NUM_MATCH_REL_TOL` / `NUM_MATCH_ABS_FLOOR` | `numeric` | Numeric-item answer matching: a typed value matches a distractor if within `max(distractor * NUM_MATCH_REL_TOL, NUM_MATCH_ABS_FLOOR)` of it. Global, not per-item — see `evaluate_numeric_answer()` in `R/utils.R` |

> **Important:** `ITEM_TYPE_LABELS` values (`"content"`, `"coding"`) must exactly match the `type_item` column in `db_item.sqlite`. If item types change in the DB, update this constant.

> **Registration requires `TIGER_REG_CODE`** (a separate env var from the constants above, read directly via `Sys.getenv()` in `mod_register.R`, not defined in `constants.R`) — the shared semester code students must enter to self-register. Unset in local dev by default, so registration will show "derzeit nicht verfügbar" unless you export it.

### `R/db.R`

All database access. Uses a simple open/close pattern (`db_with()`) — no connection pooling.

| Function | Description |
|---|---|
| `db_with(path, fn, wal=FALSE)` | Opens a DBI connection, runs `fn(con)`, closes on exit. Set `wal=TRUE` for write operations |
| `db_get_items()` | Reads the full item pool from `db_item.sqlite`. Rewrites image paths: `"www/foo.png"` → `"img_item/foo.png"` to match Shiny's resource paths |
| `db_get_userdata(user_id)` | Returns all response rows for a user, or an empty `data.frame` if none |
| `db_user_exists(user_id)` | Returns `TRUE` if the user has any recorded responses |
| `db_write_response(user_id, df)` | Appends one response row; creates the user table if it doesn't exist yet. If the table already exists but is missing columns present in `df` (e.g. `typed_value` on a table created before numeric items existed), it `ALTER TABLE ... ADD COLUMN`s them first — per-user tables are created lazily from whatever `build_response_row()` produced on that user's *first-ever* write, so old tables predate newer columns and this keeps a schema change from needing a one-off migration of every existing table |
| `db_get_credentials()` | Returns the credentials table for `shinyauthr` |
| `db_username_exists(username)` | `TRUE`/`FALSE`; case-sensitive. Used by `mod_register.R` |
| `db_register_user(username, password_plain)` | Inserts a new row into `credentials_db` inside a `BEGIN IMMEDIATE` transaction (dupe-check + insert are atomic); hashes with `sodium::password_store()`; `stop("username_taken")` if the username exists |

**DB path resolution:** All functions default to `DB_ITEMS()` / `DB_USERS()` / `DB_CREDS()` which read `TIGER_DB_DIR` from the environment. Locally this defaults to `"."` (project root). In Docker it is set to `/opt/shinyapp` (the mounted volume).

### `R/irt.R`

Two-parameter logistic (2PL) IRT model for estimating student ability.

| Function | Description |
|---|---|
| `prob_2pl(theta, a, b)` | Item response probability: `1 / (1 + exp(-a*(theta-b)))` |
| `estimate_theta(responses, a, b)` | MLE via L-BFGS-B (`optim`), bounded to `[-3, 3]`. Returns `NA` on error |
| `estimate_competency(responses, items)` | Runs `estimate_theta` per learning area using **first attempts only**. Returns a `data.frame` with columns `learning_area`, `theta`, `n_items` |

Parameters `a` (discrimination) and `b` (difficulty) come from `irt_discr` and `irt_diff` columns in `db_item.sqlite`. Items with missing IRT parameters are silently excluded from estimation.

### `R/utils.R`

#### Math/LaTeX rendering

Items and feedback can contain LaTeX math delimited by `$...$` (inline) or `$$...$$` (display). The rendering pipeline:

1. **`protect_math_delimiters(text)`** — pre-processes text before passing to `markdownToHTML`:
   - Protects inline code spans (`` `...` ``) from math detection
   - Escapes `*` and `_` inside math regions (prevents `<em>` injection by commonmark)
   - Converts `$$...$$` → `\\[...\\]` and `$...$` → `\\(...\\)` (double backslashes because commonmark strips one layer during the markdown pass)
2. **`render_md(text)`** — calls `protect_math_delimiters` then `markdown::markdownToHTML(fragment.only=TRUE)`
3. MathJax is loaded via `shiny::withMathJax()` in the practice module UI, with a manual re-typeset trigger `MathJax.Hub.Queue(['Typeset', MathJax.Hub])` injected as an inline `<script>` inside each `renderUI` output.

#### Other helpers

| Function | Description |
|---|---|
| `get_answeroptions(item)` | Extracts non-NA values from `answeroption_01`…`answeroption_06` |
| `get_feedbackoptions(item)` | Extracts non-NA values from `if_answeroption_01`…`if_answeroption_06` |
| `evaluate_answer(item, answer_idx)` | MC only. Returns `"correct"`, `"incorrect"`, or `"skip"` (last option is always the skip option) |
| `parse_numeric_input(x)` | Numeric items only. Parses a typed string to a number, accepting both `.` and `,` as the decimal separator (`gsub(",", ".", ...)` before `as.numeric()`); returns `NA_real_` for empty/unparseable input |
| `evaluate_numeric_answer(item, typed_value, rel_tol, abs_floor)` | Numeric items only. Matches `typed_value` against the item's distractor values (reused `answeroption_0X` columns, parsed as numbers — no trailing skip slot). Ties within tolerance resolve to the *closest* distractor. Returns `list(matched_idx, result)` where `result` is `"correct"` / `"incorrect"` / `"unmatched"` (never `"skip"` — that's the caller's dedicated skip action, not a matching outcome) |
| `build_response_row(item, answer_idx, user_id, session_token, typed_value = NA_real_, skipped = NULL)` | Constructs the `data.frame` row written to `db_user.sqlite`. `answer_idx` is `NA` for an unmatched numeric answer or an explicit skip; `typed_value` is numeric-only (`NA` for MC rows); `skipped` defaults to the MC last-option convention (`answer_idx == n`) when `NULL`, but numeric callers pass it explicitly since there's no last-option slot to compare against. `bool_correct` is `NA` whenever `skipped` or `is.na(answer_idx)` — i.e. skip and unmatched are both excluded from IRT scoring the same way |
| `safe_sample(x, size)` | `sample()` that handles `length(x) < size` gracefully |
| `is_img_path(x)` | Returns `TRUE` for strings ending in `.png/.jpg/.jpeg/.svg/.gif` |
| `item_id_badge(id_item, input_id, class)` | Click-to-copy item-ID badge shared by `mod_practice.R` and `mod_inspect.R`, built on `rclipboard::rclipButton()` for the `bslib::tooltip()` hover label. `input_id` must be the caller's `ns("copy_item_id")` — a fixed, namespaced ID, since each module is instantiated once. **Never observed server-side, on purpose** — confirmed against `shiny.js`: `unbindInputs()` (run before every `renderUI` re-render) tears down the JS binding but never calls the client's `InputNoResendDecorator.forget()`, so a freshly recreated `actionButton` resends its reset value (`0`), which differs from the cached post-click value and gets treated as a genuine change. Since both badges live inside a `renderUI` that reruns on every item change, a server `observeEvent` on this input would fire "copied" on every item advance, not just on real clicks — confirmed by testing this exact scenario with a debug observer before settling on the client-only approach. The "✓ Kopiert" confirmation is instead a single, page-lifetime `ClipboardJS('.practice-item-id-btn').on('success', …)` listener registered once in `ui.R`'s header script (`DOMContentLoaded`), never per-render. |

### `R/mod_home.R`

Landing panel shown immediately after login. Displays a personalised greeting, a three-step "how it works" explanation, pool stats, and links to pandar dataset documentation, otter (R practice), and the contact email. The "Jetzt üben" button calls the `go_train` callback passed from `server.R`, which uses `bslib::nav_select()` with the **app-level session** (not the module session) to switch tabs. If you need to add cross-tab navigation from a module, always capture `session` in `server.R` and pass it via closure — `bslib::nav_select()` uses `getDefaultReactiveDomain()` which resolves to the module session inside a module server.

### `R/mod_register.R`

Self-registration form on the login panel, gated by a shared **semester code** (`Sys.getenv("TIGER_REG_CODE")` — must be set in the deployment environment; if unset, registration shows an error telling the student to contact `CONTACT_EMAIL` instead of silently failing). Validates, in order: all fields non-empty → username matches `REG_USERNAME_PATTERN` (`constants.R`, 3–30 alphanumeric/`._-`) → password ≥ `REG_PW_MIN_LENGTH` chars → passwords match → semester code matches. On success, `db_register_user()` inserts into `credentials_db` (rejecting duplicate usernames with a `"username_taken"` condition, which is deliberately **not** counted toward the attempt lockout below — a taken username isn't a guessing attempt) and reloads the session so `shinyauthr::loginServer()` picks up the new row.

**Brute-force mitigation**, entirely in-module `reactiveVal`s (`attempts`, `locked`), not persisted:
- A fixed `Sys.sleep(REG_ATTEMPT_DELAY_S)` on every submit attempt (rate limiting)
- After `REG_MAX_ATTEMPTS` failed semester-code guesses (or non-`username_taken` DB errors), the form locks until the page is reloaded

### `R/mod_selector.R`

Renders one **row per learning area**, each with a pill-style "chip" toggle per item type (`Inhaltlich` / `R-Code`) — built from plain `tags$input`/`tags$label` HTML (a hidden checkbox + a `<label>` styled via the adjacent-sibling `:checked` selector in `.sel-chip`/`.sel-chip-input`), not `checkboxInput()`, same rationale as the old matrix: full CSS control over layout. A header row above the list holds one "select all" chip per item type (`type_all_1`/`type_all_2`, ids `cell_i_j` where `i` = type index, `j` = area index) — clicking it drives every cell checkbox of that type via `updateCheckboxInput()`. There is no per-area or global "select all" (deliberately dropped — only 2 chips per area made it redundant).

The reactivity is flat: cell checkboxes → `selected_combos()` → `filtered_items()` → submit handler. There's no separate selection-summary text — the selection is already visible from which chips are highlighted, and the max-available count already appears in the "Alle verfügbaren (N)" preset label, so a redundant summary line was removed. No `reactiveValues` bool matrix; each cell is read directly via `input[[paste0("cell_", i, "_", j)]]`.

Each chip shows a live count (`output$count_i_j`) — total or new-only depending on the "Nur neue Aufgaben ziehen" switch (`input$only_new`) — and, when that switch is off, an orange `.sel-chip-badge` showing how many of that cell's items are unattempted (`output$badge_i_j`). A single `observe()` block re-evaluates every cell whenever `only_new` or the new-item counts change: cells with zero new items are `shinyjs::disable()`d and auto-unchecked via `updateCheckboxInput(..., value = FALSE)` — this is why `shinyjs::useShinyjs()` in `ui.R`'s header matters here, not just for `mod_practice.R`.

**Item count** is a stepper (`n_minus`/`n_plus` buttons) plus preset buttons (5/10/20/"Alle verfügbaren (N)") — deliberately not a slider or `numericInput`: with a pool that keeps growing, a linear slider's usable range (students realistically want 5–30 items) would shrink to a sliver of the track, and "Alle verfügbaren" needs to track the *current filtered selection*, not the pool's total size. The value lives in a plain `reactiveVal` (`n_items`), not a bound Shiny input — there's no widget whose `input$id` holds the number. A second `want_all` `reactiveVal` tracks whether the "Alle verfügbaren" preset is active; `effective_n()` resolves to `nrow(filtered_items())` when `want_all()` is `TRUE`, else `n_items()`. Any stepper/preset-5/10/20 click clears `want_all`. Tests that need a specific count call `n_items(<value>)` directly inside `testServer()`'s `expr` (which runs in the module's own environment) rather than `session$setInputs(n_items = ...)` — see `selector_cell_inputs()` in `helper-modules.R`.

Below the area list, a second `bslib::card` (same `card_header` + `card_body` pattern as the main selector card, headed "Spezifische Aufgabe auswählen") holds a **direct item lookup by ID** — for students who want to jump straight to one known item (e.g. to show a lecturer), rather than a randomized selection. The ID input and its button are wired via Bootstrap's `.input-group` (not custom flex/height CSS) so they auto-align without fighting the box model by hand. Submitting a valid ID sets `inspect_id`, **not** `practice_ids` — this deliberately does not enter the practice queue (see `R/mod_inspect.R`). An unrecognized ID shows a warning notification and leaves state untouched.

### `R/mod_practice.R`

`mod_practice_ui(id, data_item, practice_ids)` — unlike other module UI functions, this one takes `data_item`/`practice_ids`, not just `id`. It needs them to bake the correct initial answer-mode visibility directly into the HTML for the *first* item of a queue (see the race-condition note below) — `mod_selector_ui(id, data_item)` already sets the precedent for a module UI function taking more than an id.

Two separate `renderUI` outputs, to avoid unnecessary re-renders:

- **`output$item_stimulus`** — only invalidates when `current_item()` changes (i.e., when moving to a new item). Never re-renders on check.
- **`output$progress_bar`** — also shows the current item's `id_item` next to "Aufgabe X von Y", so a student stuck on a question can report its ID (e.g. to a lecturer) without needing the separate lookup flow in `mod_selector`. Built with `item_id_badge()` — same component and behaviour as `mod_inspect.R`'s.

**Answer rendering is delegated**, not inline. `mod_practice_server` registers both `mod_mc_answer_server` and `mod_numeric_answer_server` as children — always both, regardless of the current item's mode, same "register once" pattern as the top-level modules — and the UI mounts both `mod_mc_answer_ui("answer_mc")`/`mod_numeric_answer_ui("answer_num")` inside `div(id=ns("mc_wrap"), ...)`/`div(id=ns("num_wrap"), ...)` wrappers. Only one is ever visible; `mod_practice_server` toggles that via `shinyjs::show()`/`hide()` on `mc_wrap`/`num_wrap` in an `observe()` keyed on the current item's `answer_mode`. Neither wrap is remounted per item — see `R/mod_numeric_answer.R` below for why that matters specifically for the numeric module's skip button.

> **Race condition, already hit and fixed once — don't reintroduce it.** `output$train_view`'s `renderUI` in `server.R` (which inserts `mod_practice_ui(...)` into the DOM) and the `observe()` above (which sends the `mc_wrap`/`num_wrap` show/hide messages) are two *separate* reactive contexts that both react to `practice_ids()`, with no dependency edge between them — so their relative evaluation order within the same flush is unspecified. If the show/hide messages for the first item of a new practice session are processed by the browser before that item's HTML has actually been inserted, `shinyjs` silently no-ops (the target element doesn't exist yet). This only ever affects the *first* item of a session (later "Weiter" transitions are safe — the DOM already exists by then), and it's exactly why `mod_practice_ui()` computes the first item's `answer_mode` itself and pre-hides the inactive wrap with `shinyjs::hidden()` directly in the initial HTML, rather than relying on that observer for the first render.

**Child-module contract** (`mod_mc_answer_server`/`mod_numeric_answer_server`, see their own files): each takes `item` (reactive single-row item `data.frame`), `checked` (reactive logical), `result` (reactive list) from the parent, and returns `list(raw_answer, ready, skip_requested)`. The children are pure UI/input components — they render and report the raw input; `mod_practice.R` owns evaluation (`evaluate_answer()`/`evaluate_numeric_answer()`) and the DB write, so scoring logic lives in one place, not duplicated per answer mode.

**Answer coloring** is done via CSS classes:

- `.radio-result-correct`, `.radio-result-incorrect`, `.radio-result-skip` — defined in `app.css`, MC only
- MC's last answer option is always the skip option (equal to `length(get_answeroptions(item))`); numeric items have their own dedicated "Aufgabe überspringen" button instead (see `R/mod_numeric_answer.R`), since there's no last-option slot to select

**State** is managed by a `reactiveValues` object inside `mod_practice_server`:
```r
state <- reactiveValues(pos = 1L, checked = FALSE, result = NULL)
```
`pos` is the index into `practice_ids()`. `result` (once `checked` is `TRUE`) is `list(category, answer_idx, typed_value)` — `category` is `"correct"`/`"incorrect"`/`"skip"`/`"unmatched"` (the last only for numeric items); this is what the answer child modules read to render their post-check view. Moving to the next item increments `pos`; finishing all items sets `practice_ids(NULL)` to return to the selector.

### `R/mod_mc_answer.R` / `R/mod_numeric_answer.R`

Child modules of `mod_practice.R` (see above for the contract and dispatch pattern). `mod_mc_answer.R` is a straightforward port of the old inline MC rendering — its own `renderUI` remounts on both item change and check, which is fine since it has no `actionButton` of its own.

`mod_numeric_answer.R` is different in three ways:

- The input is a plain **text** field (not `type="number"`), so both `"3.5"` and `"3,5"` are typeable; `parse_numeric_input()` normalizes the locale.
- It has a dedicated **skip button** (`skip_requested` in the return contract), since there's no last-radio-option slot.
- **Its input field and skip button are static UI**, never remounted per item (unlike `mod_mc_answer`'s radios) — only its post-check feedback panel (`output$feedback_ui`) re-renders. A freshly recreated `actionButton` resends its client-side reset value (`0`) on the next real render, which Shiny then treats as a genuine click if the server's last remembered value was nonzero — the same quirk documented on `item_id_badge()` in `utils.R`. Keeping the skip button's DOM node stable across item changes (clearing the text field and toggling visibility instead of remounting) avoids that entirely.

The **UI itself** is a purpose-built `.numeric-answer-card` (see CSS table below) — deliberately not styled to reuse MC's `.answer-option` list-row classes, since a single numeric field isn't a list of choices and looked like a bolted-on afterthought when it borrowed that styling.

### `R/mod_inspect.R`

Read-only "item overview" shown when a student looks up an item by ID in `mod_selector` (`inspect_id` reactive, set in `server.R`). Conceptually distinct from practice: the student isn't attempting the item, so **`db_write_response()` is never called** here — nothing in this module touches `db_user.sqlite`, keeping the response log and IRT competency estimate uncontaminated by lookups.

The stimulus and all answer options render immediately, but the correct answer and all per-option feedback texts stay hidden behind a `revealed` reactive (`FALSE` until the "Antwort & Feedback anzeigen" button is clicked, reset whenever `inspect_id()` changes) — a deliberate one-click gate so a mistyped/curious ID lookup doesn't instantly spoil the answer. The "Zurück zur Auswahl" button sets `inspect_id(NULL)`.

`output$item_answers` branches on `item$answer_mode`:

- **MC** (unchanged): every option shown; once revealed, the correct option is highlighted using the same `correct_answer_txt`/`correct_answer_img` classes `mod_practice.R` uses (suffix chosen from `item$type_answer`, same as practice — don't hardcode `_txt`), and *every* option with feedback text gets its own `.feedback-card`.
- **Numeric**: before reveal, a placeholder message ("Numerische Aufgabe — Antwort ausgeblendet") is shown instead of any option list. After reveal, only the *correct* value and its own feedback are shown — the other distractor values/feedback stay hidden even after reveal, deliberately unlike MC. Distractor feedback for numeric items is meant to be discovered by actually typing that value in practice mode, not read off the inspect view.

### `R/mod_dashboard.R`

> **Status: mockup.** The 2PL IRT competency estimate and its thresholds/labels below are a placeholder to demonstrate the dashboard concept, not an empirically validated model. Replacing it with an AI-assisted, empirically derived competency dashboard is the scope of the follow-up **"kiwi"** project — don't treat the current θ cutoffs or recommendation logic as settled design worth preserving during that work.

Reactive dependency chain:
```
write_trigger → user_data → first_attempts → competency
```
`first_attempts` de-duplicates by `(user_id, item_id)` — only the first attempt per item feeds the IRT model. All descriptive stats (accuracy %, days practiced, etc.) use `user_data` (all attempts).

**Competency labels** (mapped from θ):

| θ range | Label | Color scheme |
|---|---|---|
| θ > 1.0 | Stark | Blue-teal |
| 0 < θ ≤ 1.0 | Gut entwickelt | Green |
| -0.5 < θ ≤ 0 | Entwickelt sich | Yellow |
| θ ≤ -0.5 | Übungsbedarf | Red/Pink |
| No data | Keine Daten | Gray |

**Evidence strength** (from unique item count per area):

| Unique items | Label | Dots |
|---|---|---|
| ≥ 8 | Hoch | ●●● |
| 3–7 | Mittel | ●●○ |
| 1–2 | Niedrig | ●○○ |
| 0 | — | ○○○ |

**Recommendations** are computed by `recommend_next()`: areas with no data rank first (priority 10), then low-evidence areas (priority 20), then low-θ areas (priority 30+, scaled by `-θ`). The top 3 are shown.

---

## Databases

Three SQLite files, location controlled by the `TIGER_DB_DIR` environment variable:

### `db_item.sqlite` — item pool (read-only at runtime)

Table: `item_db`

| Column | Type | Description |
|---|---|---|
| `id_item` | integer | Primary key |
| `learning_area` | text | Must be one of `LEARNING_AREA_LEVELS` |
| `type_item` | text | `"content"` or `"coding"` |
| `bloom_taxonomy` | text | `"knowledge"`, `"comprehension"`, or `"application"` |
| `stimulus_text` | text | Markdown + LaTeX preamble (nullable) |
| `stimulus_image` | text | Path like `www/img_item/foo.png` (stripped to `img_item/foo.png` at load) |
| `answeroption_01`…`answeroption_06` | text | Answer choices. For MC (`answer_mode = "mc"`) rows, last non-NA is always "Überspringen". For numeric (`answer_mode = "num"`) rows, these are distractor *values* as numeric strings (e.g. `"4.5"`) — reused rather than adding a parallel set of columns, and with no trailing skip slot (numeric items skip via a dedicated button, not a last option) |
| `if_answeroption_01`…`if_answeroption_06` | text | Per-answer feedback text (Markdown + LaTeX); same reuse for numeric distractor feedback |
| `answer_correct` | integer | 1-based index of the correct answer/distractor value |
| `type_answer` | text | `"text"` or `"image"` — MC only, not read for numeric rows |
| `answer_mode` | text | `"mc"` or `"num"` — controls whether `mod_practice.R`/`mod_inspect.R` dispatch to the MC or numeric rendering path. **Not** the same as `type_item` (`"content"`/`"coding"`), which is an orthogonal axis — the two are easy to confuse by name |
| `irt_discr` | real | IRT discrimination parameter *a* |
| `irt_diff` | real | IRT difficulty parameter *b* |

### `db_user.sqlite` — response log (read-write at runtime)

One table per user, named by `id_user`. Each row is one response:

| Column | Type | Description |
|---|---|---|
| `id_user` | text | Username |
| `id_session` | text | Shiny session token |
| `id_date` | integer | `as.integer(Sys.Date())` |
| `id_datetime` | integer | `as.integer(Sys.time())` — used for ordering |
| `id_item` | integer | Foreign key to `db_item.sqlite` |
| `learning_area` | text | Denormalized from item (for fast dashboard queries) |
| `selected_option` | integer | 1-based index of chosen answer/matched distractor. `NA` for a skipped or unmatched-numeric response |
| `answer_correct` | integer | Correct answer index (denormalized) |
| `bool_correct` | logical | `TRUE`/`FALSE`/`NA` (`NA` = skipped **or** an unmatched numeric answer — both excluded from IRT scoring the same way) |
| `skipped` | logical | `TRUE` if last option was selected (MC) or the skip button was clicked (numeric) |
| `typed_value` | real | Numeric items only; `NA` for MC rows. The raw number the student typed, preserved even when it didn't match any distractor — otherwise that signal would be unrecoverable once discarded, and it's useful input for the kiwi project's item-generation/feedback work later. Added via `db_write_response()`'s auto-migrate-on-write (see `R/db.R` above), so pre-existing per-user tables don't need a manual migration |

### `db_credentials.sqlite` — authentication

Table: `credentials_db` — columns `user_name` and `password_hashed` (sodium-hashed). Managed outside the app; passed to `shinyauthr::loginServer()`.

---

## UI conventions

### CSS (`inst/app/www/css/app.css`)

Key CSS variables (defined on `:root`):
- `--tiger-primary: #285f8a`
- `--tiger-correct: #00618f`
- `--tiger-incorrect: #D81B60`
- `--tiger-skip: #FFA000`
- `--anim-dur: 0.35s`

Key CSS classes to be aware of when changing layout:

| Class | Purpose |
|---|---|
| `.main-content` | Centers practice/selector/home content, `max-width: 1400px` |
| `.dashboard-wrap` | Full-width dashboard container, `padding: 0 2rem 2rem` |
| `.login-page` / `.login-card` / `.login-card-header` / `.login-card-body` | Login page layout |
| `.reg-error` / `.reg-success` | Registration feedback message styling (`mod_register.R`'s `reg_msg()`) |
| `.home-steps` / `.home-step` / `.home-step-num` | Home panel numbered steps layout |
| `.sel-area-row` / `.sel-area-label` / `.sel-chip-group` | Selector's per-learning-area row layout |
| `.sel-chip-wrap` / `.sel-chip-input` / `.sel-chip` / `.sel-chip-header` | Pill-toggle chip: hidden checkbox + styled label, sibling `:checked` selector drives active state |
| `.sel-chip-count` / `.sel-chip-badge` | In-chip item count / floating "N neu" badge |
| `.sel-stepper-btn` / `.sel-count-value` / `.sel-preset-btn` | Item-count stepper (−/+) and preset pills (5/10/20/Alle verfügbaren) |
| `.sel-footer` | Selector's bottom row: selection summary text + submit button |
| `.sel-direct-row` | Direct-ID-lookup card's input row below the selector; a Bootstrap `.input-group` |
| `.practice-item-id` | Item ID badge — practice progress bar and inspect header |
| `.practice-item-id-btn` / `.is-copied` | Clickable variant built by `item_id_badge()`; `.is-copied` is the transient post-copy state applied by the client-only success listener in `ui.R` |
| `.practice-answers-section` | Gray tinted answers area below stimulus in practice card |
| `.answer-option` | Per-answer radio row; hover suppressed post-check via `:has(input:disabled)` |
| `.answer-option.is-static` | Non-interactive variant used by `mod_inspect.R` — kills hover affordance only, must **not** set `background-color` in the base state or it silently overrides `.correct_answer_txt`/`.incorrect_answer_txt` (equal-or-higher specificity beats source order) |
| `.radio-result-correct/incorrect/skip` | Post-check radio fill color (requires `!important`) |
| `.numeric-answer-card` / `.numeric-answer-label` / `.numeric-answer-input` / `.numeric-answer-hint` | Numeric item's pre-check answer widget — a bordered "answer card" purpose-built for a single field, not borrowed MC list-row styling |
| `.numeric-answer-skip` | De-emphasized (underlined text, not a bordered button) skip action, `.btn` chrome stripped with `!important` overrides |
| `.numeric-answer-value` | The "Deine Eingabe: X" sub-heading inline in the post-check `.feedback-card` header, not a separate box |
| `.feedback-card` | Per-answer feedback block with colored left border and shadow |
| `.practice-stat` / `.practice-stat-val` / `.practice-stat-lbl` | Dashboard practice behaviour grid cells |
| `.dashboard-comp-table` | Competency map table in dashboard |
| `.rec-num` | Circular number badge in recommendations card |

### Resource paths

Static files are served via Shiny's **built-in `www/` auto-serving convention** — `inst/app/www/<subdir>/*` is automatically available at `/<subdir>/*` for any app directory Shiny runs (`shiny::runApp("inst/app")` in dev, or the installed package dir via `run_app()`), with zero configuration:

| Prefix | Filesystem location | Content |
|---|---|---|
| `img_app` | `inst/app/www/img_app/` | App logos and favicon |
| `img_item` | `inst/app/www/img_item/` | Item stimulus and answer images |
| `css` | `inst/app/www/css/` | `app.css` |

`inst/app/app.R` previously called `shiny::addResourcePath()` with `system.file(...)` to register these explicitly — this was removed because it was both redundant with the built-in convention *and* fragile: `system.file()` only resolves correctly if the package is genuinely installed or `pkgload` is present (which isn't in `rv.lock`/`DESCRIPTION`), so on some setups it silently returned `""` and **overrode** the working default with a broken one, causing missing CSS/images for contributors without `pkgload` in a personal library. **Do not re-add `addResourcePath()` for these three prefixes** — the auto-serving convention already covers them and doesn't depend on how the package was loaded.

---

## Deployment

### Build process

Run once per release from the project root:

```r
Rscript deploy/build.R
```

This script:
1. Parses all package names from `rv.lock`
2. Queries the Posit PPM sysreqs API (`packagemanager.posit.co/__api__/repos/1/sysreqs`) for Ubuntu 22.04 apt packages needed by those packages
3. Generates `deploy/Dockerfile` via `{dockerfiler}` with the correct system dependencies baked in
4. Runs `devtools::document()` to regenerate `NAMESPACE`
5. Builds the `shinytigeR_*.tar.gz` tarball into `deploy/`
6. Copies `rv.lock` and `rproject.toml` into `deploy/`

Then build the image:

```bash
docker build --platform linux/amd64 -t shinytiger deploy/
```

The `--platform linux/amd64` flag is required when building on Apple Silicon; without it Docker selects `arm64` and the `x86_64` rv binary fails under Rosetta.

### Dockerfile structure (single-stage)

Based on `rocker/r-ver:4.6`. Deliberately single-stage — installs R packages as pre-compiled binaries from PPM, which avoids rv library-path complexity across stages and is faster than compiling from source, so there's no separate builder/runtime split to keep dev headers out of the final image:

- Installs apt build deps (`curl`, plus whatever the sysreqs API returned for the resolved packages — e.g. `libsodium-dev`)
- Downloads the latest `rv` binary from GitHub releases (x86_64 Linux) and runs `rv sync --locked` in `/srv`, then appends the resolved `rv` library path to `R_LIBS_USER` in `Renviron` so R can find the packages
- Installs the `shinytigeR` tarball with `R CMD INSTALL`
- Creates user `beitner` (uid 1002, gid 1003) for ShinyProxy
- Sets `TIGER_DB_DIR=/opt/shinyapp` — the SQLite files must be mounted at this path

### ShinyProxy

The container exposes port 3838. ShinyProxy should mount the three SQLite database files into `/opt/shinyapp/`:
- `/opt/shinyapp/db_item.sqlite`
- `/opt/shinyapp/db_user.sqlite`
- `/opt/shinyapp/db_credentials.sqlite`

ShinyProxy's app config must also set **`TIGER_REG_CODE`** (the semester code — see `R/mod_register.R` above) as a container env var. It is **not** baked into `deploy/Dockerfile` (that would ship a secret in the image); without it set at runtime, self-registration is disabled with an error pointing students to `CONTACT_EMAIL`.

> **Required one-time migration before deploying any build that includes numeric items:** the production `db_item.sqlite` needs the `answer_mode` column added and backfilled to `'mc'` before this code runs against it — run `TIGER_DB_DIR=<mount path> rv run dev/migrate_answer_mode.R` against it first (idempotent, safe to re-run, inserts no items). **Skipping this doesn't crash the app** — `item$answer_mode` on a column that doesn't exist yet resolves to `NULL`, not an error, and both `mod_mc_answer.R`'s and `mod_numeric_answer.R`'s dispatch checks (`identical(item$answer_mode[1], "mc"/"num")`) are `FALSE` for `NULL` either way. The practice view's answer area just renders blank and "Antwort prüfen" never enables — for *every* item, MC included, with no error shown anywhere. Do **not** use `dev/add_numeric_samples.R` for this — it also inserts fake sample items, which don't belong in the real student-facing pool.

The `CMD` starts the app as:
```
R -e "options(shiny.port=3838, shiny.host='0.0.0.0'); library(shinytigeR); shinytigeR::run_app()"
```

---

## Adding a new dependency

1. `rv add <package>` — updates `rproject.toml` and `rv.lock`
2. Add the package to `Imports:` in `DESCRIPTION`
3. Add `@importFrom pkg fn` to `R/shinytigeR-package.R` (or use `pkg::fn()` in code)
4. Run `devtools::document()` to update `NAMESPACE`
5. Run `Rscript deploy/build.R` — the sysreqs API will automatically detect any new system dependencies

## Adding a new item type or learning area

- **New learning area:** Add the full name to `LEARNING_AREA_LEVELS` and a short label to `LEARNING_AREA_LABELS` in `constants.R`. Order matters — it controls display order everywhere.
- **New item type:** Add to `ITEM_TYPE_LABELS` in `constants.R`. The value must exactly match the `type_item` value stored in `db_item.sqlite`.
- **New Bloom level:** Add handling in `mod_dashboard.R` in the `bloom_lvls` vector and the `lbl` switch statement.

## Adding a new module

1. Create `R/mod_<name>.R` with `mod_<name>_ui(id)` and `mod_<name>_server(id, ...)` following the existing module pattern
2. Add the UI call to `R/ui.R` (new `nav_panel`) or to the `renderUI` in `server.R`
3. Register the server inside the `observeEvent(credentials()$user_auth, {...})` block in `server.R`
4. If the module needs to know when the DB was written, accept `write_trigger` as a parameter
