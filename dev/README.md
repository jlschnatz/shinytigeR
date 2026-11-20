# dev/

Scripts for working on the app locally. Run them from the project root with
`rv run dev/<script>.R`. They all use the DB folder `TIGER_DB_DIR`, by default
`../db` (next to the repo, laid out like the server's `shinytigeR-DB`).

| Script | When |
|---|---|
| `run.R` | Every time: starts the app at <http://localhost:7331> |
| `check_deploy_dbs.R [--items-only] [folder]` | Before every upload to the server: checks the DB folder (files, item DB format, no sample items, all images present). Read-only. `--items-only` checks just the item pool (`db_item.sqlite`, `img_item/`), e.g. a release folder from `tigertools::build_release()` |
| `seed_db.R` | Once, on a fresh clone without the real databases: creates the DB folder with a test account and the sample pool. Never overwrites an existing pool |
| `add_mock_user.R` | To try the dashboard with data: adds a fake user who has answered every item. Local only |
| `add_showcase_user.R [username]` | Before a demo: adds a showcase account (default `tiger_demo`) with 5 weeks of simulated practice that shows every dashboard feature. Dates are relative to the run day, so run it shortly before the demo. Password in `SHOWCASE_ACCOUNT.txt` in the DB folder |
| `export_ai_lookup.R <lookup.xlsx> [folder]` | When the AI feedback's source list (Excel, sheet "Sources") changes: writes `ai_lookup.csv` to the DB folder, refuses if the list has problems. Needs `readxl` (personal library, not in rv), so run with plain `Rscript` |
| `try_ai_feedback.R [user] [--force]` | To try the AI feedback without the app: generates it for a local user (default `tiger_demo`) like the app does and prints the prompt, the feedback and the API calls (tokens, duration, outcome). `--force` ignores cooldown, "no newer estimate" and the daily limit. Saves to `db_feedback.sqlite` |

The sample pool (`db_item_sample.sqlite`, 18 items incl. 4 numeric ones, with
its images in `img_item_sample/`) is what `seed_db.R` copies over. It's a
fixed fixture (the repo is public, the real pool is exam content): if the item
format changes, edit it directly. The real pool is built with `make` in
`item_database/` (next to `app/`, see its README).

**AI feedback locally** needs three files in the DB folder, none of them in
the repo:

```
cp dev/ai_config.example.json ../db/ai_config.json
Rscript dev/export_ai_lookup.R <path>/lookup_table_ai_v3.xlsx
nano ../db/llm_key.txt        # paste the API key; not with echo (shell history)
chmod 600 ../db/llm_key.txt
```

Without them the feature is just off. Instead of `llm_key.txt`, the key can
be in the env var `TIGER_LLM_KEY`. Never put it in a `.Renviron` in the repo
folder: the repo is public. Use a different key locally than on the server
if possible.
