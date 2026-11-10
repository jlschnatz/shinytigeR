# dev/

Scripts for working on the app locally. Run them from the project root with
`rv run dev/<script>.R`. They all use the DB folder `TIGER_DB_DIR`, by default
`../db` (next to the repo, laid out like the server's `shinytigeR-DB`).

| Script | When |
|---|---|
| `run.R` | Every time: starts the app at <http://localhost:7331> |
| `check_deploy_dbs.R [folder]` | Before every upload to the server: checks the DB folder (files, item DB format, no sample items, all images present). Read-only |
| `seed_db.R` | Once, on a fresh clone without the real databases: creates the DB folder with a test account and the sample pool. Never overwrites an existing pool |
| `add_mock_user.R` | To try the dashboard with data: adds a fake user who has answered every item. Local only |
| `add_showcase_user.R [username]` | Before a demo: adds a showcase account (default `tiger_demo`) with 5 weeks of simulated practice that shows every dashboard feature. Dates are relative to the run day, so run it shortly before the demo. Password in `SHOWCASE_ACCOUNT.txt` in the DB folder |

The sample pool (`db_item_sample.sqlite`, 18 items incl. 4 numeric ones, with
its images in `img_item_sample/`) is what `seed_db.R` copies over. It's a
fixed fixture (the repo is public, the real pool is exam content): if the item
format changes, edit it directly. The real pool comes from `tigertools`.
