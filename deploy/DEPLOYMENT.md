# Deploying shinytigeR

How the app gets onto the server, what changed compared with the old version,
how to test it locally first, and what's still open. Hand-maintained (unlike
`README.md` and `Dockerfile` next to it, which `build.R` regenerates).

## Who does what

The app runs on the university's ShinyProxy server. We only have **SFTP
access** (no shell); the **admin builds the Docker image** on the server, and
ShinyProxy starts one container per student session.

| Step | Who |
|---|---|
| Build the upload folder (`rv run deploy/build.R`) | us, locally |
| Prepare and check the DB folder | us, locally |
| Upload both folders | us, SFTP |
| `docker build -t shinytiger .` on the server | admin |
| ShinyProxy config (`application.yml`) | admin, unchanged |

On the server (paths as seen via SFTP, home of the `beitner` account):

```
shinyapps/
├── shinytigeR/deploy-sd/   build folder: Dockerfile, tarball, rv.lock, ...
└── shinytigeR-DB/          DB folder, mounted into the container at /opt/shinyapp:
    ├── db_item.sqlite, db_user.sqlite, db_credentials.sqlite, db_ability.sqlite
    ├── reg_code.txt
    └── img_item/           the item images (since Nov 2026, before: in the package)
```

Locally the same layout: the repo in `app/shinytigeR/`, the DB folder next to
it in `app/db/` (`dev/run.R` sets `TIGER_DB_DIR=../db`, as the image sets
`/opt/shinyapp`). So a local run finds its files exactly as the server does,
and a backup downloaded from the server can be used directly
(`TIGER_DB_DIR=<backup> rv run dev/run.R`).

## What changed compared with the old version

| | Old (2023/24) | New |
|---|---|---|
| Images | two: `shinytiger:base` (packages) + app image on top | one Dockerfile |
| R | `Dockerfile_base` said 4.5.2, the server actually ran **4.3.0** (base never rebuilt) | 4.6.1 (`rocker/r-ver:4.6.1`, Ubuntu 24.04) |
| R packages | `renv::restore()`, then `remotes::install_local()` compiled ~14 newer packages from CRAN at build time, **unpinned** | `rv sync --locked`: exactly the versions in `rv.lock`, prebuilt binaries from Posit Package Manager, nothing compiled |
| Package caching | separate base image (went stale unnoticed) | Docker layer cache: the package layer is reused until `rv.lock` changes, so an app update rebuilds in seconds. **Requires building without `--no-cache`** |
| sass/bslib cache | workarounds: `chmod o+x /root`, `sass.cache=FALSE` in `CMD` | `R_USER_CACHE_DIR=/tmp/R-cache`, `HOME=/tmp` |
| User, volume, port | admin's block | **the admin's block, verbatim** (`chmod o+x /root; addgroup ...; adduser ... --no-create-home ...`, `VOLUME`, `WORKDIR /opt/shinyapp`, `EXPOSE 3838`, `USER beitner`) |
| Time zone | UTC | `TZ=Europe/Berlin` (calendar, "zuletzt geübt", retry time-out use the local day) |
| Fonts | – | Fira Sans/Mono bundled in the app, no request to Google |
| `CMD` | `options(port, host, fullstacktrace, sass.cache ...); getwd(); sass::sass_cache_get(); library(shinytigeR); run_app()` | `options(shiny.port = 3838, shiny.host = '0.0.0.0'); shinytigeR::run_app()`. Same port and host; the rest was debugging and cache workarounds |
| DB folder | `db_item`, `db_user`, `db_credentials` | the same plus `db_ability.sqlite`, `reg_code.txt` and `img_item/` |
| Item images | in the package (`inst/app/www/img_item`), so new items needed a new build | in the DB folder (`img_item/`), uploaded together with `db_item.sqlite`, no build |
| Semester code | – | env var `TIGER_REG_CODE`, else `reg_code.txt` in the DB folder |

Unchanged for ShinyProxy: image name `shinytiger`, port 3838, uid 1002 / gid
1003, DB folder mounted at `/opt/shinyapp`. So the admin's ShinyProxy config
shouldn't need any change.

## Will it work on the server?

**Tested locally** (Docker Desktop, `linux/amd64` emulated, with a copy of the
server's data in the DB folder):

- the image builds from `deploy/` alone (about 5 minutes from scratch, 15 s
  after an app change);
- the container starts as uid 1002 without a home directory, no warnings in
  the log;
- login with the real `db_credentials.sqlite` format, an old 2023/24 user
  table is read completely (all 372 old tables checked), the ability estimate
  is written to `db_ability.sqlite`, answers are written to `db_user.sqlite`;
- semester code read from `reg_code.txt`, fonts served by the app.

**Not testable locally**, so these are the risks on the server:

1. **Internet access during the build.** The build downloads from
   `docker.io` (base image), `archive.ubuntu.com` (system packages),
   `github.com` (the `rv` binary) and `packagemanager.posit.co` (R packages).
   The old build already reached Posit Package Manager and Docker Hub;
   **`github.com` is new.** If it's blocked, the build fails at the `rv` step
   (fallback: put the rv binary into the upload folder and `COPY` it).
2. **Write access to the DB folder.** On a Linux server, uid 1002 must be able
   to write the **folder itself**, not just the files: SQLite (WAL mode)
   creates `db_*.sqlite-wal` / `-shm` files next to the databases. Docker
   Desktop on a Mac ignores these permissions, so the local test doesn't
   cover this. If the files belong to the `beitner` SFTP account (uid 1002),
   it's fine.
3. **CPU architecture.** The image is built for `linux/amd64`; the old build
   log shows `x86_64`, so this matches.
4. **ShinyProxy config.** We don't have `application.yml`. As long as the
   admin's config still points to the image `shinytiger` and mounts
   `shinytigeR-DB` at `/opt/shinyapp`, nothing changes.
5. **The whole folder must be mounted, not single files,** or `img_item/`
   won't be visible in the container (the items then show no images). Very
   likely already the case: the old app wrote 33,787 answers, and SQLite's
   journal file next to `db_user.sqlite` needs a writable folder in the
   container, which a single-file mount wouldn't give. Still, confirm with the
   admin.

## Steps for a release

### 1. Build the upload folder (locally)

```bash
rv run deploy/build.R
```

Regenerates `deploy/Dockerfile`, `README.md` and the tarball
`shinytigeR_<version>.tar.gz`, and copies `rv.lock` / `rproject.toml`.
Run the test suite first.

### 2. Prepare the DB folder (locally)

First **download a backup** of the server's `shinytigeR-DB/` folder (with
any `-wal`/`-shm` files, if present) and keep it untouched.

Then, in a separate local folder:

- **`db_item.sqlite`**: the item pool that goes live, as exported by
  `tigertools` (already in the current format; the check below says so if
  it's an old one).
- **`db_ability.sqlite`**: an empty file (`touch db_ability.sqlite`).
- **`reg_code.txt`**: the semester code on the first line.
- **`img_item/`**: the item images, i.e. the local `app/db/img_item/` (every
  image `db_item.sqlite` refers to; the check below verifies that).
- `db_user.sqlite` and `db_credentials.sqlite` stay **the server's** (don't
  upload local copies over them). They need no migration: old user tables are
  read as they are, missing columns are added on the next write.

Check the folder (read-only):

```bash
rv run dev/check_deploy_dbs.R <folder>
```

It must end with "Ready to upload". It also lists items without `irt_diff`
(they don't count towards the ability estimate).

### 3. Upload (SFTP)

- Build folder: replace the contents of `shinyapps/shinytigeR/deploy-sd/` with
  `deploy/` (`Dockerfile`, `README.md`, `rv.lock`, `rproject.toml`,
  `shinytigeR_*.tar.gz`). Rename the old folder first (e.g. `deploy-sd.old`) to
  keep it.
- DB folder `shinyapps/shinytigeR-DB/`: add `db_item.sqlite` (replaces the old
  one), `db_ability.sqlite`, `reg_code.txt` and the folder `img_item/`.
  New items later: upload `db_item.sqlite` and their images in `img_item/`,
  no new build needed.

### 4. Admin builds

In `deploy-sd/`:

```bash
docker build -t shinytiger .
```

**Without `--no-cache`**, otherwise every build reinstalls all packages
(still works, just takes minutes instead of seconds).

### 5. Check on the server

- Open the app, log in (an existing account), open "Fortschritt".
- Answer one item, check that it shows up on the dashboard.
- Register a test account with the semester code.
- Via SFTP: `db_ability.sqlite` has grown, `db_user.sqlite-wal` may appear.

For later app updates only steps 1, 3 (build folder) and 4 are needed; the DB
folder stays as it is. New or changed items: upload `db_item.sqlite` and
their images in `img_item/`, no build needed.

## Testing locally

Needs Docker Desktop and about 4 GB of free disk. **Don't let the disk run
full during a build**: that left truncated (0-byte) files in Docker's image
layers once, which Docker then kept reusing. Fix:
`docker rmi -f rocker/r-ver:4.6.1 && docker builder prune -af`, then build
again.

```bash
rv run deploy/build.R
cd deploy && docker build --platform linux/amd64 -t shinytiger:test .
```

Prepare a test DB folder **with copies** (never the server backup itself), as
in step 2, then run it like ShinyProxy does:

```bash
docker run --rm --platform linux/amd64 -p 7333:3838 \
  -v <test folder>:/opt/shinyapp shinytiger:test
```

Wait for `Listening on http://0.0.0.0:3838` (slow under emulation on Apple
Silicon), then open <http://localhost:7333>. Stop with Ctrl+C. The log should
show no warnings apart from the two "replacing previous import" lines.

Useful checks inside the running container (`docker ps` for its name):

```bash
docker exec <name> sh -c 'id; echo $TZ $HOME; ls -la /opt/shinyapp'
```

## Questions for the admin

1. Which parts of the old Dockerfile are required? Is the `CMD` one of them
   (ours keeps port and host, drops the debugging/cache options)?
2. Can the build reach `github.com` (new: the `rv` download)?
3. Can uid 1002 write the DB folder itself (SQLite's `-wal`/`-shm` files)?
4. Optional: can `TIGER_REG_CODE` be set as `container-env` in ShinyProxy?
   (Otherwise `reg_code.txt` is used.)
5. For the planned AI feedback: can the **running** container reach the
   internet (the LiteLLM/LLM endpoint)? And where should the API key go
   (`container-env`, or a file in the DB folder via `read_secret()`)?

## Troubleshooting

| Symptom | Cause / fix |
|---|---|
| `object 'app_server' not found` at startup | `inst/app/app.R` must call `shinytigeR::app_ui()` / `shinytigeR::app_server` (the `CMD` doesn't attach the package) |
| `rv sync` fails on `cran.r-project.org/__linux__/...` | the CRAN entry in `rproject.toml` needs `force_source = true` (rv looks for Linux binaries in every repository; only PPM has them) |
| `Could not find an R version matching 4.6` | corrupted base image (see disk-full note above); remove it and rebuild |
| Practice view shows no answer options, "Antwort prüfen" never enables | `db_item.sqlite` in an old format (`answer_mode` missing): export the pool again with `tigertools`, check with `check_deploy_dbs.R` |
| Registration says "derzeit nicht verfügbar" | no semester code: `reg_code.txt` missing/empty and `TIGER_REG_CODE` unset |
| Old student sees an empty dashboard | shouldn't happen any more (`db_get_userdata()` handles old tables without `skipped`); check that the build includes that fix |
