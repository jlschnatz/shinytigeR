# Local setup

There are two ways to run the `shinytigeR` app locally:

1. **Native way** (macOS/Linux friendly)
2. **Using Docker** (any OS, fully isolated)


## Option 1: Native (macOS/Linux)

### 1. Install R 4.6

The project pins `r_version = "4.6"` in `rproject.toml`. Use [`rig`](https://github.com/r-lib/rig) to install and manage R versions:

```bash
# macOS
brew install r-rig
rig add 4.6

# Linux
curl -Ls https://github.com/r-lib/rig/releases/latest/download/rig-linux-"$(arch)".tar.gz | sudo tar xz -C /usr/local
rig add 4.6
```

### 2. Install `rv`

Dependency manager for this project (config: `rproject.toml`, lockfile: `rv.lock`).

```bash
# macOS
brew install rv-r

# Linux
curl -LsSf https://github.com/A2-ai/rv/releases/latest/download/rv-installer.sh | sh
```

### 3. Install `libsodium`

Required by the `sodium` R package (password hashing for auth). 

```bash
# macOS
brew install libsodium

# Linux Debian/Ubuntu
sudo apt-get install libsodium-dev

```

### 4. Sync R packages

From the project root:

```bash
rv sync
```

This installs all packages pinned in `rv.lock` into the local `rv/` library. 

### 5. Create the databases

```bash
rv run dev/seed_db.R
```

See [Databases](#databases) below for what this creates and why the real ones
aren't in the repository.

### 6. Run the app

```bash
rv run dev/run.R
```

This loads the package via `pkgload::load_all()` and starts the app at `http://localhost:7331`.
Log in with **`test` / `test123`**.

## Option 2: Docker

No R, `rv`, or `libsodium` install needed on the host, only Docker Desktop (or Docker Engine + Compose on Linux).

From the project root:

```bash
docker compose -f docker/docker-compose.yml up
```

The app is at `http://localhost:7331`.

Notes:

- `docker/Dockerfile.dev` builds an image with R 4.6, `rv`, and `libsodium-dev` — it auto-detects the container architecture (x86_64 vs arm64) so it works on Apple Silicon without Rosetta.
- The project root is bind-mounted into the container, so code edits, the synced `rv/` package library, and the `db_*.sqlite` files persist on the host between runs — no rebuild needed after the first `rv sync`.

To stop:

```bash
docker compose -f docker/docker-compose.yml down
```

## Databases

The app reads four SQLite files from `TIGER_DB_DIR` (default: the project root).
**None of them are in the repository**, and all four are gitignored:

| File | Contains | Why it isn't committed |
| --- | --- | --- |
| `db_credentials.sqlite` | Usernames + hashed passwords | Real student accounts |
| `db_user.sqlite` | Every response ever recorded | Personal research data — and it changes on every local practice session, so tracking it would mean a dirty binary file after every run |
| `db_ability.sqlite` | Persisted per-user ability (θ) snapshots, derived from `db_user.sqlite` | Same reason as `db_user.sqlite` — derived, changes locally, personal data |
| `db_item.sqlite` | The item pool | The full pool is the assessment content of a graded module; this repository is public |

Run the seed script to create working local copies:

```bash
rv run dev/seed_db.R
```

It creates a credentials database with a single **`test` / `test123`** account,
an empty user database, an empty ability database, and — if no item pool is
present — copies the committed 14-item sample (`dev/db_item_sample.sqlite`) into
place. That sample covers all seven learning areas, both item types, and
includes image items, which is enough to develop against.

The script **refuses to overwrite existing databases** unless run with `--force`,
and it never overwrites an existing `db_item.sqlite` at all.

> **Note on the sample pool:** only 32 of the 116 items in the full pool carry IRT
> parameters, and three learning areas have none. The sample prefers
> parameterised items where they exist, but two of its areas have none — so the
> competency dashboard will legitimately show "Keine Daten" for those. That is
> the real behaviour, not a setup problem.

### Maintainers: regenerating the sample

With the full `db_item.sqlite` present:

```bash
rv run dev/make_sample_items.R
```

This rewrites `dev/db_item_sample.sqlite`. The selection is seeded, so re-running
without pool changes produces no diff.
