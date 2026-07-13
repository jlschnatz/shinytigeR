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

### 5. Run the app

```bash
rv run dev/run.R
```

This loads the package via `pkgload::load_all()` and starts the app at `http://localhost:7331`.

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
