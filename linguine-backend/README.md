# Linguine Backend

### Prerequisites
Ensure you have the following installed:
- [Docker](https://docs.docker.com/get-docker/)
- Or [Podman](https://podman.io/docs/installation)
- [Nodemon](https://www.npmjs.com/package/nodemon) (Optional: used for live reloading)
- [The Haskell Toolchain](https://www.haskell.org/ghcup/install/)

### Setup
1. Clone the repository:
```sh
git clone https://github.com/p-febis/linguine.git
cd linguine
```

2. Copy the example environment file and configure as needed:
```sh
cp .env.example .env
```

3. Start the database using Docker Compose
```sh
docker compose up -d
```
Or
```sh
podman-compose up -d
```

And finally:
```sh
cabal run
```

### Development
The `dev_scripts/` folder contains scripts to manage the database and improve the development workflow:
- `up-migrate.sh`: Applies the latest database migration
- `down-migrate.sh`: Rolls back the latest database migration
- `liverun.sh`: Starts the project with live reloading using Nodemon

