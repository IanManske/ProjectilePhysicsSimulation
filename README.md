# Projectile Physics Simulation ![Github Pages Status](https://github.com/IanManske/ProjectilePhysicsSimulation/actions/workflows/main.yaml/badge.svg)

A simple web app that simulates a projectile being launched in 2D space.

View the live simulation at <https://ianmanske.github.io/ProjectilePhysicsSimulation/>.

Physics-related code can be found in `src/Physics.fs`.

# Running Local Server
## Requirements:
- [dotnet SDK >=6.0](https://dotnet.microsoft.com/download)
- [Node.js](https://nodejs.org/)
- [pnpm](https://pnpm.io/) (or npm)

## Installation
```
pnpm i
```

## Bulid & Run
```
pnpm build-fable
pnpm start
```

The local server will start at <http://localhost:1234>

# Debugging / Live Server
Run the installation step above, if you have not already.

If you're using vscode, simply go to the `Run and Debug` tab and run either `Debug with Chrome` or `Debug with Firefox`.

Otherwise, this can be done manually by running in one terminal:
```
pnpm watch
```
and in another terminal, run:
```
pnpm start
```
The local server at <http://localhost:1234> will automatically update/reload in response to code changes.
