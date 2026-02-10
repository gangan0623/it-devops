# Repository Guidelines

## Project Structure & Module Organization
This repository has three primary modules:
- `devops-admin/`: Spring Boot 3 backend (Java 17) with modules under `net.leoch.modules.*` and shared code in `net.leoch.common`.
- `devops-backup-agent/`: Python 3.12 FastAPI backup agent.
- `devops-ui/`: Vue 3 + Vite + TypeScript frontend.

Infrastructure and deployment assets live in `docker/`, `nginx/`, and `docker-compose.yaml` / `docker-compose.prod.yaml`.

## Build, Test, and Development Commands
Backend (`devops-admin/`):
- `mvn spring-boot:run`: run API on port `10001` with `/api` context.
- `mvn -DskipTests package`: build `target/devops-admin.jar`.
- `mvn test`: run tests (note `pom.xml` defaults to `skipTests=true`).

Frontend (`devops-ui/`):
- `npm install`: install dependencies.
- `npm run dev`: dev server on `10000`, proxying to `localhost:10001`.
- `npm run build`: production build to `dist/`.
- `npm run lint`: ESLint fixes on `src/**/*.{vue,ts}`.

Backup agent (`devops-backup-agent/`):
- `pip install -r requirements.txt`: install dependencies.
- `python -m backup_agent.main --config-file config.yaml ...`: run agent.
- `./build.sh`: build binary to `dist/backup-agent`.

## Coding Style & Naming Conventions
Backend (`devops-admin/`):
- Controllers: `*Controller`; Services: `I*Service` + `*ServiceImpl`; Mappers: `*Mapper` in `mapper/`.
- DTOs: request `*Req` in `vo.req`, response `*Rsp` in `vo.rsp`.
- Prefer `@RequiredArgsConstructor` with `private final` injection; avoid field `@Autowired` in new code.

Frontend (`devops-ui/`):
- `<script setup>` Composition API, Prettier with double quotes and semicolons.
- Reusable components use `ren-` prefix.
- Path alias `@/` maps to `src/`.

## Testing Guidelines
- `devops-admin/`: use `mvn test`; ensure new logic has coverage where feasible.
- `devops-ui/`: no test runner configured; rely on linting and manual verification.
- `devops-backup-agent/`: no repo-level test command documented; add unit tests alongside new modules if introduced.

## Commit & Pull Request Guidelines
Commits follow a Conventional Commit style: `type(scope): summary` (e.g., `refactor(common): move utils to support/`). Keep summaries imperative and scoped to a module.

PRs should include:
- A clear description of behavior changes and impacted modules.
- Linked issues or tasks if applicable.
- Screenshots for UI changes (`devops-ui/`).
- Config or schema changes called out (e.g., `devops-admin/src/main/resources/application*.yml`, `devops-admin/db/mysql.sql`).

## Configuration Tips
Common config entry points:
- Backend: `devops-admin/src/main/resources/application*.yml`.
- Frontend: `devops-ui/.env.development`, `devops-ui/.env.production`.
- Agent: `devops-backup-agent/config.yaml`.
- Docker data paths: `docker/mysql/`, `docker/redis/`, `docker/admin/logs/`, `docker/nginx/logs/`.
