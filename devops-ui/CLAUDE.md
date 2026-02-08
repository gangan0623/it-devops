# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Dev Commands

```bash
npm install          # Install dependencies
npm run dev          # Start dev server (port 10000, API proxied to localhost:10001)
npm run build        # Production build (vite build --mode production)
npm run serve        # Build then preview
npm run lint         # ESLint fix on src/**/*.{vue,ts}
```

No test runner is configured in this project.

## Architecture

Vue 3 + TypeScript SPA for an IT DevOps management platform. Uses Vite, Element Plus (Chinese locale), and Pinia.

### Dynamic Routing

Routes are **server-driven**. On login, menus are fetched from `/sys/menu/nav` and registered via `router.addRoute()`. Base routes live in `src/router/base.ts`; dynamic loading logic in `src/router/index.ts`. View components auto-match by path: route `/sys/user` resolves to `src/views/sys/user.vue`.

### CRUD Composable (`src/hooks/useView.ts`)

Nearly all list pages use `useView()` providing pagination, sorting, `query()`, `deleteHandle()`, `exportHandle()`, and permission checking. Pages follow the convention: `[module].vue` for the list, `[module]-add-or-update.vue` for the form dialog.

### Service Layer

`src/service/baseService.ts` wraps axios (`src/utils/http.ts`) with token auth (header: `token`), 30s timeout, GET cache-busting. API responses follow `{ code, msg, data }` where `code === 0` is success. 401 redirects to login.

### State (`src/store/index.ts`)

Single Pinia store `useAppStore` holds auth state, user info, permissions, dictionaries, dynamic routes, menus, and tabs. `initApp()` loads menus, permissions, user, and dictionaries in parallel.

### Cross-Component Events

Uses `mitt` event bus via `src/utils/emits.ts`.

### Backend Integration

- Dev API: `http://localhost:10001/api` (`devops-admin` Spring Boot backend)
- Prod API: `/api` (nginx reverse proxy)
- Controlled by `VITE_APP_API` in `.env.development` / `.env.production`

### View Modules (`src/views/`)

Modules mirror backend `net.leoch.modules`: `sys/` (users, roles, menus, depts, dicts), `ops/` (hosts, business systems, backups), `alert/` (media, templates, triggers, records), `monitor/`, `job/`, `oss/`.

## Code Style

- Prettier: double quotes, semicolons, no trailing commas, 100 char width (300 for `.vue` files)
- Composition API with `<script setup>`; `any` is allowed (`@typescript-eslint/no-explicit-any: off`)
- Custom reusable components use `ren-` prefix (e.g., `ren-dept-tree`, `ren-select`)
- Path alias: `@/` maps to `src/`
- Pre-commit hook runs `lint-staged` (eslint --fix on `.ts` and `.vue` files)
