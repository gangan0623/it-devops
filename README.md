# IT-devops

[中文文档](./README_ZH.md) | English

An integrated IT DevOps management platform for managing IT assets, network device backups, and alerting.

## Architecture Overview

```
┌─────────────────────────────────────────────────────────┐
│                    Browser                               │
└─────────────────────┬───────────────────────────────────┘
                      │ http://localhost:10000 (dev)
                      │ or http://localhost (prod)
                      ▼
┌─────────────────────────────────────────────────────────┐
│                  devops-ui (Vue 3 SPA)                  │
│                   Port: 10000 (dev) / 80 (prod)         │
│                     nginx reverse proxy                  │
└─────────────────────┬───────────────────────────────────┘
                      │ /api/*
                      ▼
┌─────────────────────────────────────────────────────────┐
│              devops-admin (Spring Boot 3)               │
│                   Port: 10001, Context: /api             │
│                                                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐      │
│  │   sys   │ │security │ │   log   │ │   ops   │      │
│  │  users, │ │  login, │ │  login  │ │  hosts, │      │
│  │  roles, │ │  token  │ │  audit  │ │ backups │      │
│  │  menus  │ │         │ │         │ │         │      │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘      │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐                   │
│  │  alert  │ │   job   │ │   oss   │                   │
│  │ triggers│ │ quartz  │ │ storage │                   │
│  │   SSE   │ │         │ │         │                   │
│  └─────────┘ └─────────┘ └─────────┘                   │
└────────┬────────────────────────────────────────────────┘
         │
    ┌────┴────┬─────────────┬──────────────┐
    ▼         ▼             ▼              ▼
┌───────┐ ┌───────┐ ┌─────────────┐ ┌─────────────┐
│ MySQL │ │ Redis │ │ Zabbix /    │ │   Object    │
│  8.0  │ │ 7.4.7 │ │ Prometheus  │ │   Storage   │
└───────┘ └───────┘ └─────────────┘ │ MinIO / OSS │
                                    └─────────────┘

         ┌─────────────────────────────────────────┐
         │        devops-backup-agent               │
         │         (Python FastAPI)                 │
         │          Port: 8120                      │
         │                                          │
         │  ┌─────────────────────────────────────┐ │
         │  │ SSH → Network Devices              │ │
         │  │ Huawei VRP / H3C / FortiOS / Ruijie │ │
         │  └─────────────────────────────────────┘ │
         │  ┌─────────────────────────────────────┐ │
         │  │ Upload configs to Object Storage    │ │
         │  └─────────────────────────────────────┘ │
         │  ┌─────────────────────────────────────┐ │
         │  │ Callback → devops-admin            │ │
         │  └─────────────────────────────────────┘ │
         └─────────────────────────────────────────┘
```

## Sub-projects

| Sub-project | Technology | Port | Description |
|-------------|------------|------|-------------|
| [devops-admin](./devops-admin/) | Spring Boot 3 (Java 17) | 10001 | REST API backend |
| [devops-ui](./devops-ui/) | Vue 3 + TypeScript | 10000 (dev) | Web management interface |
| [devops-backup-agent](./devops-backup-agent/) | Python 3.12 (FastAPI) | 8120 | Network device backup agent |

## Features

### IT Asset Management (`ops` module)
- **Linux hosts** — SSH-based host management with online status detection
- **Windows hosts** — RDP-based host management
- **Network devices** — Switches and routers from Huawei, H3C, Fortinet, Ruijie
- **Business systems** — Grouping and association of hosts
- **Dashboard** — Overview of asset statistics

### Network Device Backup (`ops` module)
- Automatic scheduled backup of network device configs
- Support for Huawei VRP, H3C ComWare, FortiOS, Ruijie RGOS
- Version history and diff comparison
- Backup verification and change detection

### Alerting System (`alert` module)
- **Alert media** — Notification channels (email, webhook, etc.)
- **Alert templates** — Reusable alert message templates
- **Alert triggers** — Threshold-based alert rules
- **Alert records** — Alert history with SSE real-time push to browser
- **Prometheus integration** — Webhook endpoint for Prometheus Alertmanager
- **Zabbix integration** — Alert forwarding from Zabbix

### Scheduled Jobs (`job` module)
- Quartz-based scheduled task management
- Task execution logging and history
- Configurable cron expressions

### System Management (`sys` module)
- User, role, and permission management (SA-Token auth)
- Menu management (dynamic routing from database)
- Department organization
- Dictionary data
- System parameters

### Audit Logging (`log` module)
- Login audit (success/failure tracking)
- Operation audit (via `@LogOperation` AOP)
- Exception error logs

### Object Storage (`oss` module)
- Multi-provider support: MinIO, Aliyun OSS, Tencent COS, Qiniu
- Unified API via `OSSFactory`
- Used for network device config backup files

## Tech Stack

| Layer | Component | Technology |
|-------|-----------|------------|
| Backend | Framework | Spring Boot 3.5.4 (Java 17) |
| Backend | ORM | MyBatis-Plus 3.5.8 |
| Backend | Auth | SA-Token 1.37.0 (Redis sessions) |
| Backend | API Docs | Springdoc OpenAPI 2.8.4 |
| Frontend | Framework | Vue 3.5.18 + Vite 5.4.19 |
| Frontend | UI | Element Plus 2.10.5 |
| Frontend | State | Pinia 2.3.1 |
| Database | — | MySQL 8.0 |
| Cache | — | Redis 7.4.7 |
| Backup Agent | Framework | FastAPI 0.115.6 |
| Backup Agent | SSH | Paramiko 3.5.0 |

## Quick Start

### Prerequisites

- JDK 17+
- Maven 3.8+
- Node.js 18+
- Docker & Docker Compose (for full stack)

### Backend (devops-admin)

```bash
cd devops-admin

# Install dependencies and build
mvn -DskipTests package

# Run locally (uses application-dev.yml)
mvn spring-boot:run
```

API docs available at: http://localhost:10001/swagger-ui.html

### Frontend (devops-ui)

```bash
cd devops-ui

# Install dependencies
npm install

# Start dev server (proxies /api to localhost:10001)
npm run dev
```

Open: http://localhost:10000

### Backup Agent (devops-backup-agent)

```bash
cd devops-backup-agent

# Install dependencies
pip install -r requirements.txt

# Start agent
python -m backup_agent.main --config-file config.yaml \
    --token <AGENT_TOKEN> \
    --callback-url http://localhost:10001/api/ops/backup/callback \
    --log-file agent.log --host 0.0.0.0 --port 8120

# Or build binary and run
./build.sh
./dist/backup-agent --config-file config.yaml ...
```

### Docker Compose (Full Stack)

```bash
# Start all services (dev)
docker compose up -d --build

# Services: mysql (3306), redis (6379), devops-admin (10001), nginx (80)
```

For production:
```bash
docker compose -f docker-compose.prod.yaml up -d
```

## Project Structure

```
it-devops/
├── devops-admin/                    # Spring Boot REST API
│   ├── src/main/java/net/leoch/
│   │   ├── modules/                 # Business modules
│   │   │   ├── sys/                # Users, roles, menus, depts, dicts
│   │   │   ├── security/           # Login, authentication
│   │   │   ├── log/                 # Audit logs
│   │   │   ├── ops/                 # Hosts, backups, Zabbix, Prometheus
│   │   │   ├── alert/               # Alert media, templates, triggers, records
│   │   │   ├── job/                 # Scheduled jobs
│   │   │   └── oss/                 # Object storage
│   │   ├── common/                  # Shared: integration, utils, exceptions
│   │   └── framework/              # Config, aspects, handlers
│   └── src/main/resources/
│       ├── application.yml          # Base config
│       ├── application-dev.yml      # Dev overrides
│       └── mapper/**/*.xml           # MyBatis XML mappers
├── devops-ui/                       # Vue 3 SPA
│   ├── src/
│   │   ├── views/                   # Page components (auto-routed)
│   │   ├── hooks/useView.ts        # CRUD composable
│   │   ├── service/                 # API service layer
│   │   ├── store/                   # Pinia store
│   │   └── components/             # Custom components (ren-* prefix)
│   └── CLAUDE.md                    # Frontend-specific guidance
├── devops-backup-agent/             # Python FastAPI backup agent
│   ├── backup_agent/
│   │   ├── main.py                 # FastAPI app
│   │   ├── ssh_client.py           # SSH to network devices
│   │   ├── minio_client.py         # Object storage upload
│   │   └── cleaner.py              # Output cleaning
│   └── README.md                    # Agent-specific documentation
├── docker-compose.yaml              # Dev Docker Compose
├── docker-compose.prod.yaml         # Prod Docker Compose
├── CLAUDE.md                        # AI agent guidance (this project)
├── AGENTS.md                        # Multi-agent collaboration guidance
└── scripts/build_push.sh            # Release build script
```

## Configuration

| File | Purpose |
|------|---------|
| `devops-admin/src/main/resources/application-dev.yml` | Local dev: MySQL, Redis, Zabbix, OSS |
| `devops-admin/src/main/resources/application-prod.yml` | Production overrides |
| `devops-ui/.env.development` | Dev: `VITE_APP_API=http://localhost:10001/api` |
| `devops-ui/.env.production` | Prod: `VITE_APP_API=/api` |
| `devops-backup-agent/config.yaml` | MinIO credentials, callback URL, agent token |

## Key Conventions

- **Auth**: SA-Token with `token` header; 12h timeout; max 5 concurrent logins
- **Response**: All APIs return `{ code: 0, msg: "success", data: T }`
- **Routing**: Frontend routes come from backend menu API (server-driven)
- **DB**: MySQL with `ASSIGN_ID` primary keys; manual SQL migrations in `devops-admin/db/`
- **Online status**: Stored in MySQL (not Redis); refreshed by scheduled task
- **SSE**: Alert records push via Server-Sent Events with unlimited timeout

## Release

```bash
./scripts/build_push.sh <version>
# Builds Docker images, tags with <version> + latest, pushes to taohongqiang/*
```

## License

Proprietary — Internal use only.
