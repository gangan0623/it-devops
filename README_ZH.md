# IT-devops

[English](./README.md) | 中文文档

IT 运维管理平台，用于管理 IT 资产、网络设备备份和告警。

## 系统架构

```
┌─────────────────────────────────────────────────────────┐
│                       浏览器                             │
└─────────────────────┬───────────────────────────────────┘
                      │ http://localhost:10000 (开发)
                      │ 或 http://localhost (生产)
                      ▼
┌─────────────────────────────────────────────────────────┐
│                  devops-ui (Vue 3 SPA)                  │
│                  端口: 10000 (开发) / 80 (生产)          │
│                     nginx 反向代理                       │
└─────────────────────┬───────────────────────────────────┘
                      │ /api/*
                      ▼
┌─────────────────────────────────────────────────────────┐
│              devops-admin (Spring Boot 3)                │
│                  端口: 10001, 上下文: /api               │
│                                                         │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐       │
│  │   sys   │ │security │ │   log   │ │   ops   │       │
│  │ 用户,   │ │ 登录,   │ │ 登录    │ │ 主机,   │       │
│  │ 角色,   │ │ token   │ │ 审计    │ │ 备份    │       │
│  │ 菜单    │ │         │ │         │ │         │       │
│  └─────────┘ └─────────┘ └─────────┘ └─────────┘       │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐                    │
│  │  alert  │ │   job   │ │   oss   │                    │
│  │ 告警    │ │ 定时    │ │ 对象    │                    │
│  │ 触发器  │ │ 任务    │ │ 存储    │                    │
│  └─────────┘ └─────────┘ └─────────┘                    │
└────────┬────────────────────────────────────────────────┘
         │
    ┌────┴────┬─────────────┬──────────────┐
    ▼         ▼             ▼              ▼
┌───────┐ ┌───────┐ ┌─────────────┐ ┌─────────────┐
│ MySQL │ │ Redis │ │ Zabbix /    │ │   对象存储   │
│  8.0  │ │ 7.4.7 │ │ Prometheus  │ │ MinIO / OSS │
└───────┘ └───────┘ └─────────────┘ │ / COS / 七牛 │
                                    └─────────────┘

         ┌─────────────────────────────────────────┐
         │        devops-backup-agent               │
         │         (Python FastAPI)                │
         │           端口: 8120                    │
         │                                          │
         │  ┌─────────────────────────────────────┐ │
         │  │ SSH → 网络设备                      │ │
         │  │ 华为 VRP / H3C / 飞塔 / 锐捷        │ │
         │  └─────────────────────────────────────┘ │
         │  ┌─────────────────────────────────────┐ │
         │  │ 上传配置到对象存储                  │ │
         │  └─────────────────────────────────────┘ │
         │  ┌─────────────────────────────────────┐ │
         │  │ 回调 → devops-admin                │ │
         │  └─────────────────────────────────────┘ │
         └─────────────────────────────────────────┘
```

## 子项目

| 子项目 | 技术栈 | 端口 | 说明 |
|--------|--------|------|------|
| [devops-admin](./devops-admin/) | Spring Boot 3 (Java 17) | 10001 | REST API 后端 |
| [devops-ui](./devops-ui/) | Vue 3 + TypeScript | 10000 (开发) | Web 管理界面 |
| [devops-backup-agent](./devops-backup-agent/) | Python 3.12 (FastAPI) | 8120 | 网络设备备份代理 |

## 功能模块

### IT 资产管理（`ops` 模块）
- **Linux 主机** — 基于 SSH 的主机管理，支持在线状态检测
- **Windows 主机** — 基于 RDP 的主机管理
- **网络设备** — 华为、H3C、飞塔、锐捷的交换机和路由器
- **业务系统** — 主机分组和关联管理
- **仪表盘** — 资产统计概览

### 网络设备备份（`ops` 模块）
- 网络设备配置的自动定时备份
- 支持华为 VRP、H3C ComWare、飞塔 FortiOS、锐捷 RGOS
- 版本历史和差异对比
- 备份验证和变更检测

### 告警系统（`alert` 模块）
- **告警媒体** — 通知渠道（ webhook 等）
- **告警模板** — 可复用的告警消息模板
- **告警触发器** — 基于阈值的告警规则
- **告警记录** — 告警历史，支持 SSE 实时推送至浏览器
- **Prometheus 集成** — 接收 Prometheus Alertmanager webhook
- **Zabbix 集成** — 转发 Zabbix 告警

### 定时任务（`job` 模块）
- 基于 Quartz 的定时任务管理
- 任务执行日志和历史记录
- 可配置 Cron 表达式

### 系统管理（`sys` 模块）
- 用户、角色、权限管理（SA-Token 认证）
- 菜单管理（动态路由，菜单数据来自数据库）
- 部门管理
- 字典数据
- 系统参数配置

### 审计日志（`log` 模块）
- 登录审计（成功/失败追踪）
- 操作审计（通过 `@LogOperation` AOP 切面记录）
- 异常错误日志

### 对象存储（`oss` 模块）
- 多存储商支持：MinIO、阿里云 OSS、腾讯云 COS、七牛
- 统一 API 通过 `OSSFactory`
- 用于存储网络设备配置文件

## 技术栈

| 层级 | 组件 | 技术 |
|------|------|------|
| 后端 | 框架 | Spring Boot 3.5.4 (Java 17) |
| 后端 | ORM | MyBatis-Plus 3.5.8 |
| 后端 | 认证 | SA-Token 1.37.0 (Redis 会话) |
| 后端 | API 文档 | Springdoc OpenAPI 2.8.4 |
| 前端 | 框架 | Vue 3.5.18 + Vite 5.4.19 |
| 前端 | UI 组件库 | Element Plus 2.10.5 |
| 前端 | 状态管理 | Pinia 2.3.1 |
| 数据库 | — | MySQL 8.0 |
| 缓存 | — | Redis 7.4.7 |
| 备份代理 | 框架 | FastAPI 0.115.6 |
| 备份代理 | SSH | Paramiko 3.5.0 |

## 快速开始

### 前置要求

- JDK 17+
- Maven 3.8+
- Node.js 18+
- Docker & Docker Compose（完整运行需要）

### 后端（devops-admin）

```bash
cd devops-admin

# 安装依赖并构建
mvn -DskipTests package

# 本地运行（使用 application-dev.yml）
mvn spring-boot:run
```

API 文档：http://localhost:10001/swagger-ui.html

### 前端（devops-ui）

```bash
cd devops-ui

# 安装依赖
npm install

# 启动开发服务器（代理 /api 到 localhost:10001）
npm run dev
```

打开：http://localhost:10000

### 备份代理（devops-backup-agent）

```bash
cd devops-backup-agent

# 安装依赖
pip install -r requirements.txt

# 启动代理
python -m backup_agent.main --config-file config.yaml \
    --token <AGENT_TOKEN> \
    --callback-url http://localhost:10001/api/ops/backup/callback \
    --log-file agent.log --host 0.0.0.0 --port 8120

# 或构建为二进制后运行
./build.sh
./dist/backup-agent --config-file config.yaml ...
```

### Docker Compose（完整运行）

```bash
# 启动所有服务（开发环境）
docker compose up -d --build

# 服务列表：mysql (3306)、redis (6379)、devops-admin (10001)、nginx (80)
```

生产环境：
```bash
docker compose -f docker-compose.prod.yaml up -d
```

## 目录结构

```
it-devops/
├── devops-admin/                    # Spring Boot REST API
│   ├── src/main/java/net/leoch/
│   │   ├── modules/                 # 业务模块
│   │   │   ├── sys/                # 用户、角色、菜单、部门、字典
│   │   │   ├── security/           # 登录、身份认证
│   │   │   ├── log/                 # 审计日志
│   │   │   ├── ops/                 # 主机、备份、Zabbix、Prometheus
│   │   │   ├── alert/               # 告警媒体、模板、触发器、记录
│   │   │   ├── job/                 # 定时任务
│   │   │   └── oss/                 # 对象存储
│   │   ├── common/                  # 共享：集成组件、工具类、异常处理
│   │   └── framework/              # 配置、切面、处理器
│   └── src/main/resources/
│       ├── application.yml          # 基础配置
│       ├── application-dev.yml      # 开发环境覆盖
│       └── mapper/**/*.xml           # MyBatis XML 映射器
├── devops-ui/                       # Vue 3 SPA
│   ├── src/
│   │   ├── views/                   # 页面组件（根据路由自动解析）
│   │   ├── hooks/useView.ts        # CRUD 组合式函数
│   │   ├── service/                 # API 服务层
│   │   ├── store/                   # Pinia 状态管理
│   │   └── components/             # 自定义组件（前缀 ren-）
│   └── CLAUDE.md                    # 前端专用指南
├── devops-backup-agent/             # Python FastAPI 备份代理
│   ├── backup_agent/
│   │   ├── main.py                 # FastAPI 应用
│   │   ├── ssh_client.py           # SSH 连接网络设备
│   │   ├── minio_client.py         # 对象存储上传
│   │   └── cleaner.py              # 输出清洗
│   └── README.md                    # 代理专用文档
├── docker-compose.yaml              # 开发环境 Docker Compose
├── docker-compose.prod.yaml         # 生产环境 Docker Compose
├── CLAUDE.md                        # AI Agent 指南（主项目）
├── AGENTS.md                        # 多 Agent 协作指南
└── scripts/build_push.sh            # 发布构建脚本
```

## 配置说明

| 文件 | 用途 |
|------|------|
| `devops-admin/src/main/resources/application-dev.yml` | 开发环境：MySQL、Redis、Zabbix、OSS 配置 |
| `devops-admin/src/main/resources/application-prod.yml` | 生产环境配置覆盖 |
| `devops-ui/.env.development` | 开发：`VITE_APP_API=http://localhost:10001/api` |
| `devops-ui/.env.production` | 生产：`VITE_APP_API=/api` |
| `devops-backup-agent/config.yaml` | MinIO 凭证、回调 URL、代理 Token |

## 关键约定

- **认证**：SA-Token，Header 传递 `token`；超时 12 小时；最多 5 个并发登录
- **响应格式**：所有 API 返回 `{ code: 0, msg: "success", data: T }`
- **前端路由**：路由由后端菜单 API 提供（服务端驱动），不是静态配置
- **数据库**：MySQL，主键使用 `ASSIGN_ID` 策略；数据库变更通过 `devops-admin/db/` 下的 SQL 脚本手动管理
- **在线状态**：存储在 MySQL 中（非 Redis），由定时任务刷新
- **SSE**：告警记录通过 Server-Sent Events 推送，超时时间无限制

## 发布

```bash
./scripts/build_push.sh <version>
# 构建 Docker 镜像，添加 <version> 和 latest 标签，推送到 taohongqiang/*
```

## 许可证

专有 — 仅供内部使用。
