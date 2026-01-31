# IT-devops

本仓库包含 DevOps 相关的三部分：后端服务、备份代理以及前端界面。

## 项目介绍

### devops-admin — 后端管理服务

> 详见 [devops-admin/README.md](devops-admin/README.md)

基于 Spring Boot 3（Java 17）的后端服务。

- **接口层**：REST API（统一入口 `/api`），Controller -> Service -> DAO（MyBatis-Plus + XML Mapper）
- **数据层**：MySQL（业务数据）、Redis（参数缓存/可选）
- **安全层**：Shiro + Token（OAuth2 风格），支持验证码登录与权限控制
- **可观测**：操作/登录/异常日志，仪表盘汇总
- **生态集成**：备份代理回调、对象存储（MinIO / 阿里云 OSS / 腾讯云 COS / 七牛）、Zabbix 监控接入、Prometheus 服务发现

核心模块：`sys`（系统基础）、`security`（认证授权）、`log`（审计日志）、`oss`（对象存储）、`ops`（运维资产与备份）、`alert`（告警）、`job`（定时任务）

```
devops-ui -> devops-admin(/api) -> MySQL/Redis
                     |-> OSS(对象存储)
                     |-> Zabbix/Prometheus
devops-backup-agent --(callback)--> devops-admin
```

### devops-backup-agent — 网络设备备份代理

> 详见 [devops-backup-agent/README.md](devops-backup-agent/README.md)

基于 Python 3.12（FastAPI + SSH + MinIO）的备份代理。

- **入口**：HTTP API（`/backup`）接收后端下发的设备清单
- **执行**：按设备型号选择适配器，通过 SSH 拉取配置
- **存储**：上传到对象存储（默认 MinIO，S3 协议）
- **回调**：将结果回调到 `devops-admin` 的备份回调接口
- **支持设备**：华为 VRP、H3C ComWare、飞塔 FortiOS、锐捷 RGOS

```
devops-admin -> /backup -> devops-backup-agent -> MinIO
devops-backup-agent --(callback)--> devops-admin
```

### devops-ui — 前端界面

> 详见 [devops-ui/README.md](devops-ui/README.md)

基于 Vue 3 + Vite + TypeScript 的管理前端。

- **交互**：通过 `src/service` 调用 `devops-admin` REST API（`/api` 前缀）
- **权限**：登录获取 Token，按菜单/角色渲染路由与按钮权限
- **页面模块**：系统管理（sys）、运维资产与备份（ops）、告警管理（alert）、定时任务（job）、日志查询（log）、对象存储（oss）、监控看板（monitor）

## 目录结构
- `devops-admin/`：后台服务（Spring Boot 3）
- `devops-backup-agent/`：网络设备备份代理（FastAPI + SSH + MinIO）
- `devops-ui/`：前端界面（Vue 3 + Vite）

## 快速开始
### devops-admin
- 需要：JDK 17、Maven
- 运行（开发）：`mvn spring-boot:run`
- 打包：`mvn -DskipTests package`
- 产物：`devops-admin/target/devops-admin.jar`

### devops-ui
- 需要：Node.js 18+、npm
- 安装依赖：`npm install`
- 本地开发：`npm run dev`
- 构建：`npm run build`
- 产物：`devops-ui/dist/`

### devops-backup-agent
- 需要：Python 3.12、pip
- 安装依赖：`pip install -r requirements.txt`
- 运行：
  `python -m backup_agent.main --config-file config.yaml --token <AGENT_TOKEN> --callback-url <CALLBACK_URL> --log-file agent.log --host 0.0.0.0 --port 8120`
- 二进制构建：`./build.sh`
- 产物：`devops-backup-agent/dist/backup-agent`

## Docker Compose 一键部署

### 前置条件
- 安装 Docker 和 Docker Compose

### 服务组成
| 服务 | 说明 | 端口 |
|------|------|------|
| mysql | MySQL 8.0 数据库 | 3306 |
| redis | Redis 7.4.7 缓存 | 6379 |
| devops-admin | 后端 Spring Boot 服务 | 8080 |
| nginx | 前端 + 反向代理 | 80 |

### 启动

```bash
# 构建并启动所有服务（后台运行）
docker-compose up -d --build

# 查看服务状态
docker-compose ps

# 查看日志
docker-compose logs -f
```

### 停止与清理

```bash
# 停止所有服务
docker-compose down

# 停止并删除数据卷（会清除数据库数据）
docker-compose down -v
```

### 数据持久化
- MySQL 数据：`./docker/mysql/`
- Redis 数据：`./docker/redis/`
- 后端日志：`./docker/admin/logs/`
- Nginx 日志：`./docker/nginx/logs/`

### 注意事项
- MySQL 会在首次启动时自动执行 `devops-admin/db/mysql.sql` 进行数据库初始化
- 服务启动有依赖顺序：MySQL/Redis 健康检查通过后才会启动 devops-admin，devops-admin 启动后才会启动 nginx
- 如需修改数据库或 Redis 密码，请同步修改 `docker-compose.yaml` 中对应服务的环境变量

## 配置入口
- `devops-admin/src/main/resources/application*.yml`
- `devops-ui/.env.development`、`devops-ui/.env.production`
- `devops-backup-agent/config.yaml`
