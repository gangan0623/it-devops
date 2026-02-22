# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

IT DevOps管理平台，包含三个子项目：
- **devops-admin**: Spring Boot 3 后端服务（Java 17）
- **devops-ui**: Vue 3 + TypeScript 前端（Vite）
- **devops-backup-agent**: Python 3.12 网络设备备份代理（FastAPI）

## Build & Dev Commands

### devops-admin (后端)
```bash
cd devops-admin
mvn spring-boot:run                    # 开发运行（端口 10001，路径 /api）
mvn -DskipTests package                # 打包（产物: target/devops-admin.jar）
mvn test                               # 运行测试（注意：pom.xml 默认 skipTests=true）
```

### devops-ui (前端)
```bash
cd devops-ui
npm install                            # 安装依赖
npm run dev                            # 开发服务器（端口 10000，代理到 localhost:10001）
npm run build                          # 生产构建（产物: dist/）
npm run lint                           # ESLint 修复
```

### devops-backup-agent (备份代理)
```bash
cd devops-backup-agent
pip install -r requirements.txt       # 安装依赖
python -m backup_agent.main \         # 运行代理
  --config-file config.yaml \
  --token <AGENT_TOKEN> \
  --callback-url <CALLBACK_URL> \
  --host 0.0.0.0 --port 8120
./build.sh                            # 构建二进制（产物: dist/backup-agent）
```

### Docker Compose 部署
```bash
docker-compose up -d --build          # 启动所有服务（mysql/redis/devops-admin/nginx）
docker-compose ps                     # 查看状态
docker-compose logs -f devops-admin   # 查看后端日志
docker-compose down                   # 停止服务
docker-compose down -v                # 停止并删除数据卷
```

## Architecture

### 系统交互流程
```
浏览器 -> nginx(80) -> devops-ui(静态资源)
                    -> /api -> devops-admin(10001) -> MySQL/Redis
                                                   -> MinIO/OSS/COS/七牛
                                                   -> Zabbix/Prometheus
devops-backup-agent -> SSH(网络设备) -> MinIO
                    -> 回调 devops-admin /ops/backup/callback
```

### devops-admin 后端架构

**包结构**：`net.leoch.modules.<模块>`，每个模块包含 Controller/Service/DAO/Entity/DTO

**核心模块**：
- `sys`: 系统基础（用户/角色/菜单/部门/字典/参数）
- `security`: 认证授权（SA-Token，登录/验证码/Token 管理）
- `log`: 审计日志（登录/操作/异常日志）
- `oss`: 对象存储（MinIO/阿里云 OSS/腾讯云 COS/七牛）
- `ops`: 运维资产与备份（业务系统/主机/监控组件/备份代理/设备备份/看板/Prometheus 服务发现/Zabbix 联动）
- `alert`: 告警管理（媒介/模板/触发规则/告警记录/SSE 推送）
- `job`: 定时任务（任务调度/执行日志）

**通用层**：`net.leoch.common`
- `config/`: 配置类（Knife4j/MyBatis-Plus/Web/Redis/邮件等）
- `aspect/`: AOP（操作日志记录/数据范围权限）
- `exception/`: 统一异常处理（ServiceException/全局异常处理器）
- `page/`: 分页封装（PageData）
- `utils/`: 工具类（HttpContextUtils/ConvertUtils/Result 等）
- `validator/`: 自定义校验器
- `xss/`: XSS 防护

**技术栈**：
- ORM: MyBatis-Plus 3.5.8（XML Mapper 位于 `resources/mapper/`）
- 认证: SA-Token 1.37.0（Redis session 共享）
- API 文档: Knife4j 4.5.0（OpenAPI 3）
- 工具: Hutool 5.8.29、EasyExcel 3.2.1
- 数据库: MySQL 8.0（主库），Redis 7.4.7（缓存/session）

**API 规范**：
- 统一入口：`/api`（通过 `server.servlet.context-path` 配置）
- 响应格式：`Result<T>` (`{ code, msg, data }`，`code === 0` 为成功)
- 分页查询：POST `/page`，返回 `Result<IPage<T>>`
- 列表查询：POST `/list`，返回 `Result<List<T>>`
- 单个查询：GET `/{id}`，返回 `Result<T>`
- 新增：POST `/add`，返回 `Result<Void>`
- 修改：POST `/update`，返回 `Result<Void>`
- 删除：POST `/delete`，返回 `Result<Void>`

### devops-ui 前端架构

**动态路由**：登录后从 `/sys/menu/nav` 获取菜单，通过 `router.addRoute()` 动态注册。路由路径自动匹配视图组件（如 `/sys/user` -> `src/views/sys/user.vue`）

**CRUD 模式**：大多数列表页使用 `useView()` composable（位于 `src/hooks/useView.ts`），提供分页/排序/查询/删除/导出/权限检查。页面约定：
- `[module].vue`: 列表页
- `[module]-add-or-update.vue`: 表单弹窗

**状态管理**：单一 Pinia store（`src/store/index.ts`），包含用户信息/权限/菜单/字典/动态路由/tabs

**API 层**：`src/service/baseService.ts` 封装 axios（`src/utils/http.ts`），自动携带 token（header: `token`），30s 超时，401 自动跳转登录

**环境配置**：
- 开发：`VITE_APP_API=http://localhost:10001/api`（`.env.development`）
- 生产：`VITE_APP_API=/api`（nginx 反向代理，`.env.production`）

### devops-backup-agent 架构

**入口**：HTTP API `/backup` 接收后端下发的设备清单

**执行流程**：
1. 根据设备型号选择适配器（华为 VRP/H3C ComWare/飞塔 FortiOS/锐捷 RGOS）
2. 通过 SSH 连接设备拉取配置
3. 上传到对象存储（默认 MinIO，S3 协议）
4. 回调 `devops-admin` 的 `/ops/backup/callback` 接口上报结果

## Important Development Rules

### Java/Spring Boot 规范

详见 `.claude/rules/java-spring.md`，核心要点：

**命名**：
- Service 接口: `I*Service`，实现: `*ServiceImpl`
- 请求对象: `*Req`，响应对象: `*Rsp`
- Controller 方法: `lowerCamelCase`，常量: `UPPER_SNAKE_CASE`

**依赖注入**：
- ✅ 新代码使用 `@RequiredArgsConstructor` + `private final`
- ❌ 禁止 `@Autowired` 字段注入（存量代码例外）

**日志**：
- 使用 `@Slf4j`，占位符 `{}`（禁止字符串拼接）
- 业务标识：`[业务名称]`，如 `log.info("[促销目录生成]，计划ID: {}", planId);`
- 必打日志：方法入口/数据库查询结果/外部调用/写操作/异常

**异常处理**：
- 使用 `ServiceExceptionUtil.exception()`，禁止 `throw new RuntimeException()`
- 示例：`throw exception(EXPORT_RECORD_NOT_EXISTS);`

**Controller**：
- 职责单一（接收参数/调用 Service/返回结果），禁止在 Controller 写业务逻辑
- 参数校验：类添加 `@Validated`，方法参数使用 `@Valid @RequestBody`
- 返回类型：统一 `Result<T>`

**Service**：
- 复杂度控制：使用卫语句代替嵌套 if-else，方法不超过 50 行
- 批量处理：超过 1000 条记录必须分批

**Mapper**：
- 简单查询用 MyBatis Plus Lambda API
- 复杂查询用 XML（位于 `resources/mapper/`）
- ✅ 使用 `#{}` 预编译，❌ 禁止 `${}` 直接拼接
- 实体类继承 `BaseEntity`（自动拥有 id/createTime/updateTime/deleted 等基础字段）

**事务**：
- 多表操作必须加 `@Transactional(rollbackFor = Exception.class)`
- 方法必须是 `public`，避免同类内部调用（代理失效）

**API 文档**：
- 使用 Javadoc 注释（Apifox IDEA 插件自动生成）
- 字段注释示例：`@mock` 标签提供示例值

### Vue 3/TypeScript 规范

详见 `devops-ui/CLAUDE.md`，核心要点：

**代码风格**：
- Composition API + `<script setup>`
- Prettier: 双引号/分号/100 字符宽度
- 路径别名：`@/` 映射到 `src/`

**自定义组件**：
- 使用 `ren-` 前缀（如 `ren-dept-tree`、`ren-select`）

**Pre-commit 钩子**：
- 自动运行 `lint-staged`（eslint --fix on `.ts` 和 `.vue` 文件）

## Database Schema

**初始化 SQL**：`devops-admin/db/mysql.sql`（Docker Compose 首次启动自动执行）

**表命名规范**：
- 使用 `lower_snake_case`
- 布尔字段：`is_` 前缀或动词（如 `is_active`、`deleted`）
- 时间字段：`_time` 后缀（如 `create_time`、`effect_time`）
- 状态字段：`status`（如 `order_status`）

**索引规范**：
- 唯一索引：`uk_表名_字段`
- 普通索引：`idx_表名_字段`

## Configuration Files

### devops-admin
- 主配置：`src/main/resources/application.yml`
- 环境配置：`application-dev.yml`、`application-prod.yml`
- Spring Profile 切换：`spring.profiles.active`（默认 `dev`）
- 数据库：MySQL 8.0（`jdbc:mysql://localhost:3306/devops`）
- Redis：端口 6379（用于 SA-Token session 存储和参数缓存）

### devops-ui
- 开发：`.env.development`（API: `http://localhost:10001/api`）
- 生产：`.env.production`（API: `/api`）

### devops-backup-agent
- `config.yaml`：对象存储配置（MinIO endpoint/access key/secret key）

## Docker Deployment

**服务依赖顺序**：
1. MySQL/Redis 健康检查通过
2. devops-admin 启动
3. nginx 启动（提供前端静态资源 + `/api` 反向代理）

**数据持久化**：
- `./docker/mysql/`: MySQL 数据
- `./docker/redis/`: Redis 数据
- `./docker/admin/logs/`: 后端日志
- `./docker/nginx/logs/`: Nginx 日志

**端口映射**：
- MySQL: 3306
- Redis: 6379
- devops-admin: 10001
- nginx: 80（前端 + API 网关）

## Testing

### devops-admin
- Mock 测试：`*Test.java`（隔离外部依赖）
- 集成测试：`*IT.java`（真实环境，需数据库）
- 命名：`方法名_场景`（如 `getById_success`、`getById_notFound`）
- 覆盖：正常流程/边界条件/异常分支/依赖异常

### devops-ui
- 未配置测试运行器

## Security Notes

- **XSS 防护**：全局 XssFilter 过滤请求参数，富文本使用 Jsoup 白名单
- **SQL 注入**：使用 MyBatis `#{}` 预编译，禁止 `${}` 拼接
- **认证**：SA-Token（Redis session），Token 有效期 43200s（12 小时）
- **敏感信息**：日志中脱敏（手机号/身份证/密码/Token）

## Common Gotchas

1. **devops-admin 端口**：默认 10001（不是 8080），路径前缀 `/api`
2. **事务失效**：同类内部调用 `@Transactional` 方法不生效（需通过 Spring 代理）
3. **动态路由**：devops-ui 的路由是服务端动态下发，不要手动在 router 中硬编码业务路由
4. **分页查询**：后端使用 MyBatis-Plus `IPage<T>`，前端使用 `useView()` composable
5. **备份回调**：devops-backup-agent 完成备份后必须回调 `/ops/backup/callback`，否则前端看不到备份结果
6. **Docker Compose 密码**：MySQL/Redis 密码在 `docker-compose.yaml` 中硬编码（`Leoch@@20260101`），生产环境需修改
7. **MyBatis XML**：Mapper XML 文件必须放在 `resources/mapper/` 下，路径匹配 `classpath*:/mapper/**/*.xml`
