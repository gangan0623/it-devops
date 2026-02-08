# devops-admin

后端管理服务，基于 Spring Boot 3（Java 17）。

## 系统架构（概览）
- 接口层：REST API（统一入口 `/api`），Controller -> Service -> DAO（MyBatis-Plus + XML Mapper）
- 数据层：MySQL（业务数据）、Redis（参数缓存/可选）
- 安全层：Shiro + Token（OAuth2 风格），支持验证码登录与权限控制
- 可观测：操作/登录/异常日志，仪表盘汇总
- 生态集成：
  - 备份代理：`devops-backup-agent` 通过回调上报备份结果
  - 对象存储：MinIO / 阿里云 OSS / 腾讯云 COS / 七牛（统一封装）
  - 监控：Zabbix 接入（模板/主机联动），Prometheus 服务发现输出

简化数据流：
```
devops-ui -> devops-admin(/api) -> MySQL/Redis
                     |-> OSS(对象存储)
                     |-> Zabbix/Prometheus
devops-backup-agent --(callback)--> devops-admin
```

## net.leoch.modules 模块说明
> 以包为业务边界，模块通常包含 Controller/Service/DAO/Entity/DTO。

- `sys`（系统基础）
  - 用户/角色/菜单/部门/字典/参数/数据范围管理
  - 用户状态与菜单类型枚举、参数缓存（Redis 可选）
- `security`（认证授权）
  - 登录、验证码、Token 生成与校验
  - Shiro 过滤链、用户上下文、密码加密
- `log`（审计日志）
  - 登录日志、操作日志、异常日志的记录/查询/导出
- `oss`（对象存储）
  - 统一文件上传/管理与元数据维护
  - 支持 MinIO / 阿里云 OSS / 腾讯云 COS / 七牛
- `ops`（运维资产与备份）
  - 资产与拓扑：业务系统、Linux/Windows 主机、监控组件
  - 备份体系：备份代理、设备备份计划、备份记录/历史/差异比对
  - 监控联动：Prometheus 服务发现、Zabbix 模板/主机联动
  - 数据看板：资产数量、告警概览、备份统计
  - 备份回调：接收 `devops-backup-agent` 的结果通知
- `alert`（告警）
  - 告警媒介配置（邮件/Webhook）
  - 告警模板、触发规则、告警记录
  - SSE 实时推送与测试发送
- `job`（定时任务）
  - 任务调度与日志（任务管理/执行记录）
  - 内置任务：设备备份任务（与 `ops` 关联）

## 开发与构建
- 运行（开发）：`mvn spring-boot:run`
- 打包：`mvn -DskipTests package`
- 产物：`target/devops-admin.jar`

## 配置
- 主配置：`src/main/resources/application.yml`
- 环境配置：`src/main/resources/application-dev.yml`、`application-test.yml`、`application-prod.yml`

## Docker（可选）
- 构建镜像：`docker build -t devops-admin .`
- 运行容器：`docker run --rm -p 8080:8080 devops-admin`
