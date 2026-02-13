# Apifox 导入与使用指南

## 📋 概述

本项目已切换到标准的 OpenAPI 3.0 规范，完全兼容 Apifox。已移除 Knife4j 依赖，使用 Springdoc OpenAPI 生成标准文档。

---

## 🚀 快速开始

### 方式一：URL 导入（推荐）

1. **启动应用**
   ```bash
   cd devops-admin
   mvn spring-boot:run
   ```

2. **在 Apifox 中导入**
   - 打开 Apifox
   - 点击 "导入" → "URL导入" 或 "自动同步"
   - 输入 OpenAPI URL：
     ```
     http://localhost:10001/api/v3/api-docs
     ```
   - 点击 "确定" 开始导入

3. **配置环境变量**
   在 Apifox 中配置以下环境：

   **开发环境**:
   - `base_url`: `http://localhost:10001/api`
   - `token`: 登录后获取的 token 值

   **测试环境**:
   - `base_url`: `http://192.168.17.98:10001/api`
   - `token`: 登录后获取的 token 值

   **生产环境**:
   - `base_url`: `https://devops.leoch.com/api`
   - `token`: 登录后获取的 token 值

---

### 方式二：文件导入

1. **导出 OpenAPI JSON**
   访问以下 URL 并保存为 JSON 文件：
   ```
   http://localhost:10001/api/v3/api-docs
   ```

2. **在 Apifox 中导入**
   - 点击 "导入" → "OpenAPI"
   - 选择保存的 JSON 文件
   - 点击 "确定"

---

## 🔑 认证配置

### Token 认证说明

本项目使用 SA-Token 进行身份认证。所有需要认证的接口都需要在请求头中携带 `token` 字段。

### 获取 Token

1. **调用登录接口**
   ```
   POST /security/login
   Content-Type: application/json

   {
     "username": "admin",
     "password": "admin"
   }
   ```

2. **从响应中获取 token**
   响应示例：
   ```json
   {
     "code": 0,
     "msg": "success",
     "data": {
       "token": "4d2e5a3f..."
     }
   }
   ```

3. **在 Apifox 中配置**
   - 方式 1：在环境变量中设置 `{{token}}` 的值
   - 方式 2：在 "认证" 标签页中配置 API Key 认证
     - 参数名称: `token`
     - 添加到: `Header`
     - 参数值: 从登录接口获取的 token

---

## 📡 API 访问地址

| 环境 | 访问地址 | OpenAPI JSON |
|------|---------|--------------|
| 开发 | http://localhost:10001/api | http://localhost:10001/api/v3/api-docs |
| 测试 | http://192.168.17.98:10001/api | http://192.168.17.98:10001/api/v3/api-docs |
| 生产 | https://devops.leoch.com/api | （生产环境已禁用文档） |

### Swagger UI（仅开发/测试环境）

开发和测试环境可以访问 Swagger UI 进行在线调试：
```
http://localhost:10001/api/swagger-ui.html
```

> ⚠️ **注意**：生产环境已禁用 API 文档和 Swagger UI（安全考虑）

---

## 🔄 自动同步

Apifox 支持定时自动同步 API 文档。

### 配置自动同步

1. 在 Apifox 项目设置中，选择 "数据管理" → "导入数据"
2. 选择 "自动同步"
3. 配置同步源：
   - 类型：OpenAPI 3.0
   - URL：`http://localhost:10001/api/v3/api-docs`
   - 同步频率：按需选择（如每小时、每天）

---

## 📚 API 模块说明

### 系统管理 (sys)
- **用户管理**: `/sys/user` - 用户增删改查、密码修改、导出
- **角色管理**: `/sys/role` - 角色权限管理
- **菜单管理**: `/sys/menu` - 菜单树结构、导航菜单
- **部门管理**: `/sys/dept` - 部门树结构
- **字典管理**: `/sys/dict/*` - 字典类型和字典数据
- **参数管理**: `/sys/params` - 系统参数配置

### 安全认证 (security)
- **登录/登出**: `/security/login`, `/security/logout`
- **验证码**: `/security/captcha`
- **用户信息**: `/security/user/info`

### 日志管理 (log)
- **登录日志**: `/log/login`
- **操作日志**: `/log/operation`
- **异常日志**: `/log/error`

### 对象存储 (oss)
- **文件管理**: `/oss/file` - 文件上传、分页查询、删除
- **存储配置**: `/oss/config` - 云存储配置管理

### 运维管理 (ops)
- **主机管理**: `/ops/linux`, `/ops/windows`, `/ops/business` - 主机增删改查、导入导出
- **监控组件**: `/ops/monitor` - Prometheus/Zabbix/Alertmanager 组件管理
- **设备备份**: `/ops/backup/*` - 备份代理、备份任务、备份记录
- **看板**: `/ops/dashboard` - 统计数据、设备差异、备份统计

### 告警管理 (alert)
- **告警媒介**: `/alert/media` - 邮件媒介配置
- **告警模板**: `/alert/template` - 邮件模板管理
- **告警触发器**: `/alert/trigger` - 触发规则配置
- **告警记录**: `/alert/record` - 告警记录查询、操作（抑制/确定/关闭）
- **Webhook**: `/alert/webhook` - Alertmanager Webhook 接收

### 定时任务 (job)
- **任务管理**: `/job/schedule` - 定时任务增删改查、执行、暂停
- **执行日志**: `/job/log` - 任务执行日志查询

---

## 🎯 最佳实践

### 1. Mock 数据
Apifox 可以根据 OpenAPI 规范自动生成 Mock 数据。在接口详情中可以查看和自定义 Mock 规则。

### 2. 测试用例
基于导入的 API 创建测试用例，支持：
- 参数化测试
- 断言验证
- 前置/后置脚本

### 3. 接口调试
- 使用环境变量管理多环境配置
- 保存常用的请求为示例
- 使用前置脚本自动获取 token

### 4. 团队协作
- 定期同步最新的 API 文档
- 使用 Apifox 云端同步功能共享给团队
- 创建测试集合并分享

---

## 🛠️ 常见问题

### Q: 导入后接口数量不对？
**A**: 检查以下几点：
- OpenAPI URL 是否正确
- 应用是否正常启动
- 配置文件中 `springdoc.api-docs.enabled` 是否为 `true`

### Q: 认证失败？
**A**:
- 确保先调用登录接口获取 token
- 检查 token 是否正确配置在请求头的 `token` 字段
- 检查 token 是否过期（默认 12 小时）

### Q: 生产环境无法访问文档？
**A**:
- 生产环境已禁用 API 文档（安全考虑）
- 可以在开发/测试环境导出文档后手动导入

### Q: 如何批量修改请求头？
**A**:
- 在 Apifox 环境管理中配置全局请求头
- 或在项目设置的 "全局参数" 中配置

---

## 📞 技术支持

如有问题，请联系：
- **邮箱**: devops@leoch.com
- **GitHub**: https://github.com/your-org/it-devops/issues

---

**版本**: 5.5.0
**更新时间**: 2026-02-13
**文档生成**: Springdoc OpenAPI 2.8.4
