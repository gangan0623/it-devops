# API 文档框架迁移说明

## 📋 迁移概述

**迁移时间**: 2026-02-13
**原框架**: Knife4j 4.5.0
**新框架**: Springdoc OpenAPI 2.8.4 (标准 OpenAPI 3.0)
**目标**: 支持 Apifox 标准规范，提升 API 文档的兼容性和标准化

---

## 🔄 主要变更

### 1. 依赖变更

#### 移除
```xml
<!-- 已移除 Knife4j -->
<dependency>
    <groupId>com.github.xiaoymin</groupId>
    <artifactId>knife4j-openapi3-jakarta-spring-boot-starter</artifactId>
    <version>4.5.0</version>
</dependency>
```

#### 保留
```xml
<!-- 保留标准 Springdoc OpenAPI -->
<dependency>
    <groupId>org.springdoc</groupId>
    <artifactId>springdoc-openapi-starter-webmvc-ui</artifactId>
    <version>2.8.4</version>
</dependency>
```

---

### 2. 配置文件变更

#### SwaggerConfig.java 增强

**新增特性**:
- ✅ 多环境服务器配置（开发/测试/生产）
- ✅ 完整的 API 信息（联系人/许可证/外部文档）
- ✅ SecurityScheme 定义（SA-Token 认证）
- ✅ 详细的 API 描述和使用说明

**完整配置**: 
- Info: 标题、描述、版本、联系人、许可证
- Servers: 多环境 URL 配置
- Security: Token 认证方案
- ExternalDocs: 项目文档链接

---

#### application.yml 配置

**application.yml (全局)**:
```yaml
springdoc:
  api-docs:
    enabled: true
    path: /v3/api-docs
  swagger-ui:
    enabled: true
    path: /swagger-ui.html
    tags-sorter: alpha
    operations-sorter: alpha
  group-configs:
    - group: default
      paths-to-match: /**
      packages-to-scan: net.leoch.modules
```

**application-dev.yml (开发环境)**:
```yaml
springdoc:
  api-docs:
    enabled: true          # 开发环境启用
  swagger-ui:
    enabled: true          # 开发环境启用
```

**application-prod.yml (生产环境)**:
```yaml
springdoc:
  api-docs:
    enabled: false         # 生产环境禁用（安全考虑）
  swagger-ui:
    enabled: false         # 生产环境禁用
```

---

## 🎯 使用方式对比

### Knife4j (旧)

**访问地址**:
```
http://localhost:10001/api/doc.html
```

**特点**:
- 美化的 UI 界面
- 中文化支持
- 增强功能（离线文档、调试等）

---

### Springdoc + Apifox (新)

**OpenAPI JSON**:
```
http://localhost:10001/api/v3/api-docs
```

**Swagger UI** (可选):
```
http://localhost:10001/api/swagger-ui.html
```

**Apifox 导入**:
1. 打开 Apifox
2. 导入 → URL 导入
3. 输入: `http://localhost:10001/api/v3/api-docs`

**优势**:
- ✅ 标准 OpenAPI 3.0 规范
- ✅ 完美兼容 Apifox、Postman、Insomnia 等工具
- ✅ 支持自动同步
- ✅ 强大的 Mock、测试、协作功能
- ✅ 团队云端共享
- ✅ 更好的版本控制

---

## 📝 Controller 注解对比

### 原注解（兼容）

以下注解**完全兼容**，无需修改：

```java
@Tag(name = "用户管理")
@Operation(summary = "用户列表")
@Parameter(name = "id", description = "用户ID")
@ApiResponse(responseCode = "200", description = "成功")
```

### 推荐增强

如需更好的 Apifox 体验，可以添加：

```java
@Schema(description = "用户信息", example = "{\"name\": \"张三\"}")
@Schema(name = "username", description = "用户名", example = "admin")
```

---

## 🔑 认证配置

### Token 认证流程

1. **登录获取 Token**:
   ```
   POST /api/security/login
   Body: {"username": "admin", "password": "admin"}
   Response: {"data": {"token": "xxx"}}
   ```

2. **在请求头中携带**:
   ```
   token: xxx
   ```

3. **Apifox 配置**:
   - 环境变量: `{{token}}`
   - 或在接口认证中配置 API Key
   - 参数名: `token`
   - 添加到: `Header`

---

## 📊 功能对比

| 功能 | Knife4j | Springdoc + Apifox |
|------|---------|-------------------|
| OpenAPI 规范 | 支持 | ✅ 标准支持 |
| UI 美化 | ✅ 内置 | Apifox 更强 |
| 离线文档 | ✅ | ✅ Apifox 导出 |
| 接口调试 | ✅ | ✅ Apifox 更强 |
| Mock 数据 | ❌ | ✅ Apifox 支持 |
| 自动化测试 | ❌ | ✅ Apifox 支持 |
| 团队协作 | ❌ | ✅ Apifox 支持 |
| 版本管理 | ❌ | ✅ Apifox 支持 |
| 代码生成 | ❌ | ✅ Apifox 支持 |
| 环境管理 | 基础 | ✅ Apifox 强大 |
| Postman 兼容 | ❌ | ✅ 完全兼容 |
| 云端同步 | ❌ | ✅ Apifox 支持 |

---

## 🚀 迁移后的优势

### 1. 标准化
- 完全符合 OpenAPI 3.0 标准
- 可被任何支持 OpenAPI 的工具导入
- 更好的行业兼容性

### 2. 工具链
- Apifox: API 设计、调试、Mock、测试一体化
- Postman: 接口测试
- Insomnia: API 客户端
- Swagger Editor: 在线编辑

### 3. 团队协作
- 云端同步 API 文档
- 统一的 Mock 服务
- 自动化测试集成
- 版本历史追踪

### 4. 开发效率
- 自动生成客户端代码
- Mock 服务加速前端开发
- 环境变量管理多环境
- 请求历史和收藏

---

## 📚 相关文档

- **Apifox 导入指南**: `APIFOX_GUIDE.md`
- **OpenAPI 规范**: https://spec.openapis.org/oas/v3.0.3
- **Springdoc 文档**: https://springdoc.org/
- **Apifox 官网**: https://www.apifox.cn/

---

## ⚠️ 注意事项

1. **生产环境安全**
   - 生产环境已禁用 API 文档和 Swagger UI
   - 如需文档，请在开发/测试环境导出

2. **兼容性**
   - 所有原有的 OpenAPI 3.0 注解完全兼容
   - Controller 代码无需修改

3. **自动同步**
   - 建议在 Apifox 中配置自动同步
   - 同步频率: 每天或每小时

4. **Token 管理**
   - 开发环境 Token 有效期: 12 小时
   - 需要定期刷新

---

## 📞 支持

如有问题，请查阅：
1. `APIFOX_GUIDE.md` - 详细使用指南
2. Apifox 官方文档
3. 联系开发团队

---

**迁移完成时间**: 2026-02-13
**迁移状态**: ✅ 完成
**编译状态**: ✅ 通过
