# 参数配置页面优化设计

**日期**: 2026-03-17
**范围**: `devops-ui/src/views/sys/config*.vue`
**策略**: 分层推进（代码质量 → 性能 → UX → 视觉）

---

## 背景

`params.vue` 已拆分为 `config.vue`（tab 壳）+ 4 个子组件（`config-storage.vue`、`config-zabbix.vue`、`config-zabbix-mapping.vue`、`config-ai.vue`）。拆分后各子组件存在重复 CSS、全量初始化、UX 细节不一致等问题，需要系统性优化。

---

## 第一层：代码质量 — 提取共享 CSS

### 问题
四个子组件重复定义约 20+ 个 CSS 类：`panel-card` 系列、`config-grid`、`stack-panels`、`kv-list/kv-item`、`action-row/action-tip`、`info-list` 系列、`config-form` 系列。

### 方案
新建 `devops-ui/src/views/sys/config-shared.css`，将所有公共样式集中管理。

**提取到共享文件的类：**
- 布局：`.config-grid`、`.config-grid--storage`、`.stack-panels`
- 卡片：`.panel-card`、`.panel-card__header`、`.panel-card--muted`、`.panel-card--highlight`
- 表单：`.config-form`、`.config-form--compact`、`.action-row`、`.action-row--wrap`、`.action-tip`、`.action-tip--ok`
- 数据展示：`.kv-list`、`.kv-item`、`.kv-item__label`、`.kv-item__value`、`.side-actions`
- 文字：`.info-list`、`.info-list__title`、`.info-list__item`、`.info-list--inline`
- 响应式：`@media (max-width: 960px)` 公共断点

**保留在各组件的样式：**
- `config-zabbix-mapping.vue`：所有 `.mapping-*`、`.preview-*`、`.rule-*` 样式（仅该组件使用）
- 其余三个子组件：提取后无独有样式，`<style scoped>` 块仅保留 `@import`

**导入方式：**
```css
/* 每个子组件 <style scoped> 块 */
<style scoped>
@import "./config-shared.css";
/* 该组件独有样式（如有） */
</style>
```

> `scoped` 块中 `@import` 的类会自动获得 scoped hash，行为与直接书写一致，无样式污染风险。

---

## 第二层：性能 — Tab 懒加载

### 问题
`config.vue` 挂载时四个子组件全部初始化，`onMounted` 并发触发 6 个接口（含调用外部 Zabbix API 的慢接口），用户仅查看存储配置也会承担所有开销。

### 方案
在 `config.vue` 的每个 `<el-tab-pane>` 添加 `:lazy="true"`：

```vue
<el-tab-pane label="存储配置" name="storage" :lazy="true">
  <config-storage />
</el-tab-pane>
<el-tab-pane label="Zabbix配置" name="zabbix" :lazy="true">
  <config-zabbix />
</el-tab-pane>
<el-tab-pane label="Zabbix映射" name="zabbixMapping" :lazy="true">
  <config-zabbix-mapping />
</el-tab-pane>
<el-tab-pane label="AI配置" name="ai" :lazy="true">
  <config-ai />
</el-tab-pane>
```

**效果：**
- 页面打开只触发 1 个接口（当前激活 tab 的存储配置）
- 其余子组件在首次切换到对应 tab 时才挂载并请求
- 切换后组件保持挂载，不重复请求

**改动范围：** 仅 `config.vue` 4 行，子组件零改动。

---

## 第三层：UX 细节

### ① 保存期间禁用整个表单
`el-form` 增加 `:disabled="loading.save"`，保存请求进行中禁止用户修改字段或触发其他操作。涉及文件：`config-storage.vue`、`config-zabbix.vue`、`config-ai.vue`。

### ② 测试连接通过后引导保存
测试成功后（`tested = true`）保存按钮从禁用变为可用，同时触发短暂的 CSS `pulse` 动画吸引注意，引导用户执行保存。涉及文件：`config-zabbix.vue`、`config-ai.vue`。

### ③ 危险操作二次确认
`config-storage.vue` 中「删除 MinIO 对象」按钮点击后先弹 `ElMessageBox.confirm`：
```
确定要删除该对象吗？此操作不可恢复。
```
确认后才执行删除接口。

### ④ Zabbix 同步加确认弹窗
`config-zabbix-mapping.vue` 中「同步」按钮点击后先弹确认框，避免误触发全量同步。

### ⑤ 统一 ElMessage 时长
全部 4 个子组件统一：
| 类型 | 时长 |
|------|------|
| 成功（success） | 2000ms |
| 信息（info）    | 2000ms |
| 警告（warning） | 3000ms |
| 错误（error）   | 不自动关闭（`duration: 0`） |

---

## 第四层：视觉微调

### ① 卡片圆角统一
`config-shared.css` 中 `.panel-card` 的 `border-radius` 从 `12px` 改为 `8px`，与系统其他 `el-card` 默认圆角一致。

### 已对齐项（无需改动）
- 外层 `padding: 8px` — 与 `user.vue`、`menu.vue`、`log-login.vue` 一致 ✅
- 标题字重 `font-weight: 600` — 系统标准 ✅
- 主文字色 `#0f172a` — 系统标准 ✅
- 响应式断点 `960px` — 随共享 CSS 统一 ✅
- 菜单名称「参数配置」+ 路由 `sys/config` — 已完成 ✅

---

## 改动文件汇总

| 文件 | 操作 | 说明 |
|------|------|------|
| `config-shared.css` | **新增** | 所有公共样式 |
| `config.vue` | 修改 | 每个 tab-pane 加 `:lazy="true"` |
| `config-storage.vue` | 修改 | @import 共享 CSS、表单 disabled、删除二次确认、统一 ElMessage |
| `config-zabbix.vue` | 修改 | @import 共享 CSS、表单 disabled、pulse 动画、统一 ElMessage |
| `config-zabbix-mapping.vue` | 修改 | @import 共享 CSS（保留 mapping-* 独有样式）、同步二次确认、统一 ElMessage |
| `config-ai.vue` | 修改 | @import 共享 CSS、表单 disabled、pulse 动画、统一 ElMessage |

---

## 不在范围内

- 后端接口、权限码、数据库表结构 — 不改
- `SysParamsController` 及相关 Service — 不改
- 其他系统模块页面 — 不改
