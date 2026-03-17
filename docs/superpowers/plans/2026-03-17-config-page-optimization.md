# 参数配置页面优化 Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** 对 config.vue 系列组件进行四层优化：提取共享 CSS、Tab 懒加载、UX 细节增强、视觉微调。

**Architecture:** 新建 `config-shared.css` 集中管理公共样式，四个子组件通过 `@import` 引用；`config.vue` 加 `:lazy` 属性实现按需加载；各子组件补充表单禁用、危险操作确认、ElMessage 时长统一和保存按钮引导动画。

**Tech Stack:** Vue 3 (Composition API + `<script setup>`)、Element Plus (`ElMessage`、`ElMessageBox`)、CSS `@import`（scoped）

> ⚠️ 本项目**无测试框架**（CLAUDE.md 明确），所有验证步骤均为浏览器手动验证。

---

## 文件结构

| 文件 | 操作 | 说明 |
|------|------|------|
| `devops-ui/src/views/sys/config-shared.css` | **新建** | 所有公共 CSS 类，含 pulse 动画 |
| `devops-ui/src/views/sys/config.vue` | 修改 | 4 个 tab-pane 加 `:lazy="true"` |
| `devops-ui/src/views/sys/config-storage.vue` | 修改 | @import、表单 disabled、删除二次确认、ElMessage 时长 |
| `devops-ui/src/views/sys/config-zabbix.vue` | 修改 | @import、表单 disabled、保存按钮 pulse、ElMessage 时长 |
| `devops-ui/src/views/sys/config-zabbix-mapping.vue` | 修改 | @import 保留 mapping-* 独有样式、同步二次确认、ElMessage 时长 |
| `devops-ui/src/views/sys/config-ai.vue` | 修改 | @import、表单 disabled、保存按钮 pulse、ElMessage 时长 |

---

## Task 1: 新建 config-shared.css

**Files:**
- Create: `devops-ui/src/views/sys/config-shared.css`

- [ ] **Step 1: 新建文件，写入全部公共样式**

```css
/* devops-ui/src/views/sys/config-shared.css */

/* ── 布局 ── */
.config-grid {
  display: grid;
  gap: 12px;
}

.config-grid--storage {
  grid-template-columns: minmax(0, 1.8fr) minmax(280px, 1fr);
}

.config-grid--default {
  grid-template-columns: minmax(0, 1.6fr) minmax(260px, 1fr);
}

.stack-panels {
  display: grid;
  gap: 12px;
  align-content: start;
}

/* ── 卡片 ── */
.panel-card {
  border-radius: 8px;
}

.panel-card__header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
  font-weight: 600;
  color: #0f172a;
}

.panel-card--muted {
  background: #f8fafc;
}

.panel-card--highlight {
  border: 1px solid #dbeafe;
  background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
}

/* ── 表单 ── */
.config-form {
  max-width: 100%;
}

.config-form--compact :deep(.el-form-item__label) {
  width: 84px !important;
}

.action-row {
  margin-bottom: 0;
}

.action-row--wrap :deep(.el-form-item__content) {
  gap: 8px;
  flex-wrap: wrap;
}

.action-tip {
  margin-top: 6px;
  font-size: 13px;
  color: #64748b;
  line-height: 1.5;
}

.action-tip--ok {
  color: #15803d;
}

/* ── 数据展示 ── */
.kv-list {
  display: grid;
  gap: 10px;
}

.kv-item {
  display: flex;
  justify-content: space-between;
  gap: 12px;
  padding: 10px 12px;
  border-radius: 10px;
  background: #fff;
  border: 1px solid #e5e7eb;
}

.kv-item__label {
  color: #64748b;
}

.kv-item__value {
  color: #0f172a;
  font-weight: 600;
  text-align: right;
  word-break: break-word;
}

.side-actions {
  margin-top: 12px;
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}

/* ── 文字说明 ── */
.info-list {
  display: grid;
  gap: 8px;
}

.info-list__title {
  font-size: 13px;
  font-weight: 700;
  color: #334155;
}

.info-list__item {
  color: #475569;
  line-height: 1.55;
  font-size: 13px;
}

.info-list--inline {
  margin-top: 10px;
}

/* ── 保存按钮引导动画 ── */
@keyframes config-pulse {
  0%   { box-shadow: 0 0 0 0 rgba(59, 130, 246, 0.5); }
  70%  { box-shadow: 0 0 0 8px rgba(59, 130, 246, 0); }
  100% { box-shadow: 0 0 0 0 rgba(59, 130, 246, 0); }
}

.btn-save-pulse {
  animation: config-pulse 0.7s ease-out;
}

/* ── 响应式 ── */
@media (max-width: 960px) {
  .config-grid--storage,
  .config-grid--default {
    grid-template-columns: 1fr;
  }
}
```

- [ ] **Step 2: 验证文件存在**

```bash
ls devops-ui/src/views/sys/config-shared.css
```

期望：文件存在，无报错。

- [ ] **Step 3: Commit**

```bash
git add devops-ui/src/views/sys/config-shared.css
git commit -m "feat: 新增 config-shared.css 提取配置页公共样式"
```

---

## Task 2: 更新 config.vue — Tab 懒加载

**Files:**
- Modify: `devops-ui/src/views/sys/config.vue`

- [ ] **Step 1: 为每个 el-tab-pane 加 `:lazy="true"`**

将 `config.vue` 中 4 个 `<el-tab-pane>` 改为：

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

- [ ] **Step 2: 浏览器验证**

打开页面，打开 Network 面板：
- 页面加载时只有 `GET /sys/config-center/storage` 触发
- 点击「Zabbix配置」tab 后才出现 `GET /sys/config-center/zabbix`
- 切换回「Zabbix配置」不重复请求

- [ ] **Step 3: Commit**

```bash
git add devops-ui/src/views/sys/config.vue
git commit -m "perf: config.vue Tab 改为懒加载，首屏只触发当前 tab 接口"
```

---

## Task 3: 更新 config-storage.vue

**Files:**
- Modify: `devops-ui/src/views/sys/config-storage.vue`

- [ ] **Step 1: `<style>` 块替换为 `@import`**

将整个 `<style scoped>` 块替换为：

```vue
<style scoped>
@import "./config-shared.css";
</style>
```

- [ ] **Step 2: 修复 config-grid 类名**

模板中 `<div class="config-grid config-grid--storage">` 保持不变，已匹配共享 CSS 中的类名，无需修改。

- [ ] **Step 3: el-form 加 `:disabled`**

存储配置表单标签改为：
```html
<el-form ref="storageFormRef" :model="storageForm" :rules="storageRules"
         label-width="120px" class="config-form"
         :disabled="loading.storage">
```

- [ ] **Step 4: 删除 MinIO 对象加二次确认**

在 `deleteByUrl` 函数中，校验通过后、发起请求前插入确认弹窗：

```ts
import { ElMessage, ElMessageBox } from "element-plus";

const deleteByUrl = async () => {
  const valid = await validateForm(deleteUrlFormRef);
  if (!valid) return;
  try {
    await ElMessageBox.confirm(
      "确定要删除该 MinIO 对象吗？此操作不可恢复。",
      "危险操作",
      { confirmButtonText: "确定删除", cancelButtonText: "取消", type: "warning" }
    );
  } catch {
    return;
  }
  loading.deleteUrl = true;
  baseService
    .post("/sys/config-center/storage/delete-by-url", { url: deleteUrlForm.url })
    .then(() => {
      ElMessage.success({ message: "删除成功", duration: 2000 });
      deleteUrlForm.url = "";
    })
    .finally(() => {
      loading.deleteUrl = false;
    });
};
```

- [ ] **Step 5: 统一 ElMessage 时长**

`saveStorage` 中：
```ts
ElMessage.success({ message: "存储配置已保存", duration: 2000 });
```

- [ ] **Step 6: 浏览器验证**

1. 点「保存存储配置」时表单变灰不可操作
2. 点「删除MinIO对象」弹出确认框，点取消不执行，点确认才执行
3. 成功��示 2 秒后消失

- [ ] **Step 7: Commit**

```bash
git add devops-ui/src/views/sys/config-storage.vue
git commit -m "feat: config-storage 提取共享CSS、表单禁用、删除二次确认、统一消息时长"
```

---

## Task 4: 更新 config-zabbix.vue

**Files:**
- Modify: `devops-ui/src/views/sys/config-zabbix.vue`

- [ ] **Step 1: `<style>` 块替换为 `@import`**

```vue
<style scoped>
@import "./config-shared.css";
</style>
```

- [ ] **Step 2: 修复 config-grid 类名**

模板中将 `<div class="config-grid">` 改为：
```html
<div class="config-grid config-grid--default">
```

（对应共享 CSS 中的 `.config-grid--default`，即 `1.6fr / 260px` 的两列布局）

- [ ] **Step 3: el-form 加 `:disabled`**

```html
<el-form ref="zabbixFormRef" :model="zabbixForm" :rules="zabbixRules"
         label-width="110px" class="config-form"
         :disabled="loading.save">
```

- [ ] **Step 4: 保存按钮 pulse 动画**

文件顶部 import 行已有 `watch`，只需**追加** `nextTick`：

```ts
// 原来：import { onMounted, reactive, ref, watch } from "vue";
// 改为：
import { onMounted, nextTick, reactive, ref, watch } from "vue";
```

在 `<script setup>` 中新增：

```ts
const pulseSave = ref(false);

watch(tested, (val) => {
  if (val) {
    nextTick(() => {
      pulseSave.value = true;
      setTimeout(() => { pulseSave.value = false; }, 700);
    });
  }
});
```

保存按钮加 `:class="{ 'btn-save-pulse': pulseSave }"`

- [ ] **Step 5: 统一 ElMessage 时长**

```ts
// testZabbix 成功
ElMessage.success({ message: "Zabbix连接测试成功", duration: 2000 });

// checkZabbixVersion 成功
if (showSuccess) ElMessage.success({ message: "Zabbix版本检测完成", duration: 2000 });

// saveZabbix 成功
ElMessage.success({ message: "Zabbix配置已保存", duration: 2000 });

// saveZabbix 警告
ElMessage.warning({ message: "请先测试连接", duration: 3000 });
```

- [ ] **Step 6: 浏览器验证**

1. 保存中表单变灰不可操作
2. 测试通过后保存按钮有短暂蓝色光晕
3. 直接点保存弹出警告提示 3 秒

- [ ] **Step 7: Commit**

```bash
git add devops-ui/src/views/sys/config-zabbix.vue
git commit -m "feat: config-zabbix 提取共享CSS、表单禁用、pulse动画、统一消息时长"
```

---

## Task 5: 更新 config-zabbix-mapping.vue

**Files:**
- Modify: `devops-ui/src/views/sys/config-zabbix-mapping.vue`

- [ ] **Step 1: `<style>` 块改为 `@import` + 保留 mapping 独有样式**

开头加 `@import "./config-shared.css";`，然后**删除**以下与共享文件重复的类定义（这些类在 config-zabbix-mapping.vue 的 `<style scoped>` 中存在，但已由共享文件提供）：

- `.panel-card`
- `.panel-card__header`
- `.panel-card--muted`
- `.info-list`
- `.info-list__item`

**保留**以下 mapping 独有样式（不在共享文件中）：

- `.mapping-workbench` 及所有 `.mapping-*` 类
- `.preview-header-tags`、`.preview-toolbar`、`.mapping-preview`、`.unmatched-list`
- `.mapping-rule-tabs`、`.mapping-rule-list`、`.mapping-rule-scroll`
- `.rule-empty`、`.mapping-rule-row`
- `@media (max-width: 960px)` 中关于 mapping 布局的断点

最终 `<style scoped>` 结构：

```vue
<style scoped>
@import "./config-shared.css";

/* mapping 独有样式保留在此 */
.mapping-workbench { ... }
/* ... 其余 mapping-* 样式 */
</style>
```

- [ ] **Step 2: 同步操作加二次确认**

`syncZabbixNetworkHosts` 函数开头加确认弹窗：

```ts
import { ElMessage, ElMessageBox } from "element-plus";

const syncZabbixNetworkHosts = async () => {
  try {
    await ElMessageBox.confirm(
      "将从 Zabbix 拉取主机数据并同步到本地，确定执行吗？",
      "确认同步",
      { confirmButtonText: "确定同步", cancelButtonText: "取消", type: "warning" }
    );
  } catch {
    return;
  }
  loading.sync = true;
  // ... 原有请求逻辑不变
};
```

- [ ] **Step 3: 统一 ElMessage 时长**

```ts
// 预览完成
ElMessage.info({ message: "预览完成，未匹配到可展示的主机群组", duration: 2000 });
ElMessage.success({ message: `预览完成，共 ${...} 条，未映射 ${...} 条`, duration: 2000 });

// 保存完成
ElMessage.success({ message: "设备映射配置已保存", duration: 2000 });

// 同步未执行警告
ElMessage.warning({ message: data.message || "同步未执行", duration: 3000 });

// 同步完成
ElMessage.success({ message: `同步完成：...`, duration: 2000 });
```

- [ ] **Step 4: 浏览器验证**

1. 点「同步」弹出确认框，取消不执行
2. 点「保存」成功提示 2 秒消失

- [ ] **Step 5: Commit**

```bash
git add devops-ui/src/views/sys/config-zabbix-mapping.vue
git commit -m "feat: config-zabbix-mapping 提取共享CSS、同步二次确认、统一消息时长"
```

---

## Task 6: 更新 config-ai.vue

**Files:**
- Modify: `devops-ui/src/views/sys/config-ai.vue`

- [ ] **Step 1: `<style>` 块替换为 `@import`**

```vue
<style scoped>
@import "./config-shared.css";
</style>
```

- [ ] **Step 2: 修复 config-grid 类名**

```html
<div class="config-grid config-grid--default">
```

- [ ] **Step 3: el-form 加 `:disabled`**

```html
<el-form ref="aiFormRef" :model="aiForm" :rules="aiRules"
         label-width="100px" class="config-form"
         :disabled="loading.save">
```

- [ ] **Step 4: 保存按钮 pulse 动画**

与 Task 4 Step 4 相同模式，在 `<script setup>` 中：

```ts
import { onMounted, reactive, ref, watch, nextTick } from "vue";

const pulseSave = ref(false);

watch(tested, (val) => {
  if (val) {
    nextTick(() => {
      pulseSave.value = true;
      setTimeout(() => { pulseSave.value = false; }, 700);
    });
  }
});
```

保存按钮加 `:class="{ 'btn-save-pulse': pulseSave }"`

- [ ] **Step 5: 统一 ElMessage 时长**

```ts
// testAi 成功
ElMessage.success({ message: "AI连接测试成功", duration: 2000 });

// saveAi 警告
ElMessage.warning({ message: "请先测试连接", duration: 3000 });

// saveAi 成功
ElMessage.success({ message: "AI配置已保存", duration: 2000 });
```

- [ ] **Step 6: 浏览器验证**

1. 保存中表单变灰
2. 测试通过后保存按钮有短暂蓝色光晕
3. 成功提示 2 秒消失

- [ ] **Step 7: Commit**

```bash
git add devops-ui/src/views/sys/config-ai.vue
git commit -m "feat: config-ai 提取共享CSS、表单禁用、pulse动画、统一消息时长"
```

---

## Task 7: 最终验证 + 整体回归

- [ ] **Step 1: 启动前端开发服务器**

```bash
cd devops-ui && npm run dev
```

- [ ] **Step 2: 回归验证清单**

打开浏览器访问参数配置页面，逐项确认：

| 项目 | 预期 |
|------|------|
| Network 面板 — 初始加载 | 只有 1 个 storage 接口 |
| 切换 Zabbix tab | 触发 zabbix 相关接口 |
| 切换回 Zabbix tab | 不重复请求 |
| 存储配置保存中 | 表单变灰，按钮转圈 |
| 删除 MinIO 对象 | 弹出确认框 |
| Zabbix 测试通过 | 保存按钮有光晕 |
| Zabbix 同步按钮 | 弹出确认框 |
| AI 测试通过 | 保存按钮有光晕 |
| 所有成功提示 | 2 秒消失 |
| 所有卡片圆角 | 视觉上比之前略小（8px） |

- [ ] **Step 3: lint 检查**

```bash
cd devops-ui && npm run lint
```

期望：无错误。

- [ ] **Step 4: 最终 Commit（如有 lint 自动修复）**

```bash
git add -A
git commit -m "chore: lint fix after config page optimization"
```

---

## 注意事项

- `config-zabbix-mapping.vue` 的 `<style scoped>` 中 `@import` 之后，删除与共享文件重复的类时要逐一核对，避免误删 `.mapping-*` 独有样式
- `config-grid--default` 是新增类名（原来 zabbix 和 ai 用的是 `.config-grid` 但没有列宽设定），Task 2 和 Task 6 中模板需同步改类名
- pulse 动画依赖 `tested` 是 `ref(false)` 而不是 `reactive` 中的属性，watch 语法使用 `watch(tested, ...)` 而不是 `watch(() => tested.value, ...)`
