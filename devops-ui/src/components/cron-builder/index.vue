<template>
  <div class="cron-builder">
    <!-- 字段选择 + 手动输入 -->
    <div class="cron-builder__header">
      <div class="cron-builder__fields">
        <div
          v-for="tab in tabList"
          :key="tab.key"
          class="cron-builder__field"
          :class="{ 'is-active': activeTab === tab.key }"
          @click="activeTab = tab.key"
        >
          <span class="cron-builder__field-val">{{ fieldToCron(tab.key) }}</span>
          <span class="cron-builder__field-name">{{ tab.label }}</span>
        </div>
      </div>
      <el-input v-model="cronText" class="cron-builder__manual" placeholder="手动输入" size="small" @blur="onManualInput" @keyup.enter="onManualInput" />
    </div>

    <!-- 当前字段的模式选择 + 参数 -->
    <div v-for="tab in tabList" v-show="activeTab === tab.key" :key="tab.key" class="cron-builder__body">
      <!-- 模式切换：横向分段按钮 -->
      <el-radio-group v-model="fields[tab.key].type" size="small" class="cron-builder__modes" @change="onFieldChange">
        <el-radio-button label="every">{{ tab.everyLabel }}</el-radio-button>
        <el-radio-button v-if="tab.key === 'day' || tab.key === 'week'" label="none">不指定</el-radio-button>
        <el-radio-button label="period">周期</el-radio-button>
        <el-radio-button label="range">区间</el-radio-button>
        <el-radio-button label="specify">指定</el-radio-button>
      </el-radio-group>

      <!-- 周期参数 -->
      <div v-if="fields[tab.key].type === 'period'" class="cron-builder__params">
        <span>从</span>
        <el-input-number v-model="fields[tab.key].periodStart" :min="tab.min" :max="tab.max" :controls="false" size="small" class="cron-builder__num" @change="onFieldChange" />
        <span>{{ tab.unit }}开始，每</span>
        <el-input-number v-model="fields[tab.key].periodStep" :min="1" :max="tab.max - tab.min + 1" :controls="false" size="small" class="cron-builder__num" @change="onFieldChange" />
        <span>{{ tab.unit }}执行一次</span>
      </div>

      <!-- 区间参数 -->
      <div v-if="fields[tab.key].type === 'range'" class="cron-builder__params">
        <span>从</span>
        <el-input-number v-model="fields[tab.key].rangeStart" :min="tab.min" :max="tab.max" :controls="false" size="small" class="cron-builder__num" @change="onFieldChange" />
        <span>到</span>
        <el-input-number v-model="fields[tab.key].rangeEnd" :min="tab.min" :max="tab.max" :controls="false" size="small" class="cron-builder__num" @change="onFieldChange" />
        <span>{{ tab.unit }}</span>
      </div>

      <!-- 指定值多选 -->
      <div v-if="fields[tab.key].type === 'specify'" class="cron-builder__checks">
        <el-checkbox-group v-model="fields[tab.key].specified" @change="onFieldChange">
          <el-checkbox
            v-for="v in tab.values"
            :key="v.value"
            :label="v.value"
            class="cron-builder__check-item"
            :class="checkItemClass(tab.key)"
          >{{ v.label }}</el-checkbox>
        </el-checkbox-group>
      </div>
    </div>

    <!-- 执行时间预览 -->
    <div class="cron-builder__preview">
      <span class="cron-builder__preview-dot"></span>
      <span class="cron-builder__preview-label">下次执行</span>
      <template v-if="nextTimes.length">
        <span v-for="(t, i) in nextTimes" :key="i" class="cron-builder__preview-time">{{ t }}</span>
      </template>
      <span v-else class="cron-builder__preview-empty">--</span>
    </div>
  </div>
</template>

<script lang="ts" setup>
import { reactive, ref, watch, computed } from "vue";

const props = defineProps<{ modelValue: string }>();
const emit = defineEmits<{ (e: "update:modelValue", value: string): void }>();

const WEEK_LABELS = ["一", "二", "三", "四", "五", "六", "日"];

interface TabDef {
  key: string;
  label: string;
  everyLabel: string;
  unit: string;
  min: number;
  max: number;
  values: { value: number; label: string }[];
}

const tabList: TabDef[] = [
  { key: "second", label: "秒", everyLabel: "每秒", unit: "秒", min: 0, max: 59, values: Array.from({ length: 60 }, (_, i) => ({ value: i, label: String(i) })) },
  { key: "minute", label: "分", everyLabel: "每分", unit: "分", min: 0, max: 59, values: Array.from({ length: 60 }, (_, i) => ({ value: i, label: String(i) })) },
  { key: "hour", label: "时", everyLabel: "每时", unit: "时", min: 0, max: 23, values: Array.from({ length: 24 }, (_, i) => ({ value: i, label: String(i) })) },
  { key: "day", label: "日", everyLabel: "每日", unit: "日", min: 1, max: 31, values: Array.from({ length: 31 }, (_, i) => ({ value: i + 1, label: String(i + 1) })) },
  { key: "month", label: "月", everyLabel: "每月", unit: "月", min: 1, max: 12, values: Array.from({ length: 12 }, (_, i) => ({ value: i + 1, label: `${i + 1}月` })) },
  { key: "week", label: "周", everyLabel: "每周", unit: "周", min: 1, max: 7, values: Array.from({ length: 7 }, (_, i) => ({ value: i + 1, label: `周${WEEK_LABELS[i]}` })) }
];

interface FieldState {
  type: string;
  periodStart: number;
  periodStep: number;
  rangeStart: number;
  rangeEnd: number;
  specified: number[];
}

function createFieldState(min: number, max: number): FieldState {
  return { type: "every", periodStart: min, periodStep: 1, rangeStart: min, rangeEnd: max, specified: [] };
}

const fields = reactive<Record<string, FieldState>>({
  second: createFieldState(0, 59),
  minute: createFieldState(0, 59),
  hour: createFieldState(0, 23),
  day: createFieldState(1, 31),
  month: createFieldState(1, 12),
  week: createFieldState(1, 7)
});

const activeTab = ref("second");
const cronText = ref("");
let ignoreWatch = false;

function checkItemClass(key: string): string {
  if (key === "month" || key === "week") return "cron-builder__check-item--wide";
  return "";
}

// --- Cron 生成 ---

function fieldToCron(key: string): string {
  const f = fields[key];
  switch (f.type) {
    case "every": return "*";
    case "none": return "?";
    case "period": return `${f.periodStart}/${f.periodStep}`;
    case "range": return `${f.rangeStart}-${f.rangeEnd}`;
    case "specify":
      if (f.specified.length === 0) return "*";
      return [...f.specified].sort((a, b) => a - b).join(",");
    default: return "*";
  }
}

function buildCron(): string {
  return ["second", "minute", "hour", "day", "month", "week"].map(fieldToCron).join(" ");
}

// --- 日/周互斥 ---

function handleDayWeekConflict(changedKey: string) {
  if (changedKey === "day" && fields.day.type === "specify") {
    if (["specify", "range", "period"].includes(fields.week.type)) fields.week.type = "none";
  }
  if (changedKey === "week" && fields.week.type === "specify") {
    if (["specify", "range", "period"].includes(fields.day.type)) fields.day.type = "none";
  }
}

function onFieldChange() {
  handleDayWeekConflict(activeTab.value);
  const cron = buildCron();
  cronText.value = cron;
  ignoreWatch = true;
  emit("update:modelValue", cron);
}

// --- Cron 反解析 ---

function parseCronField(expr: string, key: string, min: number) {
  const f = fields[key];
  const t = expr.trim();
  if (t === "*") { f.type = "every"; return; }
  if (t === "?") { f.type = "none"; return; }
  let m = t.match(/^(\d+)\/(\d+)$/);
  if (m) { f.type = "period"; f.periodStart = +m[1]; f.periodStep = +m[2]; return; }
  m = t.match(/^\*\/(\d+)$/);
  if (m) { f.type = "period"; f.periodStart = min; f.periodStep = +m[1]; return; }
  m = t.match(/^(\d+)-(\d+)$/);
  if (m) { f.type = "range"; f.rangeStart = +m[1]; f.rangeEnd = +m[2]; return; }
  if (/^[\d,]+$/.test(t)) { f.type = "specify"; f.specified = t.split(",").map((s) => +s.trim()); return; }
  f.type = "every";
}

function parseCron(cron: string) {
  const parts = cron.trim().split(/\s+/);
  if (parts.length < 5 || parts.length > 7) return;
  let sec = "0", min = "*", hour = "*", day = "*", month = "*", week = "*";
  if (parts.length === 5) [min, hour, day, month, week] = parts;
  else if (parts.length === 6) [sec, min, hour, day, month, week] = parts;
  else [sec, min, hour, day, month, week] = parts;
  parseCronField(sec, "second", 0);
  parseCronField(min, "minute", 0);
  parseCronField(hour, "hour", 0);
  parseCronField(day, "day", 1);
  parseCronField(month, "month", 1);
  parseCronField(week, "week", 1);
}

function onManualInput() {
  parseCron(cronText.value);
  const cron = buildCron();
  cronText.value = cron;
  ignoreWatch = true;
  emit("update:modelValue", cron);
}

// --- 下次执行时间 ---

function expandField(expr: string, min: number, max: number): number[] | null {
  const t = expr.trim();
  if (t === "*" || t === "?") return null;
  let m = t.match(/^(\d+)\/(\d+)$/);
  if (m) { const vals: number[] = []; for (let v = +m[1]; v <= max; v += +m[2]) vals.push(v); return vals; }
  m = t.match(/^\*\/(\d+)$/);
  if (m) { const vals: number[] = []; for (let v = min; v <= max; v += +m[1]) vals.push(v); return vals; }
  m = t.match(/^(\d+)-(\d+)$/);
  if (m) { const vals: number[] = []; for (let v = +m[1]; v <= +m[2]; v++) vals.push(v); return vals; }
  if (/^[\d,]+$/.test(t)) return t.split(",").map((s) => +s.trim()).sort((a, b) => a - b);
  return null;
}

function matches(value: number, allowed: number[] | null): boolean {
  return allowed === null || allowed.includes(value);
}

function getNextTimes(cronExpr: string, count: number): string[] {
  const parts = cronExpr.trim().split(/\s+/);
  if (parts.length < 5) return [];
  let sec: string, min: string, hour: string, day: string, month: string, week: string;
  if (parts.length === 5) { sec = "0"; [min, hour, day, month, week] = parts; }
  else { [sec, min, hour, day, month, week] = parts; }

  const secV = expandField(sec, 0, 59), minV = expandField(min, 0, 59);
  const hourV = expandField(hour, 0, 23), dayV = expandField(day, 1, 31);
  const monthV = expandField(month, 1, 12), weekV = expandField(week, 1, 7);

  const results: string[] = [];
  const now = new Date();
  const cursor = new Date(now.getFullYear(), now.getMonth(), now.getDate(), now.getHours(), now.getMinutes(), now.getSeconds() + 1, 0);
  let iterations = 0;

  while (results.length < count && iterations < 500000) {
    iterations++;
    const cm = cursor.getMonth() + 1;
    if (!matches(cm, monthV)) { cursor.setMonth(cursor.getMonth() + 1, 1); cursor.setHours(0, 0, 0, 0); if (cursor.getFullYear() > now.getFullYear() + 2) break; continue; }
    const cd = cursor.getDate();
    if (!matches(cd, dayV)) { cursor.setDate(cd + 1); cursor.setHours(0, 0, 0, 0); continue; }
    const jsDay = cursor.getDay();
    if (!matches(jsDay === 0 ? 7 : jsDay, weekV)) { cursor.setDate(cursor.getDate() + 1); cursor.setHours(0, 0, 0, 0); continue; }
    const ch = cursor.getHours();
    if (!matches(ch, hourV)) { cursor.setHours(ch + 1, 0, 0, 0); continue; }
    const cmi = cursor.getMinutes();
    if (!matches(cmi, minV)) { cursor.setMinutes(cmi + 1, 0, 0); continue; }
    const cs = cursor.getSeconds();
    if (!matches(cs, secV)) { cursor.setSeconds(cs + 1, 0); continue; }

    const pad = (n: number) => String(n).padStart(2, "0");
    results.push(`${cursor.getFullYear()}-${pad(cursor.getMonth() + 1)}-${pad(cursor.getDate())} ${pad(cursor.getHours())}:${pad(cursor.getMinutes())}:${pad(cursor.getSeconds())}`);
    cursor.setSeconds(cursor.getSeconds() + 1, 0);
  }
  return results;
}

const nextTimes = computed(() => {
  if (!cronText.value) return [];
  return getNextTimes(cronText.value, 5);
});

// --- 同步 props ---

watch(
  () => props.modelValue,
  (val) => {
    if (ignoreWatch) { ignoreWatch = false; return; }
    if (val && val !== cronText.value) { cronText.value = val; parseCron(val); }
  },
  { immediate: true }
);
</script>

<style lang="less" scoped>
@c-primary: #3b82f6;
@c-text: #0f172a;
@c-text-2: #475569;
@c-text-3: #94a3b8;
@c-bg: #f8fafc;
@c-border: #e2e8f0;
@c-radius: 6px;
@c-mono: "SF Mono", "Cascadia Code", "Consolas", monospace;

.cron-builder { width: 100%; }

// ── 头部：字段选择 + 手动输入 ──
.cron-builder__header {
  display: flex;
  align-items: center;
  gap: 8px;
  padding: 6px 8px;
  background: @c-bg;
  border: 1px solid @c-border;
  border-radius: @c-radius;
  margin-bottom: 10px;
}

.cron-builder__fields { display: flex; gap: 2px; flex-shrink: 0; }

.cron-builder__field {
  display: flex;
  flex-direction: column;
  align-items: center;
  min-width: 34px;
  padding: 2px 4px;
  border-radius: 4px;
  cursor: pointer;
  transition: all 0.15s;
  border: 1px solid transparent;
  &:hover { background: #eef2ff; }
  &.is-active { background: #eff6ff; border-color: @c-primary; }
}

.cron-builder__field-val {
  font-size: 12px;
  font-weight: 600;
  font-family: @c-mono;
  color: @c-text;
  line-height: 1.2;
  max-width: 46px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.cron-builder__field-name { font-size: 10px; color: @c-text-3; line-height: 1.1; }

.cron-builder__manual { flex: 1; min-width: 0; }

// ── 主体：模式 + 参数 ──
.cron-builder__body { min-height: 36px; }

.cron-builder__modes {
  margin-bottom: 8px;

  :deep(.el-radio-button__inner) {
    padding: 5px 12px;
    font-size: 12px;
  }
}

.cron-builder__params {
  display: flex;
  align-items: center;
  gap: 4px;
  font-size: 13px;
  color: @c-text-2;
  padding: 2px 0;
}

.cron-builder__num {
  width: 58px;
  :deep(.el-input__inner) { text-align: center; font-family: @c-mono; }
}

// ── 多选 ──
.cron-builder__checks {
  :deep(.el-checkbox-group) { display: flex; flex-wrap: wrap; gap: 1px; }
}

.cron-builder__check-item {
  width: 50px;
  margin-right: 0 !important;
  :deep(.el-checkbox__label) { font-size: 12px; padding-left: 3px; font-family: @c-mono; }
  &--wide { width: 68px; }
}

// ── 执行时间预览：紧凑单行 ──
.cron-builder__preview {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 4px 8px;
  margin-top: 10px;
  padding: 6px 8px;
  background: @c-bg;
  border: 1px solid @c-border;
  border-radius: @c-radius;
  font-size: 12px;
}

.cron-builder__preview-dot {
  width: 5px;
  height: 5px;
  border-radius: 50%;
  background: @c-primary;
  flex-shrink: 0;
}

.cron-builder__preview-label {
  color: @c-text-3;
  flex-shrink: 0;
}

.cron-builder__preview-time {
  font-family: @c-mono;
  color: @c-text-2;
}

.cron-builder__preview-empty {
  color: @c-text-3;
}
</style>
