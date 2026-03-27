<template>
  <el-form-item :label="label" class="diff-form-item" label-width="120px">
    <div v-if="hasDiff" class="diff-changed">
      <span class="diff-old" :title="String(displayBefore)">{{ displayBefore }}</span>
      <span class="diff-arrow"><svg viewBox="0 0 1024 1024" width="14" height="14" xmlns="http://www.w3.org/2000/svg"><path fill="currentColor" d="M754.752 480H160a32 32 0 1 0 0 64h594.752L521.344 777.344a32 32 0 0 0 45.312 45.312l288-288a32 32 0 0 0 0-45.312l-288-288a32 32 0 1 0-45.312 45.312L754.752 480z"></path></svg></span>
      <span class="diff-new" :title="String(displayAfter)">{{ displayAfter }}</span>
    </div>
    <div v-else class="diff-unchanged">
      {{ displayAfter }}
    </div>
  </el-form-item>
</template>

<script lang="ts" setup>
import { computed } from 'vue';

const props = defineProps({
  label: String,
  b: Object,
  a: Object,
  field: { type: String, required: true },
  type: { type: String, default: 'text' }
});

const getPathVal = (obj: any, path: string) => {
  if (!obj) return undefined;
  return path.split('.').reduce((o, i) => o?.[i], obj);
};

const formatVal = (val: any) => {
  if (val === null || val === undefined || val === '') return '-';
  if (props.type === 'bool') return val === 1 || val === true ? '是' : '否';
  if (typeof val === 'object') return JSON.stringify(val);
  return val;
};

const displayBefore = computed(() => formatVal(getPathVal(props.b, props.field)));
const displayAfter = computed(() => formatVal(getPathVal(props.a, props.field)));

const hasDiff = computed(() => {
  const bVal = getPathVal(props.b, props.field);
  const aVal = getPathVal(props.a, props.field);
  return bVal !== aVal;
});
</script>

<style scoped>
.diff-form-item {
  margin-bottom: 20px;
}
.diff-changed {
  display: inline-flex;
  align-items: center;
  gap: 12px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  padding: 4px 14px;
  border-radius: 6px;
  line-height: 1.5;
  word-break: break-all;
}
.diff-old {
  color: #ef4444;
  text-decoration: line-through;
  opacity: 0.8;
  max-width: 300px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.diff-arrow {
  color: #94a3b8;
  display: flex;
  align-items: center;
}
.diff-new {
  color: #10b981;
  font-weight: 600;
  max-width: 300px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}
.diff-unchanged {
  color: #334155;
  padding-left: 2px;
  word-break: break-all;
}
</style>
