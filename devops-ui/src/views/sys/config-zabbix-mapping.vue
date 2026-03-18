<template>
  <div class="mapping-workbench">
    <div class="mapping-config-pane">
      <el-card shadow="never" class="panel-card mapping-config-card">
        <template #header>
          <div class="panel-card__header">
            <div class="panel-card__heading">
              <div class="panel-card__eyebrow">Workbench</div>
              <div class="panel-card__title">映射规则配置</div>
              <div class="panel-card__desc">先选主机群组，再维护模板、分类和区域规则，最后保存并预览结果。</div>
            </div>
            <el-tag size="small" type="info">{{ zabbixMappingForm.zabbixName || "Zabbix" }}</el-tag>
          </div>
        </template>
        <div class="mapping-metrics">
          <div class="mapping-metric">
            <div class="mapping-metric__label">主机群组</div>
            <div class="mapping-metric__value">{{ mappingStats.hostGroupCount }}</div>
          </div>
          <div class="mapping-metric">
            <div class="mapping-metric__label">模板规则</div>
            <div class="mapping-metric__value">{{ mappingStats.templateRuleCount }}</div>
          </div>
          <div class="mapping-metric">
            <div class="mapping-metric__label">分类规则</div>
            <div class="mapping-metric__value">{{ mappingStats.categoryRuleCount }}</div>
          </div>
          <div class="mapping-metric">
            <div class="mapping-metric__label">区域规则</div>
            <div class="mapping-metric__value">{{ mappingStats.areaRuleCount }}</div>
          </div>
        </div>
        <div class="mapping-hostgroup">
          <div class="mapping-hostgroup__title">主机群组（多选）</div>
          <el-select
            v-model="zabbixMappingForm.selectedHostGroupIds"
            multiple
            collapse-tags
            collapse-tags-tooltip
            filterable
            placeholder="请选择Zabbix主机群组"
            style="width: 100%"
          >
            <el-option
              v-for="item in zabbixMappingOptions.hostGroupOptions"
              :key="item.groupId"
              :label="item.name"
              :value="item.groupId"
            />
          </el-select>
        </div>
        <div class="mapping-actions">
          <el-button :loading="loading.preview" @click="previewZabbixMapping">预览结果</el-button>
          <el-button type="success" :loading="loading.sync" @click="syncZabbixNetworkHosts">同步数据</el-button>
          <el-button type="primary" :loading="loading.save" @click="saveZabbixMapping">保存配置</el-button>
        </div>
      </el-card>

      <el-card shadow="never" class="panel-card mapping-rules-card">
        <div class="mapping-section-head">
          <div class="mapping-section-head__title">规则维护</div>
          <div class="mapping-section-head__desc">按模板、分类和区域三个维度维护映射规则。</div>
        </div>
        <el-tabs v-model="activeRuleTab" class="mapping-rule-tabs" stretch>
          <el-tab-pane label="模板型号" name="template">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll">
                <div v-for="(item, idx) in zabbixMappingForm.templateModelRules" :key="`tm-${idx}`" class="mapping-rule-row">
                  <el-select v-model="item.templateName" filterable placeholder="Zabbix模板">
                    <el-option v-for="v in zabbixMappingOptions.templateOptions" :key="v" :label="v" :value="v" />
                  </el-select>
                  <el-select v-model="item.deviceModel" placeholder="设备型号">
                    <el-option v-for="v in zabbixMappingOptions.deviceModelOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                  </el-select>
                  <el-button link type="danger" @click="removeTemplateModelRule(idx)">删除</el-button>
                </div>
                <div v-if="!zabbixMappingForm.templateModelRules.length" class="rule-empty">暂无规则</div>
              </div>
              <el-button link type="primary" @click="addTemplateModelRule">+ 新增模板规则</el-button>
            </div>
          </el-tab-pane>

          <el-tab-pane label="分类映射" name="category">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll">
                <div v-for="(item, idx) in zabbixMappingForm.categoryGroupRules" :key="`cg-${idx}`" class="mapping-rule-row">
                  <el-input v-model="item.zabbixCategory" placeholder="Zabbix分类，如 网络设备" />
                  <el-select v-model="item.deviceGroup" placeholder="设备分组">
                    <el-option v-for="v in zabbixMappingOptions.deviceGroupOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                  </el-select>
                  <el-button link type="danger" @click="removeCategoryGroupRule(idx)">删除</el-button>
                </div>
                <div v-if="!zabbixMappingForm.categoryGroupRules.length" class="rule-empty">暂无规则</div>
              </div>
              <el-button link type="primary" @click="addCategoryGroupRule">+ 新增分类规则</el-button>
            </div>
          </el-tab-pane>

          <el-tab-pane label="区域关键字" name="area">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll">
                <div v-for="(item, idx) in zabbixMappingForm.areaKeywordRules" :key="`ak-${idx}`" class="mapping-rule-row">
                  <el-input v-model="item.keyword" placeholder="关键字，如 安徽新能源 / 销售公司" />
                  <el-select v-model="item.areaName" filterable placeholder="区域（area_name_type）">
                    <el-option v-for="v in zabbixMappingOptions.areaOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                  </el-select>
                  <el-button link type="danger" @click="removeAreaKeywordRule(idx)">删除</el-button>
                </div>
                <div v-if="!zabbixMappingForm.areaKeywordRules.length" class="rule-empty">暂无规则</div>
              </div>
              <el-button link type="primary" @click="addAreaKeywordRule">+ 新增区域规则</el-button>
            </div>
          </el-tab-pane>
        </el-tabs>
      </el-card>
    </div>

    <div class="mapping-preview-pane">
      <el-card shadow="never" class="panel-card panel-card--muted">
        <template #header>
          <div class="panel-card__header">
            <div class="panel-card__heading">
              <div class="panel-card__eyebrow">Preview</div>
              <div class="panel-card__title">预览结果</div>
              <div class="panel-card__desc">用筛选和仅未映射视图快速定位需要补充的规则。</div>
            </div>
            <div class="preview-header-tags">
              <el-tag size="small" type="info">总计 {{ zabbixMappingPreview.groupList.length }}</el-tag>
              <el-tag v-if="zabbixMappingPreview.unmatchedAreas.length" type="warning" size="small">
                未映射 {{ zabbixMappingPreview.unmatchedAreas.length }}
              </el-tag>
              <el-tag v-else size="small" type="success">已匹配</el-tag>
            </div>
          </div>
        </template>
        <div class="preview-toolbar">
          <el-switch v-model="previewFilter.onlyUnmatched" inline-prompt active-text="仅未映射" inactive-text="全部" />
          <el-input v-model="previewFilter.keyword" placeholder="筛选：群组/分类/区域" clearable />
          <el-button :loading="loading.preview" @click="previewZabbixMapping">刷新结果</el-button>
        </div>
        <div v-if="filteredPreviewRows.length" class="mapping-preview">
          <el-table :data="filteredPreviewRows" :row-class-name="previewRowClassName" size="small" border height="100%">
            <el-table-column prop="groupName" label="主机群组" min-width="260" />
            <el-table-column prop="zabbixCategory" label="分类" width="120" />
            <el-table-column prop="rawAreaSegment" label="原区域" min-width="200" />
            <el-table-column prop="matchedAreaName" label="映射区域" width="140" />
          </el-table>
        </div>
        <div v-else class="info-list">
          <div class="info-list__item">暂无可显示数据，请调整筛选条件或点击“刷新结果”。</div>
        </div>
        <div v-if="zabbixMappingPreview.unmatchedAreas.length" class="unmatched-list">
          未映射区域：{{ zabbixMappingPreview.unmatchedAreas.join("、") }}
        </div>
      </el-card>
    </div>
  </div>
</template>

<script lang="ts" setup>
import { computed, onMounted, reactive, ref } from "vue";
import baseService from "@/service/baseService";
import { ElMessage, ElMessageBox } from "element-plus";
import { MESSAGE_DURATION, assignConfig, useLoadingState } from "./config-helpers";

const zabbixMappingForm = reactive({
  zabbixName: "Zabbix",
  selectedHostGroupIds: [] as string[],
  templateModelRules: [] as Array<{ templateName: string; deviceModel: string }>,
  categoryGroupRules: [] as Array<{ zabbixCategory: string; deviceGroup: string }>,
  areaKeywordRules: [] as Array<{ keyword: string; areaName: string }>
});

const zabbixMappingOptions = reactive({
  zabbixName: "Zabbix",
  templateOptions: [] as string[],
  hostGroupOptions: [] as Array<{ groupId: string; name: string }>,
  areaOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceGroupOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceModelOptions: [] as Array<{ dictLabel: string; dictValue: string }>
});

const zabbixMappingPreview = reactive({
  groupList: [] as Array<{
    groupId: string;
    groupName: string;
    zabbixCategory: string;
    rawAreaSegment: string;
    normalizedAreaKeyword: string;
    matchedAreaName: string;
    matched: number;
  }>,
  unmatchedAreas: [] as string[]
});

const activeRuleTab = ref("area");

const previewFilter = reactive({
  onlyUnmatched: false,
  keyword: ""
});

const { loading, withLoading } = useLoadingState({
  options: false,
  save: false,
  preview: false,
  sync: false
});

const zabbixMappingDefaults = {
  zabbixName: "Zabbix",
  selectedHostGroupIds: [] as string[],
  templateModelRules: [] as Array<{ templateName: string; deviceModel: string }>,
  categoryGroupRules: [] as Array<{ zabbixCategory: string; deviceGroup: string }>,
  areaKeywordRules: [] as Array<{ keyword: string; areaName: string }>
};

const zabbixMappingOptionsDefaults = {
  zabbixName: "Zabbix",
  templateOptions: [] as string[],
  hostGroupOptions: [] as Array<{ groupId: string; name: string }>,
  areaOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceGroupOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceModelOptions: [] as Array<{ dictLabel: string; dictValue: string }>
};

const zabbixMappingPreviewDefaults = {
  groupList: [] as Array<{
    groupId: string;
    groupName: string;
    zabbixCategory: string;
    rawAreaSegment: string;
    normalizedAreaKeyword: string;
    matchedAreaName: string;
    matched: number;
  }>,
  unmatchedAreas: [] as string[]
};

const mappingStats = computed(() => ({
  hostGroupCount: zabbixMappingForm.selectedHostGroupIds.length,
  templateRuleCount: zabbixMappingForm.templateModelRules.length,
  categoryRuleCount: zabbixMappingForm.categoryGroupRules.length,
  areaRuleCount: zabbixMappingForm.areaKeywordRules.length
}));

const filteredPreviewRows = computed(() => {
  const keyword = previewFilter.keyword.trim();
  return zabbixMappingPreview.groupList.filter((row) => {
    if (previewFilter.onlyUnmatched && Number(row.matched) === 1) return false;
    if (!keyword) return true;
    return [row.groupName, row.zabbixCategory, row.rawAreaSegment, row.matchedAreaName].some((v) =>
      String(v || "").includes(keyword)
    );
  });
});

const previewRowClassName = ({ row }: { row: { matched: number } }) =>
  Number(row.matched) === 1 ? "" : "preview-row--unmatched";

const loadZabbixMapping = () => {
  baseService.get("/sys/config-center/zabbix/network-device-mapping").then((res) => {
    assignConfig(zabbixMappingForm, zabbixMappingDefaults, res.data);
    if (zabbixMappingForm.selectedHostGroupIds.length) {
      previewZabbixMapping(false);
    }
  });
};

const loadZabbixMappingOptions = () => {
  withLoading("options", () =>
    baseService.get("/sys/config-center/zabbix/network-device-mapping/options").then((res) => {
      assignConfig(zabbixMappingOptions, zabbixMappingOptionsDefaults, res.data);
      if (!zabbixMappingForm.zabbixName) {
        zabbixMappingForm.zabbixName = zabbixMappingOptions.zabbixName || "Zabbix";
      }
    })
  );
};

const previewZabbixMapping = (showMessage = true) => {
  return withLoading("preview", () =>
    baseService.post("/sys/config-center/zabbix/network-device-mapping/preview", {
      selectedHostGroupIds: [...zabbixMappingForm.selectedHostGroupIds],
      categoryGroupRules: zabbixMappingForm.categoryGroupRules.map((item) => ({ ...item })),
      areaKeywordRules: zabbixMappingForm.areaKeywordRules.map((item) => ({ ...item }))
    })
    .then((res) => {
      assignConfig(zabbixMappingPreview, zabbixMappingPreviewDefaults, res.data);
      if (showMessage) {
        if (!zabbixMappingPreview.groupList.length) {
          ElMessage.info({ message: "预览结果已更新，当前没有可显示的数据", duration: MESSAGE_DURATION.info });
          return;
        }
        ElMessage.success({
          message: `预览结果已更新，共 ${zabbixMappingPreview.groupList.length} 条，未映射 ${zabbixMappingPreview.unmatchedAreas.length} 条`,
          duration: MESSAGE_DURATION.success
        });
      }
    })
  );
};

const saveZabbixMapping = () => {
  return withLoading("save", () =>
    baseService.put("/sys/config-center/zabbix/network-device-mapping", {
      zabbixName: zabbixMappingForm.zabbixName,
      selectedHostGroupIds: [...zabbixMappingForm.selectedHostGroupIds],
      templateModelRules: zabbixMappingForm.templateModelRules.map((item) => ({ ...item })),
      categoryGroupRules: zabbixMappingForm.categoryGroupRules.map((item) => ({ ...item })),
      areaKeywordRules: zabbixMappingForm.areaKeywordRules.map((item) => ({ ...item }))
    })
    .then(() => {
      ElMessage.success({ message: "配置已保存", duration: MESSAGE_DURATION.success });
      previewZabbixMapping();
    })
  );
};

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
  return withLoading("sync", () =>
    baseService.post("/sys/config-center/zabbix/network-device-mapping/sync").then((res) => {
      const data = res.data || {};
      if (Number(data.syncSuccess) === 0) {
        ElMessage.warning({ message: data.message || "数据同步未执行", duration: MESSAGE_DURATION.warning });
        return;
      }
      ElMessage.success({
        message: `数据同步完成：拉取${data.totalFetched ?? 0}，新增${data.inserted ?? 0}，更新${data.updated ?? 0}，逻辑删除(禁用)${data.logicalDeleted ?? data.disabled ?? 0}，跳过${data.skipped ?? 0}`,
        duration: MESSAGE_DURATION.success
      });
      if (Array.isArray(data.unmatchedAreas) && data.unmatchedAreas.length) {
        zabbixMappingPreview.unmatchedAreas = [...data.unmatchedAreas];
      }
    })
  );
};

const addAreaKeywordRule = () => zabbixMappingForm.areaKeywordRules.push({ keyword: "", areaName: "" });
const removeAreaKeywordRule = (idx: number) => zabbixMappingForm.areaKeywordRules.splice(idx, 1);
const addTemplateModelRule = () => zabbixMappingForm.templateModelRules.push({ templateName: "", deviceModel: "" });
const removeTemplateModelRule = (idx: number) => zabbixMappingForm.templateModelRules.splice(idx, 1);
const addCategoryGroupRule = () => zabbixMappingForm.categoryGroupRules.push({ zabbixCategory: "", deviceGroup: "" });
const removeCategoryGroupRule = (idx: number) => zabbixMappingForm.categoryGroupRules.splice(idx, 1);

onMounted(() => {
  loadZabbixMapping();
  loadZabbixMappingOptions();
});
</script>

<style scoped>
@import "./config-shared.css";

.mapping-workbench {
  display: grid;
  grid-template-columns: minmax(320px, 360px) minmax(0, 1fr);
  gap: 16px;
  align-items: stretch;
}

.mapping-config-pane,
.mapping-preview-pane {
  display: grid;
  gap: 16px;
  height: 100%;
}

.mapping-config-card {
  position: sticky;
  top: 0;
}

.mapping-rules-card :deep(.el-card__body) {
  padding-top: 10px;
}

.mapping-section-head {
  padding-bottom: 10px;
  margin-bottom: 10px;
  border-bottom: 1px solid #e2e8f0;
}

.mapping-section-head__title {
  font-size: 14px;
  font-weight: 700;
  color: #0f172a;
}

.mapping-section-head__desc {
  margin-top: 4px;
  font-size: 12px;
  color: #64748b;
  line-height: 1.5;
}

.mapping-metrics {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 8px;
  margin-bottom: 10px;
}

.mapping-metric {
  border: 1px solid #dbeafe;
  border-radius: 8px;
  padding: 10px;
  background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
}

.mapping-metric__label {
  color: #64748b;
  font-size: 12px;
}

.mapping-metric__value {
  color: #0f172a;
  font-size: 18px;
  font-weight: 700;
  line-height: 1.2;
  margin-top: 2px;
}

.mapping-actions {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
  margin-top: 12px;
  padding-top: 12px;
  border-top: 1px solid #e2e8f0;
}

.mapping-actions :deep(.el-button) {
  min-width: 96px;
}

.mapping-hostgroup {
  margin-top: 4px;
}

.mapping-hostgroup__title {
  font-size: 12px;
  font-weight: 700;
  color: #475569;
  margin-bottom: 6px;
}

.rule-empty {
  border-radius: 10px;
  border: 1px dashed #cbd5e1;
  background: #f8fafc;
  padding: 10px;
  color: #64748b;
  font-size: 12px;
}

.mapping-rule-row {
  display: grid;
  grid-template-columns: 1fr 1fr auto;
  gap: 8px;
  align-items: center;
}

.preview-header-tags {
  display: flex;
  gap: 6px;
  font-size: 13px;
  flex-wrap: wrap;
}

.preview-toolbar {
  display: grid;
  grid-template-columns: 120px minmax(260px, 1fr) 112px;
  gap: 10px;
  margin-bottom: 12px;
  padding: 10px;
  border-radius: 10px;
  background: rgba(255, 255, 255, 0.72);
  border: 1px solid #e2e8f0;
  flex-shrink: 0;
  font-size: 14px;
}

.mapping-preview {
  flex: 1;
  min-height: 0;
}

.mapping-preview :deep(.el-table) {
  font-size: 14px;
}

.mapping-preview :deep(.el-table th) {
  font-size: 13px;
  color: #334155;
  background: #f8fafc;
}

.mapping-preview :deep(.el-table td),
.mapping-preview :deep(.el-table th) {
  padding: 6px 0;
}

.mapping-preview-pane :deep(.el-card) {
  height: 100%;
}

.mapping-preview-pane :deep(.el-card__body) {
  height: calc(100% - 2px);
  display: flex;
  flex-direction: column;
}

.unmatched-list {
  margin-top: 8px;
  padding: 10px 12px;
  border-radius: 10px;
  background: #fffbeb;
  border: 1px solid #fde68a;
  color: #b45309;
  font-size: 12px;
  line-height: 1.5;
}

.mapping-rule-tabs :deep(.el-tabs__header) {
  margin-bottom: 8px;
}

.mapping-rule-tabs :deep(.el-tabs__item) {
  font-size: 13px;
  font-weight: 700;
}

.mapping-rule-tabs :deep(.el-tabs__content) {
  height: 240px;
}

.mapping-rule-tabs :deep(.el-tab-pane) {
  height: 100%;
}

.mapping-rule-list {
  height: 100%;
  display: grid;
  grid-template-rows: minmax(0, 1fr) auto;
  gap: 8px;
}

.mapping-rule-scroll {
  min-height: 0;
  max-height: calc(3 * 44px + 2 * 8px);
  overflow: auto;
  padding-right: 4px;
}

.mapping-preview :deep(.preview-row--unmatched > td) {
  background: #fffbeb;
}
</style>
