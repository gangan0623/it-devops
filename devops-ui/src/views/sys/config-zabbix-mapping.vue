<template>
  <div class="mapping-workbench-v2">
    <!-- Top Bar: Global Workbench Config -->
    <el-card shadow="never" class="panel-card workbench-top-card">
      <div class="workbench-top-layout">
        <!-- 1. Info Area -->
        <div class="wt-info">
          <div class="panel-card__eyebrow">Zabbix Mapping Workbench</div>
          <div class="panel-card__title">
            映射规则配置
            <span class="zabbix-badge-inline">
              <span class="zabbix-badge-dot"></span>
              {{ zabbixMappingForm.zabbixName || "Zabbix" }}
            </span>
          </div>
          <div class="panel-card__desc">配置主机提取与设备映射策略。先确立范围及规则，最后预览并数据同步。</div>
        </div>

        <!-- 2. Metrics Horizontal -->
        <div class="wt-metrics">
          <div class="mini-metric">
            <div class="mini-metric__label">选用群组</div>
            <div class="mini-metric__val text-blue">{{ mappingStats.hostGroupCount }}</div>
          </div>
          <div class="mini-metric">
            <div class="mini-metric__label">模板规则</div>
            <div class="mini-metric__val text-indigo">{{ mappingStats.templateRuleCount }}</div>
          </div>
          <div class="mini-metric">
            <div class="mini-metric__label">分类规则</div>
            <div class="mini-metric__val text-emerald">{{ mappingStats.categoryRuleCount }}</div>
          </div>
          <div class="mini-metric">
            <div class="mini-metric__label">区域规则</div>
            <div class="mini-metric__val text-amber">{{ mappingStats.areaRuleCount }}</div>
          </div>
        </div>

        <!-- 3. Host Group & Actions -->
        <div class="wt-controls">
          <div class="wt-hostgroup">
            <div class="wt-hostgroup__label">同步范围 (主机群组)</div>
            <el-select
              v-model="zabbixMappingForm.selectedHostGroupIds"
              multiple
              collapse-tags
              collapse-tags-tooltip
              filterable
              placeholder="请选择 Zabbix 主机群组..."
              class="premium-select"
            >
              <el-option
                v-for="item in zabbixMappingOptions.hostGroupOptions"
                :key="item.groupId"
                :label="item.name"
                :value="item.groupId"
              />
            </el-select>
          </div>
          <div class="wt-actions">
            <el-button class="btn-modern btn-modern--default" :loading="loading.preview" @click="previewZabbixMapping">
              预览结果
            </el-button>
            <el-button class="btn-modern btn-modern--success" :loading="loading.sync" @click="syncZabbixNetworkHosts">
              同步数据
            </el-button>
            <el-button class="btn-modern btn-modern--primary" :class="{'btn-save-pulse': loading.save}" :loading="loading.save" @click="saveZabbixMapping">
              保存配置
            </el-button>
          </div>
        </div>
      </div>
    </el-card>

    <!-- Main Body: Rules (Left) and Preview (Right) -->
    <div class="mapping-main-body">
      
      <!-- 规则维护卡片 -->
      <el-card shadow="never" class="panel-card mapping-rules-card">
        <div class="mapping-section-head">
          <div class="mapping-section-head__title">规则维护矩阵</div>
          <div class="mapping-section-head__desc">按模板、分类和区域三个维度精确维护自动化映射规则。</div>
        </div>
        
        <el-tabs v-model="activeRuleTab" class="premium-tabs" stretch>
          <el-tab-pane label="模板型号" name="template">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll custom-scrollbar">
                <TransitionGroup name="list" tag="div" class="rule-transition-group">
                  <div v-for="(item, idx) in zabbixMappingForm.templateModelRules" :key="`tm-${idx}`" class="modern-rule-row">
                    <div class="rule-idx">{{ idx + 1 }}</div>
                    <el-select v-model="item.templateName" filterable placeholder="选择 Zabbix 模板" class="rule-input">
                      <el-option v-for="v in zabbixMappingOptions.templateOptions" :key="v" :label="v" :value="v" />
                    </el-select>
                    <i class="el-icon-right rule-arrow">→</i>
                    <el-select v-model="item.deviceModel" placeholder="映射至备型号" class="rule-input">
                      <el-option v-for="v in zabbixMappingOptions.deviceModelOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                    </el-select>
                    <div class="rule-action">
                      <el-button circle plain type="danger" size="small" @click="removeTemplateModelRule(idx)">
                        <template #icon>
                          <svg viewBox="0 0 1024 1024" xmlns="http://www.w3.org/2000/svg"><path fill="currentColor" d="M160 256H96a32 32 0 0 1 0-64h256V95.936a32 32 0 0 1 32-32h256a32 32 0 0 1 32 32V192h256a32 32 0 1 1 0 64h-64v672a96 96 0 0 1-96 96H256a96 96 0 0 1-96-96V256zm224-64h256V128H384v64zm-64 128v576h384V320H320z"></path></svg>
                        </template>
                      </el-button>
                    </div>
                  </div>
                </TransitionGroup>
                <div v-if="!zabbixMappingForm.templateModelRules.length" class="empty-state-modern">
                  <div class="empty-icon">📂</div>
                  <div>当前维度暂无规则，点击下方添加</div>
                </div>
              </div>
              <div class="rule-add-bar" @click="addTemplateModelRule">
                <span class="add-icon">+</span> 新增模板规则
              </div>
            </div>
          </el-tab-pane>

          <el-tab-pane label="分类映射" name="category">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll custom-scrollbar">
                <TransitionGroup name="list" tag="div" class="rule-transition-group">
                  <div v-for="(item, idx) in zabbixMappingForm.categoryGroupRules" :key="`cg-${idx}`" class="modern-rule-row">
                    <div class="rule-idx">{{ idx + 1 }}</div>
                    <el-input v-model="item.zabbixCategory" placeholder="原分类名称" class="rule-input" />
                    <i class="el-icon-right rule-arrow">→</i>
                    <el-select v-model="item.deviceGroup" placeholder="映射至备分组" class="rule-input">
                      <el-option v-for="v in zabbixMappingOptions.deviceGroupOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                    </el-select>
                    <div class="rule-action">
                      <el-button circle plain type="danger" size="small" @click="removeCategoryGroupRule(idx)">
                        <template #icon>
                          <svg viewBox="0 0 1024 1024" xmlns="http://www.w3.org/2000/svg"><path fill="currentColor" d="M160 256H96a32 32 0 0 1 0-64h256V95.936a32 32 0 0 1 32-32h256a32 32 0 0 1 32 32V192h256a32 32 0 1 1 0 64h-64v672a96 96 0 0 1-96 96H256a96 96 0 0 1-96-96V256zm224-64h256V128H384v64zm-64 128v576h384V320H320z"></path></svg>
                        </template>
                      </el-button>
                    </div>
                  </div>
                </TransitionGroup>
                <div v-if="!zabbixMappingForm.categoryGroupRules.length" class="empty-state-modern">
                  <div class="empty-icon">🗂️</div>
                  <div>当前维度暂无规则，点击下方添加</div>
                </div>
              </div>
              <div class="rule-add-bar" @click="addCategoryGroupRule">
                <span class="add-icon">+</span> 新增分类规则
              </div>
            </div>
          </el-tab-pane>

          <el-tab-pane label="区域关键字" name="area">
            <div class="mapping-rule-list">
              <div class="mapping-rule-scroll custom-scrollbar">
                <TransitionGroup name="list" tag="div" class="rule-transition-group">
                  <div v-for="(item, idx) in zabbixMappingForm.areaKeywordRules" :key="`ak-${idx}`" class="modern-rule-row">
                    <div class="rule-idx">{{ idx + 1 }}</div>
                    <el-input v-model="item.keyword" placeholder="区域关键字 (如: 新能源)" class="rule-input" />
                    <i class="el-icon-right rule-arrow">→</i>
                    <el-select v-model="item.areaName" filterable placeholder="映射至标准区域" class="rule-input">
                      <el-option v-for="v in zabbixMappingOptions.areaOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                    </el-select>
                    <div class="rule-action">
                      <el-button circle plain type="danger" size="small" @click="removeAreaKeywordRule(idx)">
                        <template #icon>
                          <svg viewBox="0 0 1024 1024" xmlns="http://www.w3.org/2000/svg"><path fill="currentColor" d="M160 256H96a32 32 0 0 1 0-64h256V95.936a32 32 0 0 1 32-32h256a32 32 0 0 1 32 32V192h256a32 32 0 1 1 0 64h-64v672a96 96 0 0 1-96 96H256a96 96 0 0 1-96-96V256zm224-64h256V128H384v64zm-64 128v576h384V320H320z"></path></svg>
                        </template>
                      </el-button>
                    </div>
                  </div>
                </TransitionGroup>
                <div v-if="!zabbixMappingForm.areaKeywordRules.length" class="empty-state-modern">
                  <div class="empty-icon">📍</div>
                  <div>当前维度暂无规则，点击下方添加</div>
                </div>
              </div>
              <div class="rule-add-bar" @click="addAreaKeywordRule">
                <span class="add-icon">+</span> 新增区域规则
              </div>
            </div>
          </el-tab-pane>
        </el-tabs>
      </el-card>

      <!-- 右侧：预览与洞察区 -->
      <el-card shadow="never" class="panel-card panel-card--preview">
        <template #header>
          <div class="panel-card__header">
            <div class="panel-card__heading">
              <div class="panel-card__eyebrow">Insight & Preview</div>
              <div class="panel-card__title">映射结果预览</div>
              <div class="panel-card__desc">实时评估当前主机群组被规则解析命中的情况，及时发现漏配项。</div>
            </div>
            <div class="preview-stats">
              <div class="preview-stat-item">
                <span class="stat-lbl">总范围</span>
                <span class="stat-val text-brand">{{ zabbixMappingPreview.groupList.length }}</span>
              </div>
              <div class="preview-stat-item">
                <span class="stat-lbl">未命中</span>
                <span class="stat-val" :class="zabbixMappingPreview.unmatchedAreas.length ? 'text-danger' : 'text-success'">
                  {{ zabbixMappingPreview.unmatchedAreas.length }}
                </span>
              </div>
            </div>
          </div>
        </template>
        
        <div class="preview-glass-toolbar">
          <div class="toolbar-left">
            <el-switch v-model="previewFilter.onlyUnmatched" active-color="#f59e0b" inline-prompt active-text="仅看未映射" inactive-text="全部数据" />
          </div>
          <div class="toolbar-center">
            <el-input v-model="previewFilter.keyword" placeholder="关键词搜索 (主机/分类/区域...)" prefix-icon="Search" class="search-input" clearable />
          </div>
          <div class="toolbar-right">
            <el-button class="btn-modern btn-modern--ghost" :loading="loading.preview" @click="previewZabbixMapping">刷新视图</el-button>
          </div>
        </div>

        <!-- 异常提醒横幅 -->
        <Transition name="fade-slide">
          <div v-if="zabbixMappingPreview.unmatchedAreas.length" class="modern-alert-banner">
            <div class="alert-icon">⚠️</div>
            <div class="alert-content">
              <strong>检测到未完全映射隔离的区域：</strong>
              <span>{{ zabbixMappingPreview.unmatchedAreas.join("，") }}</span>
            </div>
            <div class="alert-action">请在左侧补充规则</div>
          </div>
        </Transition>
        
        <div v-if="filteredPreviewRows.length" class="mapping-preview-table-wrap custom-scrollbar">
          <el-table :data="filteredPreviewRows" :row-class-name="previewRowClassName" :cell-class-name="previewCellClassName" size="medium" style="width: 100%" height="100%" class="premium-table">
            <el-table-column prop="groupName" label="Zabbix 主机群组" min-width="260">
              <template #default="{ row }">
                <div class="group-name-cell">
                   <div class="cell-icon">🖥️</div>
                   <span>{{ row.groupName }}</span>
                </div>
              </template>
            </el-table-column>
            <el-table-column prop="zabbixCategory" label="提取分类" width="130" show-overflow-tooltip>
              <template #default="{ row }">
                <el-tag size="small" type="info" class="modern-tag tag-category" disable-transitions v-if="row.zabbixCategory">{{ row.zabbixCategory }}</el-tag>
                <span v-else class="empty-dash">-</span>
              </template>
            </el-table-column>
            <el-table-column prop="rawAreaSegment" label="原文字段(区域)" min-width="180" show-overflow-tooltip>
              <template #default="{ row }">
                 <span class="raw-text">{{ row.rawAreaSegment || '-' }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="matchedAreaName" label="最终映射区域" width="160">
              <template #default="{ row }">
                <el-tag v-if="row.matched == 1" size="small" type="success" class="modern-tag tag-success" disable-transitions>
                  <span class="dot"></span> {{ row.matchedAreaName || '默认' }}
                </el-tag>
                <el-tag v-else size="small" type="warning" class="modern-tag tag-warning" disable-transitions>
                  未适配
                </el-tag>
              </template>
            </el-table-column>
          </el-table>
        </div>
        <div v-else class="preview-empty-state">
           <div class="radar-scan">
              <div class="radar-beam"></div>
           </div>
           <h3>视图范围内暂无数据</h3>
           <p>请调整过滤条件，或者点击“刷新视图”。</p>
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

const activeRuleTab = ref("template");

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
  const keyword = previewFilter.keyword.trim().toLowerCase();
  return zabbixMappingPreview.groupList.filter((row) => {
    if (previewFilter.onlyUnmatched && Number(row.matched) === 1) return false;
    if (!keyword) return true;
    return [row.groupName, row.zabbixCategory, row.rawAreaSegment, row.matchedAreaName].some((v) =>
      String(v || "").toLowerCase().includes(keyword)
    );
  });
});

const previewRowClassName = ({ row }: { row: { matched: number } }) =>
  Number(row.matched) === 1 ? "preview-row-matched" : "preview-row-unmatched";

const previewCellClassName = ({ columnIndex }: { columnIndex: number }) => {
  return columnIndex === 0 ? "fw-bold" : "";
}

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
          ElMessage.info({ message: "解析完成：暂无可映射的范围数据", duration: MESSAGE_DURATION.info });
          return;
        }
        ElMessage.success({
          message: `视图已更新 (总命中率: ${Math.round(((zabbixMappingPreview.groupList.length - zabbixMappingPreview.unmatchedAreas.length)/Math.max(1, zabbixMappingPreview.groupList.length))*100)}%)`,
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
      ElMessage.success({ message: "规则策略已持久化保存 ✨", duration: MESSAGE_DURATION.success });
      previewZabbixMapping(false);
    })
  );
};

const syncZabbixNetworkHosts = async () => {
  try {
    await ElMessageBox.confirm(
      "执行同步将会立刻连接至 Zabbix 并拉取解析最新主机数据。耗时取决于主机数，确认前往拉取？",
      "开启同步任务",
      { confirmButtonText: "确认，开始同步", cancelButtonText: "取消", type: "warning", customClass: "premium-msgbox" }
    );
  } catch {
    return;
  }
  return withLoading("sync", () =>
    baseService.post("/sys/config-center/zabbix/network-device-mapping/sync").then((res) => {
      const data = res.data || {};
      if (Number(data.syncSuccess) === 0) {
        ElMessage.warning({ message: data.message || "请求被拒绝或网络异常", duration: MESSAGE_DURATION.warning });
        return;
      }
      ElMessage.success({
        message: `同步任务达标！共探测 ${data.totalFetched ?? 0} 台，新增写入 ${data.inserted ?? 0} 台。`,
        duration: 4000
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
@import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&display=swap');

.mapping-workbench-v2 {
  font-family: 'Inter', -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif;
  display: flex;
  flex-direction: column;
  gap: 20px;
  height: calc(100vh - 120px);
  padding: 4px;
}

/* -------------------- Top Horizontal Workbench -------------------- */
.workbench-top-card {
  flex-shrink: 0;
  background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);
}

.workbench-top-layout {
  display: flex;
  align-items: flex-start;
  gap: 24px;
  flex-wrap: wrap;
}

.wt-info {
  flex: 0 0 280px;
  display: flex;
  flex-direction: column;
}

.wt-info .panel-card__eyebrow {
  color: #3b82f6;
  font-weight: 700;
  letter-spacing: 0.1em;
  margin-bottom: 6px;
}

.wt-info .panel-card__title {
  display: flex;
  align-items: center;
  gap: 8px;
}

.zabbix-badge-inline {
  display: inline-flex;
  align-items: center;
  gap: 6px;
  padding: 4px 10px;
  background: #f1f5f9;
  border-radius: 20px;
  font-size: 11px;
  font-weight: 600;
  color: #0f172a;
  border: 1px solid #e2e8f0;
}

.zabbix-badge-dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  background-color: #d92635; /* Zabbix Redish */
}

.wt-metrics {
  flex: 0 0 340px;
  display: grid;
  grid-template-columns: repeat(2, 1fr);
  gap: 12px;
}

.mini-metric {
  background: white;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 10px 14px;
  display: flex;
  flex-direction: column;
  transition: all 0.2s ease;
}
.mini-metric:hover {
  transform: translateY(-2px);
  box-shadow: 0 4px 12px rgba(0,0,0,0.04);
}
.mini-metric__label {
  font-size: 12px;
  color: #64748b;
  font-weight: 500;
}
.mini-metric__val {
  font-size: 20px;
  font-weight: 800;
  margin-top: 2px;
  font-variant-numeric: tabular-nums;
}
.text-blue { color: #3b82f6; }
.text-indigo { color: #6366f1; }
.text-emerald { color: #10b981; }
.text-amber { color: #f59e0b; }

.wt-controls {
  flex: 1;
  min-width: 300px;
  display: flex;
  flex-direction: column;
  gap: 12px;
  align-self: center;
}

.wt-hostgroup__label {
  font-size: 13px;
  font-weight: 600;
  color: #475569;
  margin-bottom: 6px;
}

.wt-actions {
  display: flex;
  align-items: center;
  gap: 10px;
}

.premium-select :deep(.el-input__wrapper) {
  border-radius: 8px;
  box-shadow: 0 1px 2px 0 rgba(0,0,0,0.05);
  background: white;
}

/* -------------------- Main Body Split -------------------- */
.mapping-main-body {
  flex: 1;
  display: grid;
  grid-template-columns: minmax(400px, 1fr) minmax(500px, 1.4fr);
  gap: 20px;
  min-height: 0; 
}

/* Rules Tab overrides */
.mapping-rules-card, .panel-card--preview {
  display: flex;
  flex-direction: column;
  align-self: stretch;
}

.mapping-rules-card :deep(.el-card__body) {
  padding: 16px 20px;
  flex: 1;
  display: flex;
  flex-direction: column;
  min-height: 0;
}

.mapping-section-head {
  padding-bottom: 6px;
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
}

.premium-tabs {
  flex: 1;
  display: flex;
  flex-direction: column;
}

.premium-tabs :deep(.el-tabs__header) {
  margin-bottom: 16px;
  border-bottom: 2px solid #f1f5f9;
}
.premium-tabs :deep(.el-tabs__nav-wrap::after) {
  display: none;
}
.premium-tabs :deep(.el-tabs__item) {
  font-size: 14px;
  color: #64748b;
  font-weight: 600;
  border-radius: 8px 8px 0 0;
  transition: all 0.3s;
}
.premium-tabs :deep(.el-tabs__item.is-active) {
  color: #3b82f6;
}
.premium-tabs :deep(.el-tabs__active-bar) {
  height: 3px;
  border-radius: 3px 3px 0 0;
  background-color: #3b82f6;
}

.premium-tabs :deep(.el-tabs__content) {
  flex: 1;
  overflow: hidden;
}
.premium-tabs :deep(.el-tab-pane) {
  height: 100%;
}

.mapping-rule-list {
  display: flex;
  flex-direction: column;
  height: 100%;
  gap: 12px;
}

.mapping-rule-scroll {
  flex: 1;
  overflow-y: auto;
  padding-right: 8px;
}

.custom-scrollbar::-webkit-scrollbar { width: 6px; height: 6px; }
.custom-scrollbar::-webkit-scrollbar-track { background: transparent; }
.custom-scrollbar::-webkit-scrollbar-thumb { background: #cbd5e1; border-radius: 10px; }
.custom-scrollbar::-webkit-scrollbar-thumb:hover { background: #94a3b8; }

.modern-rule-row {
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 10px 12px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  margin-bottom: 8px;
  transition: all 0.2s ease;
}
.modern-rule-row:hover {
  background: #ffffff;
  border-color: #cbd5e1;
  box-shadow: 0 4px 12px rgba(0,0,0,0.03);
  transform: translateY(-1px);
}

.rule-idx {
  font-size: 11px;
  font-weight: 800;
  color: #94a3b8;
  background: #e2e8f0;
  border-radius: 50%;
  width: 20px;
  height: 20px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.rule-input { flex: 1; }
.rule-input :deep(.el-input__wrapper) {
  box-shadow: 0 1px 2px rgba(0,0,0,0.02);
  background: white;
}

.rule-arrow { color: #94a3b8; font-weight: bold; }
.rule-action { flex-shrink: 0; width: 32px; display: flex; justify-content: center; }

/* Empty States */
.empty-state-modern {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  padding: 24px;
  background: repeating-linear-gradient(-45deg, #f8fafc, #f8fafc 10px, #ffffff 10px, #ffffff 20px);
  border: 2px dashed #e2e8f0;
  border-radius: 12px;
  color: #64748b;
  font-size: 13px;
  font-weight: 500;
}
.empty-icon { font-size: 24px; margin-bottom: 8px; opacity: 0.8; }

.rule-add-bar {
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 12px;
  background: #f0fdf4;
  border: 1px dashed #86efac;
  color: #166534;
  font-size: 14px;
  font-weight: 600;
  border-radius: 8px;
  cursor: pointer;
  transition: all 0.2s;
  flex-shrink: 0;
}
.rule-add-bar:hover { background: #dcfce7; border-color: #22c55e; }
.add-icon { font-size: 18px; margin-right: 6px; font-weight: bold; }

/* -------------------- Modern Buttons -------------------- */
.btn-modern {
  border-radius: 8px;
  font-weight: 600;
  padding: 0 20px;
  height: 36px;
  transition: all 0.2s ease;
  border: none;
  font-size: 13px;
}
.btn-modern:active { transform: scale(0.96); }

.btn-modern--default { background: white; border: 1px solid #cbd5e1; color: #334155; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }
.btn-modern--default:hover { background: #f8fafc; border-color: #94a3b8; color: #0f172a; }

.btn-modern--ghost { background: rgba(59,130,246,0.1); color: #2563eb; border: 1px solid rgba(59,130,246,0.3); }
.btn-modern--ghost:hover { background: rgba(59,130,246,0.2); }

.btn-modern--success { background: #10b981; color: white; box-shadow: 0 4px 12px rgba(16, 185, 129, 0.25); }
.btn-modern--success:hover { background: #059669; box-shadow: 0 6px 16px rgba(16, 185, 129, 0.35); }

.btn-modern--primary { background: linear-gradient(135deg, #2563eb 0%, #3b82f6 100%); color: white; box-shadow: 0 4px 12px rgba(37, 99, 235, 0.3); }
.btn-modern--primary:hover { box-shadow: 0 8px 20px rgba(37, 99, 235, 0.4); }

/* -------------------- Preview Area -------------------- */
.panel-card--preview { background: #ffffff; border-color: #e2e8f0; }
.panel-card--preview :deep(.el-card__body) {
  padding: 16px 20px 20px;
  display: flex;
  flex-direction: column;
  gap: 16px;
  flex: 1;
  min-height: 0;
}

.preview-stats { display: flex; gap: 16px; }
.preview-stat-item { display: flex; flex-direction: column; align-items: flex-end; }
.stat-lbl { font-size: 11px; text-transform: uppercase; color: #64748b; font-weight: 700; letter-spacing: 0.05em; }
.stat-val { font-size: 20px; font-weight: 800; line-height: 1.1; }
.text-brand { color: #2563eb; }
.text-danger { color: #dc2626; }
.text-success { color: #10b981; }

.preview-glass-toolbar {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  gap: 16px;
  padding: 10px 16px;
  background: rgba(248, 250, 252, 0.8);
  backdrop-filter: blur(12px);
  border: 1px solid #e2e8f0;
  border-radius: 12px;
}
.toolbar-left, .toolbar-right { flex-shrink: 0; }
.toolbar-center { flex: 1; }

.search-input :deep(.el-input__wrapper) { border-radius: 20px; box-shadow: 0 1px 2px rgba(0,0,0,0.05); }

.modern-alert-banner {
  flex-shrink: 0;
  display: flex;
  align-items: center;
  gap: 12px;
  padding: 12px 16px;
  background: linear-gradient(to right, #fff1f2, #ffe4e6);
  border-left: 4px solid #f43f5e;
  border-radius: 8px;
  font-size: 13px;
}
.alert-icon { font-size: 18px; }
.alert-content { flex: 1; color: #881337; line-height: 1.5; }
.alert-action { font-weight: 700; color: #e11d48; font-size: 12px; }

/* Table Improvements */
.mapping-preview-table-wrap {
  flex: 1;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  overflow: hidden;
  box-shadow: 0 4px 6px -1px rgba(0,0,0,0.02);
  min-height: 0;
}

.premium-table :deep(.el-table__header-wrapper th) {
  background: #f8fafc;
  color: #475569;
  font-weight: 700;
  text-transform: uppercase;
  font-size: 12px;
  letter-spacing: 0.02em;
  border-bottom: 2px solid #e2e8f0;
}
.premium-table :deep(.el-table__body-wrapper td) { border-bottom: 1px solid #f1f5f9; }
.premium-table :deep(.fw-bold) { font-weight: 600; color: #1e293b; }

.group-name-cell { display: flex; align-items: center; gap: 8px; }
.cell-icon { font-size: 14px; opacity: 0.8; }

.modern-tag { border-radius: 6px; font-weight: 600; padding: 0 8px; border: none; }
.tag-success { background: #dcfce7; color: #166534; }
.tag-success .dot { display: inline-block; width: 6px; height: 6px; border-radius: 50%; background: #22c55e; margin-right: 4px; }
.tag-warning { background: #fef3c7; color: #92400e; }
.tag-category { background: #f1f5f9; color: #475569; border: 1px solid #e2e8f0; }

.raw-text { font-family: monospace; background: #f8fafc; padding: 2px 6px; border-radius: 4px; color: #64748b; font-size: 12px; }
.empty-dash { color: #cbd5e1; font-weight: bold; }

.preview-row-unmatched > td { background: #fffdf5; }
.preview-row-unmatched:hover > td { background: #fffbe0 !important; }

.preview-empty-state {
  flex: 1;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-height: 200px;
  background: #f8fafc;
  border: 1px dashed #cbd5e1;
  border-radius: 12px;
  color: #64748b;
}

/* Animations */
.list-enter-active,
.list-leave-active { transition: all 0.3s cubic-bezier(0.55, 0, 0.1, 1); }
.list-enter-from { opacity: 0; transform: translateY(15px); }
.list-leave-to { opacity: 0; transform: scale(0.9); }
.fade-slide-enter-active, .fade-slide-leave-active { transition: all 0.3s ease; }
.fade-slide-enter-from { opacity: 0; transform: translateY(-10px); }
.fade-slide-leave-to { opacity: 0; transform: translateY(-10px); }

.radar-scan {
  width: 64px;
  height: 64px;
  border-radius: 50%;
  border: 2px solid #cbd5e1;
  position: relative;
  margin-bottom: 16px;
  overflow: hidden;
  background: linear-gradient(135deg, #f1f5f9, #ffffff);
}
.radar-beam {
  position: absolute;
  top: 50%; left: 50%;
  width: 50%; height: 50%;
  background: linear-gradient(45deg, transparent 50%, rgba(59, 130, 246, 0.4) 100%);
  transform-origin: top left;
  animation: radar-spin 2s linear infinite;
}
@keyframes radar-spin { from { transform: rotate(0deg); } to { transform: rotate(360deg); } }
.preview-empty-state h3 { font-size: 16px; color: #334155; margin-bottom: 4px; }
.preview-empty-state p { font-size: 13px; color: #94a3b8; }
</style>
