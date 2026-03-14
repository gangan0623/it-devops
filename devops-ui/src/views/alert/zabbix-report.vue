<template>
  <div class="mod-zabbix-report">
    <!-- 工具栏 -->
    <el-card shadow="never" class="toolbar-card">
      <el-form :inline="true" :model="form" @submit.prevent>
        <el-form-item label="周期">
          <el-radio-group v-model="form.periodType">
            <el-radio-button value="week">周报</el-radio-button>
            <el-radio-button value="month">月报</el-radio-button>
          </el-radio-group>
        </el-form-item>
        <el-form-item>
          <el-button type="primary" :loading="generateLoading" @click="generateReport">生成报告</el-button>
          <el-button @click="fetchList">刷新</el-button>
        </el-form-item>
      </el-form>
    </el-card>

    <!-- 报告卡片列表 -->
    <div v-loading="loading" class="report-cards">
      <div
        v-for="item in list"
        :key="item.id"
        :class="['rci', item.reportStatus === 2 && 'rci--failed', item.reportStatus === 0 && 'rci--pending']"
        @click="openDetail(item.id)"
      >
        <div class="rci__head">
          <el-tag :type="item.periodType === 'month' ? '' : 'info'" size="small" effect="light">
            {{ periodLabel(item.periodType) }}
          </el-tag>
          <el-tag :type="statusTagType(item.reportStatus)" size="small" effect="dark" style="margin-left: 6px">
            {{ statusLabel(item.reportStatus) }}
          </el-tag>
          <span class="rci__time">{{ item.createDate }}</span>
        </div>
        <div class="rci__summary">{{ item.summary || "暂无摘要" }}</div>
        <div class="rci__footer">
          <span class="rci__model">{{ item.modelName || "-" }}</span>
          <span class="rci__link">查看报告 →</span>
        </div>
      </div>
      <div v-if="!loading && list.length === 0" class="empty-tip">暂无报告，点击"生成报告"开始生成</div>
    </div>

    <!-- 详情弹窗 -->
    <el-dialog v-model="detailVisible" width="94%" top="2vh" class="report-dialog">
      <template #header>
        <div class="dialog-title">
          <span>网络设备智能分析报告</span>
          <template v-if="detail">
            <el-tag :type="detail.periodType === 'month' ? '' : 'info'" size="small" style="margin-left: 12px">
              {{ periodLabel(detail.periodType) }}
            </el-tag>
            <span class="dialog-period">{{ fmtDate(detail.periodStart) }} — {{ fmtDate(detail.periodEnd) }}</span>
          </template>
        </div>
      </template>

      <!-- 骨架屏 -->
      <el-skeleton v-if="!detail" :rows="10" animated style="padding: 16px" />

      <!-- 正文 -->
      <div v-else-if="parsedReport" class="report-body">
        <!-- KPI 行 -->
        <div class="kpi-row">
          <div v-for="kpi in kpiCards" :key="kpi.label" class="kpi-card" :style="{ '--kc': kpi.color }">
            <div class="kpi-card__value">{{ kpi.value }}</div>
            <div class="kpi-card__label">{{ kpi.label }}</div>
            <div class="kpi-card__sub">{{ kpi.sub }}</div>
          </div>
        </div>

        <!-- 执行摘要 + 故障分析 -->
        <div class="two-col">
          <div class="section-card">
            <div class="section-head">执行摘要</div>
            <ul class="bullet-list">
              <li v-for="(t, i) in summaryList" :key="i">{{ t }}</li>
            </ul>
          </div>
          <div class="section-card">
            <div class="section-head">故障结构分析</div>
            <ul class="bullet-list">
              <li v-for="(t, i) in faultAnalysisList" :key="i">{{ t }}</li>
            </ul>
          </div>
        </div>

        <!-- 风险 TOP5 -->
        <div class="section-card">
          <div class="section-head">风险 TOP5</div>
          <el-table :data="riskTop5List" size="small" border>
            <el-table-column type="index" width="50" align="center" />
            <el-table-column prop="host_name" label="主机名" min-width="200" show-overflow-tooltip />
            <el-table-column prop="host_ip" label="IP" width="150" />
            <el-table-column prop="fault_point" label="故障点" width="100">
              <template #default="{ row }">
                <el-tag :type="faultTagType(row.fault_point)" size="small" effect="light">{{ row.fault_point }}</el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="count" label="次数" width="80" align="center" />
            <el-table-column prop="reason" label="分析说明" min-width="320" show-overflow-tooltip />
          </el-table>
        </div>

        <!-- 高风险接口 -->
        <div class="section-card">
          <div class="section-head-row">
            <span class="section-head no-border">高风险接口 TOP20</span>
            <div class="filter-bar">
              <el-switch v-model="topRiskFilter.onlyFlapping" size="small" />
              <span class="filter-label">仅看重复抖动</span>
              <el-input-number
                v-model="topRiskFilter.minCount"
                :min="1"
                :max="100"
                size="small"
                controls-position="right"
                style="width: 80px"
              />
              <span class="filter-label">次以上</span>
            </div>
          </div>
          <el-table :data="displayedTopInterfaceRiskList" size="small" border>
            <el-table-column type="index" width="50" align="center" />
            <el-table-column prop="hostname" label="主机名" min-width="180" show-overflow-tooltip />
            <el-table-column prop="host_ip" label="IP" width="140" />
            <el-table-column prop="interface_name" label="接口" min-width="160" show-overflow-tooltip />
            <el-table-column prop="total_count" label="总次数" width="80" align="center" />
            <el-table-column prop="link_down_count" label="链路断" width="80" align="center" />
            <el-table-column prop="speed_drop_count" label="速率降" width="80" align="center" />
            <el-table-column prop="error_count" label="错包" width="70" align="center" />
            <el-table-column prop="drop_count" label="丢包" width="70" align="center" />
            <el-table-column prop="duplex_count" label="半双工" width="75" align="center" />
            <el-table-column prop="availability_count" label="可用性" width="75" align="center" />
          </el-table>
        </div>

        <!-- 优化建议 P1/P2/P3 -->
        <div class="section-card">
          <div class="section-head">优化建议</div>
          <div class="priority-row">
            <div class="priority-col priority-col--p1">
              <div class="priority-label">P1 · 立即处理</div>
              <ul class="bullet-list">
                <li v-for="(t, i) in p1List" :key="i">{{ t }}</li>
              </ul>
            </div>
            <div class="priority-col priority-col--p2">
              <div class="priority-label">P2 · 近期安排</div>
              <ul class="bullet-list">
                <li v-for="(t, i) in p2List" :key="i">{{ t }}</li>
              </ul>
            </div>
            <div class="priority-col priority-col--p3">
              <div class="priority-label">P3 · 计划优化</div>
              <ul class="bullet-list">
                <li v-for="(t, i) in p3List" :key="i">{{ t }}</li>
              </ul>
            </div>
          </div>
        </div>

        <!-- 行动清单 -->
        <div class="section-card">
          <div class="section-head">{{ actionTitle }}</div>
          <div class="checklist">
            <div v-for="(t, i) in actionList" :key="i" class="checklist-item">
              <span class="checklist-idx">{{ i + 1 }}</span>
              <span class="checklist-text">{{ t }}</span>
            </div>
          </div>
        </div>
      </div>

      <!-- 兜底：原始 Markdown -->
      <div v-else class="raw-md">
        <pre>{{ detail?.reportStatus === 0 ? "报告生成中，请稍后刷新查看。" : (detail.reportMarkdown || detail.errorMessage || "-") }}</pre>
      </div>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import { computed, reactive, ref, onBeforeUnmount, onMounted } from "vue";
import { ElMessage } from "element-plus";
import baseService from "@/service/baseService";

const REPORT_GENERATE_TIMEOUT = 300000;
const PENDING_REFRESH_INTERVAL = 10000;

const loading = ref(false);
const generateLoading = ref(false);
const list = ref<any[]>([]);
let pendingRefreshTimer: number | null = null;

const form = reactive({ periodType: "week" });
const topRiskFilter = reactive({ onlyFlapping: true, minCount: 3 });

const detailVisible = ref(false);
const detail = ref<any>(null);

const periodLabel = (type: string) => (type === "month" ? "月报" : "周报");
const statusLabel = (status: number) => (status === 1 ? "成功" : status === 2 ? "失败" : "处理中");
const statusTagType = (status: number) => (status === 1 ? "success" : status === 2 ? "danger" : "warning");

const fmtDate = (d: any) => {
  if (!d) return "-";
  try {
    return new Date(d).toLocaleDateString("zh-CN", { year: "numeric", month: "2-digit", day: "2-digit" });
  } catch {
    return String(d);
  }
};

// AI 返回的报告 JSON
const parsedReport = computed<any | null>(() => {
  const raw = detail.value?.reportJson;
  if (!raw) return null;
  try {
    return typeof raw === "string" ? JSON.parse(raw) : raw;
  } catch {
    return null;
  }
});

// 发给 AI 的统计原始数据（topInterfaceRisks 在这里，不在 reportJson 里）
const parsedInput = computed<any | null>(() => {
  const raw = detail.value?.inputJson;
  if (!raw) return null;
  try {
    return typeof raw === "string" ? JSON.parse(raw) : raw;
  } catch {
    return null;
  }
});

const toArray = (value: any): string[] => {
  if (Array.isArray(value)) return value.map((item) => String(item ?? "").trim()).filter(Boolean);
  if (value == null) return [];
  const text = String(value).trim();
  return text ? [text] : [];
};

const summaryList = computed(() => toArray(parsedReport.value?.executive_summary));
const faultAnalysisList = computed(() => toArray(parsedReport.value?.fault_analysis));
const p1List = computed(() => toArray(parsedReport.value?.optimization_p1));
const p2List = computed(() => toArray(parsedReport.value?.optimization_p2));
const p3List = computed(() => toArray(parsedReport.value?.optimization_p3));
const actionList = computed(() => toArray(parsedReport.value?.next_week_actions));

const actionTitle = computed(() => (detail.value?.periodType === "month" ? "下月行动清单" : "下周行动清单"));

const riskTop5List = computed(() => {
  const rows = parsedReport.value?.risk_top5;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 5).map((item: any) => ({
    host_name: item?.host_name || "-",
    host_ip: item?.host_ip || "-",
    fault_point: item?.fault_point || "其他",
    count: item?.count ?? 0,
    reason: item?.reason || "-"
  }));
});

// 修正：topInterfaceRisks 来自 inputJson，不是 reportJson
const topInterfaceRiskList = computed(() => {
  const rows = parsedInput.value?.topInterfaceRisks;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20).map((item: any) => ({
    host_ip: item?.host_ip || "-",
    hostname: item?.hostname || "-",
    interface_name: item?.interface_name || "-",
    total_count: Number(item?.total_count || 0),
    link_down_count: Number(item?.link_down_count || 0),
    speed_drop_count: Number(item?.speed_drop_count || 0),
    error_count: Number(item?.error_count || 0),
    drop_count: Number(item?.drop_count || 0),
    duplex_count: Number(item?.duplex_count || 0),
    availability_count: Number(item?.availability_count || 0)
  }));
});

const displayedTopInterfaceRiskList = computed(() => {
  if (!topRiskFilter.onlyFlapping) return topInterfaceRiskList.value;
  const min = Number(topRiskFilter.minCount || 1);
  return topInterfaceRiskList.value.filter((r) => r.total_count >= min);
});

// KPI 直接从 inputJson 取，数据准确
const kpiCards = computed(() => {
  const s = parsedInput.value;
  return [
    { label: "告警总数", value: s?.total ?? "-", sub: "周期内全部事件", color: "#3b82f6" },
    { label: "未恢复", value: s?.openCount ?? "-", sub: "仍在告警中", color: "#ef4444" },
    { label: "已恢复", value: s?.resolvedCount ?? "-", sub: "已闭合事件", color: "#22c55e" },
    {
      label: "灾难/高危",
      value: s ? Number(s.disasterCount || 0) + Number(s.highCount || 0) : "-",
      sub: "高优先级告警",
      color: "#f59e0b"
    },
    { label: "高风险接口", value: topInterfaceRiskList.value.length, sub: "接口风险列表", color: "#8b5cf6" },
    { label: "P1 建议数", value: p1List.value.length, sub: "需立即处理", color: "#06b6d4" }
  ];
});

const faultTagType = (point: string) => {
  if (point === "链路" || point === "可用性") return "danger";
  if (point === "速率" || point === "丢包" || point === "错包" || point === "半双工") return "warning";
  return "info";
};

const stopPendingRefresh = () => {
  if (pendingRefreshTimer !== null) {
    window.clearInterval(pendingRefreshTimer);
    pendingRefreshTimer = null;
  }
};

const refreshPendingDetail = () => {
  if (!detailVisible.value || !detail.value?.id || Number(detail.value?.reportStatus) !== 0) {
    return;
  }
  baseService.get(`/alert/zabbix/report/${detail.value.id}`).then((res) => {
    detail.value = res.data || detail.value;
  });
};

const syncPendingRefresh = () => {
  const hasPending = list.value.some((item) => Number(item?.reportStatus) === 0);
  if (!hasPending) {
    stopPendingRefresh();
    return;
  }
  if (pendingRefreshTimer !== null) {
    return;
  }
  pendingRefreshTimer = window.setInterval(() => {
    fetchList(true);
    refreshPendingDetail();
  }, PENDING_REFRESH_INTERVAL);
};

const fetchList = (silent = false) => {
  if (!silent) {
    loading.value = true;
  }
  baseService
    .get("/alert/zabbix/report/latest", { size: 50 })
    .then((res) => {
      list.value = res.data || [];
      syncPendingRefresh();
    })
    .finally(() => {
      if (!silent) {
        loading.value = false;
      }
    });
};

const generateReport = () => {
  generateLoading.value = true;
  baseService
    .post("/alert/zabbix/report/generate", { periodType: form.periodType }, undefined, { timeout: REPORT_GENERATE_TIMEOUT })
    .then(() => {
      ElMessage.success("已提交生成任务，请稍后刷新查看");
      fetchList();
    })
    .finally(() => {
      generateLoading.value = false;
    });
};

const openDetail = (id: number) => {
  detailVisible.value = true;
  detail.value = null;
  baseService.get(`/alert/zabbix/report/${id}`).then((res) => {
    detail.value = res.data || null;
  });
};

onMounted(() => {
  fetchList();
});

onBeforeUnmount(() => {
  stopPendingRefresh();
});
</script>

<style lang="less" scoped>
.mod-zabbix-report {
  .toolbar-card {
    margin-bottom: 14px;
  }

  // ── 报告卡片列表 ──────────────────────────────────
  .report-cards {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 14px;
    min-height: 80px;
  }

  .rci {
    background: #fff;
    border: 1px solid #e2e8f0;
    border-radius: 10px;
    padding: 16px;
    cursor: pointer;
    display: flex;
    flex-direction: column;
    transition: box-shadow 0.18s, border-color 0.18s;

    &:hover {
      box-shadow: 0 4px 18px rgba(0, 0, 0, 0.09);
      border-color: #93c5fd;
    }

    &--failed {
      background: #fff5f5;
      border-color: #fecaca;
    }

    &--pending {
      background: #fffbeb;
      border-color: #fcd34d;
    }

    &__head {
      display: flex;
      align-items: center;
      margin-bottom: 10px;
    }

    &__time {
      margin-left: auto;
      font-size: 12px;
      color: #94a3b8;
    }

    &__summary {
      flex: 1;
      font-size: 13px;
      color: #334155;
      line-height: 1.65;
      display: -webkit-box;
      -webkit-line-clamp: 3;
      -webkit-box-orient: vertical;
      overflow: hidden;
    }

    &__footer {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-top: 12px;
      padding-top: 10px;
      border-top: 1px solid #f1f5f9;
    }

    &__model {
      font-size: 11px;
      color: #94a3b8;
    }

    &__link {
      font-size: 12px;
      color: #3b82f6;
      font-weight: 500;
    }
  }

  .empty-tip {
    grid-column: 1 / -1;
    text-align: center;
    padding: 40px;
    color: #94a3b8;
    font-size: 13px;
  }

  // ── 弹窗标题 ──────────────────────────────────────
  .dialog-title {
    display: flex;
    align-items: center;
    font-size: 15px;
    font-weight: 700;
    color: #0f172a;
    flex-wrap: wrap;
    gap: 6px;
  }

  .dialog-period {
    font-size: 12px;
    color: #64748b;
    font-weight: 400;
    margin-left: 4px;
  }

  // ── 报告正文 ──────────────────────────────────────
  .report-body {
    padding: 4px 0;
  }

  // KPI 行
  .kpi-row {
    display: grid;
    grid-template-columns: repeat(6, 1fr);
    gap: 10px;
    margin-bottom: 14px;
  }

  .kpi-card {
    background: #fff;
    border: 1px solid #e2e8f0;
    border-left: 4px solid var(--kc, #3b82f6);
    border-radius: 8px;
    padding: 12px 14px;

    &__value {
      font-size: 24px;
      font-weight: 700;
      color: #0f172a;
      line-height: 1;
    }

    &__label {
      margin-top: 6px;
      font-size: 12px;
      color: #475569;
      font-weight: 500;
    }

    &__sub {
      margin-top: 3px;
      font-size: 11px;
      color: #94a3b8;
    }
  }

  // 两列布局
  .two-col {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 14px;
    margin-bottom: 14px;
  }

  // 通用卡片
  .section-card {
    background: #fff;
    border: 1px solid #e2e8f0;
    border-radius: 10px;
    padding: 16px;
    margin-bottom: 14px;
  }

  .section-head {
    font-size: 14px;
    font-weight: 700;
    color: #0f172a;
    margin-bottom: 12px;
    padding-bottom: 8px;
    border-bottom: 2px solid #f1f5f9;

    &.no-border {
      margin-bottom: 0;
      padding-bottom: 0;
      border-bottom: none;
    }
  }

  .section-head-row {
    display: flex;
    align-items: center;
    justify-content: space-between;
    margin-bottom: 12px;
    padding-bottom: 8px;
    border-bottom: 2px solid #f1f5f9;
  }

  .filter-bar {
    display: flex;
    align-items: center;
    gap: 8px;
  }

  .filter-label {
    font-size: 12px;
    color: #64748b;
    white-space: nowrap;
  }

  // 列表
  .bullet-list {
    margin: 0;
    padding-left: 18px;
    font-size: 13px;
    color: #334155;
    line-height: 1.75;
  }

  // P1/P2/P3
  .priority-row {
    display: grid;
    grid-template-columns: repeat(3, 1fr);
    gap: 12px;
  }

  .priority-col {
    border-radius: 8px;
    padding: 12px;
  }

  .priority-col--p1 {
    background: #fef2f2;
    border: 1px solid #fecaca;
    .priority-label {
      color: #dc2626;
    }
  }

  .priority-col--p2 {
    background: #fff7ed;
    border: 1px solid #fed7aa;
    .priority-label {
      color: #ea580c;
    }
  }

  .priority-col--p3 {
    background: #fffbeb;
    border: 1px solid #fde68a;
    .priority-label {
      color: #d97706;
    }
  }

  .priority-label {
    font-weight: 700;
    font-size: 13px;
    margin-bottom: 8px;
  }

  // 行动清单
  .checklist {
    display: flex;
    flex-direction: column;
    gap: 6px;
  }

  .checklist-item {
    display: flex;
    align-items: flex-start;
    gap: 10px;
    padding: 8px 12px;
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 6px;
  }

  .checklist-idx {
    flex-shrink: 0;
    width: 22px;
    height: 22px;
    border: 2px solid #cbd5e1;
    border-radius: 50%;
    display: flex;
    align-items: center;
    justify-content: center;
    font-size: 11px;
    color: #64748b;
    font-weight: 600;
  }

  .checklist-text {
    font-size: 13px;
    color: #334155;
    line-height: 1.6;
  }

  // 原始 Markdown 兜底
  .raw-md {
    background: #f8fafc;
    border: 1px solid #e2e8f0;
    border-radius: 8px;
    padding: 14px;
    max-height: 560px;
    overflow: auto;

    pre {
      margin: 0;
      white-space: pre-wrap;
      word-break: break-word;
      font-family: Menlo, Monaco, Consolas, "Courier New", monospace;
      font-size: 13px;
      color: #0f172a;
      line-height: 1.6;
    }
  }

  // ── 响应式 ────────────────────────────────────────
  @media (max-width: 1200px) {
    .report-cards {
      grid-template-columns: repeat(2, 1fr);
    }
    .kpi-row {
      grid-template-columns: repeat(3, 1fr);
    }
  }

  @media (max-width: 768px) {
    .report-cards {
      grid-template-columns: 1fr;
    }
    .kpi-row {
      grid-template-columns: repeat(2, 1fr);
    }
    .two-col {
      grid-template-columns: 1fr;
    }
    .priority-row {
      grid-template-columns: 1fr;
    }
    .section-head-row {
      flex-direction: column;
      align-items: flex-start;
      gap: 8px;
    }
  }
}
</style>
