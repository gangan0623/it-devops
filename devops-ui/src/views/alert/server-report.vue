<template>
  <div class="mod-prometheus-report">
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
          <span>服务器智能分析报告</span>
          <template v-if="detail">
            <el-tag :type="detail.periodType === 'month' ? '' : 'info'" size="small" style="margin-left: 12px">
              {{ periodLabel(detail.periodType) }}
            </el-tag>
            <span class="dialog-period">{{ fmtDate(detail.periodStart) }} — {{ fmtDate(detail.periodEnd) }}</span>
          </template>
        </div>
      </template>

      <el-skeleton v-if="!detail" :rows="10" animated style="padding: 16px" />

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
            <el-table-column prop="host" label="主机" min-width="200" show-overflow-tooltip />
            <el-table-column prop="issue" label="问题类型" width="120">
              <template #default="{ row }">
                <el-tag :type="issueTagType(row.issue)" size="small" effect="light">{{ row.issue }}</el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="count" label="次数" width="80" align="center" />
            <el-table-column prop="reason" label="分析说明" min-width="320" show-overflow-tooltip />
          </el-table>
        </div>

        <!-- 告警分布 TOP20 -->
        <div class="section-card">
          <div class="section-head">告警类型分布 TOP20</div>
          <el-table :data="alertNameDistList" size="small" border>
            <el-table-column type="index" width="50" align="center" />
            <el-table-column prop="alertName" label="告警名称" min-width="280" show-overflow-tooltip />
            <el-table-column prop="count" label="次数" width="80" align="center" />
          </el-table>
        </div>

        <!-- 受影响主机 TOP20 -->
        <div class="section-card">
          <div class="section-head">受影响主机 TOP20</div>
          <el-table :data="topHostsList" size="small" border>
            <el-table-column type="index" width="50" align="center" />
            <el-table-column prop="instance" label="主机" min-width="240" show-overflow-tooltip />
            <el-table-column prop="total_count" label="总次数" width="90" align="center" />
            <el-table-column prop="firing_count" label="告警中" width="90" align="center">
              <template #default="{ row }">
                <span :style="{ color: Number(row.firing_count) > 0 ? '#ef4444' : '' }">{{ row.firing_count }}</span>
              </template>
            </el-table-column>
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

const parsedReport = computed<any | null>(() => {
  const raw = detail.value?.reportJson;
  if (!raw) return null;
  try {
    return typeof raw === "string" ? JSON.parse(raw) : raw;
  } catch {
    return null;
  }
});

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
const actionList = computed(() => toArray(parsedReport.value?.next_actions));
const actionTitle = computed(() => (detail.value?.periodType === "month" ? "下月行动清单" : "下周行动清单"));

const riskTop5List = computed(() => {
  const rows = parsedReport.value?.risk_top5;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 5).map((item: any) => ({
    host: item?.host || "-",
    issue: item?.issue || "其他",
    count: item?.count ?? 0,
    reason: item?.reason || "-"
  }));
});

const alertNameDistList = computed(() => {
  const rows = parsedInput.value?.alertNameDist;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20);
});

const topHostsList = computed(() => {
  const rows = parsedInput.value?.topHosts;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20);
});

const kpiCards = computed(() => {
  const s = parsedInput.value;
  return [
    { label: "告警总数", value: s?.total ?? "-", sub: "周期内全部事件", color: "#3b82f6" },
    { label: "告警中", value: s?.firingCount ?? "-", sub: "仍在告警中", color: "#ef4444" },
    { label: "已恢复", value: s?.resolvedCount ?? "-", sub: "已闭合事件", color: "#22c55e" },
    { label: "受影响主机", value: topHostsList.value.length, sub: "受影响主机数", color: "#f59e0b" },
    { label: "告警类型", value: alertNameDistList.value.length, sub: "告警规则种类", color: "#8b5cf6" },
    { label: "P1 建议数", value: p1List.value.length, sub: "需立即处理", color: "#06b6d4" }
  ];
});

const issueTagType = (issue: string) => {
  if (issue === "宕机") return "danger";
  if (issue === "CPU" || issue === "内存") return "warning";
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
  baseService.get(`/alert/prometheus/report/${detail.value.id}`).then((res) => {
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
    .get("/alert/prometheus/report/latest", { reportType: "server", size: 50 })
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
    .post(
      `/alert/prometheus/report/generate?reportType=server&periodType=${form.periodType}`,
      undefined,
      undefined,
      { timeout: REPORT_GENERATE_TIMEOUT }
    )
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
  baseService.get(`/alert/prometheus/report/${id}`).then((res) => {
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
.mod-prometheus-report {
  .toolbar-card {
    margin-bottom: 14px;
  }

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

  .report-body {
    padding: 4px 0;
  }

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

  .two-col {
    display: grid;
    grid-template-columns: repeat(2, 1fr);
    gap: 14px;
    margin-bottom: 14px;
  }

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
  }

  .bullet-list {
    margin: 0;
    padding-left: 18px;
    font-size: 13px;
    color: #334155;
    line-height: 1.75;
  }

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
    .priority-label { color: #dc2626; }
  }

  .priority-col--p2 {
    background: #fff7ed;
    border: 1px solid #fed7aa;
    .priority-label { color: #ea580c; }
  }

  .priority-col--p3 {
    background: #fffbeb;
    border: 1px solid #fde68a;
    .priority-label { color: #d97706; }
  }

  .priority-label {
    font-weight: 700;
    font-size: 13px;
    margin-bottom: 8px;
  }

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

  @media (max-width: 1200px) {
    .report-cards { grid-template-columns: repeat(2, 1fr); }
    .kpi-row { grid-template-columns: repeat(3, 1fr); }
  }

  @media (max-width: 768px) {
    .report-cards { grid-template-columns: 1fr; }
    .kpi-row { grid-template-columns: repeat(2, 1fr); }
    .two-col { grid-template-columns: 1fr; }
    .priority-row { grid-template-columns: 1fr; }
  }
}
</style>
