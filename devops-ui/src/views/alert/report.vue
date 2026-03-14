<template>
  <div class="mod-report-center">
    <el-card shadow="never" class="toolbar-card">
      <div class="toolbar-card__head">
        <div class="toolbar-card__title-wrap">
          <div class="toolbar-card__title">报告</div>
          <div class="toolbar-card__subtitle">统一查看和生成网络设备、服务主机、网络站点智能分析报告</div>
        </div>
        <div class="toolbar-card__summary">
          <span class="toolbar-summary-pill">当前类型 {{ typeLabel(activeType) }}</span>
          <span class="toolbar-summary-pill">共 {{ currentReports.length }} 条</span>
          <span v-if="currentPendingCount > 0" class="toolbar-summary-pill toolbar-summary-pill--pending">
            处理中 {{ currentPendingCount }} 条
          </span>
        </div>
      </div>

      <div class="toolbar-card__controls">
        <div class="toolbar-block">
          <span class="toolbar-block__label">类型</span>
          <el-radio-group v-model="activeType" @change="handleTypeChange">
            <el-radio-button value="zabbix">网络设备</el-radio-button>
            <el-radio-button value="server">服务主机</el-radio-button>
            <el-radio-button value="http">网络站点</el-radio-button>
          </el-radio-group>
        </div>

        <div class="toolbar-divider"></div>

        <div class="toolbar-block">
          <span class="toolbar-block__label">生成周期</span>
          <el-radio-group v-model="generatePeriodType">
            <el-radio-button value="week">周报</el-radio-button>
            <el-radio-button value="month">月报</el-radio-button>
          </el-radio-group>
        </div>

        <div class="toolbar-block toolbar-block--actions">
          <el-button type="primary" :loading="generateLoading" @click="generateReport">生成报告</el-button>
          <el-button :loading="loading" @click="fetchReports">刷新</el-button>
        </div>
      </div>
    </el-card>

    <div v-loading="loading" class="report-cards">
      <div
        v-for="item in pagedReports"
        :key="`${item.kind}-${item.id}`"
        :class="['report-card', item.reportStatus === 2 && 'report-card--failed', item.reportStatus === 0 && 'report-card--pending']"
        @click="openDetail(item)"
      >
        <div class="report-card__head">
          <div class="report-card__tags">
            <el-tag :type="typeTagType(item.kind)" size="small" effect="light">{{ typeLabel(item.kind) }}</el-tag>
            <el-tag :type="item.periodType === 'month' ? '' : 'info'" size="small" effect="light">
              {{ periodLabel(item.periodType) }}
            </el-tag>
            <el-tag :type="statusTagType(item.reportStatus)" size="small" effect="dark">
              {{ statusLabel(item.reportStatus) }}
            </el-tag>
          </div>
          <span class="report-card__time">{{ formatDateTime(item.createDate) }}</span>
        </div>

        <div class="report-card__summary">{{ item.summary || "暂无摘要" }}</div>

        <div class="report-card__meta">
          <div class="report-card__meta-row">
            <span class="report-card__meta-label">统计周期</span>
            <span class="report-card__meta-value">{{ formatPeriodRange(item.periodStart, item.periodEnd) }}</span>
          </div>
          <div class="report-card__meta-row">
            <span class="report-card__meta-label">模型</span>
            <span class="report-card__meta-value">{{ item.modelName || "-" }}</span>
          </div>
        </div>

        <div class="report-card__footer">
          <span class="report-card__hint">{{ item.reportStatus === 0 ? "自动刷新中" : "查看报告" }}</span>
          <span class="report-card__link">查看详情 →</span>
        </div>
      </div>

      <div v-if="!loading && pagedReports.length === 0" class="empty-tip">
        暂无报告，点击“生成报告”开始生成
      </div>
    </div>

    <div class="pagination-bar">
      <div class="pagination-bar__count">共 {{ currentReports.length }} 条</div>
      <el-pagination
        v-model:current-page="pagination.page"
        :total="currentReports.length"
        :page-size="pagination.pageSize"
        layout="prev, pager, next"
        small
      />
    </div>

    <el-dialog v-model="detailVisible" width="78%" top="4vh" class="report-detail-dialog">
      <template #header>
        <div class="dialog-title">
          <span>{{ detailItem ? typeLabel(detailItem.kind) : "报告详情" }}</span>
          <el-tag v-if="detailItem" :type="statusTagType(detailItem.reportStatus)" effect="dark" style="margin-left: 10px">
            {{ statusLabel(detailItem.reportStatus) }}
          </el-tag>
        </div>
      </template>

      <el-skeleton v-if="detailLoading && !detailItem" :rows="8" animated style="padding: 16px" />

      <div v-else-if="detailItem" class="detail-body">
        <el-descriptions :column="2" border size="small" class="detail-body__meta">
          <el-descriptions-item label="报告类型">{{ typeLabel(detailItem.kind) }}</el-descriptions-item>
          <el-descriptions-item label="生成周期">{{ periodLabel(detailItem.periodType) }}</el-descriptions-item>
          <el-descriptions-item label="生成时间">{{ formatDateTime(detailItem.createDate) }}</el-descriptions-item>
          <el-descriptions-item label="统计周期">{{ formatPeriodRange(detailItem.periodStart, detailItem.periodEnd) }}</el-descriptions-item>
          <el-descriptions-item label="模型">{{ detailItem.modelName || "-" }}</el-descriptions-item>
          <el-descriptions-item label="状态">{{ statusLabel(detailItem.reportStatus) }}</el-descriptions-item>
          <el-descriptions-item :span="2" label="摘要">{{ detailItem.summary || "-" }}</el-descriptions-item>
          <el-descriptions-item v-if="detailItem.errorMessage" :span="2" label="异常">
            {{ detailItem.errorMessage }}
          </el-descriptions-item>
        </el-descriptions>

        <div v-if="Number(detailItem.reportStatus) === 0" class="pending-tip">
          报告生成中，请稍后查看。页面会自动刷新处理中记录。
        </div>

        <el-tabs v-model="detailTab">
          <el-tab-pane label="报告详情" name="rendered">
            <div v-if="detailItem.reportStatus === 0" class="pending-tip">
              报告生成中，渲染内容将在生成完成后展示。
            </div>
            <div v-else-if="renderedReportReady" class="report-render">
              <div class="kpi-row">
                <div v-for="kpi in detailKpiCards" :key="kpi.label" class="kpi-card" :style="{ '--kc': kpi.color }">
                  <div class="kpi-card__value">{{ kpi.value }}</div>
                  <div class="kpi-card__label">{{ kpi.label }}</div>
                  <div class="kpi-card__sub">{{ kpi.sub }}</div>
                </div>
              </div>

              <div class="two-col">
                <div class="section-card">
                  <div class="section-head">执行摘要</div>
                  <ul class="bullet-list">
                    <li v-for="(item, index) in detailSummaryList" :key="`summary-${index}`">{{ item }}</li>
                  </ul>
                </div>
                <div class="section-card">
                  <div class="section-head">故障结构分析</div>
                  <ul class="bullet-list">
                    <li v-for="(item, index) in detailFaultAnalysisList" :key="`analysis-${index}`">{{ item }}</li>
                  </ul>
                </div>
              </div>

              <div class="section-card">
                <div class="section-head">风险 TOP5</div>
                <el-table :data="detailRiskTop5List" size="small" border>
                  <el-table-column type="index" width="50" align="center" />
                  <el-table-column
                    v-for="column in detailRiskColumns"
                    :key="column.prop"
                    :prop="column.prop"
                    :label="column.label"
                    :width="column.width"
                    :min-width="column.minWidth"
                    :show-overflow-tooltip="column.overflow"
                    :align="column.align"
                  >
                    <template v-if="column.tag" #default="{ row }">
                      <el-tag :type="detailIssueTagType(row[column.prop])" size="small" effect="light">{{ row[column.prop] }}</el-tag>
                    </template>
                  </el-table-column>
                </el-table>
              </div>

              <div class="section-card" v-if="detailItem.kind === 'zabbix'">
                <div class="section-head">高风险接口 TOP20</div>
                <el-table :data="detailTopInterfaceRiskList" size="small" border>
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

              <div class="section-card" v-if="detailItem.kind !== 'zabbix'">
                <div class="section-head">告警类型分布 TOP20</div>
                <el-table :data="detailAlertNameDistList" size="small" border>
                  <el-table-column type="index" width="50" align="center" />
                  <el-table-column prop="alertName" label="告警名称" min-width="280" show-overflow-tooltip />
                  <el-table-column prop="count" label="次数" width="80" align="center" />
                </el-table>
              </div>

              <div class="section-card" v-if="detailItem.kind === 'server'">
                <div class="section-head">受影响主机 TOP20</div>
                <el-table :data="detailTopHostsList" size="small" border>
                  <el-table-column type="index" width="50" align="center" />
                  <el-table-column prop="instance" label="主机" min-width="240" show-overflow-tooltip />
                  <el-table-column prop="total_count" label="总次数" width="90" align="center" />
                  <el-table-column prop="firing_count" label="告警中" width="90" align="center" />
                </el-table>
              </div>

              <div class="section-card" v-if="detailItem.kind === 'http'">
                <div class="section-head">受影响探测目标 TOP20</div>
                <el-table :data="detailTopTargetsList" size="small" border>
                  <el-table-column type="index" width="50" align="center" />
                  <el-table-column prop="instance" label="探测目标" min-width="300" show-overflow-tooltip />
                  <el-table-column prop="total_count" label="总次数" width="90" align="center" />
                  <el-table-column prop="firing_count" label="告警中" width="90" align="center" />
                </el-table>
              </div>

              <div class="section-card">
                <div class="section-head">优化建议</div>
                <div class="priority-row">
                  <div class="priority-col priority-col--p1">
                    <div class="priority-label">P1 · 立即处理</div>
                    <ul class="bullet-list">
                      <li v-for="(item, index) in detailP1List" :key="`p1-${index}`">{{ item }}</li>
                    </ul>
                  </div>
                  <div class="priority-col priority-col--p2">
                    <div class="priority-label">P2 · 近期安排</div>
                    <ul class="bullet-list">
                      <li v-for="(item, index) in detailP2List" :key="`p2-${index}`">{{ item }}</li>
                    </ul>
                  </div>
                  <div class="priority-col priority-col--p3">
                    <div class="priority-label">P3 · 计划优化</div>
                    <ul class="bullet-list">
                      <li v-for="(item, index) in detailP3List" :key="`p3-${index}`">{{ item }}</li>
                    </ul>
                  </div>
                </div>
              </div>

              <div class="section-card">
                <div class="section-head">{{ detailActionTitle }}</div>
                <div class="checklist">
                  <div v-for="(item, index) in detailActionList" :key="`action-${index}`" class="checklist-item">
                    <span class="checklist-idx">{{ index + 1 }}</span>
                    <span class="checklist-text">{{ item }}</span>
                  </div>
                </div>
              </div>
            </div>
            <pre v-else class="detail-pre">{{ detailMarkdown }}</pre>
          </el-tab-pane>
          <el-tab-pane label="原始数据" name="raw">
            <el-tabs v-model="rawDetailTab">
              <el-tab-pane label="报告 JSON" name="reportJson">
                <pre class="detail-pre">{{ detailReportJson }}</pre>
              </el-tab-pane>
              <el-tab-pane label="输入 JSON" name="inputJson">
                <pre class="detail-pre">{{ detailInputJson }}</pre>
              </el-tab-pane>
              <el-tab-pane label="Markdown" name="markdown">
                <pre class="detail-pre">{{ detailMarkdown }}</pre>
              </el-tab-pane>
            </el-tabs>
          </el-tab-pane>
        </el-tabs>
      </div>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, reactive, ref, watch } from "vue";
import { ElMessage } from "element-plus";
import { useRoute, useRouter } from "vue-router";
import baseService from "@/service/baseService";

type ReportKind = "zabbix" | "server" | "http";

interface UnifiedReportItem {
  id: number;
  kind: Exclude<ReportKind, "all">;
  periodType?: string;
  periodStart?: string;
  periodEnd?: string;
  modelName?: string;
  reportStatus?: number;
  summary?: string;
  inputJson?: string;
  reportJson?: string;
  reportMarkdown?: string;
  errorMessage?: string;
  createDate?: string;
}

const REPORT_GENERATE_TIMEOUT = 300000;
const PENDING_REFRESH_INTERVAL = 10000;
const LIST_LIMIT = 200;

const route = useRoute();
const router = useRouter();

const normalizeType = (value: unknown): ReportKind => {
  if (value === "zabbix" || value === "server" || value === "http") {
    return value;
  }
  return "server";
};

const activeType = ref<ReportKind>(normalizeType(route.query.type));
const generatePeriodType = ref("week");
const loading = ref(false);
const generateLoading = ref(false);
const reports = ref<UnifiedReportItem[]>([]);
const detailVisible = ref(false);
const detailLoading = ref(false);
const detailItem = ref<UnifiedReportItem | null>(null);
const detailTab = ref("rendered");
const rawDetailTab = ref("reportJson");
const pagination = reactive({
  page: 1,
  pageSize: 6
});
let pendingRefreshTimer: number | null = null;

const syncRouteType = () => {
  if (route.path === "/workbench/report" && Object.keys(route.query || {}).length > 0) {
    router.replace({ path: "/workbench/report" });
  }
};

watch(
  () => route.query.type,
  (value) => {
    activeType.value = normalizeType(value);
    if (value !== undefined) {
      syncRouteType();
    }
  },
  { immediate: true }
);

watch(
  () => activeType.value,
  () => {
    pagination.page = 1;
  }
);

const typeLabel = (type: ReportKind | string) => {
  if (type === "zabbix") return "网络设备";
  if (type === "server") return "服务主机";
  if (type === "http") return "网络站点";
  return "服务主机";
};

const typeTagType = (type: string) => {
  if (type === "zabbix") return "warning";
  if (type === "server") return "success";
  return "info";
};

const periodLabel = (periodType?: string) => (periodType === "month" ? "月报" : "周报");
const statusLabel = (status?: number) => (status === 1 ? "成功" : status === 2 ? "失败" : "处理中");
const statusTagType = (status?: number) => (status === 1 ? "success" : status === 2 ? "danger" : "warning");

const formatDateTime = (value?: string) => {
  if (!value) return "-";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return String(value);
  return date.toLocaleString("zh-CN", {
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit"
  });
};

const formatDate = (value?: string) => {
  if (!value) return "-";
  const date = new Date(value);
  if (Number.isNaN(date.getTime())) return String(value);
  return date.toLocaleDateString("zh-CN", {
    year: "numeric",
    month: "2-digit",
    day: "2-digit"
  });
};

const formatPeriodRange = (start?: string, end?: string) => `${formatDate(start)} 至 ${formatDate(end)}`;

const formatJson = (value?: string) => {
  if (!value) return "-";
  try {
    return JSON.stringify(typeof value === "string" ? JSON.parse(value) : value, null, 2);
  } catch {
    return String(value);
  }
};

const detailMarkdown = computed(() => {
  if (!detailItem.value) return "-";
  if (Number(detailItem.value.reportStatus) === 0) {
    return "报告生成中，请稍后查看。";
  }
  return detailItem.value.reportMarkdown || detailItem.value.errorMessage || "-";
});

const detailReportJson = computed(() => formatJson(detailItem.value?.reportJson));
const detailInputJson = computed(() => formatJson(detailItem.value?.inputJson));
const parsedDetailReport = computed<any | null>(() => {
  const raw = detailItem.value?.reportJson;
  if (!raw) return null;
  try {
    return typeof raw === "string" ? JSON.parse(raw) : raw;
  } catch {
    return null;
  }
});
const parsedDetailInput = computed<any | null>(() => {
  const raw = detailItem.value?.inputJson;
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

const detailSummaryList = computed(() => toArray(parsedDetailReport.value?.executive_summary));
const detailFaultAnalysisList = computed(() => toArray(parsedDetailReport.value?.fault_analysis));
const detailP1List = computed(() => toArray(parsedDetailReport.value?.optimization_p1));
const detailP2List = computed(() => toArray(parsedDetailReport.value?.optimization_p2));
const detailP3List = computed(() => toArray(parsedDetailReport.value?.optimization_p3));
const detailActionList = computed(() =>
  detailItem.value?.kind === "zabbix"
    ? toArray(parsedDetailReport.value?.next_week_actions)
    : toArray(parsedDetailReport.value?.next_actions)
);
const detailActionTitle = computed(() => (detailItem.value?.periodType === "month" ? "下月行动清单" : "下周行动清单"));

const detailRiskTop5List = computed(() => {
  const rows = parsedDetailReport.value?.risk_top5;
  if (!Array.isArray(rows)) return [];
  if (detailItem.value?.kind === "zabbix") {
    return rows.slice(0, 5).map((item: any) => ({
      host_name: item?.host_name || "-",
      host_ip: item?.host_ip || "-",
      fault_point: item?.fault_point || "其他",
      count: item?.count ?? 0,
      reason: item?.reason || "-"
    }));
  }
  if (detailItem.value?.kind === "server") {
    return rows.slice(0, 5).map((item: any) => ({
      host: item?.host || "-",
      issue: item?.issue || "其他",
      count: item?.count ?? 0,
      reason: item?.reason || "-"
    }));
  }
  return rows.slice(0, 5).map((item: any) => ({
    target: item?.target || "-",
    issue: item?.issue || "其他",
    count: item?.count ?? 0,
    reason: item?.reason || "-"
  }));
});

const detailTopInterfaceRiskList = computed(() => {
  const rows = parsedDetailInput.value?.topInterfaceRisks;
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

const detailAlertNameDistList = computed(() => {
  const rows = parsedDetailInput.value?.alertNameDist;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20);
});

const detailTopHostsList = computed(() => {
  const rows = parsedDetailInput.value?.topHosts;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20);
});

const detailTopTargetsList = computed(() => {
  const rows = parsedDetailInput.value?.topTargets;
  if (!Array.isArray(rows)) return [];
  return rows.slice(0, 20);
});

const detailKpiCards = computed(() => {
  const s = parsedDetailInput.value;
  if (detailItem.value?.kind === "zabbix") {
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
      { label: "高风险接口", value: detailTopInterfaceRiskList.value.length, sub: "接口风险列表", color: "#8b5cf6" },
      { label: "P1 建议数", value: detailP1List.value.length, sub: "需立即处理", color: "#06b6d4" }
    ];
  }
  if (detailItem.value?.kind === "server") {
    return [
      { label: "告警总数", value: s?.total ?? "-", sub: "周期内全部事件", color: "#3b82f6" },
      { label: "告警中", value: s?.firingCount ?? "-", sub: "仍在告警中", color: "#ef4444" },
      { label: "已恢复", value: s?.resolvedCount ?? "-", sub: "已闭合事件", color: "#22c55e" },
      { label: "受影响主机", value: detailTopHostsList.value.length, sub: "受影响主机数", color: "#f59e0b" },
      { label: "告警类型", value: detailAlertNameDistList.value.length, sub: "告警规则种类", color: "#8b5cf6" },
      { label: "P1 建议数", value: detailP1List.value.length, sub: "需立即处理", color: "#06b6d4" }
    ];
  }
  return [
    { label: "告警总数", value: s?.total ?? "-", sub: "周期内全部事件", color: "#3b82f6" },
    { label: "告警中", value: s?.firingCount ?? "-", sub: "仍在告警中", color: "#ef4444" },
    { label: "已恢复", value: s?.resolvedCount ?? "-", sub: "已闭合事件", color: "#22c55e" },
    { label: "受影响目标", value: detailTopTargetsList.value.length, sub: "探测目标数", color: "#f59e0b" },
    { label: "告警类型", value: detailAlertNameDistList.value.length, sub: "探测规则种类", color: "#8b5cf6" },
    { label: "P1 建议数", value: detailP1List.value.length, sub: "需立即处理", color: "#06b6d4" }
  ];
});

const detailRiskColumns = computed(() => {
  if (detailItem.value?.kind === "zabbix") {
    return [
      { prop: "host_name", label: "主机名", minWidth: 200, overflow: true },
      { prop: "host_ip", label: "IP", width: 150 },
      { prop: "fault_point", label: "故障点", width: 100, tag: true },
      { prop: "count", label: "次数", width: 80, align: "center" },
      { prop: "reason", label: "分析说明", minWidth: 320, overflow: true }
    ];
  }
  if (detailItem.value?.kind === "server") {
    return [
      { prop: "host", label: "主机", minWidth: 200, overflow: true },
      { prop: "issue", label: "问题类型", width: 120, tag: true },
      { prop: "count", label: "次数", width: 80, align: "center" },
      { prop: "reason", label: "分析说明", minWidth: 320, overflow: true }
    ];
  }
  return [
    { prop: "target", label: "探测目标", minWidth: 260, overflow: true },
    { prop: "issue", label: "问题类型", width: 120, tag: true },
    { prop: "count", label: "次数", width: 80, align: "center" },
    { prop: "reason", label: "分析说明", minWidth: 280, overflow: true }
  ];
});

const detailIssueTagType = (value: string) => {
  if (detailItem.value?.kind === "zabbix") {
    if (value === "链路" || value === "可用性") return "danger";
    if (value === "速率" || value === "丢包" || value === "错包" || value === "半双工") return "warning";
    return "info";
  }
  if (detailItem.value?.kind === "server") {
    if (value === "宕机") return "danger";
    if (value === "CPU" || value === "内存") return "warning";
    return "info";
  }
  if (value === "探测失败") return "danger";
  if (value === "响应慢" || value === "SSL证书") return "warning";
  return "info";
};

const renderedReportReady = computed(() => {
  return Number(detailItem.value?.reportStatus) === 1 && !!parsedDetailReport.value;
});

const currentReports = computed(() => reports.value.filter((item) => item.kind === activeType.value));

const currentPendingCount = computed(() => currentReports.value.filter((item) => Number(item.reportStatus) === 0).length);

const pagedReports = computed(() => {
  const start = (pagination.page - 1) * pagination.pageSize;
  return currentReports.value.slice(start, start + pagination.pageSize);
});

const mapZabbixReports = (rows: any[]): UnifiedReportItem[] =>
  rows.map((item) => ({
    ...item,
    kind: "zabbix"
  }));

const mapPrometheusReports = (rows: any[], kind: "server" | "http"): UnifiedReportItem[] =>
  rows.map((item) => ({
    ...item,
    kind
  }));

const sortReports = (rows: UnifiedReportItem[]) =>
  rows.sort((a, b) => {
    const aPending = Number(a.reportStatus === 0);
    const bPending = Number(b.reportStatus === 0);
    if (aPending !== bPending) {
      return bPending - aPending;
    }
    const aTime = a.createDate ? new Date(a.createDate).getTime() : 0;
    const bTime = b.createDate ? new Date(b.createDate).getTime() : 0;
    return bTime - aTime;
  });

const stopPendingRefresh = () => {
  if (pendingRefreshTimer !== null) {
    window.clearInterval(pendingRefreshTimer);
    pendingRefreshTimer = null;
  }
};

const fetchDetail = (kind: "zabbix" | "server" | "http", id: number, silent = false) => {
  if (!silent) {
    detailLoading.value = true;
  }
  const path = kind === "zabbix" ? `/alert/zabbix/report/${id}` : `/alert/prometheus/report/${id}`;
  return baseService
    .get(path)
    .then((res) => {
      detailItem.value = {
        ...(res.data || {}),
        kind
      };
      reports.value = reports.value.map((item) => (item.kind === kind && item.id === id ? { ...item, ...detailItem.value! } : item));
      syncPendingRefresh();
    })
    .finally(() => {
      if (!silent) {
        detailLoading.value = false;
      }
    });
};

const refreshPendingDetail = () => {
  if (!detailVisible.value || !detailItem.value?.id || Number(detailItem.value.reportStatus) !== 0) {
    return;
  }
  fetchDetail(detailItem.value.kind, detailItem.value.id, true);
};

const syncPendingRefresh = () => {
  const hasPending = reports.value.some((item) => Number(item.reportStatus) === 0);
  if (!hasPending) {
    stopPendingRefresh();
    return;
  }
  if (pendingRefreshTimer !== null) {
    return;
  }
  pendingRefreshTimer = window.setInterval(() => {
    fetchReports(true);
    refreshPendingDetail();
  }, PENDING_REFRESH_INTERVAL);
};

const fetchReports = (silent = false) => {
  if (!silent) {
    loading.value = true;
  }
  Promise.all([
    baseService.get("/alert/zabbix/report/latest", { size: LIST_LIMIT }),
    baseService.get("/alert/prometheus/report/latest", { reportType: "server", size: LIST_LIMIT }),
    baseService.get("/alert/prometheus/report/latest", { reportType: "http", size: LIST_LIMIT })
  ])
    .then(([zabbixRes, serverRes, httpRes]) => {
      const merged = [
        ...mapZabbixReports(zabbixRes.data || []),
        ...mapPrometheusReports(serverRes.data || [], "server"),
        ...mapPrometheusReports(httpRes.data || [], "http")
      ];
      reports.value = sortReports(merged);
      const maxPage = Math.max(1, Math.ceil(currentReports.value.length / pagination.pageSize));
      if (pagination.page > maxPage) {
        pagination.page = maxPage;
      }
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
  const request =
    activeType.value === "zabbix"
      ? baseService.post(
          "/alert/zabbix/report/generate",
          { periodType: generatePeriodType.value },
          undefined,
          { timeout: REPORT_GENERATE_TIMEOUT }
        )
      : baseService.post(
          `/alert/prometheus/report/generate?reportType=${activeType.value}&periodType=${generatePeriodType.value}`,
          undefined,
          undefined,
          { timeout: REPORT_GENERATE_TIMEOUT }
        );

  request
    .then(() => {
      ElMessage.success("已提交生成任务，请稍后查看");
      fetchReports();
    })
    .finally(() => {
      generateLoading.value = false;
    });
};

const openDetail = (item: UnifiedReportItem) => {
  detailVisible.value = true;
  detailTab.value = "rendered";
  rawDetailTab.value = "reportJson";
  detailItem.value = item;
  fetchDetail(item.kind, item.id);
};

const handleTypeChange = (value: string | number | boolean) => {
  const nextType = normalizeType(value);
  activeType.value = nextType;
};

onMounted(() => {
  fetchReports();
});

onBeforeUnmount(() => {
  stopPendingRefresh();
});
</script>

<style scoped lang="less">
.mod-report-center {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.toolbar-card {
  margin-bottom: 2px;
  border: 1px solid #e2e8f0;
}

.toolbar-card :deep(.el-card__body) {
  padding: 16px 18px 14px;
}

.toolbar-card__head {
  margin-bottom: 14px;
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 16px;
  flex-wrap: wrap;
}

.toolbar-card__title-wrap {
  min-width: 260px;
}

.toolbar-card__title {
  font-size: 18px;
  font-weight: 700;
  color: #0f172a;
}

.toolbar-card__subtitle {
  margin-top: 4px;
  font-size: 12px;
  color: #64748b;
}

.toolbar-card__summary {
  display: flex;
  align-items: center;
  justify-content: flex-end;
  gap: 8px;
  flex-wrap: wrap;
}

.toolbar-summary-pill {
  display: inline-flex;
  align-items: center;
  height: 30px;
  padding: 0 12px;
  border-radius: 999px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  color: #475569;
  font-size: 12px;
}

.toolbar-summary-pill--pending {
  background: #fffbeb;
  border-color: #fde68a;
  color: #92400e;
}

.toolbar-card__controls {
  display: flex;
  align-items: center;
  flex-wrap: wrap;
  gap: 12px;
  padding-top: 2px;
}

.toolbar-block {
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
}

.toolbar-block__label {
  font-size: 12px;
  font-weight: 600;
  color: #64748b;
  flex-shrink: 0;
}

.toolbar-block--actions {
  margin-left: auto;
}

.toolbar-divider {
  width: 1px;
  height: 28px;
  background: #e2e8f0;
}

.toolbar-card :deep(.el-radio-group) {
  display: flex;
  flex-wrap: wrap;
  gap: 0;
}

.toolbar-card :deep(.el-radio-button__inner) {
  box-shadow: none;
  min-width: 86px;
  padding: 8px 14px;
}

.report-cards {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 14px;
  min-height: 80px;
}

.report-card {
  background: #fff;
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  padding: 16px;
  cursor: pointer;
  display: flex;
  flex-direction: column;
  gap: 12px;
  transition: box-shadow 0.18s, border-color 0.18s, transform 0.18s;
}

.report-card:hover {
  box-shadow: 0 6px 22px rgba(15, 23, 42, 0.08);
  border-color: #93c5fd;
  transform: translateY(-1px);
}

.report-card--failed {
  background: #fff7f7;
  border-color: #fecaca;
}

.report-card--pending {
  background: #fffbeb;
  border-color: #fcd34d;
}

.report-card__head {
  display: flex;
  align-items: flex-start;
  justify-content: space-between;
  gap: 12px;
}

.report-card__tags {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-wrap: wrap;
}

.report-card__time {
  flex-shrink: 0;
  font-size: 12px;
  color: #94a3b8;
}

.report-card__summary {
  min-height: 44px;
  font-size: 15px;
  line-height: 1.6;
  color: #0f172a;
  font-weight: 600;
}

.report-card__meta {
  display: flex;
  flex-direction: column;
  gap: 8px;
}

.report-card__meta-row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  font-size: 12px;
}

.report-card__meta-label {
  color: #64748b;
  flex-shrink: 0;
}

.report-card__meta-value {
  color: #334155;
  text-align: right;
  word-break: break-word;
}

.report-card__footer {
  margin-top: auto;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  padding-top: 6px;
}

.report-card__hint {
  font-size: 12px;
  color: #64748b;
}

.report-card__link {
  font-size: 13px;
  font-weight: 600;
  color: #2563eb;
}

.empty-tip {
  grid-column: 1 / -1;
  padding: 28px 16px;
  text-align: center;
  color: #64748b;
  background: #fff;
  border: 1px dashed #cbd5e1;
  border-radius: 12px;
}

.pagination-bar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  flex-wrap: wrap;
  padding: 4px 2px 0;
}

.pagination-bar__count {
  font-size: 12px;
  color: #64748b;
}

.dialog-title {
  display: flex;
  align-items: center;
  font-size: 16px;
  font-weight: 600;
  color: #0f172a;
}

.detail-body {
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.report-render {
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.kpi-row {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
}

.kpi-card {
  position: relative;
  overflow: hidden;
  border-radius: 12px;
  border: 1px solid #e2e8f0;
  background: #fff;
  padding: 16px;
}

.kpi-card::before {
  content: "";
  position: absolute;
  inset: 0 auto 0 0;
  width: 4px;
  background: var(--kc, #3b82f6);
}

.kpi-card__value {
  font-size: 28px;
  font-weight: 700;
  color: #0f172a;
}

.kpi-card__label {
  margin-top: 6px;
  font-size: 13px;
  font-weight: 600;
  color: #334155;
}

.kpi-card__sub {
  margin-top: 4px;
  font-size: 12px;
  color: #64748b;
}

.two-col {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 16px;
}

.section-card {
  border: 1px solid #e2e8f0;
  border-radius: 12px;
  background: #fff;
  padding: 16px;
}

.section-head {
  margin-bottom: 12px;
  font-size: 15px;
  font-weight: 700;
  color: #0f172a;
}

.bullet-list {
  margin: 0;
  padding-left: 18px;
  color: #334155;
  line-height: 1.75;
}

.bullet-list li + li {
  margin-top: 6px;
}

.priority-row {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  gap: 12px;
}

.priority-col {
  border-radius: 10px;
  padding: 14px;
}

.priority-col--p1 {
  background: #fff1f2;
}

.priority-col--p2 {
  background: #fffbeb;
}

.priority-col--p3 {
  background: #f8fafc;
}

.priority-label {
  margin-bottom: 10px;
  font-size: 13px;
  font-weight: 700;
  color: #334155;
}

.checklist {
  display: flex;
  flex-direction: column;
  gap: 10px;
}

.checklist-item {
  display: flex;
  align-items: flex-start;
  gap: 10px;
}

.checklist-idx {
  flex-shrink: 0;
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 22px;
  height: 22px;
  border-radius: 999px;
  background: #e0f2fe;
  color: #0369a1;
  font-size: 12px;
  font-weight: 700;
}

.checklist-text {
  color: #334155;
  line-height: 1.7;
}

.pending-tip {
  padding: 12px 14px;
  border: 1px solid #fde68a;
  background: #fffbeb;
  color: #92400e;
  border-radius: 10px;
}

.detail-pre {
  margin: 0;
  max-height: 460px;
  overflow: auto;
  white-space: pre-wrap;
  word-break: break-word;
  background: #0f172a;
  color: #e2e8f0;
  border-radius: 12px;
  padding: 16px;
  line-height: 1.6;
  font-size: 12px;
}

@media (max-width: 1200px) {
  .report-cards {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }

  .kpi-row,
  .priority-row {
    grid-template-columns: repeat(2, minmax(0, 1fr));
  }
}

@media (max-width: 768px) {
  .toolbar-card__head {
    flex-direction: column;
    align-items: flex-start;
  }

  .toolbar-card__summary {
    justify-content: flex-start;
  }

  .toolbar-card__controls {
    align-items: flex-start;
    flex-direction: column;
  }

  .toolbar-divider {
    display: none;
  }

  .toolbar-block--actions {
    margin-left: 0;
  }

  .report-cards {
    grid-template-columns: 1fr;
  }

  .kpi-row,
  .two-col,
  .priority-row {
    grid-template-columns: 1fr;
  }

  .pagination-bar {
    align-items: flex-start;
  }
}
</style>
