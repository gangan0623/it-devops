<template>
  <div class="mod-home">
    <div class="home-grid">
      <div class="home-overview">
        <el-card shadow="never" class="home-card home-card--overview top-card">
          <div class="overview-top">
            <div class="card-title">总览</div>
            <div class="metric-grid">
              <div class="metric-item">
                <div class="metric-value">{{ summary.hostCounts.windows }}</div>
                <div class="metric-label">Windows</div>
              </div>
              <div class="metric-item">
                <div class="metric-value">{{ summary.hostCounts.linux }}</div>
                <div class="metric-label">Linux</div>
              </div>
              <div class="metric-item">
                <div class="metric-value">{{ summary.hostCounts.business }}</div>
                <div class="metric-label">业务系统</div>
              </div>
            </div>
          </div>

          <div class="backup-divider"></div>

          <div class="overview-bottom">
            <div class="card-title card-title--row">
              <span>最近备份</span>
              <span class="card-link" @click="goBackupRecord">详情</span>
            </div>
            <div class="backup-panel">
              <div class="backup-chart">
                <el-progress type="dashboard" :percentage="backupSuccessRate" :width="110" :stroke-width="8" :color="backupColors">
                  <template #default="{ percentage }">
                    <div class="backup-percentage-value">{{ percentage }}%</div>
                    <div class="backup-percentage-label">成功率</div>
                  </template>
                </el-progress>
              </div>
              <div class="backup-info">
                <div class="backup-round-tag">第 {{ summary.backupStats.round || 0 }} 轮</div>
                <div class="backup-detail-grid">
                  <div class="backup-detail-item">
                    <div class="label">总数</div>
                    <div class="value">{{ summary.backupStats.total || 0 }}</div>
                  </div>
                  <div class="backup-detail-item">
                    <div class="label">失败</div>
                    <div class="value danger">{{ summary.backupStats.fail || 0 }}</div>
                  </div>
                </div>
                <div class="backup-time-row">
                  <div class="label">最近时间</div>
                  <div class="value">{{ summary.backupStats.lastTime || "-" }}</div>
                </div>
              </div>
            </div>
          </div>
        </el-card>
      </div>

      <div class="home-diff">
        <el-card shadow="never" class="home-card home-card--diff top-card">
          <div class="card-title card-title--row">
            <span>设备差异对比</span>
            <el-button-group>
              <el-button size="small" class="diff-switch" :type="activeDiff === 'backupOnly' ? 'primary' : 'default'" @click="activeDiff = 'backupOnly'">
                备份有 ({{ summary.deviceDiff.backupOnly.length || 0 }})
              </el-button>
              <el-button size="small" class="diff-switch" :type="activeDiff === 'zabbixOnly' ? 'primary' : 'default'" @click="activeDiff = 'zabbixOnly'">
                Zabbix有 ({{ summary.deviceDiff.zabbixOnly.length || 0 }})
              </el-button>
            </el-button-group>
          </div>
          <el-input v-model="filters.diff" placeholder="搜索 IP/名称" size="default" clearable class="diff-search" />
          <el-table :data="filteredDiffList" class="flex-table" border>
            <el-table-column prop="ip" label="IP" min-width="120" />
            <el-table-column prop="name" label="主机名称" min-width="140" show-overflow-tooltip />
          </el-table>
        </el-card>
      </div>

      <div class="home-monitor">
        <el-card shadow="never" class="home-card home-card--monitor top-card">
          <div class="card-title card-title--row">
            <span>监控组件预览</span>
            <el-button size="small" @click="handleMonitorRefresh">刷新</el-button>
          </div>
          <el-table :data="summary.monitorComponents" class="flex-table" border>
            <el-table-column prop="name" label="名称" min-width="140" show-overflow-tooltip />
            <el-table-column prop="onlineStatus" label="在线" width="80">
              <template v-slot="scope">
                <el-tag v-if="scope.row.onlineStatus === 1" size="small" type="success" effect="plain">在线</el-tag>
                <el-tag v-else-if="scope.row.onlineStatus === 0" size="small" type="danger" effect="plain">离线</el-tag>
                <el-tag v-else size="small" type="info" effect="plain">未知</el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="updateAvailable" label="可更新" width="80">
              <template v-slot="scope">
                <el-tag v-if="scope.row.updateAvailable === 1" size="small" type="warning" effect="plain">是</el-tag>
                <el-tag v-else-if="scope.row.updateAvailable === 0" size="small" type="success" effect="plain">否</el-tag>
                <el-tag v-else size="small" type="info" effect="plain">未知</el-tag>
              </template>
            </el-table-column>
          </el-table>
        </el-card>
      </div>

      <el-card shadow="never" class="home-card home-card--alerts">
        <div class="card-title card-title--row">
          <span>实时告警情况</span>
          <span class="card-link" @click="goProblem">查看详情</span>
        </div>
        <el-table :data="summary.recentAlerts" height="280" border class="alert-record-table">
          <el-table-column prop="time" label="时间" header-align="center" align="center" width="165" />
          <el-table-column label="严重性" header-align="center" align="center" width="90">
            <template #default="scope">
              <span :class="severityClass(scope.row)">{{ formatSeverity(scope.row.severity) }}</span>
            </template>
          </el-table-column>
          <el-table-column label="状态" header-align="center" align="center" width="90">
            <template #default="scope">
              <span :class="statusClass(scope.row.status)">{{ formatStatus(scope.row.status) }}</span>
            </template>
          </el-table-column>
          <el-table-column prop="hostName" label="主机名" header-align="center" align="center" min-width="180">
            <template #default="scope">
              <el-tooltip placement="top" effect="light" :show-after="250">
                <template #content>
                  <div class="event-tip">
                    <div class="event-tip__title">主机信息</div>
                    <div class="event-tip__group">
                      <div class="event-tip__row">
                        <span class="event-tip__key">主机名</span>
                        <span class="event-tip__value">{{ scope.row.hostName || "-" }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">实例</span>
                        <span class="event-tip__value">{{ scope.row.instance || "-" }}</span>
                      </div>
                    </div>
                  </div>
                </template>
                <span>{{ scope.row.hostName || "-" }}</span>
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column prop="alertName" label="告警名称" header-align="center" align="center" min-width="220">
            <template #default="scope">
              <el-tooltip placement="top" effect="light" :show-after="250">
                <template #content>
                  <div class="event-tip">
                    <div class="event-tip__title">告警详情</div>
                    <div class="event-tip__group">
                      <div class="event-tip__row">
                        <span class="event-tip__key">告警名</span>
                        <span class="event-tip__value">{{ scope.row.alertName || "-" }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">实例</span>
                        <span class="event-tip__value">{{ scope.row.instance || "-" }}</span>
                      </div>
                    </div>
                  </div>
                </template>
                <span>{{ scope.row.alertName || "-" }}</span>
              </el-tooltip>
            </template>
          </el-table-column>
        </el-table>
      </el-card>
    </div>
  </div>
</template>

<script setup lang="ts">
import {computed, onBeforeUnmount, onMounted, reactive, ref} from "vue";
import baseService from "@/service/baseService";
import {useRouter} from "vue-router";
import app from "@/constants/app";
import {getToken} from "@/utils/cache";

const router = useRouter();

const summary = reactive({
  hostCounts: {
    windows: 0,
    linux: 0,
    business: 0
  },
  backupStats: {
    round: 0,
    total: 0,
    success: 0,
    fail: 0,
    lastTime: ""
  },
  deviceDiff: {
    zabbixOnly: [],
    backupOnly: []
  },
  recentAlerts: [],
  monitorComponents: []
});

const filters = reactive({
  diff: ""
});

const activeDiff = ref<"backupOnly" | "zabbixOnly">("backupOnly");

const backupSuccessRate = computed(() => {
  const { total, success } = summary.backupStats;
  if (!total || total === 0) return 0;
  return Math.floor((success / total) * 100);
});

const backupColors = [
  { color: '#f56c6c', percentage: 60 },
  { color: '#e6a23c', percentage: 90 },
  { color: '#5cb87a', percentage: 100 },
];

const filteredDiffList = computed(() => {
  const q = filters.diff.trim().toLowerCase();
  const source = activeDiff.value === "backupOnly" ? summary.deviceDiff.backupOnly : summary.deviceDiff.zabbixOnly;
  if (!q) return source;
  return source.filter((item: any) => {
    return String(item.ip || "").toLowerCase().includes(q) || String(item.name || "").toLowerCase().includes(q);
  });
});

const monitorRefreshRequested = ref(false);

const refreshMonitorComponents = (components: Array<{ id?: number }>) => {
  if (!components || components.length === 0) {
    return;
  }
  const ids = components.map((item) => Number(item.id)).filter((id) => Number.isFinite(id));
  if (ids.length === 0) {
    return;
  }
  ids.forEach((id) => {
    baseService.get("/ops/monitorcomponent/probe", { id });
    baseService.get("/ops/monitorcomponent/version", { id });
  });
  setTimeout(() => {
    loadSummary(false);
  }, 800);
};

const loadSummary = (triggerMonitorRefresh = false) => {
  baseService.get("/ops/dashboard/summary").then((res) => {
    if (res.data) {
      Object.assign(summary, res.data);
      if (triggerMonitorRefresh && !monitorRefreshRequested.value) {
        monitorRefreshRequested.value = true;
        refreshMonitorComponents(summary.monitorComponents || []);
      }
    }
  });
};

const handleMonitorRefresh = () => {
  refreshMonitorComponents(summary.monitorComponents || []);
};

const goBackupRecord = () => {
  router.push({ path: "/ops/devicebackup-record" });
};

const goProblem = () => {
  router.push({ path: "/alert/problem" });
};

const formatSeverity = (value: string) => {
  const s = String(value || "").toLowerCase();
  if (s === "critical") return "灾难";
  if (s === "warning") return "重要";
  if (s === "info") return "信息";
  if (s === "recover" || s === "resolved") return "恢复";
  return value;
};

const formatStatus = (value: string) => {
  const s = String(value || "").toLowerCase();
  if (s === "firing") return "告警";
  if (s === "resolved") return "恢复";
  return value;
};

const severityClass = (row: any) => {
  const status = String(row?.status || "").toLowerCase();
  if (status === "resolved") return "severity-tag severity-tag--resolved";
  const severity = String(row?.severity || "").toLowerCase();
  if (severity === "critical") return "severity-tag severity-tag--critical";
  if (severity === "warning") return "severity-tag severity-tag--warning";
  return "severity-tag severity-tag--info";
};

const statusClass = (value: string) => {
  const s = String(value || "").toLowerCase();
  if (s === "resolved") return "status-tag status-tag--ok";
  return "status-tag status-tag--bad";
};

const sseRef = ref<EventSource | null>(null);

const initSse = () => {
  const token = getToken();
  if (!token) {
    return;
  }
  const url = `${app.api}/alert/record/stream?token=${token}`;
  const es = new EventSource(url);
  es.addEventListener("recentAlerts", (event: MessageEvent) => {
    try {
      const data = JSON.parse(event.data || "[]");
      summary.recentAlerts = Array.isArray(data) ? data : [];
    } catch (e) {
      summary.recentAlerts = [];
    }
  });
  sseRef.value = es;
};

onMounted(() => {
  loadSummary(true);
  initSse();
});

onBeforeUnmount(() => {
  if (sseRef.value) {
    sseRef.value.close();
    sseRef.value = null;
  }
});
</script>

<style scoped>
.mod-home {
  padding: 16px;
}

.home-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  grid-template-areas:
    "overview diff monitor"
    "alerts alerts alerts";
  gap: 14px;
}

.home-overview { grid-area: overview; }
.home-diff { grid-area: diff; }
.home-monitor { grid-area: monitor; }
.home-card--alerts { grid-area: alerts; }

.home-card {
  border-radius: 8px;
  border: 1px solid #e2e8f0;
  box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.04), 0 2px 4px -2px rgba(0, 0, 0, 0.02);
}

/* 顶部三个卡片高度统一 */
.top-card {
  height: 360px !important;
}

/* 表格 Flex 自适应填充 */
.flex-table {
  flex: 1;
  width: 100%;
}

/* 卡片内部结构 */
.home-card--overview :deep(.el-card__body),
.home-card--diff :deep(.el-card__body),
.home-card--monitor :deep(.el-card__body) {
  display: flex;
  flex-direction: column;
  height: 100%;
  padding: 20px;
}

/* --- 总览卡片 --- */
.overview-top {
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-height: 0;
}

.overview-bottom {
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center;
  min-height: 0;
  padding-top: 8px;
}

.backup-panel {
  display: flex;
  align-items: center;
  justify-content: center;
  gap: 24px;
  margin-top: 4px;
}

.backup-chart {
  flex: 1;
  display: flex;
  justify-content: flex-end;
}

.backup-percentage-value {
  font-size: 22px;
  font-weight: 700;
  color: #0f172a;
  line-height: 1.2;
}

.backup-percentage-label {
  font-size: 12px;
  color: #64748b;
}

.backup-info {
  flex: 1;
  display: flex;
  flex-direction: column;
  gap: 8px;
  align-items: flex-start;
}

.backup-round-tag {
  align-self: flex-start;
  background: #f1f5f9;
  color: #475569;
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}

.backup-detail-grid {
  display: flex;
  gap: 24px;
}

.backup-detail-item .label {
  font-size: 12px;
  color: #64748b;
  margin-bottom: 2px;
}

.backup-detail-item .value {
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
}

.backup-time-row .label {
  font-size: 12px;
  color: #64748b;
  display: none;
}

.backup-time-row .value {
  font-size: 12px;
  color: #64748b;
}

/* 指标网格 */
.metric-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 14px;
}

.metric-item {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 14px 8px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  transition: all 0.15s ease;
}

.metric-item:hover {
  background: #f1f5f9;
  border-color: #cbd5e1;
}

.metric-value {
  font-size: 26px;
  font-weight: 700;
  color: #0f172a;
  line-height: 1.2;
}

.metric-label {
  font-size: 13px;
  color: #475569;
  margin-top: 4px;
  font-weight: 500;
}

.backup-divider {
  height: 1px;
  background: #e2e8f0;
  margin: 14px 0;
  flex-shrink: 0;
}

.backup-stats {
  display: flex;
  flex-direction: column;
  gap: 12px;
}

.backup-row {
  display: flex;
  justify-content: space-between;
  font-size: 14px;
  line-height: 1.5;
  color: #334155;
}

/* 卡片标题 */
.card-title {
  font-size: 15px;
  font-weight: 600;
  margin-bottom: 14px;
  color: #0f172a;
  flex-shrink: 0;
}

.card-title--row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 10px;
  font-size: 15px;
  font-weight: 600;
  margin-bottom: 14px;
  color: #0f172a;
  flex-shrink: 0;
}

.card-title--link {
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 8px;
}

/* 卡片链接按钮 */
.card-link {
  cursor: pointer;
  font-size: 12px;
  font-weight: 500;
  padding: 4px 10px;
  border-radius: 4px;
  background: #3b82f6;
  color: #fff;
  transition: background 0.15s ease;
}

.card-link:hover {
  background: #2563eb;
}

.success { color: #10b981; }
.danger { color: #ef4444; }
.diff-search { margin-bottom: 12px; flex-shrink: 0; }
.card-title--row .el-button-group { flex-shrink: 0; }
.diff-switch { padding: 5px 10px; }

/* 告警表格 */
.alert-record-table :deep(.cell) {
  white-space: nowrap;
}

/* 事件提示框 */
.event-tip {
  min-width: 320px;
  max-width: 420px;
}

.event-tip__title {
  margin-bottom: 10px;
  padding-bottom: 8px;
  font-size: 13px;
  font-weight: 600;
  color: #0f172a;
  border-bottom: 1px solid #e2e8f0;
}

.event-tip__group {
  margin-bottom: 10px;
  padding: 10px;
  background: #f8fafc;
  border-radius: 6px;
}

.event-tip__row {
  display: flex;
  gap: 12px;
  margin-bottom: 6px;
  line-height: 1.5;
}

.event-tip__row:last-child {
  margin-bottom: 0;
}

.event-tip__key {
  flex: 0 0 64px;
  color: #64748b;
  font-size: 12px;
}

.event-tip__value {
  flex: 1;
  white-space: normal;
  word-break: break-all;
  color: #0f172a;
}

@media (max-width: 1200px) {
  .home-grid {
    grid-template-columns: 1fr;
    grid-template-areas: "overview" "diff" "monitor" "alerts";
  }
}
</style>
