<template>
  <div class="mod-home">
    <!-- 顶部指标条 -->
    <div class="stat-strip">
      <div class="stat-card">
        <div class="stat-card__value">{{ summary.hostCounts.windows }}</div>
        <div class="stat-card__label">Windows</div>
      </div>
      <div class="stat-card">
        <div class="stat-card__value">{{ summary.hostCounts.linux }}</div>
        <div class="stat-card__label">Linux</div>
      </div>
      <div class="stat-card">
        <div class="stat-card__value">{{ summary.hostCounts.business }}</div>
        <div class="stat-card__label">业务系统</div>
      </div>
      <div class="stat-card stat-card--backup" @click="goBackupRecord">
        <div class="backup-bar">
          <div class="backup-bar__label">备份成功率</div>
          <div class="backup-bar__track">
            <div class="backup-bar__fill" :style="{ width: backupSuccessRate + '%' }" :class="backupRateClass"></div>
          </div>
          <div class="backup-bar__pct">{{ backupSuccessRate }}%</div>
        </div>
        <div class="backup-meta">
          <span>第 {{ summary.backupStats.round || 0 }} 轮</span>
          <span class="backup-meta__sep"></span>
          <span>总数 {{ summary.backupStats.total || 0 }}</span>
          <span class="backup-meta__sep"></span>
          <span :class="{ 'backup-meta--danger': summary.backupStats.fail > 0 }">失败 {{ summary.backupStats.fail || 0 }}</span>
          <span class="backup-meta__sep"></span>
          <span class="backup-meta--time">{{ summary.backupStats.lastTime || "-" }}</span>
        </div>
      </div>
    </div>

    <!-- 主内容区 -->
    <div class="main-grid">
      <!-- 左：实时告警 -->
      <el-card shadow="never" class="home-card home-card--alerts">
        <div class="card-header">
          <span class="card-header__title">实时告警</span>
          <span class="card-link" @click="goProblem">查看详情</span>
        </div>
        <el-table :data="summary.recentAlerts" border class="alert-table" max-height="520">
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
          <el-table-column prop="hostName" label="主机名" header-align="center" align="center" min-width="160">
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
          <el-table-column prop="alertName" label="告警名称" header-align="center" align="center" min-width="200">
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

      <!-- 右侧栏 -->
      <div class="side-stack">
        <!-- 监控组件 -->
        <el-card shadow="never" class="home-card home-card--monitor">
          <div class="card-header">
            <span class="card-header__title">监控组件</span>
            <el-button size="small" text type="primary" @click="handleMonitorRefresh">刷新</el-button>
          </div>
          <el-table :data="summary.monitorComponents" border height="100%">
            <el-table-column prop="name" label="名称" min-width="120" show-overflow-tooltip />
            <el-table-column prop="onlineStatus" label="在线" width="70" align="center">
              <template #default="scope">
                <span :class="onlineClass(scope.row.onlineStatus)">{{ onlineText(scope.row.onlineStatus) }}</span>
              </template>
            </el-table-column>
            <el-table-column prop="updateAvailable" label="更新" width="70" align="center">
              <template #default="scope">
                <span :class="updateClass(scope.row.updateAvailable)">{{ updateText(scope.row.updateAvailable) }}</span>
              </template>
            </el-table-column>
          </el-table>
        </el-card>

        <!-- 设备差异 -->
        <el-card shadow="never" class="home-card home-card--diff">
          <div class="card-header">
            <el-input v-model="filters.diff" placeholder="搜索 IP/名称" clearable class="diff-search" />
            <el-button-group>
              <el-button size="small" :type="activeDiff === 'backupOnly' ? 'primary' : 'default'" @click="activeDiff = 'backupOnly'">
                备份有 ({{ summary.deviceDiff.backupOnly.length || 0 }})
              </el-button>
              <el-button size="small" :type="activeDiff === 'zabbixOnly' ? 'primary' : 'default'" @click="activeDiff = 'zabbixOnly'">
                Zabbix有 ({{ summary.deviceDiff.zabbixOnly.length || 0 }})
              </el-button>
            </el-button-group>
          </div>
          <el-table :data="filteredDiffList" border height="100%">
            <el-table-column prop="ip" label="IP" min-width="110" />
            <el-table-column prop="name" label="主机名称" min-width="120" show-overflow-tooltip />
          </el-table>
        </el-card>
      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, onBeforeUnmount, onMounted, reactive, ref } from "vue";
import baseService from "@/service/baseService";
import { useRouter } from "vue-router";
import app from "@/constants/app";
import { getToken } from "@/utils/cache";

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

const backupRateClass = computed(() => {
  const rate = backupSuccessRate.value;
  if (rate >= 90) return "backup-bar__fill--ok";
  if (rate >= 60) return "backup-bar__fill--warn";
  return "backup-bar__fill--bad";
});

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
  router.push({ path: "/problem" });
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

const onlineClass = (v: number) => {
  if (v === 1) return "dot-tag dot-tag--ok";
  if (v === 0) return "dot-tag dot-tag--bad";
  return "dot-tag dot-tag--unknown";
};
const onlineText = (v: number) => {
  if (v === 1) return "在线";
  if (v === 0) return "离线";
  return "未知";
};
const updateClass = (v: number) => {
  if (v === 1) return "dot-tag dot-tag--warn";
  if (v === 0) return "dot-tag dot-tag--ok";
  return "dot-tag dot-tag--unknown";
};
const updateText = (v: number) => {
  if (v === 1) return "是";
  if (v === 0) return "否";
  return "未知";
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
  display: flex;
  flex-direction: column;
  gap: 14px;
}

/* ========== 顶部指标条 ========== */
.stat-strip {
  display: flex;
  gap: 14px;
}

.stat-card {
  background: #fff;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 18px 20px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-width: 0;
  flex: 0 0 130px;
  transition: border-color 0.15s;
}

.stat-card:hover {
  border-color: #cbd5e1;
}

.stat-card__value {
  font-size: 28px;
  font-weight: 700;
  color: #0f172a;
  line-height: 1.2;
}

.stat-card__label {
  font-size: 12px;
  color: #64748b;
  margin-top: 4px;
  font-weight: 500;
}

/* 备份卡片 */
.stat-card--backup {
  flex: 1;
  align-items: stretch;
  cursor: pointer;
  padding: 14px 20px;
  gap: 8px;
}

.backup-bar {
  display: flex;
  align-items: center;
  gap: 10px;
}

.backup-bar__label {
  font-size: 12px;
  color: #64748b;
  white-space: nowrap;
  font-weight: 500;
}

.backup-bar__track {
  flex: 1;
  height: 8px;
  background: #f1f5f9;
  border-radius: 4px;
  overflow: hidden;
}

.backup-bar__fill {
  height: 100%;
  border-radius: 4px;
  transition: width 0.4s ease;
}

.backup-bar__fill--ok { background: #10b981; }
.backup-bar__fill--warn { background: #f59e0b; }
.backup-bar__fill--bad { background: #ef4444; }

.backup-bar__pct {
  font-size: 15px;
  font-weight: 700;
  color: #0f172a;
  min-width: 36px;
  text-align: right;
}

.backup-meta {
  display: flex;
  align-items: center;
  gap: 8px;
  font-size: 12px;
  color: #64748b;
}

.backup-meta__sep {
  width: 1px;
  height: 10px;
  background: #e2e8f0;
}

.backup-meta--danger {
  color: #ef4444;
  font-weight: 600;
}

.backup-meta--time {
  margin-left: auto;
  color: #94a3b8;
}

/* ========== 主内容区 ========== */
.main-grid {
  display: grid;
  grid-template-columns: 3fr 2fr;
  gap: 14px;
  align-items: stretch;
}

.home-card {
  border-radius: 8px;
  border: 1px solid #e2e8f0;
}

.home-card :deep(.el-card__body) {
  display: flex;
  flex-direction: column;
  padding: 16px;
}

.card-header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 12px;
  flex-shrink: 0;
}

.card-header__title {
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
}

.card-link {
  cursor: pointer;
  font-size: 12px;
  font-weight: 500;
  padding: 4px 10px;
  border-radius: 4px;
  background: #3b82f6;
  color: #fff;
  transition: background 0.15s;
}

.card-link:hover {
  background: #2563eb;
}

/* 告警表格 */
.alert-table :deep(.cell) {
  white-space: nowrap;
}

/* 右侧栏 */
.side-stack {
  display: flex;
  flex-direction: column;
  gap: 14px;
}

.home-card--monitor,
.home-card--diff {
  height: 260px;
  overflow: hidden;
}

.side-stack .home-card :deep(.el-card__body) {
  height: 100%;
}

.diff-search {
  width: 250px;
  flex-shrink: 0;
}

/* 小圆点状态标签 */
.dot-tag {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  font-size: 12px;
  font-weight: 500;
}

.dot-tag::before {
  content: "";
  width: 6px;
  height: 6px;
  border-radius: 50%;
}

.dot-tag--ok { color: #10b981; }
.dot-tag--ok::before { background: #10b981; }
.dot-tag--bad { color: #ef4444; }
.dot-tag--bad::before { background: #ef4444; }
.dot-tag--warn { color: #f59e0b; }
.dot-tag--warn::before { background: #f59e0b; }
.dot-tag--unknown { color: #94a3b8; }
.dot-tag--unknown::before { background: #94a3b8; }

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
  .stat-strip {
    flex-wrap: wrap;
  }

  .stat-card {
    flex: 1 1 100px;
  }

  .stat-card--backup {
    flex-basis: 100%;
  }

  .main-grid {
    grid-template-columns: 1fr;
  }

  .side-stack {
    flex-direction: row;
  }

  .home-card--monitor,
  .home-card--diff {
    flex: 1;
  }
}

@media (max-width: 768px) {
  .side-stack {
    flex-direction: column;
  }
}
</style>
