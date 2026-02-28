<template>
  <div class="mod-home">
    <el-card shadow="never" class="home-card home-card--asset">
      <div class="card-header card-header--asset">
        <span class="card-header__title">资产状态 <span class="asset-count">共 {{ assetTotal }} 项</span></span>
      </div>
      <div class="stat-strip">

        <div :class="['stat-card', 'stat-card--link', { 'stat-card--zero': !totalWindows }]" @click="goAsset('/ops/windowhost')">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="currentColor"><rect x="0" y="0" width="6" height="6" rx="1"/><rect x="8" y="0" width="6" height="6" rx="1"/><rect x="0" y="8" width="6" height="6" rx="1"/><rect x="8" y="8" width="6" height="6" rx="1"/></svg></span>
            <span class="stat-card__name">Windows</span>
          </div>
          <div class="stat-card__hero">{{ totalWindows }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.windowsOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.windowsOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.windowsOffline || 0 }}</span>
          </div>
        </div>

        <div :class="['stat-card', 'stat-card--link', { 'stat-card--zero': !totalLinux }]" @click="goAsset('/ops/linuxhost')">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><rect x="1" y="2" width="12" height="10" rx="1.5"/><polyline points="3.5,5.5 6,7 3.5,8.5"/><line x1="7.5" y1="8.5" x2="10.5" y2="8.5"/></svg></span>
            <span class="stat-card__name">Linux</span>
          </div>
          <div class="stat-card__hero">{{ totalLinux }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.linuxOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.linuxOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.linuxOffline || 0 }}</span>
          </div>
        </div>

        <div :class="['stat-card', 'stat-card--link', { 'stat-card--zero': !totalBusiness }]" @click="goAsset('/ops/businesssystem')">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"><rect x="1" y="2" width="12" height="10" rx="1.5"/><line x1="1" y1="5.5" x2="13" y2="5.5"/><circle cx="3.5" cy="3.8" r="0.8" fill="currentColor" stroke="none"/><circle cx="6" cy="3.8" r="0.8" fill="currentColor" stroke="none"/></svg></span>
            <span class="stat-card__name">业务系统</span>
          </div>
          <div class="stat-card__hero">{{ totalBusiness }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.businessOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.businessOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.businessOffline || 0 }}</span>
          </div>
        </div>

        <div :class="['stat-card', 'stat-card--link', { 'stat-card--zero': !totalNetwork }]" @click="goAsset('/ops/networkhost')">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"><rect x="1" y="5" width="12" height="4" rx="1"/><line x1="3.5" y1="5" x2="3.5" y2="2.5"/><line x1="7" y1="5" x2="7" y2="2.5"/><line x1="10.5" y1="5" x2="10.5" y2="2.5"/><circle cx="3.5" cy="7" r="0.7" fill="currentColor" stroke="none"/><circle cx="7" cy="7" r="0.7" fill="currentColor" stroke="none"/><circle cx="10.5" cy="7" r="0.7" fill="currentColor" stroke="none"/><line x1="7" y1="9" x2="7" y2="11.5"/></svg></span>
            <span class="stat-card__name">网络设备</span>
          </div>
          <div class="stat-card__hero">{{ totalNetwork }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.networkOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.networkOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.networkOffline || 0 }}</span>
          </div>
        </div>

        <div :class="['stat-card', { 'stat-card--zero': !totalPhysical }]">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round"><rect x="1" y="1.5" width="12" height="3.5" rx="1"/><rect x="1" y="6" width="12" height="3.5" rx="1"/><rect x="1" y="10.5" width="12" height="2" rx="1"/><circle cx="10.5" cy="3.3" r="0.7" fill="currentColor" stroke="none"/><circle cx="10.5" cy="7.8" r="0.7" fill="currentColor" stroke="none"/></svg></span>
            <span class="stat-card__name">实体机</span>
          </div>
          <div class="stat-card__hero">{{ totalPhysical }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.physicalOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.physicalOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.physicalOffline || 0 }}</span>
          </div>
        </div>

        <div :class="['stat-card', { 'stat-card--zero': !totalVm }]">
          <div class="stat-card__head">
            <span class="stat-card__icon"><svg viewBox="0 0 14 14" width="14" height="14" fill="none" stroke="currentColor" stroke-width="1.5" stroke-linecap="round" stroke-linejoin="round"><rect x="1" y="1" width="12" height="8" rx="1"/><line x1="1" y1="11" x2="13" y2="11"/><line x1="3.5" y1="13" x2="10.5" y2="13"/></svg></span>
            <span class="stat-card__name">虚拟机</span>
          </div>
          <div class="stat-card__hero">{{ totalVm }}</div>
          <div class="stat-card__sep"></div>
          <div class="stat-card__foot">
            <span class="stat-foot stat-foot--ok"><span class="stat-foot__dot"></span>正常 {{ summary.hostCounts.vmOnline || 0 }}</span>
            <span class="stat-foot__bar">|</span>
            <span :class="['stat-foot', summary.hostCounts.vmOffline > 0 ? 'stat-foot--bad' : 'stat-foot--zero']"><span class="stat-foot__dot"></span>异常 {{ summary.hostCounts.vmOffline || 0 }}</span>
          </div>
        </div>

      </div>
    </el-card>

    <!-- 主内容区 -->
    <div class="main-grid">
      <!-- 左：实时告警 -->
      <el-card ref="alertsCardRef" shadow="never" class="home-card home-card--alerts">
        <div class="card-header">
          <span class="card-header__title">实时告警</span>
          <span class="card-link" @click="goProblem">查看详情</span>
        </div>
        <el-table :data="summary.recentAlerts" border class="alert-table" :row-class-name="alertRowClass">
          <template #empty>
            <div class="alert-empty">
              <span class="alert-empty__icon">✓</span>
              <span>当前无活跃告警</span>
            </div>
          </template>
          <el-table-column prop="time" label="记录时间" header-align="center" align="center" width="165" />
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
      <div class="side-stack" :style="sideStackStyle">
        <el-card shadow="never" class="home-card home-card--backup">
          <div class="card-header">
            <span class="card-header__title">备份状态</span>
            <span class="card-link" @click="goBackupRecord">查看详情</span>
          </div>
          <div class="backup-rate">
            <span class="backup-rate__value" :style="{ color: backupRateColor }">{{ backupSuccessRate }}%</span>
            <span class="backup-rate__label">成功率</span>
          </div>
          <div class="backup-bar">
            <div class="backup-bar__track">
              <div class="backup-bar__fill" :style="{ width: backupSuccessRate + '%' }" :class="backupRateClass"></div>
            </div>
          </div>
          <div class="backup-metrics">
            <div class="backup-metric">
              <span class="backup-metric__name">轮次</span>
              <span class="backup-metric__value">{{ summary.backupStats.round || 0 }}</span>
            </div>
            <div class="backup-metric">
              <span class="backup-metric__name">总数</span>
              <span class="backup-metric__value">{{ summary.backupStats.total || 0 }}</span>
            </div>
            <div class="backup-metric">
              <span class="backup-metric__name">失败</span>
              <span :class="['backup-metric__value', summary.backupStats.fail > 0 ? 'backup-metric__value--danger' : '']">{{ summary.backupStats.fail || 0 }}</span>
            </div>
          </div>
          <div class="backup-time">最近备份：{{ summary.backupStats.lastTime || "-" }}</div>
        </el-card>

        <!-- 监控组件 -->
        <el-card shadow="never" class="home-card home-card--monitor">
          <div class="card-header">
            <span class="card-header__title">监控组件</span>
            <el-button size="small" text type="primary" @click="handleMonitorRefresh">刷新</el-button>
          </div>
          <div class="component-overview">
            <span class="component-pill component-pill--ok">在线 {{ componentOnlineCount }}/{{ componentTotal }}</span>
            <span :class="['component-pill', componentUpdateCount > 0 ? 'component-pill--warn' : 'component-pill--neutral']">待更新 {{ componentUpdateCount }}</span>
          </div>
          <el-table :data="summary.monitorComponents" border>
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

      </div>
    </div>
  </div>
</template>

<script setup lang="ts">
import { computed, nextTick, onBeforeUnmount, onMounted, reactive, ref } from "vue";
import baseService from "@/service/baseService";
import { useRouter } from "vue-router";
import app from "@/constants/app";
import { getToken } from "@/utils/cache";

const router = useRouter();

const summary = reactive({
  hostCounts: {
    assetTotal: 0,
    windows: 0,
    windowsOnline: 0,
    windowsOffline: 0,
    linux: 0,
    linuxOnline: 0,
    linuxOffline: 0,
    business: 0,
    businessOnline: 0,
    businessOffline: 0,
    network: 0,
    networkOnline: 0,
    networkOffline: 0,
    physical: 0,
    physicalOnline: 0,
    physicalOffline: 0,
    vm: 0,
    vmOnline: 0,
    vmOffline: 0
  },
  backupStats: {
    round: 0,
    total: 0,
    success: 0,
    fail: 0,
    lastTime: ""
  },
  recentAlerts: [],
  monitorComponents: []
});

// 资产总数（直接用 API 返回的总数字段）
const totalWindows = computed(() => Number(summary.hostCounts.windows || 0));
const totalLinux = computed(() => Number(summary.hostCounts.linux || 0));
const totalBusiness = computed(() => Number(summary.hostCounts.business || 0));
const totalNetwork = computed(() => Number(summary.hostCounts.network || 0));
const totalPhysical = computed(() => Number(summary.hostCounts.physical || 0));
const totalVm = computed(() => Number(summary.hostCounts.vm || 0));
const assetTotal = computed(() => {
  const apiTotal = Number(summary.hostCounts.assetTotal || 0);
  if (apiTotal > 0) {
    return apiTotal;
  }
  return totalWindows.value + totalLinux.value + totalBusiness.value + totalNetwork.value;
});

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

const backupRateColor = computed(() => {
  const rate = backupSuccessRate.value;
  if (rate >= 90) return "#10b981";
  if (rate >= 60) return "#f59e0b";
  return "#ef4444";
});

const componentTotal = computed(() => (summary.monitorComponents || []).length);
const componentOnlineCount = computed(() =>
  (summary.monitorComponents || []).filter((item: any) => Number(item?.onlineStatus) === 1).length
);
const componentUpdateCount = computed(() =>
  (summary.monitorComponents || []).filter((item: any) => Number(item?.updateAvailable) === 1).length
);

const alertsCardRef = ref<any>(null);
const rightColumnHeight = ref<number | null>(null);
const sideStackStyle = computed(() => {
  if (!rightColumnHeight.value) {
    return {};
  }
  return { height: `${rightColumnHeight.value}px` };
});

let alertsResizeObserver: ResizeObserver | null = null;
const syncRightColumnHeight = () => {
  const cardEl = alertsCardRef.value?.$el as HTMLElement | undefined;
  if (!cardEl) {
    return;
  }
  const h = Math.ceil(cardEl.getBoundingClientRect().height);
  rightColumnHeight.value = h > 0 ? h : null;
};

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

const goAsset = (path: string) => {
  router.push({ path });
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

const alertRowClass = ({ row }: { row: any }) => {
  if (String(row?.status || "").toLowerCase() === "resolved") return "";
  const severity = String(row?.severity || "").toLowerCase();
  if (severity === "critical") return "alert-row--critical";
  if (severity === "warning") return "alert-row--warning";
  return "";
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
  nextTick(() => {
    syncRightColumnHeight();
    const cardEl = alertsCardRef.value?.$el as HTMLElement | undefined;
    if (cardEl && typeof ResizeObserver !== "undefined") {
      alertsResizeObserver = new ResizeObserver(() => syncRightColumnHeight());
      alertsResizeObserver.observe(cardEl);
    }
  });
});

onBeforeUnmount(() => {
  if (sseRef.value) {
    sseRef.value.close();
    sseRef.value = null;
  }
  if (alertsResizeObserver) {
    alertsResizeObserver.disconnect();
    alertsResizeObserver = null;
  }
});
</script>

<style scoped>
.mod-home {
  padding: 16px;
  display: flex;
  flex-direction: column;
  gap: 14px;
  background: #f8fafc;
}

/* ========== 资产状态 ========== */
.home-card--asset :deep(.el-card__body) {
  padding: 14px 16px 16px;
  background: #f5f7fa;
}

.card-header--asset {
  margin-bottom: 12px;
}

.asset-count {
  font-size: 12px;
  font-weight: 400;
  color: #94a3b8;
  margin-left: 6px;
}

.stat-strip {
  display: grid;
  grid-template-columns: repeat(6, 1fr);
  gap: 10px;
}

/* ── 单张卡片 ── */
.stat-card {
  background: #fff;
  border-radius: 10px;
  padding: 14px 16px 12px;
  display: flex;
  flex-direction: column;
  gap: 0;
  min-width: 0;
  box-shadow: 0 1px 3px rgba(15, 23, 42, 0.07), 0 1px 2px rgba(15, 23, 42, 0.04);
  transition: box-shadow 0.15s, transform 0.15s;
}

.stat-card--link {
  cursor: pointer;
}

.stat-card--link:hover {
  box-shadow: 0 4px 12px rgba(59, 130, 246, 0.12), 0 1px 3px rgba(15, 23, 42, 0.06);
  transform: translateY(-1px);
}

/* 头部：图标 + 名称 */
.stat-card__head {
  display: flex;
  align-items: center;
  gap: 6px;
  margin-bottom: 10px;
}

.stat-card__icon {
  display: inline-flex;
  align-items: center;
  color: #64748b;
  flex-shrink: 0;
}

.stat-card__name {
  font-size: 13px;
  font-weight: 600;
  color: #475569;
}

/* 英雄数字 */
.stat-card__hero {
  font-size: 30px;
  font-weight: 700;
  color: #0f172a;
  line-height: 1;
  margin-bottom: 10px;
  letter-spacing: -0.5px;
}

/* 分隔线 */
.stat-card__sep {
  height: 1px;
  background: #f1f5f9;
  margin-bottom: 10px;
}

/* 底部状态 */
.stat-card__foot {
  display: flex;
  align-items: center;
  gap: 6px;
  flex-wrap: nowrap;
}

.stat-foot {
  display: inline-flex;
  align-items: center;
  gap: 4px;
  font-size: 12px;
  font-weight: 500;
  white-space: nowrap;
}

.stat-foot__dot {
  width: 6px;
  height: 6px;
  border-radius: 50%;
  flex-shrink: 0;
}

.stat-foot--ok { color: #059669; }
.stat-foot--ok .stat-foot__dot { background: #10b981; }

.stat-foot--bad { color: #dc2626; }
.stat-foot--bad .stat-foot__dot { background: #ef4444; }

.stat-foot--zero { color: #94a3b8; }
.stat-foot--zero .stat-foot__dot { background: #cbd5e1; }

.stat-foot__bar {
  color: #e2e8f0;
  font-size: 12px;
  user-select: none;
}

/* 零值卡片全部置灰 */
.stat-card--zero .stat-card__icon { color: #cbd5e1; }
.stat-card--zero .stat-card__name { color: #94a3b8; }
.stat-card--zero .stat-card__hero { color: #94a3b8; }
.stat-card--zero .stat-foot { color: #94a3b8 !important; }
.stat-card--zero .stat-foot__dot { background: #cbd5e1 !important; }

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
  height: 10px;
  background: #f1f5f9;
  border-radius: 999px;
  overflow: hidden;
}

.backup-bar__fill {
  height: 100%;
  border-radius: 999px;
  transition: width 0.4s ease;
}

.backup-bar__fill--ok { background: #10b981; }
.backup-bar__fill--warn { background: #f59e0b; }
.backup-bar__fill--bad { background: #ef4444; }

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

.alert-table :deep(.alert-row--critical) td {
  background: #fff5f5 !important;
}

.alert-table :deep(.alert-row--warning) td {
  background: #fffbeb !important;
}

/* 告警空状态 */
.alert-empty {
  display: flex;
  flex-direction: column;
  align-items: center;
  gap: 6px;
  padding: 20px 0;
  color: #10b981;
  font-size: 13px;
  font-weight: 500;
}

.alert-empty__icon {
  font-size: 22px;
  line-height: 1;
}

/* 右侧栏 */
.side-stack {
  display: flex;
  flex-direction: column;
  gap: 14px;
  min-height: 0;
}

.home-card--backup,
.home-card--monitor {
  min-height: 0;
  overflow: hidden;
}

.side-stack .home-card :deep(.el-card__body) {
  padding: 14px 16px;
  height: 100%;
}

.home-card--backup {
  flex: 4;
}

.home-card--monitor {
  flex: 6;
}

.backup-rate {
  display: flex;
  align-items: baseline;
  gap: 8px;
  margin: 2px 0 6px;
}

.backup-rate__value {
  font-size: 34px;
  font-weight: 700;
  line-height: 1;
  transition: color 0.3s ease;
}

.backup-rate__label {
  font-size: 12px;
  color: #64748b;
}

.backup-metrics {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 8px;
  margin-top: 8px;
}

.backup-metric {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 8px 10px;
}

.backup-metric__name {
  display: block;
  font-size: 11px;
  color: #64748b;
  margin-bottom: 2px;
}

.backup-metric__value {
  font-size: 16px;
  font-weight: 700;
  color: #0f172a;
}

.backup-metric__value--danger {
  color: #dc2626;
}

.backup-time {
  margin-top: 6px;
  padding-top: 0;
  font-size: 12px;
  color: #64748b;
}

.component-overview {
  display: flex;
  gap: 8px;
  margin-bottom: 10px;
}

.component-pill {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  height: 24px;
  padding: 0 10px;
  border-radius: 999px;
  font-size: 12px;
  font-weight: 600;
}

.component-pill--ok {
  color: #047857;
  background: #ecfdf5;
  border: 1px solid #a7f3d0;
}

.component-pill--warn {
  color: #b45309;
  background: #fffbeb;
  border: 1px solid #fde68a;
}

.component-pill--neutral {
  color: #94a3b8;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
}

.home-card--monitor :deep(.el-table) {
  flex: 1;
  min-height: 0;
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
    grid-template-columns: repeat(3, 1fr);
  }

  .main-grid {
    grid-template-columns: 1fr;
    min-height: auto;
  }

  .side-stack {
    height: auto !important;
    min-height: auto;
  }
}

@media (max-width: 768px) {
  .stat-strip {
    grid-template-columns: repeat(2, 1fr);
  }

  .backup-metrics {
    grid-template-columns: 1fr;
  }
}
</style>
