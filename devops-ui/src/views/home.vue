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
          <span class="card-link" @click="goAlertRecord">查看详情</span>
        </div>
        <el-table :data="summary.recentAlerts" height="280" border>
          <el-table-column prop="instance" label="实例" min-width="140" />
          <el-table-column prop="alertName" label="告警名称" min-width="160" />
          <el-table-column prop="severity" label="级别" min-width="80">
            <template v-slot="scope">
              <el-tag v-if="scope.row.severity === 'critical'" size="small" type="danger">灾难</el-tag>
              <el-tag v-else-if="scope.row.severity === 'warning'" size="small" type="warning">重要</el-tag>
              <el-tag v-else-if="scope.row.severity === 'info'" size="small" type="info">信息</el-tag>
              <el-tag v-else size="small" type="success">恢复</el-tag>
            </template>
          </el-table-column>
          <el-table-column prop="status" label="状态" min-width="80">
            <template v-slot="scope">
              <el-tag v-if="scope.row.status === 'firing'" size="small" type="danger">告警中</el-tag>
              <el-tag v-else size="small" type="success">已恢复</el-tag>
            </template>
          </el-table-column>
          <el-table-column prop="time" label="时间" min-width="150" />
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

const goAlertRecord = () => {
  router.push({ path: "/alert/record" });
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
  gap: 12px;
}

.home-overview { grid-area: overview; }
.home-diff { grid-area: diff; }
.home-monitor { grid-area: monitor; }
.home-card--alerts { grid-area: alerts; }

.home-card {
  border-radius: 8px;
}

/* 统一顶部三个卡片高度：改为 360px，更透气 */
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
  padding: 24px; /* 内部 Padding 再次加大 */
}

/* --- 总览卡片样式优化 --- */
.overview-top {
  flex: 1;
  display: flex;
  flex-direction: column;
  justify-content: center; /* 关键：垂直居中，消除上下留白 */
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
  font-size: 20px;
  font-weight: 700;
  color: #334155;
  line-height: 1.2;
}

.backup-percentage-label {
  font-size: 12px;
  color: #94a3b8;
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
  color: #64748b;
  padding: 2px 8px;
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
  color: #94a3b8;
  margin-bottom: 2px;
}

.backup-detail-item .value {
  font-size: 14px;
  font-weight: 600;
  color: #334155;
}

.backup-time-row .label {
  font-size: 12px;
  color: #94a3b8;
  display: none;
}

.backup-time-row .value {
  font-size: 12px;
  color: #94a3b8;
}

.metric-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 16px; /* 间距加大 */
}

.metric-item {
  background: #f8fafc;
  border-radius: 8px; /* 圆角加大 */
  padding: 16px 8px; /* 方块高度加大 */
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
}

.metric-value {
  font-size: 24px; /* 数字变大 */
  font-weight: 700;
  color: #0f172a;
  line-height: 1.2;
}

.metric-label {
  font-size: 13px; /* 文字微调 */
  color: #64748b;
  margin-top: 4px;
}

.backup-divider {
  height: 1px;
  background: #e2e8f0; /* 颜色更淡一点 */
  margin: 16px 0; /* 分割线间距加大 */
  flex-shrink: 0;
}

.backup-stats {
  display: flex;
  flex-direction: column;
  gap: 12px; /* 统计行间距加大 */
}

.backup-row {
  display: flex;
  justify-content: space-between;
  font-size: 14px; /* 字号微调 */
  line-height: 1.5;
  color: #334155;
}

.card-title {
  font-size: 16px; /* 标题加大 */
  font-weight: 600;
  margin-bottom: 12px;
  color: #0f172a;
  flex-shrink: 0;
}

.card-title--row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
  font-size: 16px; /* 标题加大 */
  font-weight: 600;
  margin-bottom: 12px;
  color: #0f172a;
  flex-shrink: 0;
}

.card-title--link {
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 6px;
}

.card-link {
  cursor: pointer;
  font-size: 12px;
  padding: 1px 8px;
  border-radius: 999px;
  background: #0ea5e9;
  color: #fff;
  transform: scale(0.9);
}

.success { color: #16a34a; }
.danger { color: #dc2626; }
.diff-search { margin-bottom: 12px; flex-shrink: 0; }
.card-title--row .el-button-group { flex-shrink: 0; }
.diff-switch { padding: 5px 8px; }

@media (max-width: 1200px) {
  .home-grid {
    grid-template-columns: 1fr;
    grid-template-areas: "overview" "diff" "monitor" "alerts";
  }
}
</style>
