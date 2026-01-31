<template>
  <div class="mod-home">
    <div class="home-grid">
      <div class="home-overview">
        <el-card shadow="never" class="home-card home-card--overview top-card">
          <div class="card-title">总览</div>
          <div class="metric-grid">
            <div class="metric-item">
              <div class="metric-value">{{ summary.hostCounts.windows }}</div>
              <div class="metric-label">Windows 主机</div>
            </div>
            <div class="metric-item">
              <div class="metric-value">{{ summary.hostCounts.linux }}</div>
              <div class="metric-label">Linux 主机</div>
            </div>
            <div class="metric-item">
              <div class="metric-value">{{ summary.hostCounts.business }}</div>
              <div class="metric-label">业务系统</div>
            </div>
          </div>
          <div class="backup-divider"></div>
          <div class="card-title card-title--link" @click="goBackupRecord">
            <span>最近备份情况</span>
            <span class="card-link">查看详情</span>
          </div>
          <div class="backup-stats">
            <div class="backup-row">
              <span>第 {{ summary.backupStats.round || 0 }} 次备份</span>
              <span>总数 {{ summary.backupStats.total || 0 }}</span>
            </div>
            <div class="backup-row">
              <span class="success">成功 {{ summary.backupStats.success || 0 }}</span>
              <span class="danger">失败 {{ summary.backupStats.fail || 0 }}</span>
            </div>
            <div class="backup-row backup-time">
              <span>最近时间</span>
              <span>{{ summary.backupStats.lastTime || "-" }}</span>
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
                备份设备有 ({{ summary.deviceDiff.backupOnly.length || 0 }})
              </el-button>
              <el-button size="small" class="diff-switch" :type="activeDiff === 'zabbixOnly' ? 'primary' : 'default'" @click="activeDiff = 'zabbixOnly'">
                Zabbix 有 ({{ summary.deviceDiff.zabbixOnly.length || 0 }})
              </el-button>
            </el-button-group>
          </div>
          <el-input v-model="filters.diff" placeholder="搜索 IP/名称" size="small" clearable class="diff-search" />
          <el-table :data="filteredDiffList" height="240" border>
            <el-table-column prop="ip" label="IP" min-width="140" />
            <el-table-column prop="name" label="主机名称" min-width="160" />
          </el-table>
        </el-card>
      </div>

      <div class="home-monitor">
        <el-card shadow="never" class="home-card home-card--monitor top-card">
          <div class="card-title">监控组件预览</div>
          <el-table :data="summary.monitorComponents" height="240" border>
            <el-table-column prop="name" label="名称" min-width="160" />
            <el-table-column prop="onlineStatus" label="在线" width="90">
              <template v-slot="scope">
                <el-tag v-if="scope.row.onlineStatus === 1" size="small" type="success">在线</el-tag>
                <el-tag v-else-if="scope.row.onlineStatus === 0" size="small" type="danger">离线</el-tag>
                <el-tag v-else size="small" type="info">未知</el-tag>
              </template>
            </el-table-column>
            <el-table-column prop="updateAvailable" label="可更新" width="90">
              <template v-slot="scope">
                <el-tag v-if="scope.row.updateAvailable === 1" size="small" type="warning">是</el-tag>
                <el-tag v-else-if="scope.row.updateAvailable === 0" size="small" type="success">否</el-tag>
                <el-tag v-else size="small" type="info">未知</el-tag>
              </template>
            </el-table-column>
          </el-table>
        </el-card>
      </div>

      <el-card shadow="never" class="home-card home-card--alerts">
        <div class="card-title card-title--link" @click="goAlertRecord">
          <span>实时告警情况</span>
          <span class="card-link">查看详情</span>
        </div>
        <el-table :data="summary.recentAlerts" height="260" border>
          <el-table-column prop="instance" label="实例" min-width="140" />
          <el-table-column prop="alertName" label="告警名称" min-width="160" />
          <el-table-column prop="severity" label="级别" min-width="90">
            <template v-slot="scope">
              <el-tag v-if="scope.row.severity === 'critical'" size="small" type="danger">灾难</el-tag>
              <el-tag v-else-if="scope.row.severity === 'warning'" size="small" type="warning">重要</el-tag>
              <el-tag v-else-if="scope.row.severity === 'info'" size="small" type="info">信息</el-tag>
              <el-tag v-else size="small" type="success">恢复</el-tag>
            </template>
          </el-table-column>
          <el-table-column prop="status" label="状态" min-width="90">
            <template v-slot="scope">
              <el-tag v-if="scope.row.status === 'firing'" size="small" type="danger">告警中</el-tag>
              <el-tag v-else size="small" type="success">已恢复</el-tag>
            </template>
          </el-table-column>
          <el-table-column prop="time" label="时间" min-width="160" />
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
    fail: 0
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

const filteredDiffList = computed(() => {
  const q = filters.diff.trim().toLowerCase();
  const source = activeDiff.value === "backupOnly" ? summary.deviceDiff.backupOnly : summary.deviceDiff.zabbixOnly;
  if (!q) return source;
  return source.filter((item: any) => {
    return String(item.ip || "").toLowerCase().includes(q) || String(item.name || "").toLowerCase().includes(q);
  });
});

const loadSummary = () => {
  baseService.get("/ops/dashboard/summary").then((res) => {
    if (res.data) {
      Object.assign(summary, res.data);
    }
  });
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
  loadSummary();
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
  padding: 12px 16px 24px;
}
.home-grid {
  display: grid;
  grid-template-columns: repeat(3, minmax(0, 1fr));
  grid-template-areas:
    "overview diff monitor"
    "alerts alerts alerts";
  gap: 16px;
}
.home-overview {
  grid-area: overview;
}
.home-diff {
  grid-area: diff;
}
.home-monitor {
  grid-area: monitor;
}
.home-card--alerts {
  grid-area: alerts;
}
.home-card {
  border-radius: 12px;
}
.top-card {
  height: 320px;
}
.card-title {
  font-size: 16px;
  font-weight: 600;
  margin-bottom: 12px;
  color: #0f172a;
}
.card-title--row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
}
.card-title--row .el-button-group {
  flex-shrink: 0;
  white-space: nowrap;
}
.diff-switch {
  white-space: nowrap;
}
.card-title--link {
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: space-between;
}
.card-link {
  font-size: 12px;
  padding: 2px 10px;
  border-radius: 999px;
  background: #0ea5e9;
  color: #fff;
}
.metric-grid {
  display: grid;
  grid-template-columns: repeat(3, 1fr);
  gap: 16px;
}
.backup-divider {
  height: 1px;
  background: #e5e7eb;
  margin: 18px 0;
}
.metric-item {
  background: #f8fafc;
  border-radius: 10px;
  padding: 16px;
}
.metric-value {
  font-size: 28px;
  font-weight: 700;
  color: #0f172a;
}
.metric-label {
  font-size: 13px;
  color: #64748b;
  margin-top: 4px;
}
.backup-stats {
  background: #f8fafc;
  border-radius: 10px;
  padding: 16px;
}
.backup-row {
  display: flex;
  justify-content: space-between;
  font-size: 14px;
  margin-bottom: 8px;
}
.backup-row:last-child {
  margin-bottom: 0;
}
.backup-time {
  font-size: 12px;
  color: #64748b;
}
.success {
  color: #16a34a;
}
.danger {
  color: #dc2626;
}
.diff-search {
  margin-bottom: 8px;
}
@media (max-width: 1200px) {
  .home-grid {
    grid-template-columns: 1fr;
    grid-template-areas:
      "overview"
      "diff"
      "monitor"
      "alerts";
  }
  .metric-grid {
    grid-template-columns: 1fr;
  }
}
</style>
