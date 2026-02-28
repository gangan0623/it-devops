<template>
  <div class="mod-ops__networkhost">
    <div class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-input v-model="state.dataForm.instance" class="query-input" placeholder="地址(模糊)" clearable @keyup.enter="queryList()" />
          <el-input v-model="state.dataForm.name" class="query-input" placeholder="名称(模糊)" clearable @keyup.enter="queryList()" />
          <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          <el-button class="query-btn" @click="handleReset">重置</el-button>
          <el-button :icon="Filter" @click="filterDrawer = true">
            筛选<span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span>
          </el-button>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="host-stats">
            <span class="host-stats__item host-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="host-stats__item host-stats__item--off">禁用 {{ disabledCount }}</span>
            <span class="host-stats__item host-stats__item--online">在线 {{ onlineCount }}</span>
            <span class="host-stats__item host-stats__item--filter">离线 {{ offlineCount }}</span>
          </div>
        </div>
      </div>
    </div>

    <el-drawer v-model="filterDrawer" title="筛选条件" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="区域名称">
          <ren-select v-model="state.dataForm.areaName" dict-type="area_name_type" label-field="dictValue" value-field="dictLabel" placeholder="全部" />
        </el-form-item>
        <el-form-item label="分组">
          <ren-select v-model="state.dataForm.groupName" dict-type="network_device_group" label-field="dictValue" value-field="dictLabel" placeholder="全部" />
        </el-form-item>
        <el-form-item label="设备型号">
          <ren-select v-model="state.dataForm.deviceModel" dict-type="network_device_model" label-field="dictValue" value-field="dictLabel" placeholder="全部" />
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="state.dataForm.status" placeholder="全部" clearable>
            <el-option label="启用" :value="1" />
            <el-option label="禁用" :value="0" />
          </el-select>
        </el-form-item>
        <el-form-item label="采集状态">
          <el-select v-model="state.dataForm.collectionStatus" placeholder="全部" clearable>
            <el-option label="正常" :value="1" />
            <el-option label="异常" :value="0" />
            <el-option label="未知" :value="2" />
          </el-select>
        </el-form-item>
        <el-form-item label="在线状态">
          <el-select v-model="state.dataForm.onlineStatus" placeholder="全部" clearable>
            <el-option label="在线" :value="1" />
            <el-option label="离线" :value="0" />
          </el-select>
        </el-form-item>
        <el-form-item label="备份配置">
          <el-select v-model="state.dataForm.backupStatus" placeholder="全部" clearable>
            <el-option label="是" :value="1" />
            <el-option label="否" :value="0" />
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="resetFilters">重置</el-button>
        <el-button type="primary" @click="confirmFilters">确定</el-button>
      </template>
    </el-drawer>

    <el-table
      v-loading="state.dataListLoading"
      :data="state.dataList"
      border
      class="ops-table-nowrap"
      @sort-change="state.dataListSortChangeHandle"
      style="width: 100%"
    >
      <el-table-column prop="instance" label="IP地址" min-width="140" header-align="center" align="center" sortable="custom" />
      <el-table-column prop="name" label="主机名" min-width="180" header-align="center" align="center" />
      <el-table-column label="区域" min-width="100" header-align="center" align="center">
        <template #default="scope">{{ state.getDictValueByLabel("area_name_type", scope.row.areaName) }}</template>
      </el-table-column>
      <el-table-column label="分组" min-width="120" header-align="center" align="center">
        <template #default="scope">{{ state.getDictValueByLabel("network_device_group", scope.row.groupName) }}</template>
      </el-table-column>
      <el-table-column label="型号" min-width="90" header-align="center" align="center">
        <template #default="scope">{{ state.getDictValueByLabel("network_device_model", scope.row.deviceModel) }}</template>
      </el-table-column>
      <el-table-column label="状态" min-width="80" header-align="center" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
          <el-tag v-else size="small" type="success">启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="采集状态" min-width="90" header-align="center" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.collectionStatus === 1" size="small" type="success">正常</el-tag>
          <el-tag v-else-if="scope.row.collectionStatus === 0" size="small" type="danger">异常</el-tag>
          <el-tag v-else size="small" type="info">未知</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="备份状态" min-width="100" header-align="center" align="center">
        <template #default="scope">
          <el-tooltip
            placement="top"
            :content="scope.row.backupAgentName ? `备份节点：${scope.row.backupAgentName}` : '备份节点：未配置'"
          >
            <el-tag :type="scope.row.backupStatus === 1 ? 'success' : 'info'" size="small">{{ scope.row.backupStatus === 1 ? "是" : "否" }}</el-tag>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column label="Ping(Sec/Loss)" min-width="150" header-align="center" align="center">
        <template v-slot="scope">
          <div class="net-quality">
            <span class="net-dot" :class="onlineDotClass(scope.row.onlineStatus)"></span>
            <span class="net-quality__text">{{ networkQualityText(scope.row) }}</span>
          </div>
        </template>
      </el-table-column>
      <el-table-column label="操作" fixed="right" width="180" header-align="center" align="center">
        <template #default="scope">
          <el-button type="primary" link @click="openBackupDetailDialog(scope.row)">备份配置</el-button>
          <el-button type="primary" link @click="openInterfaceDialog(scope.row)">接口详情</el-button>
        </template>
      </el-table-column>
    </el-table>

    <el-pagination
      :current-page="state.page"
      :page-sizes="[10, 20, 50, 100]"
      :page-size="state.limit"
      :total="state.total"
      layout="total, sizes, prev, pager, next, jumper"
      @size-change="state.pageSizeChangeHandle"
      @current-change="state.pageCurrentChangeHandle"
    />

    <el-dialog v-model="interfaceDialogVisible" title="接口详情" width="1040px" top="2vh" class="interface-dialog">
      <div class="interface-shell">
        <div class="interface-dialog__head">
          <div class="interface-dialog__title">
            <div class="title-main">{{ selectedInterfaceName || "-" }}</div>
            <div class="title-sub">{{ interfaceHostIp || "-" }} · {{ interfaceHostName || "-" }}</div>
          </div>
          <div class="interface-dialog__meta">
            <span class="meta-pill">实时趋势</span>
            <span class="meta-pill meta-pill--muted">{{ quickRangeKey.toUpperCase() }}</span>
          </div>
        </div>

        <div class="interface-toolbar" v-loading="interfaceLoading">
          <div class="interface-toolbar__left">
            <div class="field-label">接口</div>
            <el-select v-model="selectedInterfaceIndex" class="interface-toolbar__select" placeholder="请选择接口" filterable clearable>
              <el-option
                v-for="item in interfaceOptions"
                :key="item.interfaceIndex"
                :label="`${item.interfaceIndex} - ${item.interfaceName || '-'}`"
                :value="String(item.interfaceIndex)"
              />
            </el-select>
          </div>
          <div class="interface-toolbar__right">
            <el-button-group class="interface-toolbar__quick">
              <el-button :type="quickRangeKey === '15m' ? 'primary' : 'default'" @click="applyQuickRange('15m')">15m</el-button>
              <el-button :type="quickRangeKey === '1h' ? 'primary' : 'default'" @click="applyQuickRange('1h')">1h</el-button>
              <el-button :type="quickRangeKey === '6h' ? 'primary' : 'default'" @click="applyQuickRange('6h')">6h</el-button>
              <el-button :type="quickRangeKey === '1d' ? 'primary' : 'default'" @click="applyQuickRange('1d')">1d</el-button>
              <el-button :type="quickRangeKey === '7d' ? 'primary' : 'default'" @click="applyQuickRange('7d')">7d</el-button>
            </el-button-group>
            <div class="auto-refresh">
              <el-switch v-model="autoRefreshEnabled" active-text="自动刷新" />
              <el-select v-model="autoRefreshSeconds" class="interface-toolbar__interval">
                <el-option :value="15" label="15s" />
                <el-option :value="30" label="30s" />
                <el-option :value="60" label="60s" />
              </el-select>
              <span class="refresh-state">{{ refreshStateText }}</span>
            </div>
          </div>
        </div>

        <div class="chart-panel">
          <div ref="trendChartRef" class="interface-trend-chart" v-loading="trendLoading"></div>
        </div>

        <div class="stats-panel">
          <el-table :data="trendStatsRows" size="small" border class="interface-stats-table" height="132">
            <el-table-column prop="metric" label="指标" min-width="140" />
            <el-table-column prop="latest" label="最新" min-width="120" align="right" header-align="center" />
            <el-table-column prop="max" label="最大" min-width="120" align="right" header-align="center" />
            <el-table-column prop="avg" label="平均" min-width="120" align="right" header-align="center" />
            <el-table-column prop="min" label="最小" min-width="120" align="right" header-align="center" />
          </el-table>
        </div>
      </div>
    </el-dialog>

    <el-dialog v-model="backupDetailVisible" title="备份配置" width="760px" top="8vh">
      <el-form :model="backupDetailForm" label-width="108px" v-loading="backupDetailLoading">
        <el-row :gutter="12">
          <el-col :span="12">
            <el-form-item label="IP地址">
              <el-input v-model="backupDetailForm.instance" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="主机名">
              <el-input v-model="backupDetailForm.name" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="区域">
              <el-input v-model="backupDetailForm.areaName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="分组">
              <el-input v-model="backupDetailForm.groupName" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="型号">
              <el-input v-model="backupDetailForm.deviceModel" disabled />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="同步状态">
              <el-tag v-if="backupDetailForm.hostStatus === 1" type="success">启用</el-tag>
              <el-tag v-else-if="backupDetailForm.hostStatus === 0" type="danger">禁用</el-tag>
              <el-tag v-else type="info">未知</el-tag>
            </el-form-item>
          </el-col>
        </el-row>
        <el-divider />
        <el-row :gutter="12">
          <el-col :span="12">
            <el-form-item label="备份开关">
              <el-switch v-model="backupDetailForm.backupEnabledBool" active-text="启用" inactive-text="关闭" />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="可备份">
              <el-tag :type="backupDetailForm.canBackup === 1 ? 'success' : 'info'">{{ backupDetailForm.canBackup === 1 ? "是" : "否" }}</el-tag>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="账号">
              <el-input v-model="backupDetailForm.username" placeholder="请输入账号" />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="密码">
              <el-input v-model="backupDetailForm.password" type="password" show-password placeholder="留空则保持原密码" />
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="备份节点">
              <el-select v-model="backupDetailForm.agentId" placeholder="请选择备份节点" clearable filterable style="width: 100%">
                <el-option v-for="item in backupAgentOptions" :key="item.id" :label="item.label" :value="item.id" />
              </el-select>
            </el-form-item>
          </el-col>
        </el-row>
      </el-form>
      <template #footer>
        <el-button @click="backupDetailVisible = false">取消</el-button>
        <el-button type="primary" :loading="backupSaveLoading" @click="saveBackupDetail">保存</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import { computed, nextTick, onBeforeUnmount, onMounted, reactive, ref, toRefs, watch } from "vue";
import { Filter } from "@element-plus/icons-vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";
import * as echarts from "echarts";

const view = reactive({
  getDataListURL: "/ops/networkhost/page",
  getDataListIsPage: true,
  dataForm: {
    instance: "",
    name: "",
    areaName: "",
    groupName: "",
    deviceModel: "",
    status: "" as string | number,
    collectionStatus: "" as string | number,
    onlineStatus: "" as string | number,
    backupStatus: "" as string | number
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const statusSummary = ref({
  enabledCount: 0,
  disabledCount: 0,
  onlineCount: 0,
  offlineCount: 0
});
const enabledCount = computed(() => statusSummary.value.enabledCount);
const disabledCount = computed(() => statusSummary.value.disabledCount);
const onlineCount = computed(() => statusSummary.value.onlineCount);
const offlineCount = computed(() => statusSummary.value.offlineCount);
const filterDrawer = ref(false);
const interfaceDialogVisible = ref(false);
const interfaceLoading = ref(false);
const trendLoading = ref(false);
const interfaceHostIp = ref("");
const interfaceHostName = ref("");
const currentHostInstance = ref("");
const trendChartRef = ref<HTMLElement | null>(null);
let trendChart: echarts.ECharts | null = null;
const selectedInterfaceIndex = ref("");
const quickRangeKey = ref<"15m" | "1h" | "6h" | "1d" | "7d">("1h");
const autoRefreshEnabled = ref(true);
const autoRefreshSeconds = ref(30);
let autoRefreshTimer: number | null = null;
const refreshCountdown = ref(0);
const trendStatsRows = ref<
  Array<{
    metric: string;
    latest: string;
    max: string;
    avg: string;
    min: string;
  }>
>([]);
const interfaceOptions = ref<Array<{
  status: string;
  interfaceIndex: string;
  interfaceName: string;
  inTrafficKbps: number;
  outTrafficKbps: number;
  speedMbps: number;
  inDropError: string;
  outDropError: string;
}>>([]);
const backupDetailVisible = ref(false);
const backupDetailLoading = ref(false);
const backupSaveLoading = ref(false);
const backupAgentOptions = ref<Array<{ id: number; label: string }>>([]);
const backupDetailForm = reactive({
  networkHostId: "" as string,
  instance: "",
  name: "",
  areaName: "",
  groupName: "",
  deviceModel: "",
  hostStatus: 0,
  backupEnabledBool: false,
  canBackup: 0,
  username: "",
  password: "",
  agentId: null as null | string | number,
  backupAgentName: ""
});

const activeFilterCount = computed(() => {
  let count = 0;
  if (state.dataForm.areaName) count++;
  if (state.dataForm.groupName) count++;
  if (state.dataForm.deviceModel) count++;
  if (state.dataForm.status !== "" && state.dataForm.status !== null && state.dataForm.status !== undefined) count++;
  if (state.dataForm.collectionStatus !== "" && state.dataForm.collectionStatus !== null && state.dataForm.collectionStatus !== undefined) count++;
  if (state.dataForm.onlineStatus !== "" && state.dataForm.onlineStatus !== null && state.dataForm.onlineStatus !== undefined) count++;
  if (state.dataForm.backupStatus !== "" && state.dataForm.backupStatus !== null && state.dataForm.backupStatus !== undefined) count++;
  return count;
});

const selectedInterfaceName = computed(() => {
  const found = interfaceOptions.value.find((i) => String(i.interfaceIndex) === String(selectedInterfaceIndex.value || ""));
  return found?.interfaceName || "";
});

const refreshStateText = computed(() => {
  if (!autoRefreshEnabled.value) return "已暂停";
  if (trendLoading.value) return "正在刷新...";
  if (!hasSelectedInterface()) return "未选择接口";
  return `刷新倒计时 ${Math.max(refreshCountdown.value, 0)}s`;
});

const queryList = () => {
  state.getDataList();
  loadStatusSummary();
};

const loadStatusSummary = () => {
  baseService
    .get("/ops/networkhost/summary")
    .then((res) => {
      statusSummary.value = {
        enabledCount: Number(res.data?.enabledCount || 0),
        disabledCount: Number(res.data?.disabledCount || 0),
        onlineCount: Number(res.data?.onlineCount || 0),
        offlineCount: Number(res.data?.offlineCount || 0)
      };
    })
    .catch(() => {
      statusSummary.value = {
        enabledCount: 0,
        disabledCount: 0,
        onlineCount: 0,
        offlineCount: 0
      };
    });
};

const loadBackupAgents = () => {
  baseService.get("/ops/backupagent/page", { page: 1, limit: 1000 }).then((res) => {
    const list = res.data?.list || [];
    backupAgentOptions.value = list.map((item: any) => ({
      id: item.id,
      label: item.name
    }));
  });
};

const openBackupDetailDialog = (row: any) => {
  const hostId = String(row?.id ?? "").trim();
  if (!hostId) {
    ElMessage.warning("缺少网络设备ID");
    return;
  }
  backupDetailVisible.value = true;
  backupDetailLoading.value = true;
  baseService
    .get(`/ops/networkhost/backup/${hostId}`)
    .then((res) => {
      const data = res.data || {};
      backupDetailForm.networkHostId = String(data.networkHostId ?? hostId);
      backupDetailForm.instance = data.instance || "";
      backupDetailForm.name = data.name || "";
      backupDetailForm.areaName = data.areaName || "";
      backupDetailForm.groupName = data.groupName || "";
      backupDetailForm.deviceModel = data.deviceModel || "";
      backupDetailForm.hostStatus = data.hostStatus === null || data.hostStatus === undefined ? -1 : Number(data.hostStatus);
      backupDetailForm.backupEnabledBool = Number(data.backupEnabled || 0) === 1;
      backupDetailForm.canBackup = Number(data.canBackup || 0);
      backupDetailForm.username = data.username || "";
      backupDetailForm.password = "";
      backupDetailForm.agentId = data.agentId ?? null;
      backupDetailForm.backupAgentName = data.backupAgentName || "";
    })
    .finally(() => {
      backupDetailLoading.value = false;
    });
};

const saveBackupDetail = () => {
  if (!backupDetailForm.networkHostId) {
    return;
  }
  backupSaveLoading.value = true;
  baseService
    .put("/ops/networkhost/backup", {
      networkHostId: backupDetailForm.networkHostId,
      username: backupDetailForm.username,
      password: backupDetailForm.password,
      agentId: backupDetailForm.agentId,
      backupEnabled: backupDetailForm.backupEnabledBool ? 1 : 0
    })
    .then(() => {
      ElMessage.success("保存成功");
      openBackupDetailDialog({ id: backupDetailForm.networkHostId });
      queryList();
    })
    .finally(() => {
      backupSaveLoading.value = false;
    });
};

const resetFilters = () => {
  state.dataForm.areaName = "";
  state.dataForm.groupName = "";
  state.dataForm.deviceModel = "";
  state.dataForm.status = "";
  state.dataForm.collectionStatus = "";
  state.dataForm.onlineStatus = "";
  state.dataForm.backupStatus = "";
};

const confirmFilters = () => {
  filterDrawer.value = false;
  queryList();
};

const handleReset = () => {
  state.dataForm.instance = "";
  state.dataForm.name = "";
  resetFilters();
  queryList();
};

const openInterfaceDialog = (row: any) => {
  currentHostInstance.value = row.instance || "";
  interfaceHostIp.value = row.instance || "";
  interfaceHostName.value = row.name || "";
  interfaceDialogVisible.value = true;
  interfaceLoading.value = true;
  interfaceOptions.value = [];
  selectedInterfaceIndex.value = "";
  applyQuickRange("1h");
  baseService
    .get("/ops/networkhost/interfaces", {
      instance: row.instance,
      includeZeroTraffic: false
    })
    .then((res) => {
      interfaceOptions.value = (res.data || []) as any[];
      if (interfaceOptions.value.length > 0) {
        const first = interfaceOptions.value[0];
        const firstValue = String(first.interfaceIndex ?? "").trim() || String(first.interfaceName ?? "").trim();
        selectedInterfaceIndex.value = firstValue;
      }
    })
    .then(() => nextTick())
    .then(() => {
      initTrendChart();
      if (!selectedInterfaceIndex.value) {
        renderEmptyChart("无可用接口数据");
      }
    })
    .finally(() => {
      interfaceLoading.value = false;
    });
};

const applyQuickRange = (rangeKey: "15m" | "1h" | "6h" | "1d" | "7d") => {
  quickRangeKey.value = rangeKey;
  if (interfaceDialogVisible.value && selectedInterfaceIndex.value) {
    loadInterfaceTrend();
  }
  syncAutoRefresh();
};

const quickRangeDurationMs = (rangeKey: "15m" | "1h" | "6h" | "1d" | "7d") => {
  if (rangeKey === "15m") return 15 * 60 * 1000;
  if (rangeKey === "1h") return 60 * 60 * 1000;
  if (rangeKey === "6h") return 6 * 60 * 60 * 1000;
  if (rangeKey === "1d") return 24 * 60 * 60 * 1000;
  return 7 * 24 * 60 * 60 * 1000;
};

const initTrendChart = () => {
  if (!trendChartRef.value) {
    return;
  }
  if (!trendChart) {
    trendChart = echarts.init(trendChartRef.value);
  }
};

const renderEmptyChart = (title: string) => {
  initTrendChart();
  if (!trendChart) {
    return;
  }
  trendChart.setOption({
    title: {
      text: title,
      left: "center",
      top: "middle",
      textStyle: { color: "#94a3b8", fontWeight: 500, fontSize: 16 }
    },
    xAxis: { show: false, type: "category", data: [] },
    yAxis: { show: false, type: "value" },
    series: []
  }, { notMerge: true });
};

const formatTimeLabel = (sec: number) => {
  const d = new Date(sec * 1000);
  const mm = String(d.getMonth() + 1).padStart(2, "0");
  const dd = String(d.getDate()).padStart(2, "0");
  const hh = String(d.getHours()).padStart(2, "0");
  const mi = String(d.getMinutes()).padStart(2, "0");
  return `${mm}-${dd} ${hh}:${mi}`;
};

const toNumSeries = (values: any[] = []) => values.map((v) => Number(v || 0));
const formatAutoUnit = (kbps: number) => {
  const abs = Math.abs(kbps);
  if (abs >= 1_000_000) return `${(kbps / 1_000_000).toFixed(2)} Gbps`;
  if (abs >= 1_000) return `${(kbps / 1_000).toFixed(2)} Mbps`;
  return `${kbps.toFixed(2)} Kbps`;
};
const calcSeriesStats = (series: number[]) => {
  if (!series.length) {
    return { latest: 0, max: 0, avg: 0, min: 0 };
  }
  const latest = series[series.length - 1];
  const max = Math.max(...series);
  const min = Math.min(...series);
  const avg = series.reduce((sum, v) => sum + v, 0) / series.length;
  return { latest, max, avg, min };
};
const buildStatRow = (metric: string, stats: { latest: number; max: number; avg: number; min: number }) => {
  return {
    metric,
    latest: formatAutoUnit(stats.latest),
    max: formatAutoUnit(stats.max),
    avg: formatAutoUnit(stats.avg),
    min: formatAutoUnit(stats.min)
  };
};
const resolveInterfaceIndexParam = () => {
  const fromSelected = String(selectedInterfaceIndex.value ?? "").trim();
  if (fromSelected) {
    return fromSelected;
  }
  const fromSelectedName = String(selectedInterfaceName.value ?? "").trim();
  if (fromSelectedName) {
    return fromSelectedName;
  }
  const first = interfaceOptions.value[0];
  if (!first) {
    return "";
  }
  const fromFirstIndex = String(first.interfaceIndex ?? "").trim();
  if (fromFirstIndex) {
    return fromFirstIndex;
  }
  return String(first.interfaceName ?? "").trim();
};
const hasSelectedInterface = () => resolveInterfaceIndexParam() !== "";

const loadInterfaceTrend = () => {
  if (!currentHostInstance.value) {
    trendStatsRows.value = [];
    renderEmptyChart("设备地址为空");
    return;
  }
  if (!hasSelectedInterface()) {
    const first = interfaceOptions.value[0];
    if (first) {
      selectedInterfaceIndex.value = String(first.interfaceIndex ?? "").trim() || String(first.interfaceName ?? "").trim();
    }
  }
  if (!hasSelectedInterface()) {
    trendStatsRows.value = [];
    renderEmptyChart("暂无可用接口");
    return;
  }
  const now = Date.now();
  const durationMs = quickRangeDurationMs(quickRangeKey.value);
  const timeFrom = Math.floor((now - durationMs) / 1000);
  const timeTill = Math.floor(now / 1000);
  if (!Number.isFinite(timeFrom) || !Number.isFinite(timeTill) || timeTill <= timeFrom) {
    trendStatsRows.value = [];
    renderEmptyChart("时间范围无效");
    return;
  }
  if (trendLoading.value) {
    return;
  }
  trendLoading.value = true;
  baseService
    .get("/ops/networkhost/interface-trend", {
      instance: currentHostInstance.value,
      interfaceIndex: resolveInterfaceIndexParam(),
      timeFrom,
      timeTill
    })
    .then((res) => {
      const data = res.data || {};
      const timestamps: number[] = (data.timestamps || []).map((v: any) => Number(v || 0));
      if (!timestamps.length) {
        trendStatsRows.value = [];
        renderEmptyChart("该时间范围暂无数据");
        return;
      }
      const xAxisData = timestamps.map((t) => formatTimeLabel(t));
      const inBitrate = toNumSeries(data.inBitrateKbps);
      const outBitrate = toNumSeries(data.outBitrateKbps);
      const inStats = calcSeriesStats(inBitrate);
      const outStats = calcSeriesStats(outBitrate);
      trendStatsRows.value = [
        buildStatRow("入站比特率", inStats),
        buildStatRow("出站比特率", outStats)
      ];

      initTrendChart();
      if (!trendChart) {
        return;
      }
      trendChart.setOption({
        title: { show: false, text: "" },
        color: ["#1e90ff", "#28a745"],
        animationDurationUpdate: 360,
        legend: {
          top: 0,
          textStyle: { fontSize: 14 }
        },
        grid: { left: 30, right: 52, top: 48, bottom: 56, containLabel: true },
        xAxis: {
          show: true,
          type: "category",
          boundaryGap: false,
          data: xAxisData,
          axisLabel: { fontSize: 12, hideOverlap: true, showMaxLabel: true },
          splitLine: { show: false }
        },
        yAxis: {
          show: true,
          type: "value",
          name: "比特率(自动单位)",
          nameGap: 14,
          nameTextStyle: { fontSize: 12, color: "#334155" },
          axisLabel: {
            fontSize: 13,
            formatter: (value: number) => formatAutoUnit(Number(value))
          },
          splitLine: { show: true, lineStyle: { color: "#dbe4f0", type: "dashed" } }
        },
        tooltip: {
          trigger: "axis",
          axisPointer: { type: "cross", label: { backgroundColor: "#1e293b" } },
          formatter: (params: any) => {
            if (!Array.isArray(params) || params.length === 0) return "";
            const time = params[0].axisValueLabel || "";
            const lines = params.map((p: any) => `${p.marker}${p.seriesName}: ${formatAutoUnit(Number(p.value || 0))}`);
            return [time, ...lines].join("<br/>");
          }
        },
        series: [
          {
            name: "入站比特率",
            type: "line",
            smooth: true,
            showSymbol: false,
            lineStyle: { width: 2.8, type: "solid" },
            areaStyle: {
              opacity: 0.18,
              color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                { offset: 0, color: "rgba(30, 144, 255, 0.28)" },
                { offset: 1, color: "rgba(30, 144, 255, 0.03)" }
              ])
            },
            emphasis: { focus: "series" },
            data: inBitrate
          },
          {
            name: "出站比特率",
            type: "line",
            smooth: true,
            showSymbol: false,
            lineStyle: { width: 2.8, type: "dashed" },
            areaStyle: {
              opacity: 0.14,
              color: new echarts.graphic.LinearGradient(0, 0, 0, 1, [
                { offset: 0, color: "rgba(22, 163, 74, 0.24)" },
                { offset: 1, color: "rgba(22, 163, 74, 0.02)" }
              ])
            },
            emphasis: { focus: "series" },
            data: outBitrate
          }
        ],
        dataZoom: [
          { type: "inside", start: 0, end: 100 },
          {
            type: "slider",
            height: 12,
            bottom: 8,
            brushSelect: false,
            borderColor: "rgba(148,163,184,0.25)",
            backgroundColor: "rgba(226,232,240,0.35)",
            dataBackground: { lineStyle: { color: "rgba(148,163,184,0.55)" }, areaStyle: { color: "rgba(203,213,225,0.35)" } },
            fillerColor: "rgba(59,130,246,0.15)",
            handleSize: "90%",
            handleStyle: { color: "#cbd5e1", borderColor: "#94a3b8" }
          }
        ]
      }, { notMerge: true });
    })
    .finally(() => {
      trendLoading.value = false;
    });
};

const clearAutoRefresh = () => {
  if (autoRefreshTimer !== null) {
    window.clearInterval(autoRefreshTimer);
    autoRefreshTimer = null;
  }
};

const syncAutoRefresh = () => {
  clearAutoRefresh();
  if (!interfaceDialogVisible.value || !autoRefreshEnabled.value || !hasSelectedInterface()) {
    refreshCountdown.value = 0;
    return;
  }
  refreshCountdown.value = autoRefreshSeconds.value;
  autoRefreshTimer = window.setInterval(() => {
    if (trendLoading.value) {
      return;
    }
    refreshCountdown.value = refreshCountdown.value - 1;
    if (refreshCountdown.value <= 0) {
      loadInterfaceTrend();
      refreshCountdown.value = autoRefreshSeconds.value;
    }
  }, 1000);
};

const onlineDotClass = (v: number | null | undefined) => {
  if (v === 1) return "net-dot--online";
  if (v === 0) return "net-dot--offline";
  return "net-dot--unknown";
};

const normalizeMetric = (v: any) => {
  const value = String(v ?? "").trim();
  return value || "-";
};

const networkQualityText = (row: any) => {
  const response = normalizeMetric(row.responseTime);
  const loss = normalizeMetric(row.packetLossRate);
  return `${response}/${loss}`;
};

onMounted(() => {
  queryList();
  loadBackupAgents();
});

watch(interfaceDialogVisible, (visible) => {
  if (!visible) {
    clearAutoRefresh();
    trendStatsRows.value = [];
    refreshCountdown.value = 0;
    return;
  }
  nextTick(() => {
    initTrendChart();
    trendChart?.resize();
    syncAutoRefresh();
  });
});

watch(selectedInterfaceIndex, () => {
  if (!interfaceDialogVisible.value) {
    return;
  }
  loadInterfaceTrend();
  syncAutoRefresh();
});

watch(autoRefreshEnabled, () => {
  syncAutoRefresh();
});

watch(autoRefreshSeconds, () => {
  syncAutoRefresh();
});

onBeforeUnmount(() => {
  clearAutoRefresh();
  trendChart?.dispose();
  trendChart = null;
});
</script>

<style scoped>
.ops-table-nowrap :deep(.cell) {
  white-space: nowrap;
}

.host-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

.host-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}

.net-quality {
  display: inline-flex;
  align-items: center;
  gap: 6px;
}

.net-dot {
  width: 8px;
  height: 8px;
  border-radius: 50%;
  display: inline-block;
  flex-shrink: 0;
}

.net-dot--online {
  background: #22c55e;
}

.net-dot--offline {
  background: #ef4444;
}

.net-dot--unknown {
  background: #94a3b8;
}

.net-quality__text {
  color: #334155;
  font-size: 14px;
}

.interface-dialog__head {
  margin-bottom: 12px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
}

.interface-dialog__title {
  color: #0f172a;
  line-height: 1.4;
}

.title-main {
  font-size: 20px;
  font-weight: 700;
  letter-spacing: 0.2px;
  color: #0f172a;
}

.title-sub {
  margin-top: 2px;
  font-size: 13px;
  color: #475569;
  font-weight: 500;
}

.interface-dialog__meta {
  display: flex;
  align-items: center;
  gap: 8px;
}

.meta-pill {
  display: inline-flex;
  align-items: center;
  height: 24px;
  padding: 0 10px;
  border-radius: 999px;
  font-size: 12px;
  font-weight: 600;
  color: #1d4ed8;
  background: #dbeafe;
}

.meta-pill--muted {
  color: #334155;
  background: #e2e8f0;
}

.interface-shell {
  border: 1px solid #dbeafe;
  border-radius: 12px;
  background: #f8fafc;
  padding: 8px;
}

.interface-toolbar {
  display: flex;
  align-items: flex-end;
  justify-content: space-between;
  gap: 8px;
  margin-bottom: 8px;
  border: 1px solid #d6e0ef;
  border-radius: 10px;
  background: #ffffff;
  padding: 8px;
  box-shadow: 0 2px 8px rgba(15, 23, 42, 0.05);
}

.interface-toolbar__left {
  display: flex;
  flex-direction: column;
  gap: 6px;
  min-width: 320px;
}

.field-label {
  font-size: 12px;
  font-weight: 600;
  color: #475569;
}

.interface-toolbar__select {
  width: 360px;
  max-width: 100%;
}

.interface-toolbar__right {
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
}

.interface-toolbar__quick {
  display: inline-flex;
}

.interface-toolbar__interval {
  width: 84px;
}

.auto-refresh {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  padding-left: 8px;
  border-left: 1px solid #e2e8f0;
}

.refresh-state {
  min-width: 96px;
  color: #475569;
  font-size: 12px;
  font-weight: 600;
}

.chart-panel {
  border: 1px solid #d6e0ef;
  border-radius: 10px;
  background: #ffffff;
  padding: 8px;
  box-shadow: 0 2px 8px rgba(15, 23, 42, 0.05);
}

.interface-trend-chart {
  width: 100%;
  height: 34vh;
  min-height: 250px;
  max-height: 320px;
}

.stats-panel {
  margin-top: 8px;
  border: 1px solid #d6e0ef;
  border-radius: 10px;
  background: #ffffff;
  padding: 6px 8px;
  box-shadow: 0 2px 8px rgba(15, 23, 42, 0.05);
}

.interface-stats-table {
  --el-table-header-bg-color: #f8fafc;
}

.interface-stats-table :deep(.el-table__cell) {
  font-size: 12px;
  padding-top: 4px;
  padding-bottom: 4px;
}

.interface-dialog :deep(.el-dialog__body) {
  padding: 6px 12px 8px;
  overflow: hidden;
}

.interface-dialog :deep(.el-dialog) {
  max-height: 96vh;
}

@media (max-width: 1024px) {
  .interface-toolbar {
    flex-direction: column;
    align-items: stretch;
  }

  .interface-toolbar__left {
    min-width: 100%;
  }

  .interface-toolbar__select {
    width: 100%;
  }

  .interface-toolbar__right {
    justify-content: space-between;
  }

  .chart-panel {
    padding: 8px;
  }

  .interface-trend-chart {
    height: 30vh;
    min-height: 220px;
    max-height: 260px;
  }
}
</style>
