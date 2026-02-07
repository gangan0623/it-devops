<template>
  <div class="mod-alert__problem">
    <el-form :inline="true" :model="filters" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="filters.hostName" class="query-input" placeholder="主机名(模糊)" clearable @keyup.enter="query" />
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :loading="loading" @click="query">查询</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" @click="handleToolbarReset">重置</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :icon="Filter" @click="filterDrawer = true">
              筛选
              <span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span>
            </el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-checkbox-group v-model="filters.severity" size="small" @change="handleSeverityChange">
            <el-checkbox-button label="critical">灾难</el-checkbox-button>
            <el-checkbox-button label="warning">重要</el-checkbox-button>
            <el-checkbox-button label="info">信息</el-checkbox-button>
          </el-checkbox-group>
          <div class="record-stats">
            <span class="record-stats__item record-stats__item--bad">告警 {{ firingCount }}</span>
            <span class="record-stats__item record-stats__item--ok">恢复 {{ resolvedCount }}</span>
          </div>
        </div>
      </div>
    </el-form>

    <el-drawer v-model="filterDrawer" title="筛选条件" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="分类">
          <el-select v-model="filters.category" @change="onCategoryChange">
            <el-option label="实时告警情况" value="realtime" />
            <el-option label="历史告警" value="history" />
          </el-select>
        </el-form-item>
        <el-form-item v-if="filters.category === 'history'" label="开始时间">
          <el-date-picker v-model="filters.startTime" type="datetime" value-format="YYYY-MM-DDTHH:mm:ss[Z]" clearable />
        </el-form-item>
        <el-form-item v-if="filters.category === 'history'" label="结束时间">
          <el-date-picker v-model="filters.endTime" type="datetime" value-format="YYYY-MM-DDTHH:mm:ss[Z]" clearable />
        </el-form-item>
        <el-form-item label="设备类型">
          <el-select v-model="filters.deviceType" clearable>
            <el-option label="Linux设备" value="linux" />
            <el-option label="Windows设备" value="windows" />
            <el-option label="业务系统" value="business" />
          </el-select>
        </el-form-item>
        <el-form-item label="实例(模糊)">
          <el-input v-model="filters.instance" clearable />
        </el-form-item>
        <el-form-item label="告警级别">
          <el-select v-model="filters.severity" multiple clearable collapse-tags>
            <el-option label="灾难" value="critical" />
            <el-option label="重要" value="warning" />
            <el-option label="信息" value="info" />
            <el-option label="恢复" value="recover" />
          </el-select>
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="filters.statusFilter" clearable>
            <el-option label="恢复" value="auto" />
            <el-option label="问题" value="problem" />
            <el-option label="手动解决" value="manual" />
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="handleFilterReset">重置</el-button>
        <el-button type="primary" @click="handleFilterConfirm">确定</el-button>
      </template>
    </el-drawer>

    <el-table v-loading="loading" :data="list" border class="alert-record-table" style="width: 100%">
      <el-table-column prop="startsAt" label="时间" header-align="center" align="center" width="165" />
      <el-table-column label="严重性" header-align="center" align="center" width="90">
        <template #default="scope">
          <span :class="severityClass(scope.row)">{{ formatSeverity(scope.row.severity) }}</span>
        </template>
      </el-table-column>
      <el-table-column label="状态" header-align="center" align="center" width="110">
        <template #default="scope">
          <span :class="statusClass(scope.row.status)">
            {{ formatProblemStatus(scope.row.status) }}
          </span>
        </template>
      </el-table-column>
      <el-table-column prop="hostName" label="主机名" header-align="center" align="center" min-width="180" fixed="left">
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
      <el-table-column prop="problem" label="问题" header-align="center" align="center" min-width="220">
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
                    <span class="event-tip__key">摘要</span>
                    <span class="event-tip__value">{{ scope.row.summary || scope.row.problem || "-" }}</span>
                  </div>
                  <div class="event-tip__row">
                    <span class="event-tip__key">描述</span>
                    <span class="event-tip__value">{{ scope.row.description || "-" }}</span>
                  </div>
                </div>
              </div>
            </template>
            <span>{{ scope.row.problem || "-" }}</span>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column label="持续时间" header-align="center" align="center" width="120">
        <template #default="scope">
          {{ formatDuration(scope.row) }}
        </template>
      </el-table-column>
      <el-table-column label="恢复时间" header-align="center" align="center" width="165">
        <template #default="scope">
          {{ formatEndTime(scope.row) }}
        </template>
      </el-table-column>
      <el-table-column label="动作" header-align="center" align="center" min-width="140">
        <template #default="scope">
          <el-tooltip placement="left" :show-after="250" effect="light">
            <template #content>
              <div class="event-tip">
                <div class="event-tip__title">事件详情</div>
                <template v-if="buildEventActions(scope.row).length">
                  <div v-for="item in buildEventActions(scope.row)" :key="item.key" class="event-tip__group">
                    <div class="event-tip__group-title">{{ item.title }}</div>
                    <template v-if="item.type === 'notify'">
                      <div class="event-tip__row">
                        <span class="event-tip__key">发送时间</span>
                        <span class="event-tip__value">{{ item.time || '-' }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">发送媒介</span>
                        <span class="event-tip__value">{{ item.media || '-' }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">接收对象</span>
                        <span class="event-tip__value">{{ item.receivers || '-' }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">发送状态</span>
                        <span class="event-tip__value">
                          <el-tag size="small" :type="actionTagType(item.sendStatus)">{{ item.sendStatus || '-' }}</el-tag>
                        </span>
                      </div>
                    </template>
                    <template v-else>
                      <div class="event-tip__row">
                        <span class="event-tip__key">时间</span>
                        <span class="event-tip__value">{{ item.time || '-' }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">操作人</span>
                        <span class="event-tip__value">{{ item.operator || '-' }}</span>
                      </div>
                      <div class="event-tip__row">
                        <span class="event-tip__key">消息</span>
                        <span class="event-tip__value">{{ item.message || '-' }}</span>
                      </div>
                    </template>
                  </div>
                </template>
                <div v-else class="event-tip__group">
                  <div class="event-tip__group-title">暂无动作记录</div>
                </div>
              </div>
            </template>
            <span class="action-hover">事件详情</span>
          </el-tooltip>
        </template>
      </el-table-column>
      <el-table-column label="更新" header-align="center" align="center" width="70">
        <template #default="scope">
          <el-button type="primary" link @click="showUpdate(scope.row)">更新</el-button>
        </template>
      </el-table-column>
    </el-table>

    <el-pagination
      :current-page="page"
      :page-sizes="[10, 20, 50, 100]"
      :page-size="limit"
      :total="total"
      layout="total, sizes, prev, pager, next, jumper"
      @size-change="onSizeChange"
      @current-change="onCurrentChange"
    />

    <el-dialog v-model="updateVisible" title="告警更新" width="980px" :close-on-click-modal="false" class="update-dialog">
      <div class="update-dialog__meta">
        <div class="update-dialog__meta-label">问题内容</div>
        <div class="update-dialog__meta-value">{{ updateForm.problem }}</div>
      </div>
      <div class="update-dialog__content">
        <section class="update-card">
          <div class="update-card__title">处理动作</div>
          <el-form label-position="top" class="update-form">
            <el-form-item label="处理说明">
              <el-input v-model="updateForm.message" type="textarea" :rows="4" placeholder="可选：填写本次操作说明，方便后续追溯"></el-input>
            </el-form-item>
            <el-form-item label="更改严重性">
              <div class="op-row">
                <el-select v-model="updateForm.targetSeverity" class="op-input">
                  <el-option label="信息" value="info"></el-option>
                  <el-option label="重要" value="warning"></el-option>
                  <el-option label="灾难" value="critical"></el-option>
                </el-select>
                <el-button type="primary" plain :loading="actionLoading" @click="handleChangeSeverity">立即执行</el-button>
              </div>
            </el-form-item>
            <el-form-item label="抑制告警">
              <div class="op-row">
                <el-input-number v-model="updateForm.suppressDays" :min="1" :max="30" class="op-input"></el-input-number>
                <span class="hint">在设定天数内不再触发该主机的同类告警</span>
                <el-button type="warning" plain :loading="actionLoading" @click="handleSuppress">立即执行</el-button>
              </div>
            </el-form-item>
            <el-form-item label="状态动作">
              <div class="op-row">
                <el-button type="success" :loading="actionLoading" @click="handleAck">确认问题</el-button>
                <el-button type="danger" :loading="actionLoading" @click="handleClose">关闭问题</el-button>
              </div>
            </el-form-item>
          </el-form>
        </section>
        <section class="update-card update-card--history">
          <div class="update-card__title">历史记录</div>
          <el-table :data="actionHistory" border max-height="420" v-loading="actionLoading" class="history-table">
            <el-table-column prop="createDate" label="时间" width="176"></el-table-column>
            <el-table-column prop="operatorName" label="用户" width="110"></el-table-column>
            <el-table-column prop="action" label="动作" width="96"></el-table-column>
            <el-table-column prop="message" label="消息"></el-table-column>
          </el-table>
        </section>
      </div>
      <template #footer>
        <el-button @click="updateVisible = false">关闭</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script setup lang="ts">
import { computed, onMounted, reactive, ref } from "vue";
import baseService from "@/service/baseService";
import { ElMessage, ElMessageBox } from "element-plus";
import { Filter } from "@element-plus/icons-vue";

const loading = ref(false);
const list = ref<any[]>([]);
const page = ref(1);
const limit = ref(20);
const total = ref(0);
const filterDrawer = ref(false);

const filters = reactive({
  category: "realtime",
  startTime: "",
  endTime: "",
  deviceType: "",
  hostName: "",
  severity: [] as string[],
  instance: "",
  statusFilter: ""
});

const activeFilterCount = computed(() => {
  let count = 0;
  if (filters.category !== "realtime") count++;
  if (filters.startTime) count++;
  if (filters.endTime) count++;
  if (filters.deviceType) count++;
  if (filters.severity.length) count++;
  if (filters.instance) count++;
  if (filters.statusFilter) count++;
  return count;
});

const firingCount = ref(0);
const resolvedCount = ref(0);

const updateVisible = ref(false);
const actionLoading = ref(false);
const actionHistory = ref<any[]>([]);
const updateForm = reactive({
  id: 0,
  problem: "",
  targetSeverity: "warning",
  suppressDays: 1,
  message: ""
});

const query = () => {
  loading.value = true;
  const params: any = {
    page: String(page.value),
    limit: String(limit.value),
    category: filters.category,
    deviceType: filters.deviceType,
    hostName: filters.hostName,
    severity: filters.severity.join(","),
    instance: filters.instance,
    statusFilter: filters.statusFilter,
    startTime: filters.startTime,
    endTime: filters.endTime
  };
  baseService
    .get("/alert/record/problem/page", params)
    .then((res) => {
      const data = res.data || {};
      list.value = Array.isArray(data.list) ? data.list : [];
      total.value = Number(data.total || 0);
      firingCount.value = Number(data.firingCount || 0);
      resolvedCount.value = Number(data.resolvedCount || 0);
    })
    .finally(() => {
      loading.value = false;
    });
};

const onCategoryChange = () => {
  if (filters.category === "realtime") {
    filters.startTime = "";
    filters.endTime = "";
  }
};

const handleToolbarReset = () => {
  filters.hostName = "";
  handleFilterReset();
  query();
};

const handleSeverityChange = () => {
  page.value = 1;
  query();
};

const handleFilterReset = () => {
  filters.category = "realtime";
  filters.startTime = "";
  filters.endTime = "";
  filters.deviceType = "";
  filters.severity = [];
  filters.instance = "";
  filters.statusFilter = "";
};

const handleFilterConfirm = () => {
  filterDrawer.value = false;
  page.value = 1;
  query();
};

const onSizeChange = (val: number) => {
  limit.value = val;
  page.value = 1;
  query();
};

const onCurrentChange = (val: number) => {
  page.value = val;
  query();
};

const showUpdate = (row: any) => {
  updateForm.id = row.id;
  updateForm.problem = row.problem;
  updateForm.targetSeverity = normalizeSeverityValue(row.severity);
  updateForm.suppressDays = 1;
  updateForm.message = "";
  actionHistory.value = [];
  updateVisible.value = true;
  loadHistory();
};

const loadHistory = () => {
  if (!updateForm.id) return;
  actionLoading.value = true;
  baseService
    .get(`/alert/record/${updateForm.id}/action/history`)
    .then((res) => {
      actionHistory.value = Array.isArray(res.data) ? res.data : [];
    })
    .finally(() => {
      actionLoading.value = false;
    });
};

const postAction = (path: string, data: any, doneText: string) => {
  if (!updateForm.id) return;
  actionLoading.value = true;
  baseService
    .post(`/alert/record/${updateForm.id}/action/${path}`, data)
    .then(() => {
      ElMessage.success(doneText);
      query();
    })
    .finally(() => {
      actionLoading.value = false;
      updateVisible.value = false;
    });
};

const confirmAndPostAction = (title: string, message: string, path: string, data: any, doneText: string) => {
  ElMessageBox.confirm(message, title, {
    confirmButtonText: "确认",
    cancelButtonText: "取消",
    type: "warning"
  })
    .then(() => {
      postAction(path, data, doneText);
    })
    .catch(() => {});
};

const handleChangeSeverity = () =>
  confirmAndPostAction("确认更改严重性", "确定要更改该告警的严重性吗？", "severity", { severity: updateForm.targetSeverity, message: updateForm.message }, "严重性已更新");
const handleSuppress = () =>
  confirmAndPostAction("确认抑制告警", "确定要执行抑制操作吗？", "suppress", { days: updateForm.suppressDays, message: updateForm.message }, "抑制已生效");
const handleAck = () => confirmAndPostAction("确认问题", "确定将该告警标记为已确认吗？", "ack", { message: updateForm.message }, "已确定");
const handleClose = () => confirmAndPostAction("关闭问题", "确定关闭该告警吗？", "close", { message: updateForm.message }, "已关闭");

const normalizeSeverityValue = (value: string) => {
  const normalized = String(value || "").toLowerCase();
  if (normalized === "critical" || normalized === "灾难") return "critical";
  if (normalized === "warning" || normalized === "重要") return "warning";
  if (normalized === "info" || normalized === "信息") return "info";
  return "warning";
};

const formatSeverity = (value: string) => {
  const normalized = String(value || "").toLowerCase();
  if (normalized === "critical") return "灾难";
  if (normalized === "warning") return "重要";
  if (normalized === "info") return "信息";
  if (normalized === "recover" || normalized === "resolved") return "恢复";
  return value;
};

const formatProblemStatus = (value: string) => {
  const normalized = String(value || "").toLowerCase();
  if (normalized === "auto" || normalized === "manual" || normalized === "resolved") {
    return "已解决";
  }
  return "问题";
};

const statusClass = (value: string) => {
  const normalized = String(value || "").toLowerCase();
  if (normalized === "auto" || normalized === "manual" || normalized === "resolved") {
    return "status-tag status-tag--ok";
  }
  return "status-tag status-tag--bad";
};

const severityClass = (row: any) => {
  const normalizedStatus = String(row?.status || "").toLowerCase();
  if (normalizedStatus === "auto" || normalizedStatus === "manual" || normalizedStatus === "resolved") {
    return "severity-tag severity-tag--resolved";
  }
  const normalized = String(row?.severity || "").toLowerCase();
  if (normalized === "critical") return "severity-tag severity-tag--critical";
  if (normalized === "warning") return "severity-tag severity-tag--warning";
  return "severity-tag severity-tag--info";
};

const formatEndTime = (row: any) => {
  if (!row) {
    return "-";
  }
  const status = String(row.status || "").toLowerCase();
  if (status !== "auto" && status !== "manual" && status !== "resolved") {
    return "-";
  }
  return row.endsAt || "-";
};

const formatDuration = (row: any) => {
  if (!row) return "-";
  const resolved = String(row.status || "").toLowerCase();
  if (resolved !== "auto" && resolved !== "manual" && resolved !== "resolved") {
    return "-";
  }
  const startsAt = row.startsAt ? new Date(row.startsAt).getTime() : NaN;
  const endsAt = row.endsAt ? new Date(row.endsAt).getTime() : NaN;
  if (!Number.isFinite(startsAt) || !Number.isFinite(endsAt) || endsAt <= startsAt) {
    return row.duration || "-";
  }
  const seconds = Math.floor((endsAt - startsAt) / 1000);
  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  if (days > 0) {
    return `${days}天${hours}小时`;
  }
  if (hours > 0) {
    return `${hours}小时${minutes}分钟`;
  }
  return `${minutes}分钟`;
};

const actionTagType = (status: string) => {
  const normalized = String(status || "").toLowerCase();
  if (normalized.includes("成功")) return "success";
  if (normalized.includes("失败")) return "danger";
  return "info";
};

const parseTimeValue = (value: any) => {
  if (!value) return 0;
  const time = new Date(value).getTime();
  return Number.isFinite(time) ? time : 0;
};

const buildEventActions = (row: any) => {
  const actions: any[] = [];
  if (row?.actionTime) {
    actions.push({
      key: `notify-${row.id || ""}-${row.actionTime}`,
      type: "notify",
      title: "发送消息",
      time: row.actionTime,
      media: row.actionMedia,
      receivers: row.actionReceivers,
      sendStatus: row.actionSendStatus
    });
  }
  if (row?.ackTime) {
    actions.push({
      key: `ack-${row.id || ""}-${row.ackTime}`,
      type: "ack",
      title: "确定操作",
      time: row.ackTime,
      operator: row.ackOperator,
      message: row.ackMessage
    });
  }
  if (row?.closeTime) {
    actions.push({
      key: `close-${row.id || ""}-${row.closeTime}`,
      type: "close",
      title: "关闭操作",
      time: row.closeTime,
      operator: row.closeOperator,
      message: row.closeMessage
    });
  }
  return actions.sort((a, b) => parseTimeValue(b.time) - parseTimeValue(a.time));
};

onMounted(() => {
  query();
});
</script>

<style scoped lang="less">
/* 统计标签容器 */
.record-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

/* 统计标签基础样式 */
.record-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}

/* 表格单元格不换行 */
.alert-record-table :deep(.cell) {
  white-space: nowrap;
}

/* 操作链接 */
.action-hover {
  color: #3b82f6;
  cursor: pointer;
  transition: color 0.15s;
}

.action-hover:hover {
  color: #2563eb;
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

.event-tip__group-title {
  margin-bottom: 8px;
  font-size: 12px;
  font-weight: 500;
  color: #3b82f6;
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

/* 更新弹窗 */
.update-dialog__meta {
  padding: 14px 16px;
  margin-bottom: 14px;
  background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
  border: 1px solid #e2e8f0;
  border-radius: 8px;
}

.update-dialog__meta-label {
  margin-bottom: 6px;
  font-size: 12px;
  color: #64748b;
}

.update-dialog__meta-value {
  color: #0f172a;
  line-height: 1.6;
  word-break: break-all;
}

.update-dialog__content {
  display: grid;
  grid-template-columns: minmax(360px, 1fr) minmax(400px, 1.2fr);
  gap: 14px;
}

.update-card {
  padding: 16px;
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
}

.update-card__title {
  margin-bottom: 12px;
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
}

.update-form :deep(.el-form-item) {
  margin-bottom: 16px;
}

.op-row {
  display: flex;
  align-items: center;
  gap: 10px;
  flex-wrap: wrap;
}

.op-input {
  width: 190px;
}

.hint {
  color: #64748b;
  font-size: 12px;
}

.history-table :deep(.cell) {
  line-height: 1.5;
}

@media (max-width: 980px) {
  .update-dialog__content {
    grid-template-columns: 1fr;
  }
}
</style>
