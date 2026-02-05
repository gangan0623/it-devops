<template>
  <div class="mod-ops__devicebackup">
    <div class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-input v-model="state.dataForm.instance" class="query-input" placeholder="地址(模糊)" clearable @keyup.enter="queryList()"></el-input>
          <el-input v-model="state.dataForm.name" class="query-input" placeholder="名称(模糊)" clearable @keyup.enter="queryList()"></el-input>
          <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          <el-button class="query-btn" @click="handleToolbarReset">重置</el-button>
          <el-button :icon="Filter" @click="filterDrawer = true">筛选<span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span></el-button>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="device-stats">
            <span class="device-stats__item device-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="device-stats__item device-stats__item--off">禁用 {{ disabledCount }}</span>
            <span class="device-stats__item device-stats__item--online">在线 {{ onlineCount }}</span>
            <span class="device-stats__item device-stats__item--filter">离线 {{ offlineCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('ops:devicebackup:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:update')" type="success" @click="handleBatchToggle">启用/禁用</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:export')" type="info" @click="state.exportHandle()">导出</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:import')" type="primary" @click="importDialogVisible = true">导入</el-button>
        </div>
      </div>
    </div>
    <el-drawer v-model="filterDrawer" title="筛选条件" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="区域名称">
          <ren-select v-model="state.dataForm.areaName" dict-type="area_name_type" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="分组名称">
          <ren-select v-model="state.dataForm.groupName" dict-type="network_device_group" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="设备型号">
          <ren-select v-model="state.dataForm.deviceModel" dict-type="network_device_model" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="state.dataForm.status" placeholder="全部" clearable>
            <el-option label="启用" :value="1"></el-option>
            <el-option label="禁用" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="备份节点">
          <el-select v-model="state.dataForm.agentId" placeholder="全部" clearable filterable>
            <el-option v-for="item in backupAgentOptions" :key="item.id" :label="item.label" :value="item.id"></el-option>
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="handleFilterReset">重置</el-button>
        <el-button type="primary" @click="handleFilterConfirm">确定</el-button>
      </template>
    </el-drawer>
    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" class="device-table" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
              <el-table-column prop="instance" label="地址" header-align="center" align="center"></el-table-column>
              <el-table-column prop="name" label="名称" header-align="center" align="center"></el-table-column>
              <el-table-column prop="username" label="用户名" header-align="center" align="center"></el-table-column>
              <el-table-column prop="password" label="密码" header-align="center" align="center">
                <template v-slot="scope">
                  <span>***</span>
                </template>
              </el-table-column>
              <el-table-column label="区域名称" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("area_name_type", scope.row.areaName) }}</template>
              </el-table-column>
              <el-table-column label="分组名称" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("network_device_group", scope.row.groupName) }}</template>
              </el-table-column>
              <el-table-column label="设备型号" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("network_device_model", scope.row.deviceModel) }}</template>
              </el-table-column>
              <el-table-column prop="status" label="状态" header-align="center" align="center">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
                  <el-tag v-else size="small" type="success">启用</el-tag>
                </template>
              </el-table-column>
              <el-table-column prop="agentName" label="节点名称" header-align="center" align="center"></el-table-column>
              <el-table-column prop="onlineStatus" label="在线状态" header-align="center" align="center">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.onlineStatus === true" size="small" type="success">在线</el-tag>
                  <el-tag v-else-if="scope.row.onlineStatus === false" size="small" type="danger">不在线</el-tag>
                  <el-tag v-else size="small" type="info">检测中</el-tag>
                </template>
              </el-table-column>
            <el-table-column label="操作" fixed="right" header-align="center" align="center" width="150">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:devicebackup:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:save')" type="primary" link @click="cloneHandle(scope.row)">克隆</el-button>
          <el-button v-if="state.hasPermission('ops:devicebackup:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>
    <el-pagination :current-page="state.page" :page-sizes="[10, 20, 50, 100]" :page-size="state.limit" :total="state.total" layout="total, sizes, prev, pager, next, jumper" @size-change="state.pageSizeChangeHandle" @current-change="state.pageCurrentChangeHandle"> </el-pagination>
    <el-dialog v-model="importDialogVisible" title="导入" width="420px">
      <div class="import-actions">
        <el-button v-if="state.hasPermission('ops:devicebackup:template')" type="info" @click="handleTemplateDownload">下载示例表格</el-button>
        <el-upload v-if="state.hasPermission('ops:devicebackup:import')" :action="importUrl" :headers="uploadHeaders" :show-file-list="false" :before-upload="beforeImportUpload" :on-success="handleImportSuccess" accept=".xls,.xlsx">
          <el-button type="primary">选择文件</el-button>
        </el-upload>
      </div>
    </el-dialog>
    <!-- 弹窗, 新增 / 修改 -->
    <add-or-update ref="addOrUpdateRef" @refreshDataList="queryList">确定</add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, onMounted, reactive, ref, toRefs} from "vue";
import AddOrUpdate from "./devicebackup-add-or-update.vue";
import baseService from "@/service/baseService";
import {ElMessage, ElMessageBox} from "element-plus";
import {Filter} from "@element-plus/icons-vue";
import app from "@/constants/app";
import {getToken} from "@/utils/cache";
import {IObject} from "@/types/interface";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/devicebackup/page",
  getDataListIsPage: true,
  exportURL: "/ops/devicebackup/export",
  deleteURL: "/ops/devicebackup",
  dataForm: {
    instance: "",
    name: "",
    areaName: "",
    groupName: "",
    deviceModel: "",
    status: "" as string | number,
    agentId: ""
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

const backupAgentOptions = ref<{ id: number; label: string }[]>([]);

const filterDrawer = ref(false);

const activeFilterCount = computed(() => {
  let count = 0;
  if (state.dataForm.areaName) count++;
  if (state.dataForm.groupName) count++;
  if (state.dataForm.deviceModel) count++;
  if (state.dataForm.status !== "" && state.dataForm.status !== null && state.dataForm.status !== undefined) count++;
  if (state.dataForm.agentId) count++;
  return count;
});

const handleFilterConfirm = () => {
  filterDrawer.value = false;
  queryList();
};

const handleFilterReset = () => {
  state.dataForm.areaName = "";
  state.dataForm.groupName = "";
  state.dataForm.deviceModel = "";
  state.dataForm.status = "";
  state.dataForm.agentId = "";
};

const handleToolbarReset = () => {
  state.dataForm.instance = "";
  state.dataForm.name = "";
  handleFilterReset();
  queryList();
};

const addOrUpdateRef = ref();
const importDialogVisible = ref(false);
const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const cloneHandle = (row: any) => {
  addOrUpdateRef.value.initClone(row);
};

const importUrl = `${app.api}/ops/devicebackup/import?token=${getToken()}`;
const templateUrl = `${app.api}/ops/devicebackup/template?token=${getToken()}`;
const uploadHeaders = {
  token: getToken()
};

const beforeImportUpload = (file: File) => {
  const isExcel = file.type === "application/vnd.ms-excel" || file.type === "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
  if (!isExcel) {
    ElMessage.error("只支持xls、xlsx格式文件！");
  }
  return isExcel;
};

const handleImportSuccess = (res: IObject) => {
  if (res.code !== 0) {
    return ElMessage.error(res.msg);
  }
  ElMessage.success({
    message: "成功",
      duration: 500,
      onClose: () => {
      queryList();
    }
  });
};

const handleTemplateDownload = () => {
  window.location.href = templateUrl;
};

const loadBackupAgents = () => {
  backupAgentOptions.value = [];
  baseService.get("/ops/backupagent/page", { page: 1, limit: 1000 }).then((res) => {
    const list = res.data?.list || [];
    backupAgentOptions.value = list.map((item: any) => ({
      id: item.id,
      label: item.name
    }));
  });
};

const loadStatusSummary = () => {
  baseService
    .get("/ops/devicebackup/summary")
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

const queryList = () => {
  state.getDataList();
  loadStatusSummary();
};

loadBackupAgents();
onMounted(() => {
  loadStatusSummary();
});

const handleBatchToggle = () => {
  if (!state.dataListSelections || state.dataListSelections.length === 0) {
    ElMessage.warning({
      message: "请选择操作项",
      duration: 500
    });
    return;
  }
  const statusSet = new Set(state.dataListSelections.map((item: { status?: number }) => item.status));
  if (statusSet.size !== 1) {
    ElMessage.warning({
      message: "请选择相同状态的设备",
      duration: 500
    });
    return;
  }
  const currentStatus = Number(state.dataListSelections[0].status);
  const nextStatus = currentStatus === 1 ? 0 : 1;
  updateStatusHandle(nextStatus);
};

const updateStatusHandle = (status: number) => {
  if (!state.dataListSelections || state.dataListSelections.length === 0) {
    ElMessage.warning({
      message: "请选择操作项",
      duration: 500
    });
    return;
  }
  const actionLabel = status === 1 ? "启用" : "禁用";
  const ids = state.dataListSelections.map((item: { id: number }) => item.id);
  ElMessageBox.confirm(`确定进行[${actionLabel}]操作?`, "提示", {
    confirmButtonText: "确定",
    cancelButtonText: "取消",
    type: "warning"
  }).then(() => {
    baseService.put("/ops/devicebackup/status", { ids, status }).then(() => {
      ElMessage.success({
        message: "成功",
        duration: 500,
        onClose: () => {
          queryList();
        }
      });
    });
  });
};
</script>

<style scoped>
.ops-toolbar {
  padding: 12px 16px;
  margin-bottom: 12px;
  background: #fff;
  border-radius: 8px;
  box-shadow: 0 6px 16px rgba(15, 23, 42, 0.06);
}
.ops-toolbar__row {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 12px;
  flex-wrap: nowrap;
  overflow-x: auto;
}
.ops-toolbar__group {
  display: flex;
  align-items: center;
  gap: 8px;
  flex-wrap: nowrap;
  white-space: nowrap;
}
.device-stats {
  display: flex;
  align-items: center;
  gap: 6px;
}
.device-stats__item {
  padding: 2px 8px;
  border-radius: 999px;
  font-size: 12px;
}
.device-stats__item--on {
  color: #065f46;
  background: #d1fae5;
}
.device-stats__item--off {
  color: #991b1b;
  background: #fee2e2;
}
.device-stats__item--online {
  color: #1d4ed8;
  background: #dbeafe;
}
.device-stats__item--filter {
  color: #4338ca;
  background: #e0e7ff;
}
.ops-filters .el-form-item {
  margin-bottom: 0;
}
.query-input {
  width: 220px;
}
.query-btn {
  height: 32px;
  padding: 0 14px;
}
.ops-toolbar__group :deep(.el-input__wrapper),
.ops-toolbar__group :deep(.el-select__wrapper) {
  height: 32px;
}
.mod-ops__devicebackup :deep(.el-table .cell) {
  white-space: nowrap;
}
.filter-badge {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  min-width: 16px;
  height: 16px;
  margin-left: 4px;
  padding: 0 4px;
  font-size: 11px;
  line-height: 1;
  color: #fff;
  background: #409eff;
  border-radius: 8px;
}
.filter-form .el-select,
.filter-form .ren-select {
  width: 100%;
}
.filter-form .el-form-item {
  margin-bottom: 18px;
}
.import-actions {
  display: flex;
  align-items: center;
  gap: 12px;
}
.device-table :deep(.el-table__row:hover > td) {
  background: #f8fafc;
}
</style>
