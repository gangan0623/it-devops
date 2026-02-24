<template>
  <div class="mod-ops__windowhost">
    <div class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-input v-model="state.dataForm.instance" class="query-input" placeholder="地址(模糊)" clearable @keyup.enter="queryList()"></el-input>
          <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          <el-button class="query-btn" @click="handleToolbarReset">重置</el-button>
          <el-button :icon="Filter" @click="filterDrawer = true">筛选<span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span></el-button>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="host-stats">
            <span class="host-stats__item host-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="host-stats__item host-stats__item--off">禁用 {{ disabledCount }}</span>
            <span class="host-stats__item host-stats__item--online">在线 {{ onlineCount }}</span>
            <span class="host-stats__item host-stats__item--filter">离线 {{ offlineCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('ops:windowhost:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:update')" type="success" @click="handleBatchToggle">启用/禁用</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:export')" type="info" @click="state.exportHandle()">导出</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:import')" type="primary" @click="importDialogVisible = true">导入</el-button>
        </div>
      </div>
    </div>
    <el-drawer v-model="filterDrawer" title="筛选条件" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="站点位置">
          <ren-select v-model="state.dataForm.siteLocation" dict-type="base_site_location" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="区域名称">
          <ren-select v-model="state.dataForm.areaName" dict-type="area_name_type" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="名称(模糊)">
          <el-input v-model="state.dataForm.name" placeholder="名称(模糊)" clearable></el-input>
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="state.dataForm.status" placeholder="全部" clearable>
            <el-option label="启用" :value="1"></el-option>
            <el-option label="禁用" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="在线状态">
          <el-select v-model="state.dataForm.onlineStatus" placeholder="全部" clearable>
            <el-option label="在线" :value="1"></el-option>
            <el-option label="不在线" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="分组名称">
          <ren-select v-model="state.dataForm.menuName" dict-type="server_host_group" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="主机类型">
          <ren-select v-model="state.dataForm.type" dict-type="server_machine_type" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="handleFilterReset">重置</el-button>
        <el-button type="primary" @click="handleFilterConfirm">确定</el-button>
      </template>
    </el-drawer>
    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" @sort-change="state.dataListSortChangeHandle" class="ops-table-nowrap" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
              <el-table-column prop="instance" label="地址" header-align="center" align="center" min-width="180"></el-table-column>
              <el-table-column prop="name" label="名称" header-align="center" align="center" min-width="180"></el-table-column>
              <el-table-column label="区域名称" header-align="center" align="center" min-width="60">
                <template v-slot="scope">{{ state.getDictValueByLabel("area_name_type", scope.row.areaName) }}</template>
              </el-table-column>
              <el-table-column label="站点位置" header-align="center" align="center" min-width="60">
                <template v-slot="scope">{{ state.getDictValueByLabel("base_site_location", scope.row.siteLocation) }}</template>
              </el-table-column>
              <el-table-column label="分组名称" header-align="center" align="center" min-width="60">
                <template v-slot="scope">{{ state.getDictValueByLabel("server_host_group", scope.row.menuName) }}</template>
              </el-table-column>
              <el-table-column label="主机类型" header-align="center" align="center" min-width="60">
                <template v-slot="scope">{{ state.getDictValueByLabel("server_machine_type", scope.row.type) }}</template>
              </el-table-column>
              <el-table-column prop="subMenuName" label="子组名称" header-align="center" align="center" min-width="60"></el-table-column>
              <el-table-column prop="status" label="状态" header-align="center" align="center" min-width="60">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
                  <el-tag v-else size="small" type="success">启用</el-tag>
                </template>
              </el-table-column>
              <el-table-column prop="onlineStatus" label="在线状态" header-align="center" align="center" min-width="60">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.onlineStatus === true" size="small" type="success">在线</el-tag>
                  <el-tag v-else-if="scope.row.onlineStatus === false" size="small" type="danger">不在线</el-tag>
                  <el-tag v-else size="small" type="info">检测中</el-tag>
                </template>
              </el-table-column>
            <el-table-column label="操作" fixed="right" header-align="center" align="center" width="260">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:windowhost:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:save')" type="primary" link @click="cloneHandle(scope.row)">克隆</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>
    <el-pagination :current-page="state.page" :page-sizes="[10, 20, 50, 100]" :page-size="state.limit" :total="state.total" layout="total, sizes, prev, pager, next, jumper" @size-change="state.pageSizeChangeHandle" @current-change="state.pageCurrentChangeHandle"> </el-pagination>
    <el-dialog v-model="importDialogVisible" title="导入" width="420px">
      <div class="import-actions">
        <el-button v-if="state.hasPermission('ops:windowhost:template')" type="info" @click="handleTemplateDownload">下载示例表格</el-button>
        <el-upload v-if="state.hasPermission('ops:windowhost:import')" :action="importUrl" :headers="uploadHeaders" :show-file-list="false" :before-upload="beforeImportUpload" :on-success="handleImportSuccess" accept=".xls,.xlsx">
          <el-button type="primary">选择文件</el-button>
        </el-upload>
      </div>
    </el-dialog>
    <!-- 弹窗, 新增 / 修改 -->
    <add-or-update ref="addOrUpdateRef" @refreshDataList="queryList">确定</add-or-update>
  </div>
</template>

<style scoped>
.ops-table-nowrap :deep(.cell) {
  white-space: nowrap;
}
</style>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, onMounted, reactive, ref, toRefs} from "vue";
import AddOrUpdate from "./windowhost-add-or-update.vue";
import baseService from "@/service/baseService";
import {ElMessage, ElMessageBox} from "element-plus";
import {Filter} from "@element-plus/icons-vue";
import app from "@/constants/app";
import {getToken} from "@/utils/cache";
import {IObject} from "@/types/interface";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/windowhost/page",
  getDataListIsPage: true,
  exportURL: "/ops/windowhost/export",
  deleteURL: "/ops/windowhost",
  dataForm: {
    instance: "",
    name: "",
    siteLocation: "",
    areaName: "",
    status: "" as string | number,
    onlineStatus: "" as string | number,
    menuName: "",
    type: ""
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

const activeFilterCount = computed(() => {
  let count = 0;
  if (state.dataForm.siteLocation) count++;
  if (state.dataForm.areaName) count++;
  if (state.dataForm.name) count++;
  if (state.dataForm.status !== "" && state.dataForm.status !== null && state.dataForm.status !== undefined) count++;
  if (state.dataForm.onlineStatus !== "" && state.dataForm.onlineStatus !== null && state.dataForm.onlineStatus !== undefined) count++;
  if (state.dataForm.menuName) count++;
  if (state.dataForm.type) count++;
  return count;
});

const handleFilterConfirm = () => {
  filterDrawer.value = false;
  queryList();
};

const handleFilterReset = () => {
  state.dataForm.siteLocation = "";
  state.dataForm.areaName = "";
  state.dataForm.status = "";
  state.dataForm.onlineStatus = "";
  state.dataForm.menuName = "";
  state.dataForm.type = "";
};

const handleToolbarReset = () => {
  state.dataForm.instance = "";
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

const importUrl = `${app.api}/ops/windowhost/import?token=${getToken()}`;
const templateUrl = `${app.api}/ops/windowhost/template?token=${getToken()}`;
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

const loadStatusSummary = () => {
  baseService
    .get("/ops/windowhost/summary")
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
    baseService.put("/ops/windowhost/status", { ids, status }).then(() => {
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
/* 统计标签容器 */
.host-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

/* 统计标签基础样式 */
.host-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}
</style>
