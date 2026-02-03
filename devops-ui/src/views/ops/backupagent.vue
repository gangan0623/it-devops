<template>
  <div class="mod-ops__backupagent">
    <div class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-input v-model="state.dataForm.instance" placeholder="地址(模糊)" clearable @keyup.enter="state.getDataList()"></el-input>
          <el-input v-model="state.dataForm.name" placeholder="名称(模糊)" clearable @keyup.enter="state.getDataList()"></el-input>
          <el-button @click="state.getDataList()">查询</el-button>
          <el-button :icon="Filter" @click="filterDrawer = true">筛选<span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span></el-button>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-button v-if="state.hasPermission('ops:backupagent:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:update')" type="success" @click="handleBatchToggle">启用/禁用</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:export')" type="info" @click="state.exportHandle()">导出</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:import')" type="primary" @click="importDialogVisible = true">导入</el-button>
        </div>
      </div>
    </div>
    <el-drawer v-model="filterDrawer" title="筛选条件" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="区域名称">
          <ren-select v-model="state.dataForm.areaName" dict-type="area_name_type" label-field="dictValue" value-field="dictLabel" placeholder="全部"></ren-select>
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="state.dataForm.status" placeholder="全部" clearable>
            <el-option label="启用" :value="1"></el-option>
            <el-option label="禁用" :value="0"></el-option>
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="handleFilterReset">重置</el-button>
        <el-button type="primary" @click="handleFilterConfirm">确定</el-button>
      </template>
    </el-drawer>
    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
              <el-table-column prop="instance" label="地址" header-align="center" align="center"></el-table-column>
              <el-table-column prop="name" label="名称" header-align="center" align="center"></el-table-column>
              <el-table-column label="区域名称" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("area_name_type", scope.row.areaName) }}</template>
              </el-table-column>
              <el-table-column prop="token" label="Token" header-align="center" align="center">
                <template v-slot="scope">
                  <span>***</span>
                </template>
              </el-table-column>
              <el-table-column prop="status" label="状态" header-align="center" align="center">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
                  <el-tag v-else size="small" type="success">启用</el-tag>
                </template>
              </el-table-column>
              <el-table-column prop="onlineStatus" label="在线状态" header-align="center" align="center">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.onlineStatus === true" size="small" type="success">在线</el-tag>
                  <el-tag v-else-if="scope.row.onlineStatus === false" size="small" type="danger">不在线</el-tag>
                  <el-tag v-else size="small" type="info">检测中</el-tag>
                </template>
              </el-table-column>
            <el-table-column label="操作" fixed="right" header-align="center" align="center" width="150">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:backupagent:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:save')" type="primary" link @click="cloneHandle(scope.row)">克隆</el-button>
          <el-button v-if="state.hasPermission('ops:backupagent:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>
    <el-pagination :current-page="state.page" :page-sizes="[10, 20, 50, 100]" :page-size="state.limit" :total="state.total" layout="total, sizes, prev, pager, next, jumper" @size-change="state.pageSizeChangeHandle" @current-change="state.pageCurrentChangeHandle"> </el-pagination>
    <el-dialog v-model="importDialogVisible" title="导入" width="420px">
      <div class="import-actions">
        <el-button v-if="state.hasPermission('ops:backupagent:template')" type="info" @click="handleTemplateDownload">下载示例表格</el-button>
        <el-upload v-if="state.hasPermission('ops:backupagent:import')" :action="importUrl" :headers="uploadHeaders" :show-file-list="false" :before-upload="beforeImportUpload" :on-success="handleImportSuccess" accept=".xls,.xlsx">
          <el-button type="primary">选择文件</el-button>
        </el-upload>
      </div>
    </el-dialog>
    <!-- 弹窗, 新增 / 修改 -->
    <add-or-update ref="addOrUpdateRef" @refreshDataList="state.getDataList">确定</add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, reactive, ref, toRefs, watch} from "vue";
import AddOrUpdate from "./backupagent-add-or-update.vue";
import baseService from "@/service/baseService";
import {ElMessage, ElMessageBox} from "element-plus";
import {Filter} from "@element-plus/icons-vue";
import app from "@/constants/app";
import {getToken} from "@/utils/cache";
import {IObject} from "@/types/interface";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/backupagent/page",
  getDataListIsPage: true,
  exportURL: "/ops/backupagent/export",
  deleteURL: "/ops/backupagent",
  dataForm: {
    instance: "",
    name: "",
    areaName: "",
    status: "" as string | number
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });

const filterDrawer = ref(false);

const activeFilterCount = computed(() => {
  let count = 0;
  if (state.dataForm.areaName) count++;
  if (state.dataForm.status !== "" && state.dataForm.status !== null && state.dataForm.status !== undefined) count++;
  return count;
});

const handleFilterConfirm = () => {
  filterDrawer.value = false;
  state.getDataList();
};

const handleFilterReset = () => {
  state.dataForm.areaName = "";
  state.dataForm.status = "";
};

const addOrUpdateRef = ref();
const importDialogVisible = ref(false);
const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const cloneHandle = (row: any) => {
  addOrUpdateRef.value.initClone(row);
};

const importUrl = `${app.api}/ops/backupagent/import?token=${getToken()}`;
const templateUrl = `${app.api}/ops/backupagent/template?token=${getToken()}`;
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
      state.getDataList();
    }
  });
};

const handleTemplateDownload = () => {
  window.location.href = templateUrl;
};

const refreshOnlineStatus = () => {
  if (!state.dataList || state.dataList.length === 0) {
    return;
  }
  state.dataList.forEach((row: { instance?: string; onlineStatus?: boolean | null }) => {
    row.onlineStatus = null;
    if (!row.instance) {
      row.onlineStatus = false;
      return;
    }
    baseService
      .get("/ops/backupagent/online", { instance: row.instance })
      .then((res) => {
        row.onlineStatus = !!res.data;
      })
      .catch(() => {
        row.onlineStatus = false;
      });
  });
};

watch(
  () => state.dataList,
  () => {
    refreshOnlineStatus();
  }
);

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
    baseService.put("/ops/backupagent/status", { ids, status }).then(() => {
      ElMessage.success({
        message: "成功",
        duration: 500,
        onClose: () => {
          state.getDataList();
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
.ops-filters .el-form-item {
  margin-bottom: 0;
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
</style>
