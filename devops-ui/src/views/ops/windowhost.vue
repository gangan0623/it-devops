<template>
  <div class="mod-ops__windowhost">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="state.getDataList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.instance" placeholder="地址" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.name" placeholder="名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <ren-select
              v-model="state.dataForm.areaName"
              dict-type="area_name_type"
              label-field="dictValue"
              value-field="dictLabel"
              placeholder="区域名称"
            ></ren-select>
          </el-form-item>
          <el-form-item>
            <el-button @click="state.getDataList()">查询</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-button v-if="state.hasPermission('ops:windowhost:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:update')" type="success" @click="updateStatusHandle(1)">批量启用</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:update')" type="warning" @click="updateStatusHandle(0)">批量禁用</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:export')" type="info" @click="state.exportHandle()">导出</el-button>
          <el-upload v-if="state.hasPermission('ops:windowhost:import')" :action="importUrl" :headers="uploadHeaders" :show-file-list="false" :before-upload="beforeImportUpload" :on-success="handleImportSuccess" accept=".xls,.xlsx">
            <el-button type="primary">导入</el-button>
          </el-upload>
          <el-button v-if="state.hasPermission('ops:windowhost:template')" type="info" @click="handleTemplateDownload">下载示例表格</el-button>
        </div>
      </div>
    </el-form>
    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" @sort-change="state.dataListSortChangeHandle" class="ops-table-nowrap" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
              <el-table-column prop="instance" label="地址" header-align="center" align="center" min-width="180"></el-table-column>
              <el-table-column prop="name" label="名称" header-align="center" align="center"></el-table-column>
              <el-table-column label="区域名称" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("area_name_type", scope.row.areaName) }}</template>
              </el-table-column>
              <el-table-column label="站点位置" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("base_site_location", scope.row.siteLocation) }}</template>
              </el-table-column>
              <el-table-column label="分组名称" header-align="center" align="center">
                <template v-slot="scope">{{ state.getDictValueByLabel("virtual_host_group", scope.row.menuName) }}</template>
              </el-table-column>
              <el-table-column prop="subMenuName" label="子组名称" header-align="center" align="center"></el-table-column>
              <el-table-column prop="status" label="状态" header-align="center" align="center" width="80">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
                  <el-tag v-else size="small" type="success">启用</el-tag>
                </template>
              </el-table-column>
              <el-table-column prop="onlineStatus" label="在线状态" header-align="center" align="center" width="90" sortable="custom">
                <template v-slot="scope">
                  <el-tag v-if="scope.row.onlineStatus === true" size="small" type="success">在线</el-tag>
                  <el-tag v-else-if="scope.row.onlineStatus === false" size="small" type="danger">不在线</el-tag>
                  <el-tag v-else size="small" type="info">检测中</el-tag>
                </template>
              </el-table-column>
            <el-table-column label="操作" fixed="right" header-align="center" align="center" width="150">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:windowhost:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:save')" type="primary" link @click="cloneHandle(scope.row)">克隆</el-button>
          <el-button v-if="state.hasPermission('ops:windowhost:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>
    <el-pagination :current-page="state.page" :page-sizes="[10, 20, 50, 100]" :page-size="state.limit" :total="state.total" layout="total, sizes, prev, pager, next, jumper" @size-change="state.pageSizeChangeHandle" @current-change="state.pageCurrentChangeHandle"> </el-pagination>
    <!-- 弹窗, 新增 / 修改 -->
    <add-or-update ref="addOrUpdateRef" @refreshDataList="state.getDataList">确定</add-or-update>
  </div>
</template>

<style scoped>
.ops-table-nowrap :deep(.cell) {
  white-space: nowrap;
}
</style>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {reactive, ref, toRefs} from "vue";
import AddOrUpdate from "./windowhost-add-or-update.vue";
import baseService from "@/service/baseService";
import {ElMessage, ElMessageBox} from "element-plus";
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
    areaName: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });

const addOrUpdateRef = ref();
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
      state.getDataList();
    }
  });
};

const handleTemplateDownload = () => {
  window.location.href = templateUrl;
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
</style>
