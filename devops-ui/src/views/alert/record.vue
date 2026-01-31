<template>
  <div class="mod-alert__record">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="state.getDataList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.alertName" placeholder="告警名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.severity" placeholder="级别" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.status" placeholder="状态" clearable>
              <el-option label="告警" value="firing"></el-option>
              <el-option label="恢复" value="resolved"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-button @click="state.getDataList()">查询</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-button v-if="state.hasPermission('alert:record:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" class="alert-record-table" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="alertName" label="告警名称" header-align="center" align="center"></el-table-column>
      <el-table-column prop="status" label="告警状态" header-align="center" align="center" width="90">
        <template v-slot="scope">
          {{ formatStatus(scope.row.status) }}
        </template>
      </el-table-column>
      <el-table-column prop="severity" label="级别" header-align="center" align="center" width="90">
        <template v-slot="scope">
          {{ formatSeverity(scope.row.severity) }}
        </template>
      </el-table-column>
      <el-table-column prop="instance" label="实例" header-align="center" align="center"></el-table-column>
      <el-table-column prop="summary" label="摘要" header-align="center" align="center"></el-table-column>
      <el-table-column prop="description" label="描述" header-align="center" align="center"></el-table-column>
      <el-table-column prop="startsAt" label="开始时间" header-align="center" align="center" width="180"></el-table-column>
      <el-table-column prop="endsAt" label="结束时间" header-align="center" align="center" width="180"></el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="150">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('alert:record:info')" type="primary" link @click="showDetail(scope.row.id)">详情</el-button>
          <el-button v-if="state.hasPermission('alert:record:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
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
    ></el-pagination>

    <el-dialog v-model="detailVisible" title="告警详情" width="760px" :close-on-click-modal="false">
      <el-form :model="detail" label-width="100px">
        <el-form-item label="告警名称">{{ detail.alertName }}</el-form-item>
        <el-form-item label="级别">{{ formatSeverity(detail.severity) }}</el-form-item>
        <el-form-item label="状态">{{ formatStatus(detail.status) }}</el-form-item>
        <el-form-item label="实例">{{ detail.instance }}</el-form-item>
        <el-form-item label="摘要">{{ detail.summary }}</el-form-item>
        <el-form-item label="描述">{{ detail.description }}</el-form-item>
        <el-form-item label="原始JSON">
          <el-input :model-value="detail.rawJson" type="textarea" :rows="10" readonly></el-input>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="detailVisible = false">关闭</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {reactive, ref, toRefs} from "vue";
import baseService from "@/service/baseService";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/alert/record/page",
  getDataListIsPage: true,
  deleteURL: "/alert/record",
  dataForm: {
    alertName: "",
    severity: "",
    status: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const detailVisible = ref(false);
const detail = reactive({
  alertName: "",
  severity: "",
  status: "",
  instance: "",
  summary: "",
  description: "",
  rawJson: ""
});

const showDetail = (id: number) => {
  baseService.get("/alert/record/" + id).then((res) => {
    Object.assign(detail, res.data);
    detailVisible.value = true;
  });
};

const formatSeverity = (value: string) => {
  if (!value) {
    return "";
  }
  const normalized = String(value).toLowerCase();
  if (normalized === "critical") {
    return "灾难";
  }
  if (normalized === "warning") {
    return "重要";
  }
  if (normalized === "info") {
    return "信息";
  }
  if (normalized === "recover" || normalized === "resolved") {
    return "恢复";
  }
  return value;
};

const formatStatus = (value: string) => {
  if (!value) {
    return "";
  }
  const normalized = String(value).toLowerCase();
  if (normalized === "firing") {
    return "告警";
  }
  if (normalized === "resolved") {
    return "恢复";
  }
  return value;
};
</script>

<style lang="less" scoped>
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
.alert-record-table :deep(.cell) {
  white-space: nowrap;
}
</style>
