<template>
  <div class="mod-ops__domain-record">
    <el-form :inline="true" :model="state.dataForm" class="ops-toolbar" @keyup.enter="queryList()">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.projectName" class="query-input" placeholder="项目名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.domainName" class="query-input" placeholder="域名" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.projectOwner" class="query-input" placeholder="项目负责人" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.adEnabled" placeholder="应用交付" clearable style="width: 140px">
              <el-option label="走AD" :value="1"></el-option>
              <el-option label="不走AD" :value="0"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.externalEnabled" placeholder="外网状态" clearable style="width: 140px">
              <el-option label="启用" :value="1"></el-option>
              <el-option label="未启用" :value="0"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" @click="handleReset">重置</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-button v-if="state.hasPermission('ops:domain-record:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:domain-record:delete')" type="danger" @click="deleteBatchHandle">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table
      v-loading="state.dataListLoading"
      :data="state.dataList"
      border
      style="width: 100%"
      @selection-change="state.dataListSelectionChangeHandle"
    >
      <el-table-column type="selection" width="50" header-align="center" align="center"></el-table-column>
      <el-table-column prop="projectName" label="项目名称" min-width="140" show-overflow-tooltip></el-table-column>
      <el-table-column prop="domainName" label="域名" min-width="180" show-overflow-tooltip></el-table-column>
      <el-table-column prop="virtualServiceName" label="虚拟服务名称" min-width="150" show-overflow-tooltip></el-table-column>
      <el-table-column prop="virtualServiceIp" label="虚拟服务IP" min-width="140"></el-table-column>
      <el-table-column prop="virtualServicePort" label="虚拟服务端口" width="120"></el-table-column>
      <el-table-column prop="virtualServiceProtocol" label="虚拟服务协议" width="130"></el-table-column>
      <el-table-column label="外网状态/是否启用" width="140">
        <template #default="{ row }">
          <el-tag :type="row.externalEnabled === 1 ? 'success' : 'info'" size="small">
            {{ row.externalEnabled === 1 ? "启用" : "未启用" }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="externalAddress" label="外网地址" min-width="180" show-overflow-tooltip></el-table-column>
      <el-table-column prop="description" label="描述" min-width="160" show-overflow-tooltip></el-table-column>
      <el-table-column prop="projectOwner" label="项目负责人" width="120"></el-table-column>
      <el-table-column prop="applyTime" label="申请时间" min-width="170"></el-table-column>
      <el-table-column prop="remark" label="备注" min-width="160" show-overflow-tooltip></el-table-column>
      <el-table-column label="操作" fixed="right" width="220" header-align="center" align="center">
        <template #default="{ row }">
          <el-button v-if="state.hasPermission('ops:domain-record:update')" type="primary" link @click="addOrUpdateHandle(row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('ops:domain-record:history')" type="primary" link @click="openHistory(row)">操作记录</el-button>
          <el-button v-if="state.hasPermission('ops:domain-record:delete')" type="danger" link @click="deleteSingleHandle(row.id)">删除</el-button>
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

    <el-drawer v-model="historyVisible" title="操作记录" size="900px" :append-to-body="true">
      <el-table v-loading="historyLoading" :data="historyList" border style="width: 100%">
        <el-table-column prop="operationType" label="操作类型" width="100"></el-table-column>
        <el-table-column prop="operatorName" label="操作人" width="120"></el-table-column>
        <el-table-column prop="operationTime" label="操作时间" min-width="170"></el-table-column>
        <el-table-column prop="operationSummary" label="操作摘要" min-width="260" show-overflow-tooltip></el-table-column>
        <el-table-column label="操作" width="90">
          <template #default="{ row }">
            <el-button type="primary" link @click="loadHistoryDetail(row.id)">详情</el-button>
          </template>
        </el-table-column>
      </el-table>

      <div v-if="historyDetail" class="history-detail">
        <div class="history-detail__title">详情</div>
        <el-descriptions :column="2" border>
          <el-descriptions-item label="操作类型">{{ historyDetail.operationType }}</el-descriptions-item>
          <el-descriptions-item label="操作人">{{ historyDetail.operatorName }}</el-descriptions-item>
          <el-descriptions-item label="操作时间">{{ historyDetail.operationTime }}</el-descriptions-item>
          <el-descriptions-item label="操作摘要">{{ historyDetail.operationSummary }}</el-descriptions-item>
        </el-descriptions>

        <el-table v-if="historyDetail.details?.length" :data="historyDetail.details" border style="margin-top: 16px">
          <el-table-column prop="fieldName" label="字段" min-width="160"></el-table-column>
          <el-table-column prop="beforeValue" label="变更前" min-width="260" show-overflow-tooltip></el-table-column>
          <el-table-column prop="afterValue" label="变更后" min-width="260" show-overflow-tooltip></el-table-column>
        </el-table>

        <el-row :gutter="16" style="margin-top: 16px">
          <el-col :span="12">
            <div class="history-detail__subtitle">修改前快照</div>
            <el-input v-model="historyDetail.snapshotBefore" type="textarea" :rows="12" readonly></el-input>
          </el-col>
          <el-col :span="12">
            <div class="history-detail__subtitle">修改后快照</div>
            <el-input v-model="historyDetail.snapshotAfter" type="textarea" :rows="12" readonly></el-input>
          </el-col>
        </el-row>
      </div>
    </el-drawer>

    <add-or-update ref="addOrUpdateRef" @refreshDataList="queryList"></add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import { reactive, ref, toRefs } from "vue";
import baseService from "@/service/baseService";
import { ElMessage, ElMessageBox } from "element-plus";
import AddOrUpdate from "./domain-record-add-or-update.vue";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/domain-record/page",
  getDataListIsPage: true,
  dataForm: {
    projectName: "",
    domainName: "",
    projectOwner: "",
    adEnabled: "" as string | number,
    externalEnabled: "" as string | number
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const addOrUpdateRef = ref();
const historyVisible = ref(false);
const historyLoading = ref(false);
const historyList = ref<any[]>([]);
const historyDetail = ref<any>(null);

const queryList = () => {
  state.getDataList();
};

const handleReset = () => {
  state.dataForm.projectName = "";
  state.dataForm.domainName = "";
  state.dataForm.projectOwner = "";
  state.dataForm.adEnabled = "";
  state.dataForm.externalEnabled = "";
  queryList();
};

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const deleteRequest = (ids: number[]) => {
  return baseService.delete("/ops/domain-record", { ids }).then(() => {
    ElMessage.success({
      message: "成功",
      duration: 500,
      onClose: () => queryList()
    });
  });
};

const deleteSingleHandle = (id: number) => {
  ElMessageBox.confirm("确定进行[删除]操作?", "提示", {
    confirmButtonText: "确定",
    cancelButtonText: "取消",
    type: "warning"
  }).then(() => deleteRequest([id]));
};

const deleteBatchHandle = () => {
  const ids = (state.dataListSelections || []).map((item: any) => item.id).filter(Boolean);
  if (!ids.length) {
    ElMessage.warning({ message: "请选择操作项", duration: 500 });
    return;
  }
  ElMessageBox.confirm("确定进行[删除]操作?", "提示", {
    confirmButtonText: "确定",
    cancelButtonText: "取消",
    type: "warning"
  }).then(() => deleteRequest(ids));
};

const openHistory = (row: any) => {
  historyVisible.value = true;
  historyDetail.value = null;
  historyLoading.value = true;
  baseService
    .get("/ops/domain-record/history/page", {
      page: 1,
      limit: 100,
      domainRecordId: row.id
    })
    .then((res) => {
      historyList.value = res.data?.list || [];
    })
    .finally(() => {
      historyLoading.value = false;
    });
};

const loadHistoryDetail = (id: number) => {
  baseService.get(`/ops/domain-record/history/${id}`).then((res) => {
    historyDetail.value = res.data;
  });
};

queryList();
</script>

<style scoped>
.history-detail {
  margin-top: 20px;
}

.history-detail__title {
  margin-bottom: 12px;
  font-size: 15px;
  font-weight: 600;
}

.history-detail__subtitle {
  margin-bottom: 8px;
  font-size: 14px;
  font-weight: 600;
}
</style>
