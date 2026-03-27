<template>
  <div class="mod-ops__domain-record-history">
    <el-form :inline="true" :model="state.dataForm" class="ops-toolbar" @keyup.enter="queryList()">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-select v-model="state.dataForm.operationType" placeholder="操作类型" clearable style="width: 140px">
              <el-option label="新增" value="CREATE"></el-option>
              <el-option label="修改" value="UPDATE"></el-option>
              <el-option label="删除" value="DELETE"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.operatorName" class="query-input" placeholder="操作人" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-date-picker
              v-model="timeRange"
              type="datetimerange"
              value-format="YYYY-MM-DD HH:mm:ss"
              range-separator="至"
              start-placeholder="开始时间"
              end-placeholder="结束时间"
            ></el-date-picker>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" @click="handleReset">重置</el-button>
          </el-form-item>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border style="width: 100%">
      <el-table-column prop="operationType" label="操作类型" width="100" align="center">
        <template #default="{ row }">
          <el-tag :type="tagTypeMap[row.operationType] || 'info'" size="small">
            {{ operationTypeText(row.operationType) }}
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="operatorName" label="操作人" width="140"></el-table-column>
      <el-table-column prop="operationTime" label="操作时间" min-width="180"></el-table-column>
      <el-table-column prop="operationSummary" label="操作摘要" min-width="420" show-overflow-tooltip></el-table-column>
      <el-table-column label="操作" width="100" fixed="right" align="center">
        <template #default="{ row }">
          <el-button type="primary" link @click="loadDetail(row.id)">详情</el-button>
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

    <el-drawer v-model="detailVisible" title="域名操作记录详情" size="980px" :append-to-body="true">
      <div v-loading="detailLoading" class="history-detail">
        <div v-if="detailData" style="margin-bottom: 20px;">
          <el-descriptions :column="3" border size="small">
            <el-descriptions-item label="操作类型">
              <el-tag :type="tagTypeMap[detailData.operationType] || 'info'" size="small">
                {{ operationTypeText(detailData.operationType) }}
              </el-tag>
            </el-descriptions-item>
            <el-descriptions-item label="操作人">{{ detailData.operatorName || "-" }}</el-descriptions-item>
            <el-descriptions-item label="操作时间">{{ detailData.operationTime || "-" }}</el-descriptions-item>
            <el-descriptions-item label="操作摘要" :span="3">{{ detailData.operationSummary || "-" }}</el-descriptions-item>
          </el-descriptions>
        </div>
        
        <domain-record-history-diff v-if="detailData" ref="diffRef" />
      </div>
    </el-drawer>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import { nextTick, reactive, ref, toRefs } from "vue";
import baseService from "@/service/baseService";
import DomainRecordHistoryDiff from "./domain-record-history-diff.vue";

const view = reactive({
  getDataListURL: "/ops/domain-record/history/page",
  getDataListIsPage: true,
  dataForm: {
    operationType: "",
    operatorName: "",
    operationTimeStart: "",
    operationTimeEnd: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const timeRange = ref<string[]>([]);
const detailVisible = ref(false);
const detailLoading = ref(false);
const detailData = ref<any>(null);
const diffRef = ref();

const tagTypeMap: Record<string, string> = {
  CREATE: "success",
  UPDATE: "warning",
  DELETE: "danger"
};

const operationTypeText = (value: string) => {
  if (value === "CREATE") return "新增";
  if (value === "UPDATE") return "修改";
  if (value === "DELETE") return "删除";
  return value || "-";
};



const queryList = () => {
  state.dataForm.operationTimeStart = timeRange.value?.[0] || "";
  state.dataForm.operationTimeEnd = timeRange.value?.[1] || "";
  state.getDataList();
};

const handleReset = () => {
  state.dataForm.operationType = "";
  state.dataForm.operatorName = "";
  state.dataForm.operationTimeStart = "";
  state.dataForm.operationTimeEnd = "";
  timeRange.value = [];
  queryList();
};

const loadDetail = (id: number) => {
  detailVisible.value = true;
  detailLoading.value = true;
  detailData.value = null;
  baseService
    .get(`/ops/domain-record/history/${id}`)
    .then((res) => {
      detailData.value = res.data;
      nextTick(() => {
        if (diffRef.value) {
          diffRef.value.init(res.data);
        }
      });
    })
    .finally(() => {
      detailLoading.value = false;
    });
};

queryList();
</script>

<style scoped>
.history-detail {
  margin-top: 4px;
}
</style>
