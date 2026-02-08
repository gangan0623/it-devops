<template>
  <div class="mod-alert__trigger">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="queryList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.name" class="query-input" placeholder="触发器名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.status" placeholder="状态" clearable>
              <el-option label="启用" :value="1"></el-option>
              <el-option label="禁用" :value="0"></el-option>
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
          <el-radio-group v-model="severityQuickFilter" size="small">
            <el-radio-button label="">全部级别</el-radio-button>
            <el-radio-button label="critical">灾难</el-radio-button>
            <el-radio-button label="warning">重要</el-radio-button>
            <el-radio-button label="info">信息</el-radio-button>
          </el-radio-group>
          <div class="trigger-stats">
            <span class="trigger-stats__item trigger-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="trigger-stats__item trigger-stats__item--critical">含灾难 {{ criticalCount }}</span>
            <span class="trigger-stats__item trigger-stats__item--filter">显示 {{ filteredCount }}</span>
          </div>
          <el-button type="warning" plain :loading="resourceLoading" @click="loadResources">刷新资源</el-button>
          <el-button v-if="state.hasPermission('alert:trigger:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('alert:trigger:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="filteredDataList" border @selection-change="state.dataListSelectionChangeHandle" class="trigger-table" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="name" label="触发器名称" header-align="center" align="center" min-width="180" show-overflow-tooltip></el-table-column>
      <el-table-column prop="templateId" label="模板" header-align="center" align="center" min-width="160" show-overflow-tooltip>
        <template v-slot="scope">{{ templateMap[scope.row.templateId] || scope.row.templateId }}</template>
      </el-table-column>
      <el-table-column prop="mediaId" label="媒介" header-align="center" align="center" min-width="150" show-overflow-tooltip>
        <template v-slot="scope">{{ mediaMap[scope.row.mediaId] || scope.row.mediaId }}</template>
      </el-table-column>
      <el-table-column prop="severity" label="告警级别" header-align="center" align="center" min-width="180" show-overflow-tooltip>
        <template v-slot="scope">
          <el-tag size="small" type="warning" effect="plain">{{ formatSeverity(scope.row.severity) }}</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="status" label="状态" header-align="center" align="center" width="90">
        <template v-slot="scope">
          <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
          <el-tag v-else size="small" type="success">启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="180">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('alert:trigger:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('alert:trigger:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
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

    <add-or-update ref="addOrUpdateRef" @refreshDataList="refresh"></add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, onMounted, reactive, ref, toRefs} from "vue";
import baseService from "@/service/baseService";
import AddOrUpdate from "./trigger-add-or-update.vue";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/alert/trigger/page",
  getDataListIsPage: true,
  deleteURL: "/alert/trigger",
  dataForm: {
    name: "",
    status: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const addOrUpdateRef = ref();
const resourceLoading = ref(false);
const severityQuickFilter = ref("");
const severityLabelMap: Record<string, string> = {
  critical: "灾难告警",
  warning: "重要告警",
  info: "信息提示",
  recover: "告警恢复"
};

const formatSeverity = (value?: string) => {
  if (!value) {
    return "-";
  }
  return value
    .split(",")
    .map((item) => {
      const key = item.trim().toLowerCase();
      return severityLabelMap[key] || item.trim();
    })
    .filter(Boolean)
    .join("、");
};
const templateMap = reactive({} as Record<string, string>);
const mediaMap = reactive({} as Record<string, string>);
const enabledCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.status) === 1).length);
const criticalCount = computed(() =>
  (state.dataList || []).filter((item: any) => String(item?.severity || "").toLowerCase().includes("critical")).length
);
const filteredDataList = computed(() => {
  if (!severityQuickFilter.value) {
    return state.dataList || [];
  }
  const target = severityQuickFilter.value.toLowerCase();
  return (state.dataList || []).filter((item: any) => String(item?.severity || "").toLowerCase().includes(target));
});
const filteredCount = computed(() => filteredDataList.value.length);

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const queryList = () => {
  state.getDataList();
};

const handleReset = () => {
  state.dataForm.name = "";
  state.dataForm.status = "";
  severityQuickFilter.value = "";
  queryList();
};

const loadResources = () => {
  resourceLoading.value = true;
  baseService
    .get("/alert/trigger/resources")
    .then((res) => {
      Object.keys(templateMap).forEach((key) => delete templateMap[key]);
      Object.keys(mediaMap).forEach((key) => delete mediaMap[key]);
      (res.data.templates || []).forEach((item: any) => {
        templateMap[item.id] = item.name;
      });
      (res.data.medias || []).forEach((item: any) => {
        mediaMap[item.id] = item.name;
      });
    })
    .finally(() => {
      resourceLoading.value = false;
    });
};

const refresh = () => {
  queryList();
  loadResources();
};

onMounted(() => {
  loadResources();
});
</script>

<style lang="less" scoped>
/* 统计标签容器 */
.trigger-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

/* 统计标签基础样式 */
.trigger-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}
</style>
