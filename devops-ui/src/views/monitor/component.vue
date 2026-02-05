<template>
  <div class="mod-monitor__component">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="state.getDataList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.name" placeholder="名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.ip" placeholder="IP" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <ren-select v-model="state.dataForm.type" dict-type="monitor_component_type" placeholder="类型"></ren-select>
          </el-form-item>
          <el-form-item>
            <el-button @click="state.getDataList()">查询</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="component-stats">
            <span class="component-stats__item component-stats__item--online">在线 {{ onlineCount }}</span>
            <span class="component-stats__item component-stats__item--offline">离线 {{ offlineCount }}</span>
            <span class="component-stats__item component-stats__item--upgrade">可更新 {{ updatableCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('ops:monitorcomponent:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:monitorcomponent:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" class="ops-table-nowrap" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="name" label="名称" header-align="center" align="center" min-width="160"></el-table-column>
      <el-table-column prop="type" label="类型" header-align="center" align="center" width="140">
        <template v-slot="scope">
          <span>{{ typeLabels[scope.row.type] || scope.row.type }}</span>
        </template>
      </el-table-column>
      <el-table-column prop="ip" label="IP" header-align="center" align="center" min-width="140"></el-table-column>
      <el-table-column prop="port" label="端口" header-align="center" align="center" width="90"></el-table-column>
      <el-table-column prop="webUrl" label="Web地址" header-align="center" align="center" min-width="220"></el-table-column>
      <el-table-column prop="onlineStatus" label="在线" header-align="center" align="center" width="80">
        <template v-slot="scope">
          <el-tag v-if="scope.row.onlineStatus === 1" size="small" type="success">在线</el-tag>
          <el-tag v-else-if="scope.row.onlineStatus === 0" size="small" type="danger">不在线</el-tag>
          <el-tag v-else size="small" type="info">未检测</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="version" label="当前版本" header-align="center" align="center" min-width="120"></el-table-column>
      <el-table-column prop="latestVersion" label="最新版本" header-align="center" align="center" min-width="120"></el-table-column>
      <el-table-column prop="updateAvailable" label="可更新" header-align="center" align="center" width="90">
        <template v-slot="scope">
          <el-tag v-if="scope.row.updateAvailable === 1" size="small" type="warning">是</el-tag>
          <el-tag v-else-if="scope.row.updateAvailable === 0" size="small" type="success">否</el-tag>
          <el-tag v-else size="small" type="info">未检测</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="lastCheckTime" label="检测时间" header-align="center" align="center" min-width="160"></el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="260">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:monitorcomponent:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button type="primary" link @click="openLink(scope.row)">打开链接</el-button>
          <el-button v-if="state.hasPermission('ops:monitorcomponent:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>

    <el-pagination :current-page="state.page" :page-sizes="[10, 20, 50, 100]" :page-size="state.limit" :total="state.total" layout="total, sizes, prev, pager, next, jumper" @size-change="state.pageSizeChangeHandle" @current-change="state.pageCurrentChangeHandle"> </el-pagination>

    <add-or-update ref="addOrUpdateRef" @refreshDataList="state.getDataList"></add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, reactive, ref, toRefs, watch} from "vue";
import baseService from "@/service/baseService";
import {ElMessage} from "element-plus";
import AddOrUpdate from "./component-add-or-update.vue";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/monitorcomponent/page",
  getDataListIsPage: true,
  deleteURL: "/ops/monitorcomponent",
  dataForm: {
    name: "",
    ip: "",
    type: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const onlineCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.onlineStatus) === 1).length);
const offlineCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.onlineStatus) === 0).length);
const updatableCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.updateAvailable) === 1).length);
const addOrUpdateRef = ref();

const typeLabels: Record<string, string> = {
  prometheus: "Prometheus",
  vmalert: "vmalert",
  alertmanager: "Alertmanager",
  victoriametrics: "VictoriaMetrics",
  blackbox: "blackbox"
};

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const openLink = (row: any) => {
  let url = row.webUrl;
  if (!url) {
    const host = row.ip || "";
    const port = row.port ? `:${row.port}` : "";
    url = host ? `http://${host}${port}` : "";
  }
  if (!url) {
    ElMessage.warning("未配置Web地址");
    return;
  }
  window.open(url, "_blank");
};

const autoDetect = () => {
  if (!state.dataList || state.dataList.length === 0) {
    return;
  }
  state.dataList.forEach((row: { id?: number }) => {
    if (!row.id) {
      return;
    }
    baseService.get("/ops/monitorcomponent/probe", { id: row.id });
    baseService.get("/ops/monitorcomponent/version", { id: row.id });
  });
};

watch(
  () => state.dataList,
  () => {
    autoDetect();
  }
);
</script>

<style lang="less" scoped>
.ops-table-nowrap :deep(.cell) {
  white-space: nowrap;
}
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
.component-stats {
  display: flex;
  align-items: center;
  gap: 6px;
}
.component-stats__item {
  padding: 2px 8px;
  border-radius: 999px;
  font-size: 12px;
}
.component-stats__item--online {
  color: #065f46;
  background: #d1fae5;
}
.component-stats__item--offline {
  color: #991b1b;
  background: #fee2e2;
}
.component-stats__item--upgrade {
  color: #9a3412;
  background: #ffedd5;
}
.ops-filters .el-form-item {
  margin-bottom: 0;
}
.ops-table-nowrap :deep(.el-table__row:hover > td) {
  background: #f8fafc;
}
</style>
