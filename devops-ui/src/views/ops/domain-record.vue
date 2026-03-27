<template>
  <div class="mod-ops__domain-record">
    <div class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-input v-model="state.dataForm.domainName" class="query-input" placeholder="域名(模糊)" clearable @keyup.enter="queryList()" />
          <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          <el-button class="query-btn" @click="handleReset">重置</el-button>
          <el-button :icon="Filter" @click="filterDrawer = true">
            筛选<span v-if="activeFilterCount > 0" class="filter-badge">{{ activeFilterCount }}</span>
          </el-button>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="host-stats">
            <span class="host-stats__item host-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="host-stats__item host-stats__item--off">禁用 {{ disabledCount }}</span>
            <span class="host-stats__item host-stats__item--online">在线 {{ onlineCount }}</span>
            <span class="host-stats__item host-stats__item--filter">离线 {{ offlineCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('ops:domain-record:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('ops:domain-record:delete')" type="danger" @click="deleteBatchHandle">删除</el-button>
        </div>
      </div>
    </div>

    <el-drawer v-model="filterDrawer" title="高级筛选" size="360px" :append-to-body="true">
      <el-form label-position="top" class="filter-form">
        <el-form-item label="项目名称">
          <el-input v-model="state.dataForm.projectName" placeholder="请输入项目名称" clearable></el-input>
        </el-form-item>
        <el-form-item label="项目负责人">
          <el-input v-model="state.dataForm.projectOwner" placeholder="请输入负责人姓名" clearable></el-input>
        </el-form-item>
        <el-form-item label="区域名称">
          <ren-select
            v-model="state.dataForm.areaName"
            dict-type="area_name_type"
            label-field="dictValue"
            value-field="dictLabel"
            placeholder="全部"
            style="width: 100%"
          ></ren-select>
        </el-form-item>
        <el-form-item label="分组名称">
          <ren-select
            v-model="state.dataForm.groupName"
            dict-type="server_host_group"
            label-field="dictValue"
            value-field="dictLabel"
            placeholder="全部"
            style="width: 100%"
          ></ren-select>
        </el-form-item>
        <el-form-item label="站点位置">
          <ren-select
            v-model="state.dataForm.siteLocation"
            dict-type="base_site_location"
            label-field="dictValue"
            value-field="dictLabel"
            placeholder="全部"
            style="width: 100%"
          ></ren-select>
        </el-form-item>
        <el-form-item label="应用交付(AD)">
          <el-select v-model="state.dataForm.adEnabled" placeholder="全部" clearable style="width: 100%">
            <el-option label="走AD" :value="1"></el-option>
            <el-option label="不走AD" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="状态">
          <el-select v-model="state.dataForm.status" placeholder="全部" clearable style="width: 100%">
            <el-option label="启用" :value="1"></el-option>
            <el-option label="禁用" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="在线状态">
          <el-select v-model="state.dataForm.onlineStatus" placeholder="全部" clearable style="width: 100%">
            <el-option label="在线" :value="1"></el-option>
            <el-option label="不在线" :value="0"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="公网解析状态">
          <el-select v-model="state.dataForm.externalEnabled" placeholder="全部" clearable style="width: 100%">
            <el-option label="启用" :value="1"></el-option>
            <el-option label="未启用" :value="0"></el-option>
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="resetFilters">重置</el-button>
        <el-button type="primary" @click="confirmFilters">确定</el-button>
      </template>
    </el-drawer>

    <el-table
      v-loading="state.dataListLoading"
      :data="state.dataList"
      border
      class="ops-table-nowrap"
      style="width: 100%"
      @selection-change="state.dataListSelectionChangeHandle"
    >
      <el-table-column type="selection" width="50" header-align="center" align="center"></el-table-column>
      <el-table-column prop="projectName" label="项目名称" min-width="140" header-align="center" align="center" show-overflow-tooltip></el-table-column>
      <el-table-column prop="domainName" label="域名" min-width="180" header-align="center" align="center" show-overflow-tooltip></el-table-column>
      <el-table-column label="区域名称" min-width="120" header-align="center" align="center">
        <template #default="{ row }">
          {{ state.getDictValueByLabel("area_name_type", row.areaName) }}
        </template>
      </el-table-column>
      <el-table-column label="分组名称" min-width="120" header-align="center" align="center">
        <template #default="{ row }">
          {{ state.getDictValueByLabel("server_host_group", row.groupName) }}
        </template>
      </el-table-column>
      <el-table-column label="站点位置" min-width="120" header-align="center" align="center">
        <template #default="{ row }">
          {{ state.getDictValueByLabel("base_site_location", row.siteLocation) }}
        </template>
      </el-table-column>
      <el-table-column label="状态" width="90" header-align="center" align="center">
        <template #default="{ row }">
          <el-tag v-if="row.status === 0" size="small" type="danger">禁用</el-tag>
          <el-tag v-else size="small" type="success">启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="在线状态" width="90" header-align="center" align="center">
        <template #default="{ row }">
          <el-tag v-if="row.onlineStatus === true" size="small" type="success">在线</el-tag>
          <el-tag v-else-if="row.onlineStatus === false" size="small" type="danger">离线</el-tag>
          <el-tag v-else size="small" type="info">检测中</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="应用交付(AD)" width="120" header-align="center" align="center">
        <template #default="{ row }">
          <el-popover v-if="row.adEnabled === 1" placement="right" :width="280" trigger="hover">
            <template #reference>
              <el-tag type="success" size="small" style="cursor: pointer">走AD</el-tag>
            </template>
            <el-descriptions :column="1" size="small" border>
              <el-descriptions-item label="名称">{{ row.virtualServiceName || '-' }}</el-descriptions-item>
              <el-descriptions-item label="虚拟IP">{{ row.virtualServiceIp || '-' }}</el-descriptions-item>
              <el-descriptions-item label="端口">{{ row.virtualServicePort || '-' }}</el-descriptions-item>
              <el-descriptions-item label="协议">{{ row.virtualServiceProtocol || '-' }}</el-descriptions-item>
              <el-descriptions-item label="节点池">{{ row.poolName || '-' }}</el-descriptions-item>
              <el-descriptions-item label="包含节点">
                <div style="max-height: 80px; overflow-y: auto; white-space: pre-wrap; line-height: 1.4; color: #409eff;">{{ row.poolNodes || '-' }}</div>
              </el-descriptions-item>
            </el-descriptions>
          </el-popover>
          <el-tag v-else type="info" size="small">不走AD</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="内网解析" width="110" header-align="center" align="center">
        <template #default="{ row }">
          <el-popover v-if="row.internalEnabled === 1" placement="right" :width="240" trigger="hover">
            <template #reference>
              <el-tag type="success" size="small" style="cursor: pointer">已启用</el-tag>
            </template>
            <el-descriptions :column="1" size="small" border>
              <el-descriptions-item label="目标IP">{{ row.internalTargetIp || '-' }}</el-descriptions-item>
            </el-descriptions>
          </el-popover>
          <el-tag v-else type="info" size="small">未启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="公网解析" width="110" header-align="center" align="center">
        <template #default="{ row }">
          <el-popover v-if="row.externalEnabled === 1" placement="right" :width="row.fmPublicIp ? 300 : 240" trigger="hover">
            <template #reference>
              <el-tag type="success" size="small" style="cursor: pointer">已启用</el-tag>
            </template>
            <el-descriptions :column="1" size="small" border>
              <el-descriptions-item label="公网IP">{{ row.externalRecordValue || '-' }}</el-descriptions-item>
              <template v-if="row.fmPublicIp || row.fmInternalIp">
                <el-descriptions-item label="NAT公网IP">
                  <span style="color: #67c23a; font-weight: bold">{{ row.fmPublicIp || '-' }}</span>
                </el-descriptions-item>
                <el-descriptions-item label="NAT外部端口">
                  <span style="color: #67c23a; font-weight: bold">{{ row.fmExternalPort || '-' }}</span>
                </el-descriptions-item>
                <el-descriptions-item label="映射内网IP">
                  <span style="color: #409eff; font-weight: bold">{{ row.fmInternalIp || '-' }}</span>
                </el-descriptions-item>
                <el-descriptions-item label="映射内网端口">
                  <span style="color: #409eff; font-weight: bold">{{ row.fmInternalPort || '-' }}</span>
                </el-descriptions-item>
              </template>
            </el-descriptions>
          </el-popover>
          <el-tag v-else type="info" size="small">未启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="apiUrl" label="访问地址" min-width="220" header-align="center" align="center" show-overflow-tooltip></el-table-column>
      <el-table-column prop="projectOwner" label="项目负责人" width="120" header-align="center" align="center"></el-table-column>
      <el-table-column prop="applyTime" label="申请时间" min-width="170" header-align="center" align="center"></el-table-column>
      <el-table-column label="操作" fixed="right" width="280" header-align="center" align="center">
        <template #default="{ row }">
          <el-button type="primary" link @click="openDetail(row.id)">详情</el-button>
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
        <el-descriptions :column="3" border size="small" style="margin-bottom: 20px;">
          <el-descriptions-item label="操作类型">
            <el-tag size="small">{{ historyDetail.operationType }}</el-tag>
          </el-descriptions-item>
          <el-descriptions-item label="操作人">{{ historyDetail.operatorName }}</el-descriptions-item>
          <el-descriptions-item label="操作时间">{{ historyDetail.operationTime }}</el-descriptions-item>
          <el-descriptions-item label="操作摘要" :span="3">{{ historyDetail.operationSummary }}</el-descriptions-item>
        </el-descriptions>

        <domain-record-history-diff ref="diffRef" />
      </div>
    </el-drawer>

    <el-drawer v-model="detailVisible" title="域名记录详情" size="980px" :append-to-body="true">
      <div v-loading="detailLoading">
        <template v-if="detailData">
          <div class="group-panel">
            <div class="group-panel__header">基础信息</div>
          <el-descriptions :column="2" border>
            <el-descriptions-item label="项目名称">{{ detailData.projectName || "-" }}</el-descriptions-item>
            <el-descriptions-item label="域名">{{ detailData.domainName || "-" }}</el-descriptions-item>
            <el-descriptions-item label="区域名称">
              {{ state.getDictValueByLabel("area_name_type", detailData.areaName) || "-" }}
            </el-descriptions-item>
            <el-descriptions-item label="分组名称">
              {{ state.getDictValueByLabel("server_host_group", detailData.groupName) || "-" }}
            </el-descriptions-item>
            <el-descriptions-item label="站点位置">
              {{ state.getDictValueByLabel("base_site_location", detailData.siteLocation) || "-" }}
            </el-descriptions-item>
            <el-descriptions-item label="状态">
              <el-tag v-if="detailData.status === 0" size="small" type="danger">禁用</el-tag>
              <el-tag v-else size="small" type="success">启用</el-tag>
            </el-descriptions-item>
            <el-descriptions-item label="在线状态">
              <el-tag v-if="detailData.onlineStatus === true" size="small" type="success">在线</el-tag>
              <el-tag v-else-if="detailData.onlineStatus === false" size="small" type="danger">不在线</el-tag>
              <el-tag v-else size="small" type="info">检测中</el-tag>
            </el-descriptions-item>
            <el-descriptions-item label="项目负责人">{{ detailData.projectOwner || "-" }}</el-descriptions-item>
            <el-descriptions-item label="申请时间">{{ detailData.applyTime || "-" }}</el-descriptions-item>
            <el-descriptions-item label="公网解析状态">{{ detailData.externalEnabled === 1 ? "启用" : "未启用" }}</el-descriptions-item>
            <el-descriptions-item label="是否走AD">{{ detailData.adEnabled === 1 ? "走AD" : "不走AD" }}</el-descriptions-item>
            <el-descriptions-item label="访问地址" :span="2">{{ detailData.apiUrl || "-" }}</el-descriptions-item>
            <el-descriptions-item label="描述" :span="2">{{ detailData.description || "-" }}</el-descriptions-item>
            <el-descriptions-item label="备注" :span="2">{{ detailData.remark || "-" }}</el-descriptions-item>
          </el-descriptions>
          </div>

          <div class="group-panel">
            <div class="group-panel__header">应用交付</div>
            <template v-if="detailData.adEnabled === 1 && detailData.delivery">
              <el-descriptions :column="2" border>
                <el-descriptions-item label="虚拟服务名称">{{ detailData.delivery.virtualServiceName || "-" }}</el-descriptions-item>
                <el-descriptions-item label="虚拟服务IP">{{ detailData.delivery.virtualServiceIp || "-" }}</el-descriptions-item>
                <el-descriptions-item label="虚拟服务端口">{{ detailData.delivery.virtualServicePort || "-" }}</el-descriptions-item>
                <el-descriptions-item label="虚拟服务协议">{{ detailData.delivery.virtualServiceProtocol || "-" }}</el-descriptions-item>
                <el-descriptions-item label="节点池名称">{{ detailData.delivery.poolName || "-" }}</el-descriptions-item>
                <el-descriptions-item label="备注" :span="2">{{ detailData.delivery.remark || "-" }}</el-descriptions-item>
              </el-descriptions>

              <div class="detail-subtitle">节点池明细</div>
              <el-table :data="detailData.delivery.nodes || []" border style="width: 100%">
                <el-table-column prop="nodeIp" label="节点IP" min-width="180"></el-table-column>
                <el-table-column prop="nodePort" label="节点端口" width="140"></el-table-column>
                <el-table-column prop="sort" label="排序" width="100"></el-table-column>
                <el-table-column prop="remark" label="备注" min-width="180" show-overflow-tooltip></el-table-column>
              </el-table>
            </template>
            <el-empty v-else description="不走AD，无应用交付链路"></el-empty>
          </div>

          <div class="group-panel">
            <div class="group-panel__header">解析配置</div>
          <el-row :gutter="16">
            <el-col :span="12" v-if="detailData.internalEnabled === 1">
              <div class="detail-subtitle">内网解析</div>
              <el-descriptions :column="1" border>
                <el-descriptions-item label="解析链路">{{ formatInternalChain(detailData) }}</el-descriptions-item>
                <el-descriptions-item :label="detailData.adEnabled === 1 ? '虚拟服务IP' : '目标IP'">
                  {{ detailData.dnsInternal?.dnsTargetIp || "-" }}
                </el-descriptions-item>
                <el-descriptions-item label="备注">{{ detailData.dnsInternal?.remark || "-" }}</el-descriptions-item>
              </el-descriptions>
            </el-col>
            <el-col :span="12" v-if="detailData.externalEnabled === 1">
              <div class="detail-subtitle">公网解析</div>
              <el-descriptions :column="1" border>
                <el-descriptions-item label="解析链路">{{ formatExternalChain(detailData) }}</el-descriptions-item>
                <el-descriptions-item :label="detailData.adEnabled === 1 ? '公网IP' : '目标公网IP'">
                  {{ detailData.dnsExternal?.recordValue || "-" }}
                </el-descriptions-item>
                <el-descriptions-item label="备注">{{ detailData.dnsExternal?.remark || "-" }}</el-descriptions-item>
              </el-descriptions>
            </el-col>
          </el-row>

          </div>

          <template v-if="detailData.adEnabled === 1 && detailData.externalEnabled === 1 && detailData.firewallMapping">
            <div class="group-panel">
              <div class="group-panel__header">防火墙映射</div>
            <el-descriptions :column="2" border>
              <el-descriptions-item label="公网IP">{{ detailData.firewallMapping.publicIp || "-" }}</el-descriptions-item>
              <el-descriptions-item label="外部端口">{{ detailData.firewallMapping.externalPort || "-" }}</el-descriptions-item>
              <el-descriptions-item label="内部IP">{{ detailData.firewallMapping.internalIp || "-" }}</el-descriptions-item>
              <el-descriptions-item label="内部端口">{{ detailData.firewallMapping.internalPort || "-" }}</el-descriptions-item>
              <el-descriptions-item label="映射描述" :span="2">{{ detailData.firewallMapping.mappingDesc || "-" }}</el-descriptions-item>
            </el-descriptions>
            </div>
          </template>
        </template>
      </div>
    </el-drawer>

    <add-or-update ref="addOrUpdateRef" @refreshDataList="queryList"></add-or-update>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import { reactive, ref, toRefs, computed, nextTick, onMounted } from "vue";
import { Filter } from "@element-plus/icons-vue";
import baseService from "@/service/baseService";
import { ElMessage, ElMessageBox } from "element-plus";
import AddOrUpdate from "./domain-record-add-or-update.vue";
import DomainRecordHistoryDiff from "./domain-record-history-diff.vue";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/domain-record/page",
  getDataListIsPage: true,
  dataForm: {
    projectName: "",
    domainName: "",
    areaName: "",
    groupName: "",
    siteLocation: "",
    projectOwner: "",
    adEnabled: "" as string | number,
    externalEnabled: "" as string | number,
    status: "" as string | number,
    onlineStatus: "" as string | number
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const addOrUpdateRef = ref();
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
const detailVisible = ref(false);
const detailLoading = ref(false);
const detailData = ref<any>(null);
const historyVisible = ref(false);
const historyLoading = ref(false);
const historyList = ref<any[]>([]);
const historyDetail = ref<any>(null);
const diffRef = ref();

const filterDrawer = ref(false);

const activeFilterCount = computed(() => {
  let count = 0;
  if (state.dataForm.projectName) count++;
  if (state.dataForm.projectOwner) count++;
  if (state.dataForm.areaName) count++;
  if (state.dataForm.groupName) count++;
  if (state.dataForm.siteLocation) count++;
  if (state.dataForm.adEnabled !== "" && state.dataForm.adEnabled !== null && state.dataForm.adEnabled !== undefined) count++;
  if (state.dataForm.status !== "" && state.dataForm.status !== null && state.dataForm.status !== undefined) count++;
  if (state.dataForm.onlineStatus !== "" && state.dataForm.onlineStatus !== null && state.dataForm.onlineStatus !== undefined) count++;
  if (state.dataForm.externalEnabled !== "" && state.dataForm.externalEnabled !== null && state.dataForm.externalEnabled !== undefined) count++;
  return count;
});

const resetFilters = () => {
  state.dataForm.projectName = "";
  state.dataForm.projectOwner = "";
  state.dataForm.areaName = "";
  state.dataForm.groupName = "";
  state.dataForm.siteLocation = "";
  state.dataForm.adEnabled = "";
  state.dataForm.status = "";
  state.dataForm.onlineStatus = "";
  state.dataForm.externalEnabled = "";
};

const confirmFilters = () => {
  filterDrawer.value = false;
  queryList();
};

const queryList = () => {
  state.getDataList();
  loadStatusSummary();
};

const loadStatusSummary = () => {
  baseService
    .get("/ops/domain-record/summary")
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

const handleReset = () => {
  state.dataForm.domainName = "";
  resetFilters();
  queryList();
};

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const formatInternalChain = (row: any) => {
  if (row.adEnabled === 1) {
    return "DNS -> 深信服应用交付虚拟IP -> 节点池IP+端口";
  }
  return "DNS -> 目标IP";
};

const formatExternalChain = (row: any) => {
  if (row.adEnabled === 1) {
    return "阿里云DNS -> 公网IP -> 防火墙映射 -> 虚拟IP+内部端口 -> 节点池IP+端口";
  }
  return "DNS -> 目标公网IP";
};

const openDetail = (id: number) => {
  detailVisible.value = true;
  detailLoading.value = true;
  detailData.value = null;
  baseService
    .get(`/ops/domain-record/${id}`)
    .then((res) => {
      detailData.value = res.data;
    })
    .finally(() => {
      detailLoading.value = false;
    });
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
    nextTick(() => {
      if (diffRef.value) {
        diffRef.value.init(res.data);
      }
    });
  });
};

onMounted(() => {
  loadStatusSummary();
});

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

.group-panel {
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 6px;
  padding: 20px 24px;
  margin-bottom: 20px;
}
.group-panel__header {
  margin-bottom: 20px;
  font-size: 15px;
  font-weight: 600;
  color: #0f172a;
  display: flex;
  align-items: center;
}
.group-panel__header::before {
  content: "";
  display: inline-block;
  width: 4px;
  height: 16px;
  background-color: #3b82f6;
  border-radius: 2px;
  margin-right: 8px;
}
.group-panel :deep(.el-descriptions__body) {
  border-radius: 4px;
  overflow: hidden;
}

.detail-subtitle {
  margin: 16px 0 12px;
  font-size: 14px;
  font-weight: 600;
  color: #475569;
}
</style>
