<template>
  <el-form label-width="120px" class="domain-form readonly-diff-form">
    <el-tabs v-model="activeTab" class="domain-tabs" type="border-card">
      <el-tab-pane label="基础设置" name="base">
        <el-row :gutter="16" class="tab-pane-content">
          <el-col :span="12"><diff-item label="项目名称" field="projectName" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="域名" field="domainName" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="区域名称" field="areaName" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="分组名称" field="groupName" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="项目负责人" field="projectOwner" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="申请时间" field="applyTime" :b="b" :a="a" /></el-col>
          <el-col :span="12"><diff-item label="描述" field="description" :b="b" :a="a" /></el-col>
          <el-col :span="24"><diff-item label="备注" field="remark" :b="b" :a="a" /></el-col>
        </el-row>
      </el-tab-pane>

      <el-tab-pane label="深信服(AD)配置" name="delivery">
        <el-row class="tab-pane-content">
          <el-col :span="24" style="margin-bottom: 24px;">
            <diff-item label="开启深信服(AD)" field="adEnabled" :b="b" :a="a" type="bool" />
          </el-col>
        </el-row>
        <template v-if="(b.adEnabled === 1) || (a.adEnabled === 1)">
          <el-row :gutter="16">
            <el-col :span="12"><diff-item label="虚拟服务名称" field="delivery.virtualServiceName" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="虚拟服务IP" field="delivery.virtualServiceIp" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="虚拟服务端口" field="delivery.virtualServicePort" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="虚拟服务协议" field="delivery.virtualServiceProtocol" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="节点池名称" field="delivery.poolName" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="应用交付备注" field="delivery.remark" :b="b" :a="a" /></el-col>
          </el-row>

          <div class="node-toolbar">
            <span class="node-toolbar__title">节点池明细对比</span>
          </div>

          <div v-if="nodesChanged" class="diff-nodes-container">
            <div v-if="b.delivery?.nodes?.length" class="diff-nodes-old">
              <div class="nodes-badge nodes-badge--danger">变更前</div>
              <el-table :data="b.delivery.nodes" border class="table-danger">
                <el-table-column prop="nodeIp" label="节点IP" min-width="180"></el-table-column>
                <el-table-column prop="nodePort" label="节点端口" width="140"></el-table-column>
                <el-table-column prop="sort" label="排序" width="120"></el-table-column>
                <el-table-column prop="remark" label="备注" min-width="180"></el-table-column>
              </el-table>
            </div>
            <div v-if="a.delivery?.nodes?.length" class="diff-nodes-new">
              <div class="nodes-badge nodes-badge--success">变更后</div>
              <el-table :data="a.delivery.nodes" border class="table-success">
                <el-table-column prop="nodeIp" label="节点IP" min-width="180"></el-table-column>
                <el-table-column prop="nodePort" label="节点端口" width="140"></el-table-column>
                <el-table-column prop="sort" label="排序" width="120"></el-table-column>
                <el-table-column prop="remark" label="备注" min-width="180"></el-table-column>
              </el-table>
            </div>
          </div>
          <div v-else>
            <el-table :data="a.delivery?.nodes || []" border>
              <el-table-column prop="nodeIp" label="节点IP" min-width="180"></el-table-column>
              <el-table-column prop="nodePort" label="节点端口" width="140"></el-table-column>
              <el-table-column prop="sort" label="排序" width="120"></el-table-column>
              <el-table-column prop="remark" label="备注" min-width="180"></el-table-column>
            </el-table>
          </div>
        </template>
      </el-tab-pane>

      <el-tab-pane label="解析配置" name="dns">
        <el-row class="tab-pane-content" :gutter="16">
          <el-col :span="12" style="margin-bottom: 24px;">
            <diff-item label="开启内网解析" field="internalEnabled" :b="b" :a="a" type="bool" />
          </el-col>
          <el-col :span="12" style="margin-bottom: 24px;">
            <diff-item label="开启公网解析" field="externalEnabled" :b="b" :a="a" type="bool" />
          </el-col>
        </el-row>

        <el-row :gutter="16">
          <el-col :span="12" v-if="(b.internalEnabled === 1) || (a.internalEnabled === 1)">
            <div class="sub-section-title">内网解析</div>
            <diff-item :label="getAdLabel(b, a) ? '虚拟服务IP' : '目标IP'" field="dnsInternal.dnsTargetIp" :b="b" :a="a" />
            <diff-item label="备注" field="dnsInternal.remark" :b="b" :a="a" />
          </el-col>
          <el-col :span="12" v-if="(b.externalEnabled === 1) || (a.externalEnabled === 1)">
            <div class="sub-section-title">公网解析</div>
            <diff-item :label="getAdLabel(b, a) ? '公网IP' : '目标公网IP'" field="dnsExternal.recordValue" :b="b" :a="a" />
            <diff-item label="备注" field="dnsExternal.remark" :b="b" :a="a" />
          </el-col>
        </el-row>
      </el-tab-pane>

      <el-tab-pane label="防火墙 NAT 映射" name="firewall">
        <template v-if="((b.adEnabled === 1 && b.externalEnabled === 1) || (a.adEnabled === 1 && a.externalEnabled === 1))">
          <el-row :gutter="16" class="tab-pane-content">
            <el-col :span="12"><diff-item label="公网IP" field="firewallMapping.publicIp" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="外部端口" field="firewallMapping.externalPort" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="内部IP" field="firewallMapping.internalIp" :b="b" :a="a" /></el-col>
            <el-col :span="12"><diff-item label="内部端口" field="firewallMapping.internalPort" :b="b" :a="a" /></el-col>
            <el-col :span="24"><diff-item label="映射描述" field="firewallMapping.mappingDesc" :b="b" :a="a" /></el-col>
          </el-row>
        </template>
        <el-empty v-else description="未开启防火墙NAT映射相关配置"></el-empty>
      </el-tab-pane>
    </el-tabs>
  </el-form>
</template>

<script lang="ts" setup>
import { computed, ref } from 'vue';
import DiffItem from "./domain-record-history-diff-item.vue";

const activeTab = ref("base");
const detailData = ref<any>(null);

const b = computed(() => {
  if (!detailData.value || !detailData.value.snapshotBefore) return {};
  try {
    const parsed = JSON.parse(detailData.value.snapshotBefore);
    if (parsed && typeof parsed.base === 'object') {
      Object.assign(parsed, parsed.base);
    }
    return parsed;
  } catch (e) {
    return {};
  }
});

const a = computed(() => {
  if (!detailData.value || !detailData.value.snapshotAfter) return {};
  try {
    const parsed = JSON.parse(detailData.value.snapshotAfter);
    if (parsed && typeof parsed.base === 'object') {
      Object.assign(parsed, parsed.base);
    }
    return parsed;
  } catch (e) {
    return {};
  }
});

const nodesChanged = computed(() => {
  const bNodes = b.value.delivery?.nodes || [];
  const aNodes = a.value.delivery?.nodes || [];
  return JSON.stringify(bNodes) !== JSON.stringify(aNodes);
});

const getAdLabel = (before: any, after: any) => {
  return (before.adEnabled === 1 || after.adEnabled === 1);
};

const init = (data: any) => {
  detailData.value = data;
  activeTab.value = "base";
  
  if (data.details && data.details.length > 0) {
    const firstField = data.details[0].fieldCode || '';
    if (firstField.startsWith('delivery.')) activeTab.value = 'delivery';
    else if (firstField.startsWith('dnsInternal.') || firstField.startsWith('dnsExternal.')) activeTab.value = 'dns';
    else if (firstField.startsWith('firewallMapping.')) activeTab.value = 'firewall';
    else if (firstField === 'adEnabled' || firstField === 'internalEnabled' || firstField === 'externalEnabled') {
      // stay on base or move to matching tab, base is okay.
      activeTab.value = "base";
    }
  }
};

defineExpose({ init });
</script>

<style scoped>
.domain-form {
  padding: 4px;
}

.domain-tabs {
  min-height: 480px;
  box-shadow: 0 1px 3px rgba(0,0,0,0.02);
  border: 1px solid #e4e7ed;
  border-radius: 6px;
}
.domain-tabs :deep(.el-tabs__content) {
  padding: 24px;
  max-height: 60vh;
  overflow-y: auto;
}
.tab-pane-content {
  margin-top: 8px;
}

.sub-section-title {
  margin-bottom: 12px;
  font-size: 14px;
  font-weight: 600;
  color: #475569;
}

.node-toolbar {
  margin-bottom: 12px;
  margin-top: 16px;
}

.node-toolbar__title {
  font-size: 14px;
  font-weight: 600;
}

.diff-nodes-container {
  display: flex;
  flex-direction: column;
  gap: 16px;
}

.nodes-badge {
  display: inline-block;
  padding: 4px 12px;
  font-size: 12px;
  font-weight: bold;
  border-radius: 4px 4px 0 0;
  width: max-content;
}

.nodes-badge--danger {
  background-color: #fecaca;
  color: #b91c1c;
}

.nodes-badge--success {
  background-color: #bbf7d0;
  color: #15803d;
}

.table-danger {
  border-top: 2px solid #ef4444;
}

.table-success {
  border-top: 2px solid #10b981;
}

.readonly-diff-form :deep(.el-form-item) {
  margin-bottom: 20px;
}
</style>
