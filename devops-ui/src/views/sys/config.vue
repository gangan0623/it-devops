<template>
  <div class="sys-config-container">
    <el-tabs v-model="activeTab" class="sys-config-tabs">
      <el-tab-pane label="存储" name="storage">
        <div class="tab-content-wrap">
          <config-storage v-if="activeTab === 'storage'" />
        </div>
      </el-tab-pane>
      <el-tab-pane label="Zabbix" name="zabbix">
        <div class="tab-content-wrap">
          <config-zabbix v-if="activeTab === 'zabbix'" />
        </div>
      </el-tab-pane>
      <el-tab-pane label="Zabbix映射" name="zabbixMapping">
        <div class="tab-content-wrap">
          <config-zabbix-mapping v-if="activeTab === 'zabbixMapping'" />
        </div>
      </el-tab-pane>
      <el-tab-pane label="AI配置" name="ai">
        <div class="tab-content-wrap">
          <config-ai v-if="activeTab === 'ai'" />
        </div>
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script lang="ts" setup>
import { ref } from "vue";
import ConfigStorage from "./config-storage.vue";
import ConfigZabbix from "./config-zabbix.vue";
import ConfigZabbixMapping from "./config-zabbix-mapping.vue";
import ConfigAi from "./config-ai.vue";

const activeTab = ref("storage");
</script>

<style scoped>
.sys-config-container {
  height: 100%;
}

.sys-config-tabs {
  height: 100%;
  display: flex;
  flex-direction: column;
}

.sys-config-tabs :deep(.el-tabs__header) {
  margin: 0 0 16px 0;
  padding: 0 16px;
  background-color: transparent;
  border-bottom: 1px solid #e2e8f0;
}

.sys-config-tabs :deep(.el-tabs__nav-wrap::after) {
  display: none; /* Hide default thick border */
}

.sys-config-tabs :deep(.el-tabs__item) {
  height: 40px;
  line-height: 40px;
  font-size: 14px;
}

.sys-config-tabs :deep(.el-tabs__item.is-active) {
  color: #1bbf9e; /* Typical system green, based on user image */
  font-weight: 500;
}
.sys-config-tabs :deep(.el-tabs__active-bar) {
  background-color: #1bbf9e;
}

.tab-content-wrap {
  flex: 1;
}
</style>
