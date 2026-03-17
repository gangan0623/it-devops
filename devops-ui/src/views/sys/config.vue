<template>
  <div class="mod-sys__config">
    <el-tabs v-model="activeTab">
      <el-tab-pane label="存储配置" name="storage" :lazy="true">
        <config-storage />
      </el-tab-pane>
      <el-tab-pane label="Zabbix配置" name="zabbix" :lazy="true">
        <config-zabbix />
      </el-tab-pane>
      <el-tab-pane label="Zabbix映射" name="zabbixMapping" :lazy="true">
        <config-zabbix-mapping />
      </el-tab-pane>
      <el-tab-pane label="AI配置" name="ai" :lazy="true">
        <config-ai />
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
.mod-sys__config {
  padding: 8px;
}
</style>
