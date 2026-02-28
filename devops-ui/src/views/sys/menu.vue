<template>
  <div class="mod-sys__system-manage">
    <el-tabs v-model="activeTab">
      <el-tab-pane label="菜单管理" name="menu">
        <menu-management />
      </el-tab-pane>
      <el-tab-pane label="字典管理" name="dict">
        <dict-type />
      </el-tab-pane>
      <el-tab-pane label="定时任务" name="schedule">
        <schedule-manage />
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script lang="ts" setup>
import { ref } from "vue";
import MenuManagement from "./menu-management.vue";
import DictType from "./dict-type.vue";
import ScheduleManage from "../job/schedule.vue";

const activeTab = ref("menu");
</script>

<style scoped>
.mod-sys__system-manage {
  padding: 8px;
}
</style>
