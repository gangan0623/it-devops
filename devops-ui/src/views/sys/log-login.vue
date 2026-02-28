<template>
  <div class="mod-sys__log-manage">
    <el-tabs v-model="activeTab">
      <el-tab-pane label="登录日志" name="login">
        <log-login-management />
      </el-tab-pane>
      <el-tab-pane label="操作日志" name="operation">
        <log-operation />
      </el-tab-pane>
      <el-tab-pane label="异常日志" name="error">
        <log-error />
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script lang="ts" setup>
import { ref } from "vue";
import LogLoginManagement from "./log-login-management.vue";
import LogOperation from "./log-operation.vue";
import LogError from "./log-error.vue";

const activeTab = ref("login");
</script>

<style scoped>
.mod-sys__log-manage {
  padding: 8px;
}
</style>
