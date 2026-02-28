<template>
  <div class="mod-sys__permission">
    <el-tabs v-model="activeTab">
      <el-tab-pane label="用户管理" name="user">
        <user-management />
      </el-tab-pane>
      <el-tab-pane label="角色管理" name="role">
        <role-management />
      </el-tab-pane>
    </el-tabs>
  </div>
</template>

<script lang="ts" setup>
import { ref } from "vue";
import UserManagement from "./user-management.vue";
import RoleManagement from "./role.vue";

const activeTab = ref("user");
</script>

<style scoped>
.mod-sys__permission {
  padding: 8px;
}
</style>
