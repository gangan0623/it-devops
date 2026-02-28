<script lang="ts">
import SvgIcon from "@/components/base/svg-icon";
import baseService from "@/service/baseService";
import {useFullscreen} from "@vueuse/core";
import {defineComponent} from "vue";
import {useRouter} from "vue-router";
import {useAppStore} from "@/store";
import "@/assets/css/header.less";
import {ElMessageBox} from "element-plus";

interface IExpand {
  userName?: string;
}

/**
 * 顶部右侧扩展区域
 */
export default defineComponent({
  name: "Expand",
  components: { SvgIcon },
  props: {
    userName: String
  },
  setup(props: IExpand) {
    const router = useRouter();
    const store = useAppStore();
    const { isFullscreen, toggle } = useFullscreen();

    const onClickUserMenus = (path: string) => {
      if (path === "/login") {
        ElMessageBox.confirm("确定进行[退出]操作?", "提示", {
          confirmButtonText: "确定",
          cancelButtonText: "取消",
          type: "warning"
        })
          .then(() => {
            baseService.post("/logout").finally(() => {
              router.push(path);
            });
          })
          .catch(() => {
            //
          });
      } else {
        router.push(path);
      }
    };
    return {
      props,
      store,
      isFullscreen,
      onClickUserMenus,
      toggle
    };
  }
});
</script>
<template>
  <div class="rr-header-right-items">
    <div @click="toggle" class="hidden-xs-only">
      <span>
        <svg-icon :name="isFullscreen ? 'tuichuquanping' : 'fullscreen2'"></svg-icon>
      </span>
    </div>
    <div style="display: flex; justify-content: center; align-items: center">
      <el-dropdown @command="onClickUserMenus">
        <template #dropdown>
          <el-dropdown-menu>
            <el-dropdown-item icon="user" command="/user/profile"> 更改个人信息 </el-dropdown-item>
            <el-dropdown-item icon="switch-button" divided command="/login"> 退出登录 </el-dropdown-item>
          </el-dropdown-menu>
        </template>
        <span class="el-dropdown-link" style="display: flex">
          {{ props.userName }}
          <el-icon class="el-icon--right" style="font-size: 14px"><arrow-down /></el-icon>
        </span>
      </el-dropdown>
    </div>
  </div>
</template>
