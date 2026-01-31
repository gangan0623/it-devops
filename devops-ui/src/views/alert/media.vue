<template>
  <div class="mod-alert__media">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="state.getDataList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.name" placeholder="媒介名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.status" placeholder="状态" clearable>
              <el-option label="启用" :value="1"></el-option>
              <el-option label="禁用" :value="0"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-button @click="state.getDataList()">查询</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <el-button v-if="state.hasPermission('alert:media:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('alert:media:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="name" label="媒介名称" header-align="center" align="center"></el-table-column>
      <el-table-column prop="host" label="SMTP Host" header-align="center" align="center"></el-table-column>
      <el-table-column prop="port" label="端口" header-align="center" align="center"></el-table-column>
      <el-table-column prop="username" label="用户名" header-align="center" align="center"></el-table-column>
      <el-table-column prop="fromAddr" label="发件人" header-align="center" align="center"></el-table-column>
      <el-table-column prop="status" label="状态" header-align="center" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
          <el-tag v-else size="small" type="success">启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="220">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('alert:media:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('alert:media:test')" type="primary" link @click="openTest(scope.row)">测试</el-button>
          <el-button v-if="state.hasPermission('alert:media:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
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

    <add-or-update ref="addOrUpdateRef" @refreshDataList="state.getDataList"></add-or-update>

    <el-dialog v-model="testVisible" title="媒介测试" :close-on-click-modal="false">
      <el-form :model="testForm" label-width="100px">
        <el-form-item label="收件人">
          <el-input v-model="testForm.to" placeholder="多个邮箱用逗号分隔"></el-input>
        </el-form-item>
        <el-form-item label="主题">
          <el-input v-model="testForm.subject" placeholder="主题"></el-input>
        </el-form-item>
        <el-form-item label="HTML内容">
          <el-input v-model="testForm.html" type="textarea" :rows="4" placeholder="HTML内容"></el-input>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="testVisible = false">取消</el-button>
        <el-button type="primary" @click="sendTest">发送测试</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {reactive, ref, toRefs} from "vue";
import baseService from "@/service/baseService";
import {ElMessage} from "element-plus";
import AddOrUpdate from "./media-add-or-update.vue";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/alert/media/page",
  getDataListIsPage: true,
  deleteURL: "/alert/media",
  dataForm: {
    name: "",
    status: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const addOrUpdateRef = ref();
const testVisible = ref(false);
const testForm = reactive({
  mediaId: "",
  to: "",
  subject: "",
  html: ""
});

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const openTest = (row: any) => {
  testForm.mediaId = row.id;
  testForm.to = "";
  testForm.subject = "";
  testForm.html = "";
  testVisible.value = true;
};

const sendTest = () => {
  baseService.post("/alert/media/test", testForm).then(() => {
    ElMessage.success("发送成功");
    testVisible.value = false;
  });
};
</script>

<style lang="less" scoped>
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
.ops-filters .el-form-item {
  margin-bottom: 0;
}
</style>
