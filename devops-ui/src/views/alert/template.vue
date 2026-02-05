<template>
  <div class="mod-alert__template">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="queryList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.name" class="query-input" placeholder="模板名称" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.status" placeholder="状态" clearable>
              <el-option label="启用" :value="1"></el-option>
              <el-option label="禁用" :value="0"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" @click="handleReset">重置</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="tpl-stats">
            <span class="tpl-stats__item tpl-stats__item--on">启用 {{ enabledCount }}</span>
            <span class="tpl-stats__item tpl-stats__item--off">禁用 {{ disabledCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('alert:template:save')" type="primary" @click="addOrUpdateHandle()">新增</el-button>
          <el-button v-if="state.hasPermission('alert:template:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
          <el-button v-if="state.hasPermission('alert:template:test')" type="info" @click="openTest()">测试</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" class="tpl-table" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="name" label="模板名称" header-align="center" align="center" min-width="180" show-overflow-tooltip></el-table-column>
      <el-table-column prop="emailSubject" label="主题" header-align="center" align="center" min-width="260" show-overflow-tooltip></el-table-column>
      <el-table-column prop="status" label="状态" header-align="center" align="center" width="100">
        <template v-slot="scope">
          <el-tag v-if="scope.row.status === 0" size="small" type="danger">禁用</el-tag>
          <el-tag v-else size="small" type="success">启用</el-tag>
        </template>
      </el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="180">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('alert:template:update')" type="primary" link @click="addOrUpdateHandle(scope.row.id)">修改</el-button>
          <el-button v-if="state.hasPermission('alert:template:test')" type="primary" link @click="openTest(scope.row)">测试</el-button>
          <el-button v-if="state.hasPermission('alert:template:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
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

    <add-or-update ref="addOrUpdateRef" @refreshDataList="queryList"></add-or-update>

    <el-dialog v-model="testVisible" title="模板测试" width="960px" :close-on-click-modal="false" class="test-dialog">
      <div class="test-meta">
        <span class="test-meta__label">当前模板</span>
        <span class="test-meta__value">{{ currentTemplateName || "-" }}</span>
      </div>
      <el-form :model="testForm" label-width="100px" class="test-form">
        <el-form-item label="模板">
          <el-select v-model="testForm.templateId" placeholder="选择模板">
            <el-option v-for="item in templateOptions" :key="item.id" :label="item.name" :value="item.id"></el-option>
          </el-select>
        </el-form-item>
        <el-form-item label="模板内容">
          <el-input :model-value="previewTemplateContent" type="textarea" :rows="4" readonly></el-input>
        </el-form-item>
        <el-form-item label="原始内容">
          <el-input v-model="testForm.rawJson" type="textarea" :rows="6" placeholder="alertmanager原始JSON"></el-input>
        </el-form-item>
        <el-form-item label="格式化内容">
          <div class="alert-preview" v-html="testForm.previewResult"></div>
        </el-form-item>
        <el-form-item label="主题">
          <el-input :model-value="testForm.previewSubject" readonly></el-input>
        </el-form-item>
        <el-form-item label="触发器">
          <el-select v-model="testForm.triggerId" placeholder="选择触发器" clearable>
            <el-option v-for="item in triggerOptions" :key="item.id" :label="item.name" :value="item.id"></el-option>
          </el-select>
        </el-form-item>
      </el-form>
      <template #footer>
        <el-button @click="testVisible = false">取消</el-button>
        <el-button @click="resetTestForm">重置</el-button>
        <el-button @click="fillSampleJson">填充样例</el-button>
        <el-button @click="formatRawJson">格式化JSON</el-button>
        <el-button :loading="previewLoading" @click="handlePreview">预览</el-button>
        <el-button type="primary" :loading="sendLoading" @click="handleTestSend">发送测试</el-button>
      </template>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, reactive, ref, toRefs} from "vue";
import AddOrUpdate from "./template-add-or-update.vue";
import baseService from "@/service/baseService";
import {ElMessage} from "element-plus";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/alert/template/page",
  getDataListIsPage: true,
  deleteURL: "/alert/template",
  dataForm: {
    name: "",
    status: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const addOrUpdateRef = ref();
const testVisible = ref(false);
const previewLoading = ref(false);
const sendLoading = ref(false);
const templateOptions = ref<any[]>([]);
const triggerOptions = ref<any[]>([]);
const sampleRawJson = `{\"receiver\":\"web.hook.prometheusalert\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertname\":\"MerchantServiceHighErrorRate\",\"method\":\"check\",\"service\":\"merchant_service\",\"severity\":\"warning\",\"util_class\":\"ShanDaiMiao11Util\"},\"annotations\":{\"description\":\"工具类 ShanDaiMiao11Util 的 check 方法10分钟内错误率为 5.556%，超过5%阈值\",\"summary\":\"机构服务错误率过高\"},\"startsAt\":\"2026-01-22T10:46:17.966Z\",\"endsAt\":\"2026-01-22T10:55:02.966Z\",\"generatorURL\":\"http://build:9090/graph?g0.expr=(sum+by+(util_class,+method)+(increase(merchant_service_method_calls_total{status=\\\"error\\\"}[10m]))+/+sum+by+(util_class,+method)+(increase(merchant_service_method_calls_total[10m])))+>+0.05+and+sum+by+(util_class,+method)+(increase(merchant_service_method_calls_total[10m]))+>+0&g0.tab=1\",\"fingerprint\":\"fb2ce429827b0ff3\"}],\"groupLabels\":{},\"commonLabels\":{\"alertname\":\"MerchantServiceHighErrorRate\",\"method\":\"check\",\"service\":\"merchant_service\",\"severity\":\"warning\",\"util_class\":\"ShanDaiMiao11Util\"},\"commonAnnotations\":{\"description\":\"工具类 ShanDaiMiao11Util 的 check 方法10分钟内错误率为 5.556%，超过5%阈值\",\"summary\":\"机构服务错误率过高\"},\"externalURL\":\"http://gitlab:9093\",\"version\":\"4\",\"groupKey\":\"{}:{}\",\"truncatedAlerts\":0}`;
const testForm = reactive({
  templateId: "",
  rawJson: sampleRawJson,
  previewResult: "",
  previewSubject: "",
  triggerId: ""
});
const enabledCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.status) === 1).length);
const disabledCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.status) === 0).length);

const previewTemplateContent = computed(() => {
  const template = templateOptions.value.find((item) => item.id === testForm.templateId);
  return template?.content || "";
});
const currentTemplateName = computed(() => {
  const template = templateOptions.value.find((item) => item.id === testForm.templateId);
  return template?.name || "";
});

const queryList = () => {
  state.getDataList();
};

const handleReset = () => {
  state.dataForm.name = "";
  state.dataForm.status = "";
  queryList();
};

const addOrUpdateHandle = (id?: number) => {
  addOrUpdateRef.value.init(id);
};

const loadTriggers = () => {
  baseService.get("/alert/trigger/options").then((res) => {
    triggerOptions.value = res.data || [];
  });
};

const loadTemplates = () => {
  baseService.get("/alert/template/page", { page: 1, limit: 1000 }).then((res) => {
    templateOptions.value = (res.data.list || []).map((item: any) => ({
      id: item.id,
      name: item.name,
      content: item.emailHtml || ""
    }));
  });
};

const openTest = (row?: any) => {
  loadTriggers();
  loadTemplates();
  testForm.previewResult = "";
  testForm.previewSubject = "";
  testForm.triggerId = "";
  testForm.templateId = row?.id || testForm.templateId;
  testVisible.value = true;
};

const resetTestForm = () => {
  testForm.rawJson = sampleRawJson;
  testForm.previewResult = "";
  testForm.previewSubject = "";
  testForm.triggerId = "";
};

const fillSampleJson = () => {
  testForm.rawJson = sampleRawJson;
};

const formatRawJson = () => {
  if (!testForm.rawJson.trim()) {
    ElMessage.warning("请先输入原始内容");
    return;
  }
  try {
    testForm.rawJson = JSON.stringify(JSON.parse(testForm.rawJson), null, 2);
  } catch (e) {
    ElMessage.error("JSON格式不正确");
  }
};

const handlePreview = () => {
  if (!testForm.templateId) {
    return ElMessage.warning("请选择模板");
  }
  if (!testForm.rawJson.trim()) {
    return ElMessage.warning("请填写原始内容");
  }
  previewLoading.value = true;
  baseService
    .post("/alert/template/preview", {
      templateId: testForm.templateId,
      rawJson: testForm.rawJson
    })
    .then((res) => {
      testForm.previewResult = res.data.html || "";
      testForm.previewSubject = res.data.subject || "";
    })
    .finally(() => {
      previewLoading.value = false;
    });
};

const handleTestSend = () => {
  if (!testForm.templateId) {
    return ElMessage.warning("请选择模板");
  }
  if (!testForm.triggerId) {
    return ElMessage.warning("请选择触发器");
  }
  if (!testForm.rawJson.trim()) {
    return ElMessage.warning("请填写原始内容");
  }
  sendLoading.value = true;
  baseService
    .post("/alert/template/test-send", {
      templateId: testForm.templateId,
      triggerId: testForm.triggerId,
      rawJson: testForm.rawJson
    })
    .then(() => {
      ElMessage.success("发送成功");
      testVisible.value = false;
    })
    .finally(() => {
      sendLoading.value = false;
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
.query-input {
  width: 220px;
}
.query-btn {
  height: 32px;
  padding: 0 14px;
}
.ops-toolbar__group :deep(.el-input__wrapper),
.ops-toolbar__group :deep(.el-select__wrapper) {
  height: 32px;
}
.tpl-stats {
  display: flex;
  align-items: center;
  gap: 6px;
}
.tpl-stats__item {
  padding: 2px 8px;
  border-radius: 999px;
  font-size: 12px;
}
.tpl-stats__item--on {
  color: #065f46;
  background: #d1fae5;
}
.tpl-stats__item--off {
  color: #991b1b;
  background: #fee2e2;
}
.tpl-table :deep(.el-table__row:hover > td) {
  background: #f8fafc;
}
.alert-preview {
  width: 100%;
  min-height: 180px;
  padding: 12px;
  border: 1px solid #e4e7ed;
  border-radius: 6px;
  background: #fff;
}
.test-meta {
  padding: 10px 12px;
  margin-bottom: 10px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 6px;
}
.test-meta__label {
  margin-right: 8px;
  color: #64748b;
  font-size: 12px;
}
.test-meta__value {
  color: #0f172a;
  font-weight: 600;
}
.test-form :deep(.el-form-item) {
  margin-bottom: 14px;
}
.test-dialog :deep(.el-dialog__body) {
  max-height: 68vh;
  overflow-y: auto;
}
</style>
