<template>
  <el-dialog v-model="visible" :title="!dataForm.id ? '新增' : '修改'" width="760px" :close-on-click-modal="false" :close-on-press-escape="false">
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px">
      <el-form-item label="触发器名称" prop="name">
        <el-input v-model="dataForm.name" placeholder="触发器名称"></el-input>
      </el-form-item>
      <el-form-item label="模板" prop="templateId">
        <el-select v-model="dataForm.templateId" placeholder="选择模板">
          <el-option v-for="item in templateOptions" :key="item.id" :label="item.name" :value="item.id"></el-option>
        </el-select>
      </el-form-item>
      <el-form-item label="媒介" prop="mediaId">
        <el-select v-model="dataForm.mediaId" placeholder="选择媒介">
          <el-option v-for="item in mediaOptions" :key="item.id" :label="item.name" :value="item.id"></el-option>
        </el-select>
      </el-form-item>
      <el-form-item label="接收人" prop="receiverUserIdList">
        <el-select v-model="dataForm.receiverUserIdList" multiple filterable placeholder="选择用户邮箱">
          <el-option v-for="item in userOptions" :key="item.id" :label="item.name + ' (' + item.email + ')'" :value="item.id"></el-option>
        </el-select>
      </el-form-item>
      <el-form-item label="告警级别" prop="severity">
        <el-select v-model="severity" placeholder="选择级别" multiple clearable>
          <el-option label="灾难告警" value="critical"></el-option>
          <el-option label="重要告警" value="warning"></el-option>
          <el-option label="信息提示" value="info"></el-option>
          <el-option label="告警恢复" value="recover"></el-option>
        </el-select>
      </el-form-item>
      <el-form-item>
        <template #label>
          <span>匹配标签(JSON)</span>
          <el-tooltip placement="top" :content="matchTip">
            <el-icon class="help-icon"><question-filled /></el-icon>
          </el-tooltip>
        </template>
        <el-input v-model="matchLabelsExtra" type="textarea" :rows="4" placeholder='{"alertname":"xxx"}'></el-input>
      </el-form-item>
      <el-form-item label="状态" prop="status">
        <el-select v-model="dataForm.status" placeholder="请选择状态">
          <el-option label="启用" :value="1"></el-option>
          <el-option label="禁用" :value="0"></el-option>
        </el-select>
      </el-form-item>
    </el-form>
    <template #footer>
      <el-button @click="visible = false">取消</el-button>
      <el-button type="primary" @click="dataFormSubmitHandle()">确定</el-button>
    </template>
  </el-dialog>
</template>

<script lang="ts" setup>
import {reactive, ref} from "vue";
import baseService from "@/service/baseService";
import {ElMessage} from "element-plus";
import {QuestionFilled} from "@element-plus/icons-vue";

const emit = defineEmits(["refreshDataList"]);
const visible = ref(false);
const dataFormRef = ref();

const templateOptions = ref<any[]>([]);
const mediaOptions = ref<any[]>([]);
const userOptions = ref<any[]>([]);

const matchTip = "先按“告警级别”匹配，再按 JSON 过滤其他 labels，例如：{\"alertname\":\"xxx\",\"service\":\"merchant_service\"}。空则全部匹配。";
const severity = ref([] as string[]);
const matchLabelsExtra = ref("");

const dataForm = reactive({
  id: "",
  name: "",
  templateId: "",
  mediaId: "",
  receiverUserIdList: [] as number[],
  severity: "",
  matchLabels: "",
  status: ""
});

const rules = ref({
  name: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  templateId: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  mediaId: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  receiverUserIdList: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
});

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.status = "";
  dataForm.severity = "";
  severity.value = [];
  matchLabelsExtra.value = "";
  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }
  loadResources();
  if (id) {
    baseService.get("/alert/trigger/" + id).then((res) => {
      Object.assign(dataForm, res.data);
      parseMatchLabels();
    });
  }
};

const loadResources = () => {
  baseService.get("/alert/trigger/resources").then((res) => {
    templateOptions.value = res.data.templates || [];
    mediaOptions.value = res.data.medias || [];
    userOptions.value = res.data.users || [];
  });
};

const parseMatchLabels = () => {
  if (!dataForm.matchLabels) {
    matchLabelsExtra.value = "";
  } else {
    try {
      const obj = JSON.parse(dataForm.matchLabels);
      matchLabelsExtra.value = Object.keys(obj).length ? JSON.stringify(obj, null, 2) : "";
    } catch (e) {
      matchLabelsExtra.value = "";
    }
  }
  if (dataForm.severity) {
    severity.value = dataForm.severity.split(",").map((item) => item.trim()).filter(Boolean);
  } else {
    severity.value = [];
  }
};

const buildMatchLabels = () => {
  let match: any = {};
  if (matchLabelsExtra.value) {
    try {
      match = JSON.parse(matchLabelsExtra.value);
    } catch (e) {
      ElMessage.error("匹配标签JSON格式错误");
      return false;
    }
  }
  dataForm.matchLabels = Object.keys(match).length ? JSON.stringify(match) : "";
  dataForm.severity = severity.value.length ? severity.value.join(",") : "";
  return true;
};

const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    if (!buildMatchLabels()) {
      return;
    }
    const submitData = { ...dataForm } as Record<string, any>;
    (!dataForm.id ? baseService.post : baseService.put)("/alert/trigger", submitData).then(() => {
      ElMessage.success({
        message: "成功",
        duration: 500,
        onClose: () => {
          visible.value = false;
          emit("refreshDataList");
        }
      });
    });
  });
};

defineExpose({ init });
</script>

<style lang="less" scoped>
.help-icon {
  margin-left: 6px;
  color: #909399;
  cursor: pointer;
}
</style>
