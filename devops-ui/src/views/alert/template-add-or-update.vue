<template>
  <el-dialog v-model="visible" :title="!dataForm.id ? '新增模板' : '修改模板'" width="820px" :close-on-click-modal="false" :close-on-press-escape="false">
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px" class="dialog-form">
      <el-form-item prop="name">
        <template #label>
          <span>模板名称</span>
          <el-tooltip placement="top" :content="templateTip">
            <el-icon class="help-icon"><question-filled /></el-icon>
          </el-tooltip>
        </template>
        <el-input v-model="dataForm.name" placeholder="模板名称"></el-input>
      </el-form-item>
      <el-form-item prop="emailSubject">
        <template #label>
          <span>主题</span>
          <el-tooltip placement="top" :content="templateTip">
            <el-icon class="help-icon"><question-filled /></el-icon>
          </el-tooltip>
        </template>
        <el-input v-model="dataForm.emailSubject" placeholder="主题"></el-input>
      </el-form-item>
      <el-form-item prop="emailHtml">
        <template #label>
          <span>HTML内容</span>
          <el-tooltip placement="top" :content="templateTip">
            <el-icon class="help-icon"><question-filled /></el-icon>
          </el-tooltip>
        </template>
        <el-input v-model="dataForm.emailHtml" type="textarea" :rows="10" placeholder="HTML内容，可引用${annotations.summary}"></el-input>
      </el-form-item>
      <el-form-item label="状态" prop="status">
        <el-select v-model="dataForm.status" placeholder="请选择状态">
          <el-option label="启用" :value="1"></el-option>
          <el-option label="禁用" :value="0"></el-option>
        </el-select>
      </el-form-item>
    </el-form>
    <template #footer>
      <div class="dialog-footer">
        <el-button @click="visible = false">取消</el-button>
        <el-button @click="fillSample">填充示例</el-button>
        <el-button type="primary" :loading="submitLoading" @click="dataFormSubmitHandle()">确定</el-button>
      </div>
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
const submitLoading = ref(false);

const templateTip = "支持变量：${alertname}、${severity}、${instance}、${summary}、${description}、${labels.xxx}、${annotations.xxx}、${startsAt}";

const dataForm = reactive({
  id: "",
  name: "",
  emailSubject: "",
  emailHtml: "",
  status: ""
});

const rules = ref({
  name: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  emailHtml: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
});

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.status = "";
  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }
  if (id) {
    baseService.get("/alert/template/" + id).then((res) => {
      Object.assign(dataForm, res.data);
    });
  }
};

const fillSample = () => {
  if (!dataForm.emailHtml) {
    dataForm.emailHtml = [
      "<h3>${alertname}</h3>",
      "<p>级别：${severity}</p>",
      "<p>实例：${instance}</p>",
      "<p>摘要：${summary}</p>",
      "<p>描述：${description}</p>",
      "<p>时间：${startsAt}</p>"
    ].join("");
  }
};

const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    const submitData = { ...dataForm } as Record<string, any>;
    const headers = { "Content-Type": "application/x-www-form-urlencoded" };
    submitLoading.value = true;
    (!dataForm.id ? baseService.post : baseService.put)("/alert/template", submitData, headers)
      .then(() => {
        ElMessage.success({
          message: "成功",
          duration: 500,
          onClose: () => {
            visible.value = false;
            emit("refreshDataList");
          }
        });
      })
      .finally(() => {
        submitLoading.value = false;
      });
  });
};

defineExpose({ init });
</script>

<style scoped>
.help-icon {
  margin-left: 6px;
  color: #94a3b8;
  cursor: pointer;
}
</style>
