<template>
  <el-dialog v-model="visible" :title="!dataForm.id ? '新增媒介' : '修改媒介'" width="760px" :close-on-click-modal="false" :close-on-press-escape="false">
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px" class="media-form">
      <div class="form-section-title">基础信息</div>
      <el-form-item label="媒介名称" prop="name">
        <el-input v-model="dataForm.name" placeholder="媒介名称"></el-input>
      </el-form-item>
      <el-form-item label="SMTP Host" prop="host">
        <el-input v-model="dataForm.host" placeholder="SMTP Host"></el-input>
      </el-form-item>
      <el-form-item label="端口" prop="port">
        <el-input v-model="dataForm.port" placeholder="端口" type="number"></el-input>
      </el-form-item>
      <el-form-item label="用户名" prop="username">
        <el-input v-model="dataForm.username" placeholder="用户名"></el-input>
      </el-form-item>
      <el-form-item label="密码" prop="password">
        <el-input v-model="dataForm.password" placeholder="密码" show-password></el-input>
      </el-form-item>
      <el-form-item label="协议" prop="protocol">
        <el-input v-model="dataForm.protocol" placeholder="smtp"></el-input>
      </el-form-item>
      <div class="form-section-title">安全与连接</div>
      <el-form-item label="SMTP认证" prop="smtpAuth">
        <el-switch v-model="dataForm.smtpAuth" :active-value="1" :inactive-value="0"></el-switch>
      </el-form-item>
      <el-form-item label="STARTTLS" prop="starttlsEnable">
        <el-switch v-model="dataForm.starttlsEnable" :active-value="1" :inactive-value="0"></el-switch>
      </el-form-item>
      <el-form-item label="TLS/SSL" prop="tlsEnable">
        <el-switch v-model="dataForm.tlsEnable" :active-value="1" :inactive-value="0"></el-switch>
      </el-form-item>
      <el-form-item label="发件人" prop="fromAddr">
        <el-input v-model="dataForm.fromAddr" placeholder="发件人"></el-input>
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
        <el-button type="primary" :loading="submitLoading" @click="dataFormSubmitHandle()">确定</el-button>
      </div>
    </template>
  </el-dialog>
</template>

<script lang="ts" setup>
import {reactive, ref} from "vue";
import baseService from "@/service/baseService";
import {ElMessage} from "element-plus";

const emit = defineEmits(["refreshDataList"]);
const visible = ref(false);
const dataFormRef = ref();
const submitLoading = ref(false);

const dataForm = reactive({
  id: "",
  name: "",
  host: "",
  port: "",
  username: "",
  password: "",
  protocol: "smtp",
  smtpAuth: 1,
  starttlsEnable: 0,
  tlsEnable: 0,
  fromAddr: "",
  status: ""
});

const rules = ref({
  name: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  host: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  port: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  username: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  password: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
});

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.status = "";
  dataForm.smtpAuth = 1;
  dataForm.starttlsEnable = 0;
  dataForm.tlsEnable = 0;
  dataForm.protocol = "smtp";
  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }
  if (id) {
    baseService.get("/alert/media/" + id).then((res) => {
      Object.assign(dataForm, res.data);
    });
  }
};

const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    const submitData = { ...dataForm } as Record<string, any>;
    submitLoading.value = true;
    (!dataForm.id ? baseService.post : baseService.put)("/alert/media", submitData)
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

<style lang="less" scoped>
.media-form {
  max-height: 60vh;
  overflow-y: auto;
  padding-right: 6px;
}
.form-section-title {
  margin: 4px 0 10px;
  padding-left: 8px;
  border-left: 3px solid #409eff;
  color: #1e293b;
  font-weight: 600;
  font-size: 13px;
}
.dialog-footer {
  display: flex;
  justify-content: flex-end;
  gap: 8px;
}
.dialog-footer :deep(.el-button) {
  height: 32px;
  padding: 0 16px;
}
</style>
