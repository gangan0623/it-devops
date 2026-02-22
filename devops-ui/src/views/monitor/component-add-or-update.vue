<template>
  <el-dialog
    v-model="visible"
    :title="!dataForm.id ? '新增组件' : '修改组件'"
    width="680px"
    :close-on-click-modal="false"
    :close-on-press-escape="false"
  >
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px" class="dialog-form">
      <div class="form-section-title">基础信息</div>
      <el-form-item label="名称" prop="name">
        <el-input v-model="dataForm.name" placeholder="名称" @blur="checkUnique('name')"></el-input>
      </el-form-item>
      <el-form-item label="类型" prop="type">
        <ren-select v-model="dataForm.type" dict-type="monitor_component_type" placeholder="类型"></ren-select>
      </el-form-item>
      <div class="form-section-title">网络与访问</div>
      <el-form-item label="IP" prop="ip">
        <el-input v-model="dataForm.ip" placeholder="IP" @blur="checkUnique('ip')"></el-input>
      </el-form-item>
      <el-form-item label="端口" prop="port">
        <el-input v-model.number="dataForm.port" placeholder="端口"></el-input>
      </el-form-item>
      <el-form-item label="Web地址" prop="webUrl">
        <el-input v-model="dataForm.webUrl" placeholder="可选，用于嵌套访问"></el-input>
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
  type: "",
  ip: "",
  port: "",
  webUrl: "",
});

const duplicateFlags = reactive({
  ip: false,
  name: false
});

const validateName = (_rule: any, value: string, callback: (error?: Error) => void) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (duplicateFlags.name) {
    return callback(new Error("名称已存在"));
  }
  return callback();
};

const validateIp = (_rule: any, value: string, callback: (error?: Error) => void) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (duplicateFlags.ip) {
    return callback(new Error("IP端口已存在"));
  }
  return callback();
};

const rules = ref({
  name: [{ validator: validateName, trigger: "blur" }],
  type: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  ip: [{ validator: validateIp, trigger: "blur" }],
  port: [{ required: true, message: "必填项不能为空", trigger: "blur" }]
});

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.type = "";
  dataForm.webUrl = "";
  duplicateFlags.ip = false;
  duplicateFlags.name = false;

  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }

  if (id) {
    getInfo(id);
  }
};

const getInfo = (id: number) => {
  baseService.get("/ops/monitorcomponent/" + id).then((res) => {
    Object.assign(dataForm, res.data);
    duplicateFlags.ip = false;
    duplicateFlags.name = false;
  });
};

const checkUnique = (field: "ip" | "name") => {
  const value = dataForm[field];
  if (!value) {
    duplicateFlags[field] = false;
    return;
  }
  const params: Record<string, any> = { id: dataForm.id };
  if (field === "ip") {
    params.ip = value;
    params.port = dataForm.port;
  } else {
    params.name = value;
  }
  baseService.get("/ops/monitorcomponent/check", params).then((res) => {
    duplicateFlags[field] = !!res.data;
    if (duplicateFlags[field]) {
      ElMessage.error(field === "ip" ? "IP端口已存在" : "名称已存在");
    }
    if (dataFormRef.value) {
      dataFormRef.value.validateField(field);
    }
  });
};

const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    const submitData = { ...dataForm } as Record<string, any>;
    delete submitData.creator;
    delete submitData.createDate;
    delete submitData.updater;
    delete submitData.updateDate;
    submitLoading.value = true;
    (!dataForm.id ? baseService.post : baseService.put)("/ops/monitorcomponent", submitData)
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

defineExpose({
  init
});
</script>

<style scoped>
</style>
