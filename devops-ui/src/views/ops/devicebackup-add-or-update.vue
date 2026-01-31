<template>
  <el-dialog v-model="visible" :title="!dataForm.id ? '新增' : '修改'" :close-on-click-modal="false" :close-on-press-escape="false">
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px">
          <el-form-item label="地址" prop="instance">
        <el-input v-model="dataForm.instance" placeholder="地址" @blur="checkUnique('instance')"></el-input>
      </el-form-item>
          <el-form-item label="名称" prop="name">
        <el-input v-model="dataForm.name" placeholder="名称" @blur="checkUnique('name')"></el-input>
      </el-form-item>
          <el-form-item label="用户名" prop="username">
        <el-input v-model="dataForm.username" placeholder="用户名"></el-input>
      </el-form-item>
      <el-form-item label="密码" prop="password">
        <el-input v-model="dataForm.password" placeholder="修改时留空不变" show-password></el-input>
      </el-form-item>
          <el-form-item label="区域名称" prop="areaName">
        <ren-select v-model="dataForm.areaName" dict-type="area_name" placeholder="区域名称"></ren-select>
      </el-form-item>
      <el-form-item label="分组名称" prop="groupName">
        <ren-select v-model="dataForm.groupName" dict-type="network_device_group" placeholder="分组名称"></ren-select>
      </el-form-item>
      <el-form-item label="设备型号" prop="deviceModel">
        <ren-select v-model="dataForm.deviceModel" dict-type="device_model" placeholder="设备型号"></ren-select>
      </el-form-item>
      <el-form-item label="状态" prop="status">
        <el-select v-model="dataForm.status" placeholder="请选择状态">
          <el-option label="启用" :value="1"></el-option>
          <el-option label="禁用" :value="0"></el-option>
        </el-select>
      </el-form-item>
          <el-form-item label="备份节点" prop="agentId">
        <el-select v-model="dataForm.agentId" placeholder="选择备份节点" filterable>
          <el-option v-for="item in backupAgentOptions" :key="item.id" :label="item.label" :value="item.id"></el-option>
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

const emit = defineEmits(["refreshDataList"]);

const visible = ref(false);
const dataFormRef = ref();

const dataForm = reactive({
  id: "",
  instance: "",
  name: "",
  username: "",
  password: "",
  areaName: "",
  groupName: "",
  deviceModel: "",
  status: "",
  agentId: ""
});

const duplicateFlags = reactive({
  instance: false,
  name: false
});

const backupAgentOptions = ref<{ id: number; label: string }[]>([]);

const validateInstance = (_rule: any, value: string, callback: (error?: Error) => void) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (duplicateFlags.instance) {
    return callback(new Error("地址已存在"));
  }
  return callback();
};

const validateName = (_rule: any, value: string, callback: (error?: Error) => void) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (duplicateFlags.name) {
    return callback(new Error("名称已存在"));
  }
  return callback();
};

const validatePassword = (_rule: any, value: string, callback: (error?: Error) => void) => {
  if (!dataForm.id && !value) {
    return callback(new Error("必填项不能为空"));
  }
  return callback();
};

const rules = ref({
          instance: [
      { validator: validateInstance, trigger: 'blur' }
    ],
          name: [
      { validator: validateName, trigger: 'blur' }
    ],
          username: [
      { required: true, message: '必填项不能为空', trigger: 'blur' }
    ],
          password: [
      { validator: validatePassword, trigger: 'blur' }
    ],
          areaName: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
  groupName: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          deviceModel: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          status: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          agentId: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          });

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.status = "";
  duplicateFlags.instance = false;
  duplicateFlags.name = false;
  backupAgentOptions.value = [];

  // 重置表单数据
  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }

  loadBackupAgents();

  if (id) {
    getInfo(id);
  }
};

// 获取信息
const getInfo = (id: number) => {
  baseService.get("/ops/devicebackup/" + id).then((res) => {
    Object.assign(dataForm, res.data);
    duplicateFlags.instance = false;
    duplicateFlags.name = false;
  });
};

const loadBackupAgents = () => {
  baseService
    .get("/ops/backupagent/page", { page: 1, limit: 1000 })
    .then((res) => {
      const list = res.data?.list || [];
      backupAgentOptions.value = list.map((item: any) => ({
        id: item.id,
        label: item.name || item.instance || String(item.id)
      }));
    });
};

const checkUnique = (field: "instance" | "name") => {
  const value = dataForm[field];
  if (!value) {
    duplicateFlags[field] = false;
    return;
  }
  const params: Record<string, any> = { id: dataForm.id };
  params[field] = value;
  baseService.get("/ops/devicebackup/check", params).then((res) => {
    duplicateFlags[field] = !!res.data;
    if (duplicateFlags[field]) {
      ElMessage.error(field === "instance" ? "地址已存在" : "名称已存在");
    }
    if (dataFormRef.value) {
      dataFormRef.value.validateField(field);
    }
  });
};

// 表单提交
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
    (!dataForm.id ? baseService.post : baseService.put)("/ops/devicebackup", submitData).then((res) => {
      ElMessage.success({
        message: '成功',
        duration: 500,
        onClose: () => {
          visible.value = false;
          emit("refreshDataList");
        }
      });
    });
  });
};

defineExpose({
  init,
  initClone: (row: any) => {
    visible.value = true;
    if (dataFormRef.value) {
      dataFormRef.value.resetFields();
    }
    Object.assign(dataForm, row);
    dataForm.id = "";
    duplicateFlags.instance = false;
    duplicateFlags.name = false;
    loadBackupAgents();
  }
});
</script>
