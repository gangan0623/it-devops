<template>
  <el-dialog v-model="visible" :title="!dataForm.id ? '新增Linux主机' : '修改Linux主机'" width="760px" :close-on-click-modal="false" :close-on-press-escape="false">
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px" class="dialog-form">
      <div class="form-section-title">主机信息</div>
      <el-form-item label="地址" prop="instance">
        <div class="host-instance">
          <el-input v-model="dataForm.ip" placeholder="IP" @blur="handleInstanceBlur"></el-input>
          <span class="host-instance__sep">:</span>
          <el-input v-model="dataForm.port" placeholder="9100" class="host-instance__port" @blur="handleInstanceBlur"></el-input>
        </div>
      </el-form-item>
          <el-form-item label="名称" prop="name">
        <el-input v-model="dataForm.name" placeholder="名称" @blur="checkUnique('name')"></el-input>
      </el-form-item>
      <el-form-item label="区域名称" prop="areaName">
        <ren-select
          v-model="dataForm.areaName"
          dict-type="area_name_type"
          label-field="dictValue"
          value-field="dictLabel"
          placeholder="区域名称"
        ></ren-select>
      </el-form-item>
      <el-form-item label="站点位置" prop="siteLocation">
        <ren-select
          v-model="dataForm.siteLocation"
          dict-type="base_site_location"
          label-field="dictValue"
          value-field="dictLabel"
          placeholder="站点位置"
        ></ren-select>
      </el-form-item>
      <div class="form-section-title">资产归属</div>
      <el-form-item label="分组名称" prop="menuName">
        <ren-select
          v-model="dataForm.menuName"
          dict-type="server_host_group"
          label-field="dictValue"
          value-field="dictLabel"
          placeholder="分组名称"
        ></ren-select>
      </el-form-item>
      <el-form-item label="主机类型" prop="type">
        <ren-select
          v-model="dataForm.type"
          dict-type="server_machine_type"
          label-field="dictValue"
          value-field="dictLabel"
          placeholder="主机类型"
        ></ren-select>
      </el-form-item>
          <el-form-item label="子组名称" prop="subMenuName">
        <el-input v-model="dataForm.subMenuName" placeholder="子组名称"></el-input>
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
  instance: "",
  ip: "",
  port: "9100",
  name: "",
  areaName: "",
  siteLocation: "",
  menuName: "",
  subMenuName: "",
  type: "",
  status: ""
});

const duplicateFlags = reactive({
  instance: false,
  name: false
});

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

const rules = ref({
          instance: [
      { validator: validateInstance, trigger: 'blur' }
    ],
          name: [
      { validator: validateName, trigger: 'blur' }
    ],
          areaName: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          menuName: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          type: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          subMenuName: [
      { required: true, message: '必填项不能为空', trigger: 'blur' }
    ],
          status: [
      { required: true, message: '必填项不能为空', trigger: 'change' }
    ],
          });

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";
  dataForm.status = "";
  dataForm.ip = "";
  dataForm.port = "9100";
  duplicateFlags.instance = false;
  duplicateFlags.name = false;

  // 重置表单数据
  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }

  if (id) {
    getInfo(id);
  }
};

// 获取信息
const getInfo = (id: number) => {
  baseService.get("/ops/linuxhost/" + id).then((res) => {
    Object.assign(dataForm, res.data);
    const parsed = parseInstance(dataForm.instance);
    dataForm.ip = parsed.ip;
    dataForm.port = parsed.port;
    duplicateFlags.instance = false;
    duplicateFlags.name = false;
  });
};

const handleInstanceBlur = () => {
  dataForm.instance = buildInstance();
  checkUnique("instance");
};

const checkUnique = (field: "instance" | "name") => {
  const value = field === "instance" ? buildInstance() : dataForm[field];
  if (!value) {
    duplicateFlags[field] = false;
    return;
  }
  const params: Record<string, any> = { id: dataForm.id };
  params[field] = value;
  baseService.get("/ops/linuxhost/check", params).then((res) => {
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
  dataForm.instance = buildInstance();
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    const submitData = { ...dataForm } as Record<string, any>;
    submitData.instance = buildInstance();
    delete submitData.ip;
    delete submitData.port;
    delete submitData.creator;
    delete submitData.createDate;
    delete submitData.updater;
    delete submitData.updateDate;
    submitLoading.value = true;
    (!dataForm.id ? baseService.post : baseService.put)("/ops/linuxhost", submitData)
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
  init,
  initClone: (row: any) => {
    visible.value = true;
    if (dataFormRef.value) {
      dataFormRef.value.resetFields();
    }
    Object.assign(dataForm, row);
    const parsed = parseInstance(dataForm.instance);
    dataForm.ip = parsed.ip;
    dataForm.port = parsed.port;
    dataForm.id = "";
    duplicateFlags.instance = false;
    duplicateFlags.name = false;
  }
});

const buildInstance = () => {
  const rawIp = (dataForm.ip || "").trim();
  if (!rawIp) {
    return "";
  }
  const rawPort = String(dataForm.port || "").trim();
  if (rawIp.includes(":") && !rawPort) {
    return rawIp;
  }
  const ipPart = rawIp.includes(":") ? rawIp.split(":")[0] : rawIp;
  const port = rawPort || "9100";
  return `${ipPart}:${port}`;
};

const parseInstance = (instance: string) => {
  const result = { ip: "", port: "9100" };
  if (!instance) {
    return result;
  }
  const value = String(instance);
  const idx = value.lastIndexOf(":");
  if (idx > 0 && idx < value.length - 1) {
    result.ip = value.substring(0, idx);
    result.port = value.substring(idx + 1);
    return result;
  }
  result.ip = value;
  return result;
};
</script>

<style scoped>
.host-instance {
  display: flex;
  align-items: center;
  width: 100%;
}
.host-instance__sep {
  margin: 0 8px;
  color: #94a3b8;
}
.host-instance__port {
  width: 120px;
  flex: 0 0 120px;
}
</style>
