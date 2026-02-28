<template>
  <el-form :model="dataForm" :rules="rules" ref="dataFormRef" @keyup.enter="dataFormSubmitHandle()" label-width="120px">
    <el-form-item label="账号">
      <span>{{ user.username }}</span>
    </el-form-item>
    <el-form-item prop="realName" label="真实姓名">
      <el-input v-model="dataForm.realName" placeholder="真实姓名"></el-input>
    </el-form-item>
    <el-form-item prop="gender" label="性别">
      <el-radio-group v-model="dataForm.gender">
        <el-radio :label="0">男</el-radio>
        <el-radio :label="1">女</el-radio>
        <el-radio :label="2">保密</el-radio>
      </el-radio-group>
    </el-form-item>
    <el-form-item prop="email" label="邮箱">
      <el-input v-model="dataForm.email" placeholder="邮箱"></el-input>
    </el-form-item>
    <el-form-item prop="mobile" label="手机号">
      <el-input v-model="dataForm.mobile" placeholder="手机号"></el-input>
    </el-form-item>
    <el-form-item prop="password" label="原密码">
      <el-input v-model="dataForm.password" type="password" placeholder="原密码" show-password></el-input>
    </el-form-item>
    <el-form-item prop="newPassword" label="新密码">
      <el-input v-model="dataForm.newPassword" type="password" placeholder="新密码" show-password></el-input>
    </el-form-item>
    <el-form-item prop="confirmPassword" label="确认密码">
      <el-input v-model="dataForm.confirmPassword" type="password" placeholder="确认密码" show-password></el-input>
    </el-form-item>
    <el-form-item>
      <el-button type="primary" @click="dataFormSubmitHandle">确定</el-button>
    </el-form-item>
  </el-form>
</template>

<script lang="ts" setup>
import {computed, onMounted, reactive, ref} from "vue";
import {IObject} from "@/types/interface";
import baseService from "@/service/baseService";
import {useAppStore} from "@/store";
import {ElMessage} from "element-plus";
import {isEmail, isMobile} from "@/utils/utils";

const dataFormRef = ref();
const store = useAppStore();
const user = computed(() => store.state.user);

const dataForm = reactive({
  realName: "",
  gender: 0,
  email: "",
  mobile: "",
  password: "",
  newPassword: "",
  confirmPassword: ""
});

const initDataForm = () => {
  dataForm.realName = user.value?.realName || "";
  dataForm.gender = typeof user.value?.gender === "number" ? user.value.gender : 0;
  dataForm.email = user.value?.email || "";
  dataForm.mobile = user.value?.mobile || "";
  dataForm.password = "";
  dataForm.newPassword = "";
  dataForm.confirmPassword = "";
};

const validateConfirmPassword = (rule: IObject, value: string, callback: (e?: Error) => any) => {
  if (dataForm.newPassword !== value) {
    return callback(new Error("确认密码与新密码输入不一致"));
  }
  callback();
};

const validateEmail = (rule: IObject, value: string, callback: (e?: Error) => any) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (!isEmail(value)) {
    return callback(new Error("邮箱格式错误"));
  }
  callback();
};

const validateMobile = (rule: IObject, value: string, callback: (e?: Error) => any) => {
  if (!value) {
    return callback(new Error("必填项不能为空"));
  }
  if (!isMobile(value)) {
    return callback(new Error("手机格式错误"));
  }
  callback();
};

const rules = ref({
  realName: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  gender: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  email: [{ validator: validateEmail, trigger: "blur" }],
  mobile: [{ validator: validateMobile, trigger: "blur" }],
  password: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  newPassword: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  confirmPassword: [
    { required: true, message: "必填项不能为空", trigger: "blur" },
    { validator: validateConfirmPassword, trigger: "blur" }
  ]
});

// 表单提交
const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    baseService.put("/sys/user/profile", {
      realName: dataForm.realName,
      gender: dataForm.gender,
      email: dataForm.email,
      mobile: dataForm.mobile,
      password: dataForm.password,
      newPassword: dataForm.newPassword
    }).then(() => {
      store.updateState({
        user: {
          ...store.state.user,
          realName: dataForm.realName,
          gender: dataForm.gender,
          email: dataForm.email,
          mobile: dataForm.mobile
        }
      });
      ElMessage.success("成功");
      initDataForm();
    });
  });
};

onMounted(() => {
  initDataForm();
});
</script>
