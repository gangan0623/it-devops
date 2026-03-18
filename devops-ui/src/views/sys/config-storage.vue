<template>
  <div class="config-grid config-grid--storage">
    <el-card shadow="never" class="panel-card">
      <template #header>
        <div class="panel-card__header">
          <div class="panel-card__heading">
            <div class="panel-card__eyebrow">Storage</div>
            <div class="panel-card__title">存储连接配置</div>
            <div class="panel-card__desc">用于对象上传、访问地址拼接和服务端 MinIO API 连接。</div>
          </div>
          <el-tag size="small" type="info">必填</el-tag>
        </div>
      </template>
      <el-form ref="storageFormRef" :model="storageForm" :rules="storageRules" label-width="120px" class="config-form" :disabled="loading.storage">
        <el-form-item label="公开访问地址" prop="minioDomain">
          <el-input v-model="storageForm.minioDomain" placeholder="例如 https://minioback.example.com"></el-input>
        </el-form-item>
        <el-form-item label="基础文件夹" prop="minioPath">
          <el-input v-model="storageForm.minioPath" placeholder="例如 /devops"></el-input>
        </el-form-item>
        <el-form-item label="接口地址" prop="minioEndPoint">
          <el-input v-model="storageForm.minioEndPoint" placeholder="例如 https://minioback.example.com"></el-input>
        </el-form-item>
        <el-form-item label="访问密钥" prop="minioAccessKey">
          <el-input v-model="storageForm.minioAccessKey" placeholder="Access Key"></el-input>
        </el-form-item>
        <el-form-item label="私有密钥" prop="minioSecretKey">
          <el-input v-model="storageForm.minioSecretKey" type="password" show-password placeholder="Secret Key"></el-input>
        </el-form-item>
        <el-form-item label="存储桶名称" prop="minioBucketName">
          <el-input v-model="storageForm.minioBucketName" placeholder="Bucket Name"></el-input>
        </el-form-item>
        <el-form-item class="action-row">
          <el-button type="primary" :loading="loading.storage" @click="saveStorage">保存配置</el-button>
        </el-form-item>
      </el-form>
    </el-card>

    <div class="stack-panels">
      <el-card shadow="never" class="panel-card panel-card--danger">
        <template #header>
          <div class="panel-card__header">
            <div class="panel-card__heading">
              <div class="panel-card__eyebrow">Danger Zone</div>
              <div class="panel-card__title">对象删除</div>
              <div class="panel-card__desc">仅在确认对象无业务引用时使用，删除后不可恢复。</div>
            </div>
            <el-tag size="small" type="danger">危险操作</el-tag>
          </div>
        </template>
        <div class="danger-note">
          <div class="danger-note__title">删除前检查</div>
          <div class="danger-note__desc">请确认 URL 指向的是目标对象，而不是目录或已复用的公共资源。</div>
        </div>
        <el-form ref="deleteUrlFormRef" :model="deleteUrlForm" :rules="deleteUrlRules" label-width="84px" class="config-form config-form--compact">
          <el-form-item label="文件URL" prop="url">
            <el-input v-model="deleteUrlForm.url" placeholder="请输入完整MinIO访问URL"></el-input>
          </el-form-item>
          <el-form-item class="action-row">
            <el-button type="danger" :loading="loading.deleteUrl" @click="deleteByUrl">执行删除</el-button>
          </el-form-item>
        </el-form>
      </el-card>

      <el-card shadow="never" class="panel-card panel-card--muted">
        <div class="info-list">
          <div class="info-list__title">操作说明</div>
          <div class="info-list__item">公开访问地址用于拼接外部访问 URL。</div>
          <div class="info-list__item">接口地址用于服务端连接 MinIO API。</div>
          <div class="info-list__item">删除操作会根据 URL 解析对象路径并执行实际删除。</div>
          <div class="info-list__item">推荐先保存连接参数，再执行对象清理类操作。</div>
        </div>
      </el-card>
    </div>
  </div>
</template>

<script lang="ts" setup>
import { onMounted, reactive, ref } from "vue";
import baseService from "@/service/baseService";
import { ElMessage, ElMessageBox } from "element-plus";
import { MESSAGE_DURATION, assignConfig, useLoadingState, validateForm } from "./config-helpers";

const storageFormRef = ref();
const deleteUrlFormRef = ref();

const storageForm = reactive({
  type: 4,
  minioDomain: "",
  minioPath: "",
  minioEndPoint: "",
  minioAccessKey: "",
  minioSecretKey: "",
  minioBucketName: ""
});

const deleteUrlForm = reactive({
  url: ""
});

const { loading, withLoading } = useLoadingState({
  storage: false,
  deleteUrl: false
});

const storageRules = {
  minioDomain: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioPath: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioEndPoint: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioAccessKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioSecretKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioBucketName: [{ required: true, message: "必填项不能为空", trigger: "blur" }]
};

const deleteUrlRules = {
  url: [{ required: true, message: "必填项不能为空", trigger: "blur" }]
};

const storageDefaults = {
  type: 4,
  minioDomain: "",
  minioPath: "",
  minioEndPoint: "",
  minioAccessKey: "",
  minioSecretKey: "",
  minioBucketName: ""
};

const loadStorage = () => {
  baseService.get("/sys/config-center/storage").then((res) => {
    if (res.data) {
      assignConfig(storageForm, storageDefaults, res.data);
    }
  });
};

const saveStorage = async () => {
  const valid = await validateForm(storageFormRef);
  if (!valid) return;
  await withLoading("storage", () =>
    baseService.put("/sys/config-center/storage", { ...storageForm, type: 4 }).then(() => {
      ElMessage.success({ message: "配置已保存", duration: MESSAGE_DURATION.success });
    })
  );
};

const deleteByUrl = async () => {
  const valid = await validateForm(deleteUrlFormRef);
  if (!valid) return;
  try {
    await ElMessageBox.confirm(
      "确定要删除该对象吗？此操作不可恢复。",
      "危险操作",
      { confirmButtonText: "确定删除", cancelButtonText: "取消", type: "warning" }
    );
  } catch {
    return;
  }
  await withLoading("deleteUrl", () =>
    baseService.post("/sys/config-center/storage/delete-by-url", { url: deleteUrlForm.url }).then(() => {
      ElMessage.success({ message: "对象已删除", duration: MESSAGE_DURATION.success });
      deleteUrlForm.url = "";
    })
  );
};

onMounted(() => {
  loadStorage();
});
</script>

<style scoped>
@import "./config-shared.css";
</style>
