<template>
  <div class="config-grid config-grid--default">
    <el-card shadow="never" class="panel-card">
      <template #header>
        <div class="panel-card__header">
          <span>Zabbix连接配置</span>
          <el-tag size="small" :type="zabbixForm.status === 1 ? 'success' : 'info'">{{ zabbixForm.status === 1 ? "启用中" : "已禁用" }}</el-tag>
        </div>
      </template>
      <el-form ref="zabbixFormRef" :model="zabbixForm" :rules="zabbixRules" label-width="110px" class="config-form" :disabled="loading.save">
        <el-form-item label="Zabbix URL" prop="url">
          <el-input v-model="zabbixForm.url" placeholder="http://x.x.x.x/api_jsonrpc.php"></el-input>
        </el-form-item>
        <el-form-item label="名称" prop="name">
          <el-input v-model="zabbixForm.name" placeholder="例如 生产Zabbix"></el-input>
        </el-form-item>
        <el-form-item label="用户名" prop="username">
          <el-input v-model="zabbixForm.username" placeholder="用户名"></el-input>
        </el-form-item>
        <el-form-item label="密码" prop="password">
          <el-input v-model="zabbixForm.password" type="password" show-password placeholder="密码"></el-input>
        </el-form-item>
        <el-form-item label="状态" prop="status">
          <el-radio-group v-model="zabbixForm.status">
            <el-radio :label="1">启用</el-radio>
            <el-radio :label="0">禁用</el-radio>
          </el-radio-group>
        </el-form-item>
        <el-form-item class="action-row action-row--wrap">
          <el-button :loading="loading.test" @click="testZabbix">测试连接</el-button>
          <el-button :loading="loading.version" @click="checkZabbixVersion">检测版本</el-button>
          <el-button type="primary" :disabled="!tested" :loading="loading.save" :class="{ 'btn-save-pulse': pulseSave }" @click="saveZabbix">
            保存Zabbix配置
          </el-button>
        </el-form-item>
        <div class="action-tip" :class="{ 'action-tip--ok': tested }">
          {{ tested ? "连接测试通过，可保存" : "请先测试连接，测试成功后才能保存" }}
        </div>
      </el-form>
    </el-card>

    <div class="stack-panels">
      <el-card shadow="never" class="panel-card panel-card--highlight">
        <template #header>
          <div class="panel-card__header">
            <span>版本检测</span>
          </div>
        </template>
        <div class="kv-list">
          <div class="kv-item">
            <span class="kv-item__label">当前版本</span>
            <span class="kv-item__value">{{ zabbixVersion.currentVersion || "-" }}</span>
          </div>
          <div class="kv-item">
            <span class="kv-item__label">最新版本</span>
            <span class="kv-item__value">{{ zabbixVersion.latestVersion || "-" }}</span>
          </div>
          <div class="kv-item">
            <span class="kv-item__label">状态</span>
            <span class="kv-item__value">
              <el-tag v-if="zabbixVersion.updateAvailable === 1" type="warning" size="small">可更新</el-tag>
              <el-tag v-else-if="zabbixVersion.updateAvailable === 0" type="success" size="small">已最新</el-tag>
              <el-tag v-else size="small" type="info">未检测</el-tag>
            </span>
          </div>
        </div>
        <div class="side-actions">
          <el-button v-if="zabbixVersion.updateAvailable === 1 && zabbixVersion.upgradeUrl" type="primary" @click="openZabbixUpgradeUrl">
            前往更新
          </el-button>
          <el-button :loading="loading.version" @click="checkZabbixVersion">重新检测</el-button>
        </div>
        <div class="info-list info-list--inline">
          <div class="info-list__title">建议流程</div>
          <div class="info-list__item">先测试连接，再确认版本状态，最后保存配置。</div>
        </div>
      </el-card>

      <el-card shadow="never" class="panel-card panel-card--muted">
        <div class="info-list">
          <div class="info-list__title">提示</div>
          <div class="info-list__item">建议先完成并保存 Zabbix 连接配置，再维护映射规则。</div>
        </div>
      </el-card>
    </div>
  </div>
</template>

<script lang="ts" setup>
import { nextTick, onMounted, reactive, ref, watch } from "vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";

const zabbixFormRef = ref();

const zabbixForm = reactive({
  url: "",
  name: "Zabbix",
  username: "",
  password: "",
  status: 1
});

const zabbixVersion = reactive({
  currentVersion: "",
  latestVersion: "",
  updateAvailable: null as null | number,
  upgradeUrl: "https://www.zabbix.com/download"
});

const loading = reactive({
  test: false,
  version: false,
  save: false
});

const tested = ref(false);

const pulseSave = ref(false);

watch(tested, (val) => {
  if (val) {
    nextTick(() => {
      pulseSave.value = true;
      setTimeout(() => {
        pulseSave.value = false;
      }, 700);
    });
  }
});

const zabbixRules = {
  url: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  name: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  username: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  password: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
};

watch(
  () => [zabbixForm.url, zabbixForm.name, zabbixForm.username, zabbixForm.password, zabbixForm.status],
  () => {
    tested.value = false;
    zabbixVersion.updateAvailable = null;
  }
);

const validateForm = (formRef: any) =>
  new Promise<boolean>((resolve) => {
    formRef.value.validate((valid: boolean) => resolve(valid));
  });

const loadZabbix = () => {
  baseService.get("/sys/config-center/zabbix").then((res) => {
    if (res.data) {
      Object.assign(zabbixForm, { url: "", name: "Zabbix", username: "", password: "", status: 1 }, res.data);
    }
    tested.value = false;
    loadZabbixVersion();
  });
};

const loadZabbixVersion = () => {
  loading.version = true;
  baseService
    .get("/sys/config-center/zabbix/version")
    .then((res) => {
      Object.assign(
        zabbixVersion,
        { currentVersion: "", latestVersion: "", updateAvailable: null, upgradeUrl: "https://www.zabbix.com/download" },
        res.data || {}
      );
    })
    .finally(() => {
      loading.version = false;
    });
};

const testZabbix = async () => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  loading.test = true;
  baseService
    .post("/sys/config-center/zabbix/test", { ...zabbixForm })
    .then(() => {
      tested.value = true;
      ElMessage.success({ message: "Zabbix连接测试成功", duration: 2000 });
      checkZabbixVersion(false);
    })
    .finally(() => {
      loading.test = false;
    });
};

const checkZabbixVersion = async (showSuccess = true) => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  loading.version = true;
  baseService
    .post("/sys/config-center/zabbix/version", { ...zabbixForm })
    .then((res) => {
      Object.assign(
        zabbixVersion,
        { currentVersion: "", latestVersion: "", updateAvailable: null, upgradeUrl: "https://www.zabbix.com/download" },
        res.data || {}
      );
      if (showSuccess) ElMessage.success({ message: "Zabbix版本检测完成", duration: 2000 });
    })
    .finally(() => {
      loading.version = false;
    });
};

const openZabbixUpgradeUrl = () => {
  window.open(zabbixVersion.upgradeUrl || "https://www.zabbix.com/download", "_blank");
};

const saveZabbix = async () => {
  if (!tested.value) {
    ElMessage.warning({ message: "请先测试连接", duration: 3000 });
    return;
  }
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  loading.save = true;
  baseService
    .put("/sys/config-center/zabbix", { ...zabbixForm })
    .then(() => {
      ElMessage.success({ message: "Zabbix配置已保存", duration: 2000 });
      tested.value = true;
      loadZabbix();
    })
    .finally(() => {
      loading.save = false;
    });
};

onMounted(() => {
  loadZabbix();
});
</script>

<style scoped>
@import "./config-shared.css";
</style>
