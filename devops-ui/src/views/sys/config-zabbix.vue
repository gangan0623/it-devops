<template>
  <div class="config-grid config-grid--default">
    <el-card shadow="never" class="panel-card">
      <template #header>
        <div class="panel-card__header">
          <div class="panel-card__heading">
            <div class="panel-card__eyebrow">Monitor Bridge</div>
            <div class="panel-card__title">Zabbix 连接配置</div>
            <div class="panel-card__desc">维护 API 连接、连通性验证和后续映射依赖。</div>
          </div>
          <el-tag size="small" :type="zabbixForm.status === 1 ? 'success' : 'info'">{{ zabbixForm.status === 1 ? "启用中" : "已禁用" }}</el-tag>
        </div>
      </template>
      <el-form ref="zabbixFormRef" :model="zabbixForm" :rules="zabbixRules" label-width="110px" class="config-form" :disabled="loading.save">
        <div class="status-banner" :class="{ 'status-banner--success': tested }">
          <div class="status-banner__label">当前步骤</div>
          <div class="status-banner__title">{{ tested ? "连接验证已通过，可以保存配置" : "请先完成连接验证，再保存配置" }}</div>
          <div class="status-banner__desc">推荐顺序：验证连接 -> 检查版本 -> 保存配置。</div>
        </div>
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
          <el-button :loading="loading.test" @click="testZabbix">验证连接</el-button>
          <el-button :loading="loading.version" @click="checkZabbixVersion">检查版本</el-button>
          <el-button type="primary" :disabled="!tested" :loading="loading.save" :class="{ 'btn-save-pulse': pulseSave }" @click="saveZabbix">
            保存配置
          </el-button>
        </el-form-item>
        <div class="action-tip" :class="{ 'action-tip--ok': tested }">
          {{ tested ? "连接验证通过，可保存配置" : "请先完成连接验证，验证通过后才能保存配置" }}
        </div>
      </el-form>
    </el-card>

    <div class="stack-panels">
      <el-card shadow="never" class="panel-card panel-card--highlight">
        <template #header>
          <div class="panel-card__header">
            <div class="panel-card__heading">
              <div class="panel-card__eyebrow">Version</div>
              <div class="panel-card__title">版本状态</div>
              <div class="panel-card__desc">用于判断当前接入环境是否存在可升级版本。</div>
            </div>
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
            查看更新
          </el-button>
          <el-button :loading="loading.version" @click="checkZabbixVersion">重新检查</el-button>
        </div>
        <div class="info-list info-list--inline">
          <div class="info-list__title">推荐流程</div>
          <div class="info-list__item">先验证连接，再确认版本状态，最后保存配置。</div>
        </div>
      </el-card>

      <el-card shadow="never" class="panel-card panel-card--muted">
        <div class="info-list">
          <div class="info-list__title">操作说明</div>
          <div class="info-list__item">建议先完成并保存 Zabbix 连接配置，再维护映射规则。</div>
          <div class="info-list__item">如账号、地址或状态发生变化，已验证状态会自动失效。</div>
        </div>
      </el-card>
    </div>
  </div>
</template>

<script lang="ts" setup>
import { onMounted, reactive, ref, watch } from "vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";
import { MESSAGE_DURATION, assignConfig, useLoadingState, useSavePulse, validateForm } from "./config-helpers";

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

const zabbixDefaults = {
  url: "",
  name: "Zabbix",
  username: "",
  password: "",
  status: 1
};

const zabbixVersionDefaults = {
  currentVersion: "",
  latestVersion: "",
  updateAvailable: null as null | number,
  upgradeUrl: "https://www.zabbix.com/download"
};

const { loading, withLoading } = useLoadingState({
  test: false,
  version: false,
  save: false
});

const tested = ref(false);
const { pulseSave, triggerSavePulse } = useSavePulse();

watch(tested, (val) => {
  if (val) {
    triggerSavePulse();
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

const loadZabbix = () => {
  baseService.get("/sys/config-center/zabbix").then((res) => {
    if (res.data) {
      assignConfig(zabbixForm, zabbixDefaults, res.data);
    }
    tested.value = false;
    loadZabbixVersion();
  });
};

const loadZabbixVersion = () => {
  withLoading("version", () =>
    baseService.get("/sys/config-center/zabbix/version").then((res) => {
      assignConfig(zabbixVersion, zabbixVersionDefaults, res.data);
    })
  );
};

const testZabbix = async () => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  await withLoading("test", () =>
    baseService.post("/sys/config-center/zabbix/test", { ...zabbixForm }).then(() => {
      tested.value = true;
      ElMessage.success({ message: "连接验证成功", duration: MESSAGE_DURATION.success });
      checkZabbixVersion(false);
    })
  );
};

const checkZabbixVersion = async (showSuccess = true) => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  await withLoading("version", () =>
    baseService.post("/sys/config-center/zabbix/version", { ...zabbixForm }).then((res) => {
      assignConfig(zabbixVersion, zabbixVersionDefaults, res.data);
      if (showSuccess) ElMessage.success({ message: "版本检查完成", duration: MESSAGE_DURATION.success });
    })
  );
};

const openZabbixUpgradeUrl = () => {
  window.open(zabbixVersion.upgradeUrl || "https://www.zabbix.com/download", "_blank");
};

const saveZabbix = async () => {
  if (!tested.value) {
    ElMessage.warning({ message: "请先完成连接验证", duration: MESSAGE_DURATION.warning });
    return;
  }
  const valid = await validateForm(zabbixFormRef);
  if (!valid) return;
  await withLoading("save", () =>
    baseService.put("/sys/config-center/zabbix", { ...zabbixForm }).then(() => {
      ElMessage.success({ message: "配置已保存", duration: MESSAGE_DURATION.success });
      tested.value = true;
      loadZabbix();
    })
  );
};

onMounted(() => {
  loadZabbix();
});
</script>

<style scoped>
@import "./config-shared.css";
</style>
