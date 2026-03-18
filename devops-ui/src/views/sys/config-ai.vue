<template>
  <div class="config-grid config-grid--default">
    <el-card shadow="never" class="panel-card">
      <template #header>
        <div class="panel-card__header">
          <div class="panel-card__heading">
            <div class="panel-card__eyebrow">AI Gateway</div>
            <div class="panel-card__title">AI 服务连接配置</div>
            <div class="panel-card__desc">管理基础地址、模型和密钥，供后续 AI 能力统一调用。</div>
          </div>
          <el-tag size="small" :type="aiForm.status === 1 ? 'success' : 'info'">{{ aiForm.status === 1 ? "启用中" : "已禁用" }}</el-tag>
        </div>
      </template>
      <el-form ref="aiFormRef" :model="aiForm" :rules="aiRules" label-width="100px" class="config-form" :disabled="loading.save">
        <div class="status-banner" :class="{ 'status-banner--success': tested }">
          <div class="status-banner__label">当前步骤</div>
          <div class="status-banner__title">{{ tested ? "连接验证已通过，可以保存配置" : "请先完成连接验证，再保存配置" }}</div>
          <div class="status-banner__desc">建议在切换 Base URL、模型或 API Key 后重新验证一次。</div>
        </div>
        <el-form-item label="Base URL" prop="baseUrl">
          <el-input v-model="aiForm.baseUrl" placeholder="例如 https://api.nightyu.com 或 https://api.nightyu.com/v1"></el-input>
        </el-form-item>
        <el-form-item label="API Key" prop="apiKey">
          <el-input v-model="aiForm.apiKey" type="password" show-password placeholder="sk-xxx 或 Bearer sk-xxx"></el-input>
        </el-form-item>
        <el-form-item label="Model" prop="model">
          <el-input v-model="aiForm.model" placeholder="例如 gpt-5.4"></el-input>
        </el-form-item>
        <el-form-item label="状态" prop="status">
          <el-radio-group v-model="aiForm.status">
            <el-radio :label="1">启用</el-radio>
            <el-radio :label="0">禁用</el-radio>
          </el-radio-group>
        </el-form-item>
        <el-form-item class="action-row action-row--wrap">
          <el-button :loading="loading.test" @click="testAi">验证连接</el-button>
          <el-button type="primary" :disabled="!tested" :loading="loading.save" :class="{ 'btn-save-pulse': pulseSave }" @click="saveAi">
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
              <div class="panel-card__eyebrow">Readiness</div>
              <div class="panel-card__title">连接状态</div>
              <div class="panel-card__desc">快速确认验证结果、启用状态和当前模型。</div>
            </div>
            <el-tag size="small" :type="tested ? 'success' : 'info'">{{ tested ? "已验证" : "未验证" }}</el-tag>
          </div>
        </template>
        <div class="kv-list">
          <div class="kv-item">
            <span class="kv-item__label">连接验证</span>
            <span class="kv-item__value">{{ tested ? "已通过" : "未通过" }}</span>
          </div>
          <div class="kv-item">
            <span class="kv-item__label">启用状态</span>
            <span class="kv-item__value">{{ aiForm.status === 1 ? "启用" : "禁用" }}</span>
          </div>
          <div class="kv-item">
            <span class="kv-item__label">模型</span>
            <span class="kv-item__value">{{ aiForm.model || "-" }}</span>
          </div>
        </div>
      </el-card>

      <el-card shadow="never" class="panel-card panel-card--muted">
        <div class="info-list">
          <div class="info-list__title">操作说明</div>
          <div class="info-list__item">修改任意字段后会重置"已验证"状态。</div>
          <div class="info-list__item">验证接口会调用 <code>/v1/responses</code> 并校验是否能正常返回文本。</div>
          <div class="info-list__item">API Key 支持直接填 <code>sk-xxx</code>，服务端会自动补齐 <code>Bearer</code> 前缀。</div>
          <div class="info-list__item">建议只保留当前实际使用的模型，避免测试成功后误存无效配置。</div>
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

const aiFormRef = ref();

const aiForm = reactive({
  baseUrl: "",
  apiKey: "",
  model: "",
  status: 0
});

const { loading, withLoading } = useLoadingState({
  test: false,
  save: false
});

const tested = ref(false);
const { pulseSave, triggerSavePulse } = useSavePulse();

const aiDefaults = {
  baseUrl: "",
  apiKey: "",
  model: "",
  status: 0
};

const aiRules = {
  baseUrl: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  apiKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  model: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
};

watch(tested, (val) => {
  if (val) {
    triggerSavePulse();
  }
});

watch(
  () => [aiForm.baseUrl, aiForm.apiKey, aiForm.model, aiForm.status],
  () => {
    tested.value = false;
  }
);

const loadAi = () => {
  baseService.get("/sys/config-center/ai").then((res) => {
    if (res.data) {
      assignConfig(aiForm, aiDefaults, res.data);
    }
    tested.value = false;
  });
};

const testAi = async () => {
  const valid = await validateForm(aiFormRef);
  if (!valid) return;
  await withLoading("test", () =>
    baseService.post("/sys/config-center/ai/test", { ...aiForm }).then(() => {
      tested.value = true;
      ElMessage.success({ message: "连接验证成功", duration: MESSAGE_DURATION.success });
    })
  );
};

const saveAi = async () => {
  if (!tested.value) {
    ElMessage.warning({ message: "请先完成连接验证", duration: MESSAGE_DURATION.warning });
    return;
  }
  const valid = await validateForm(aiFormRef);
  if (!valid) return;
  await withLoading("save", () =>
    baseService.put("/sys/config-center/ai", { ...aiForm }).then(() => {
      ElMessage.success({ message: "配置已保存", duration: MESSAGE_DURATION.success });
      tested.value = true;
      loadAi();
    })
  );
};

onMounted(() => {
  loadAi();
});
</script>

<style scoped>
@import "./config-shared.css";
</style>
