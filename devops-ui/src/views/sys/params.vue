<template>
  <div class="mod-sys__config-center">
    <el-tabs v-model="activeTab" class="config-tabs">
      <el-tab-pane label="存储配置" name="storage">
        <div class="config-grid config-grid--storage">
          <el-card shadow="never" class="panel-card">
            <template #header>
              <div class="panel-card__header">
                <span>MinIO连接参数</span>
                <el-tag size="small" type="info">必填</el-tag>
              </div>
            </template>
            <el-form ref="storageFormRef" :model="storageForm" :rules="storageRules" label-width="120px" class="config-form">
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
                <el-button type="primary" :loading="loading.storage" @click="saveStorage">保存存储配置</el-button>
              </el-form-item>
            </el-form>
          </el-card>

          <div class="stack-panels">
            <el-card shadow="never" class="panel-card">
              <template #header>
                <div class="panel-card__header">
                  <span>按URL删除MinIO对象</span>
                  <el-tag size="small" type="danger">危险操作</el-tag>
                </div>
              </template>
              <el-form ref="deleteUrlFormRef" :model="deleteUrlForm" :rules="deleteUrlRules" label-width="84px" class="config-form config-form--compact">
                <el-form-item label="文件URL" prop="url">
                  <el-input v-model="deleteUrlForm.url" placeholder="请输入完整MinIO访问URL"></el-input>
                </el-form-item>
                <el-form-item class="action-row">
                  <el-button type="danger" :loading="loading.deleteUrl" @click="deleteByUrl">删除MinIO对象</el-button>
                </el-form-item>
              </el-form>
            </el-card>

            <el-card shadow="never" class="panel-card panel-card--muted">
              <div class="info-list">
                <div class="info-list__title">使用说明</div>
                <div class="info-list__item">公开访问地址用于拼接外部访问 URL。</div>
                <div class="info-list__item">接口地址用于服务端连接 MinIO API。</div>
                <div class="info-list__item">删除操作会根据 URL 解析对象路径并执行实际删除。</div>
              </div>
            </el-card>
          </div>
        </div>
      </el-tab-pane>

      <el-tab-pane label="Zabbix配置" name="zabbix">
        <div class="config-grid">
          <el-card shadow="never" class="panel-card">
            <template #header>
              <div class="panel-card__header">
                <span>Zabbix连接配置</span>
                <el-tag size="small" :type="zabbixForm.status === 1 ? 'success' : 'info'">{{ zabbixForm.status === 1 ? "启用中" : "已禁用" }}</el-tag>
              </div>
            </template>
            <el-form ref="zabbixFormRef" :model="zabbixForm" :rules="zabbixRules" label-width="110px" class="config-form">
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
                <el-button :loading="loading.zabbixTest" @click="testZabbix">测试连接</el-button>
                <el-button :loading="loading.zabbixVersion" @click="checkZabbixVersion">检测版本</el-button>
                <el-button type="primary" :disabled="!tested.zabbix" :loading="loading.zabbixSave" @click="saveZabbix">
                  保存Zabbix配置
                </el-button>
              </el-form-item>
              <div class="action-tip" :class="{ 'action-tip--ok': tested.zabbix }">
                {{ tested.zabbix ? "连接测试通过，可保存" : "请先测试连接，测试成功后才能保存" }}
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
                <el-button :loading="loading.zabbixVersion" @click="checkZabbixVersion">重新检测</el-button>
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
      </el-tab-pane>

      <el-tab-pane label="Zabbix映射" name="zabbixMapping">
        <div class="mapping-workbench">
          <div class="mapping-config-pane">
            <el-card shadow="never" class="panel-card mapping-config-card">
              <template #header>
                <div class="panel-card__header">
                  <span>映射配置</span>
                  <el-tag size="small" type="info">{{ zabbixMappingForm.zabbixName || zabbixForm.name || "Zabbix" }}</el-tag>
                </div>
              </template>
              <div class="mapping-metrics">
                <div class="mapping-metric">
                  <div class="mapping-metric__label">主机群组</div>
                  <div class="mapping-metric__value">{{ mappingStats.hostGroupCount }}</div>
                </div>
                <div class="mapping-metric">
                  <div class="mapping-metric__label">模板规则</div>
                  <div class="mapping-metric__value">{{ mappingStats.templateRuleCount }}</div>
                </div>
                <div class="mapping-metric">
                  <div class="mapping-metric__label">分类规则</div>
                  <div class="mapping-metric__value">{{ mappingStats.categoryRuleCount }}</div>
                </div>
                <div class="mapping-metric">
                  <div class="mapping-metric__label">区域规则</div>
                  <div class="mapping-metric__value">{{ mappingStats.areaRuleCount }}</div>
                </div>
              </div>
              <div class="mapping-hostgroup">
                <div class="mapping-hostgroup__title">主机群组（多选）</div>
                <el-select
                  v-model="zabbixMappingForm.selectedHostGroupIds"
                  multiple
                  collapse-tags
                  collapse-tags-tooltip
                  filterable
                  placeholder="请选择Zabbix主机群组"
                  style="width: 100%"
                >
                  <el-option
                    v-for="item in zabbixMappingOptions.hostGroupOptions"
                    :key="item.groupId"
                    :label="item.name"
                    :value="item.groupId"
                  />
                </el-select>
              </div>
              <div class="mapping-actions">
                <el-button :loading="loading.zabbixMappingPreview" @click="previewZabbixMapping">预览映射</el-button>
                <el-button type="success" :loading="loading.zabbixMappingSync" @click="syncZabbixNetworkHosts">同步</el-button>
                <el-button type="primary" :loading="loading.zabbixMappingSave" @click="saveZabbixMapping">保存</el-button>
              </div>
            </el-card>

            <el-card shadow="never" class="panel-card mapping-rules-card">
              <el-tabs v-model="activeRuleTab" class="mapping-rule-tabs" stretch>
                <el-tab-pane label="模板型号" name="template">
                  <div class="mapping-rule-list">
                    <div class="mapping-rule-scroll">
                      <div v-for="(item, idx) in zabbixMappingForm.templateModelRules" :key="`tm-${idx}`" class="mapping-rule-row">
                        <el-select v-model="item.templateName" filterable placeholder="Zabbix模板">
                          <el-option v-for="v in zabbixMappingOptions.templateOptions" :key="v" :label="v" :value="v" />
                        </el-select>
                          <el-select v-model="item.deviceModel" placeholder="设备型号">
                          <el-option v-for="v in zabbixMappingOptions.deviceModelOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                        </el-select>
                        <el-button link type="danger" @click="removeTemplateModelRule(idx)">删除</el-button>
                      </div>
                      <div v-if="!zabbixMappingForm.templateModelRules.length" class="rule-empty">暂无规则</div>
                    </div>
                    <el-button link type="primary" @click="addTemplateModelRule">+ 新增模板规则</el-button>
                  </div>
                </el-tab-pane>

                <el-tab-pane label="分类映射" name="category">
                  <div class="mapping-rule-list">
                    <div class="mapping-rule-scroll">
                      <div v-for="(item, idx) in zabbixMappingForm.categoryGroupRules" :key="`cg-${idx}`" class="mapping-rule-row">
                        <el-input v-model="item.zabbixCategory" placeholder="Zabbix分类，如 网络设备" />
                        <el-select v-model="item.deviceGroup" placeholder="设备分组">
                          <el-option v-for="v in zabbixMappingOptions.deviceGroupOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                        </el-select>
                        <el-button link type="danger" @click="removeCategoryGroupRule(idx)">删除</el-button>
                      </div>
                      <div v-if="!zabbixMappingForm.categoryGroupRules.length" class="rule-empty">暂无规则</div>
                    </div>
                    <el-button link type="primary" @click="addCategoryGroupRule">+ 新增分类规则</el-button>
                  </div>
                </el-tab-pane>

                <el-tab-pane label="区域关键字" name="area">
                  <div class="mapping-rule-list">
                    <div class="mapping-rule-scroll">
                      <div v-for="(item, idx) in zabbixMappingForm.areaKeywordRules" :key="`ak-${idx}`" class="mapping-rule-row">
                        <el-input v-model="item.keyword" placeholder="关键字，如 安徽新能源 / 销售公司" />
                        <el-select v-model="item.areaName" filterable placeholder="区域（area_name_type）">
                          <el-option v-for="v in zabbixMappingOptions.areaOptions" :key="v.dictLabel" :label="v.dictValue" :value="v.dictLabel" />
                        </el-select>
                        <el-button link type="danger" @click="removeAreaKeywordRule(idx)">删除</el-button>
                      </div>
                      <div v-if="!zabbixMappingForm.areaKeywordRules.length" class="rule-empty">暂无规则</div>
                    </div>
                    <el-button link type="primary" @click="addAreaKeywordRule">+ 新增区域规则</el-button>
                  </div>
                </el-tab-pane>
              </el-tabs>
            </el-card>
          </div>

          <div class="mapping-preview-pane">
            <el-card shadow="never" class="panel-card panel-card--muted">
              <template #header>
                <div class="panel-card__header">
                  <span>映射预览</span>
                  <div class="preview-header-tags">
                    <el-tag size="small" type="info">总计 {{ zabbixMappingPreview.groupList.length }}</el-tag>
                    <el-tag v-if="zabbixMappingPreview.unmatchedAreas.length" type="warning" size="small">
                      未映射 {{ zabbixMappingPreview.unmatchedAreas.length }}
                    </el-tag>
                    <el-tag v-else size="small" type="success">已匹配</el-tag>
                  </div>
                </div>
              </template>
              <div class="preview-toolbar">
                <el-switch v-model="previewFilter.onlyUnmatched" inline-prompt active-text="仅未映射" inactive-text="全部" />
                <el-input v-model="previewFilter.keyword" placeholder="筛选：群组/分类/区域" clearable />
                <el-button :loading="loading.zabbixMappingPreview" @click="previewZabbixMapping">刷新预览</el-button>
              </div>
              <div v-if="filteredPreviewRows.length" class="mapping-preview">
                <el-table :data="filteredPreviewRows" :row-class-name="previewRowClassName" size="small" border height="100%">
                  <el-table-column prop="groupName" label="主机群组" min-width="260" />
                  <el-table-column prop="zabbixCategory" label="分类" width="120" />
                  <el-table-column prop="rawAreaSegment" label="原区域" min-width="200" />
                  <el-table-column prop="matchedAreaName" label="映射区域" width="140" />
                </el-table>
              </div>
              <div v-else class="info-list">
                <div class="info-list__item">暂无可显示数据，请调整筛选条件或点击“刷新预览”。</div>
              </div>
              <div v-if="zabbixMappingPreview.unmatchedAreas.length" class="unmatched-list">
                未映射区域：{{ zabbixMappingPreview.unmatchedAreas.join("、") }}
              </div>
            </el-card>
          </div>
        </div>
      </el-tab-pane>

      <el-tab-pane label="AI配置" name="ai">
        <div class="config-grid">
          <el-card shadow="never" class="panel-card">
            <template #header>
              <div class="panel-card__header">
                <span>AI服务连接配置</span>
                <el-tag size="small" :type="aiForm.status === 1 ? 'success' : 'info'">{{ aiForm.status === 1 ? "启用中" : "已禁用" }}</el-tag>
              </div>
            </template>
            <el-form ref="aiFormRef" :model="aiForm" :rules="aiRules" label-width="100px" class="config-form">
              <el-form-item label="Base URL" prop="baseUrl">
                <el-input v-model="aiForm.baseUrl" placeholder="例如 https://api.openai.com/v1"></el-input>
              </el-form-item>
              <el-form-item label="API Key" prop="apiKey">
                <el-input v-model="aiForm.apiKey" type="password" show-password placeholder="API Key"></el-input>
              </el-form-item>
              <el-form-item label="Model" prop="model">
                <el-input v-model="aiForm.model" placeholder="例如 gpt-4o-mini"></el-input>
              </el-form-item>
              <el-form-item label="状态" prop="status">
                <el-radio-group v-model="aiForm.status">
                  <el-radio :label="1">启用</el-radio>
                  <el-radio :label="0">禁用</el-radio>
                </el-radio-group>
              </el-form-item>
              <el-form-item class="action-row action-row--wrap">
                <el-button :loading="loading.aiTest" @click="testAi">测试连接</el-button>
                <el-button type="primary" :disabled="!tested.ai" :loading="loading.aiSave" @click="saveAi">
                  保存AI配置
                </el-button>
              </el-form-item>
              <div class="action-tip" :class="{ 'action-tip--ok': tested.ai }">
                {{ tested.ai ? "连接测试通过，可保存" : "请先测试连接，测试成功后才能保存" }}
              </div>
            </el-form>
          </el-card>

          <div class="stack-panels">
            <el-card shadow="never" class="panel-card panel-card--highlight">
              <template #header>
                <div class="panel-card__header">
                  <span>当前状态</span>
                  <el-tag size="small" :type="tested.ai ? 'success' : 'info'">{{ tested.ai ? "已测试" : "未测试" }}</el-tag>
                </div>
              </template>
              <div class="kv-list">
                <div class="kv-item">
                  <span class="kv-item__label">连接测试</span>
                  <span class="kv-item__value">{{ tested.ai ? "通过" : "未通过" }}</span>
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
                <div class="info-list__title">提示</div>
                <div class="info-list__item">修改任意字段后会重置“已测试”状态。</div>
                <div class="info-list__item">测试接口会校验模型是否存在且当前 Key 有权限访问。</div>
              </div>
            </el-card>
          </div>
        </div>
      </el-tab-pane>
    </el-tabs>

  </div>
</template>

<script lang="ts" setup>
import { computed, onMounted, reactive, ref, watch } from "vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";

const activeTab = ref("storage");

const storageFormRef = ref();
const zabbixFormRef = ref();
const aiFormRef = ref();
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

const zabbixForm = reactive({
  url: "",
  name: "Zabbix",
  username: "",
  password: "",
  status: 1
});

const deleteUrlForm = reactive({
  url: ""
});

const zabbixVersion = reactive({
  currentVersion: "",
  latestVersion: "",
  updateAvailable: null as null | number,
  upgradeUrl: "https://www.zabbix.com/download"
});

const zabbixMappingForm = reactive({
  zabbixName: "Zabbix",
  selectedHostGroupIds: [] as string[],
  templateModelRules: [] as Array<{ templateName: string; deviceModel: string }>,
  categoryGroupRules: [] as Array<{ zabbixCategory: string; deviceGroup: string }>,
  areaKeywordRules: [] as Array<{ keyword: string; areaName: string }>
});

const zabbixMappingOptions = reactive({
  zabbixName: "Zabbix",
  templateOptions: [] as string[],
  hostGroupOptions: [] as Array<{ groupId: string; name: string }>,
  areaOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceGroupOptions: [] as Array<{ dictLabel: string; dictValue: string }>,
  deviceModelOptions: [] as Array<{ dictLabel: string; dictValue: string }>
});

const zabbixMappingPreview = reactive({
  groupList: [] as Array<{
    groupId: string;
    groupName: string;
    zabbixCategory: string;
    rawAreaSegment: string;
    normalizedAreaKeyword: string;
    matchedAreaName: string;
    matched: number;
  }>,
  unmatchedAreas: [] as string[]
});

const activeRuleTab = ref("area");

const previewFilter = reactive({
  onlyUnmatched: false,
  keyword: ""
});

const mappingStats = computed(() => ({
  hostGroupCount: zabbixMappingForm.selectedHostGroupIds.length,
  templateRuleCount: zabbixMappingForm.templateModelRules.length,
  categoryRuleCount: zabbixMappingForm.categoryGroupRules.length,
  areaRuleCount: zabbixMappingForm.areaKeywordRules.length
}));

const filteredPreviewRows = computed(() => {
  const keyword = previewFilter.keyword.trim();
  return zabbixMappingPreview.groupList.filter((row) => {
    if (previewFilter.onlyUnmatched && Number(row.matched) === 1) {
      return false;
    }
    if (!keyword) {
      return true;
    }
    return [row.groupName, row.zabbixCategory, row.rawAreaSegment, row.matchedAreaName].some((v) => String(v || "").includes(keyword));
  });
});

const previewRowClassName = ({ row }: { row: { matched: number } }) => (Number(row.matched) === 1 ? "" : "preview-row--unmatched");

const aiForm = reactive({
  baseUrl: "",
  apiKey: "",
  model: "",
  status: 0
});

const loading = reactive({
  storage: false,
  deleteUrl: false,
  zabbixTest: false,
  zabbixVersion: false,
  zabbixMappingOptions: false,
  zabbixMappingSave: false,
  zabbixMappingPreview: false,
  zabbixMappingSync: false,
  zabbixSave: false,
  aiTest: false,
  aiSave: false
});

const tested = reactive({
  zabbix: false,
  ai: false
});

const storageRules = {
  minioDomain: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioPath: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioEndPoint: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioAccessKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioSecretKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  minioBucketName: [{ required: true, message: "必填项不能为空", trigger: "blur" }]
};

const zabbixRules = {
  url: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  name: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  username: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  password: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
};

const deleteUrlRules = {
  url: [{ required: true, message: "必填项不能为空", trigger: "blur" }]
};

const aiRules = {
  baseUrl: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  apiKey: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  model: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }]
};

watch(
  () => [zabbixForm.url, zabbixForm.name, zabbixForm.username, zabbixForm.password, zabbixForm.status],
  () => {
    tested.zabbix = false;
    zabbixVersion.updateAvailable = null;
  }
);

watch(
  () => [aiForm.baseUrl, aiForm.apiKey, aiForm.model, aiForm.status],
  () => {
    tested.ai = false;
  }
);

const validateForm = (formRef: any) =>
  new Promise<boolean>((resolve) => {
    formRef.value.validate((valid: boolean) => resolve(valid));
  });

const loadStorage = () => {
  baseService.get("/sys/config-center/storage").then((res) => {
    if (res.data) {
      Object.assign(storageForm, { type: 4 }, res.data);
    }
  });
};

const loadZabbix = () => {
  baseService.get("/sys/config-center/zabbix").then((res) => {
    if (res.data) {
      Object.assign(zabbixForm, {
        url: "",
        name: "Zabbix",
        username: "",
        password: "",
        status: 1
      }, res.data);
    }
    tested.zabbix = false;
    loadZabbixVersion();
    loadZabbixMapping();
    loadZabbixMappingOptions();
  });
};

const loadZabbixVersion = () => {
  loading.zabbixVersion = true;
  baseService
    .get("/sys/config-center/zabbix/version")
    .then((res) => {
      Object.assign(zabbixVersion, {
        currentVersion: "",
        latestVersion: "",
        updateAvailable: null,
        upgradeUrl: "https://www.zabbix.com/download"
      }, res.data || {});
    })
    .finally(() => {
      loading.zabbixVersion = false;
    });
};

const loadAi = () => {
  baseService.get("/sys/config-center/ai").then((res) => {
    if (res.data) {
      Object.assign(aiForm, {
        baseUrl: "",
        apiKey: "",
        model: "",
        status: 0
      }, res.data);
    }
    tested.ai = false;
  });
};

const loadZabbixMapping = () => {
  baseService.get("/sys/config-center/zabbix/network-device-mapping").then((res) => {
    Object.assign(zabbixMappingForm, {
      zabbixName: "Zabbix",
      selectedHostGroupIds: [],
      templateModelRules: [],
      categoryGroupRules: [],
      areaKeywordRules: []
    }, res.data || {});
    if (zabbixMappingForm.selectedHostGroupIds.length) {
      previewZabbixMapping(false);
    }
  });
};

const loadZabbixMappingOptions = () => {
  loading.zabbixMappingOptions = true;
  baseService
    .get("/sys/config-center/zabbix/network-device-mapping/options")
    .then((res) => {
      Object.assign(zabbixMappingOptions, {
        zabbixName: "Zabbix",
        templateOptions: [],
        hostGroupOptions: [],
        areaOptions: [],
        deviceGroupOptions: [],
        deviceModelOptions: []
      }, res.data || {});
      if (!zabbixMappingForm.zabbixName) {
        zabbixMappingForm.zabbixName = zabbixMappingOptions.zabbixName || "Zabbix";
      }
    })
    .finally(() => {
      loading.zabbixMappingOptions = false;
    });
};

const previewZabbixMapping = (showMessage = true) => {
  loading.zabbixMappingPreview = true;
  baseService
    .post("/sys/config-center/zabbix/network-device-mapping/preview", {
      selectedHostGroupIds: [...zabbixMappingForm.selectedHostGroupIds],
      categoryGroupRules: zabbixMappingForm.categoryGroupRules.map((item) => ({ ...item })),
      areaKeywordRules: zabbixMappingForm.areaKeywordRules.map((item) => ({ ...item }))
    })
    .then((res) => {
      Object.assign(zabbixMappingPreview, {
        groupList: [],
        unmatchedAreas: []
      }, res.data || {});
      if (showMessage) {
        if (!zabbixMappingPreview.groupList.length) {
          ElMessage.info("预览完成，未匹配到可展示的主机群组");
          return;
        }
        ElMessage.success(
          `预览完成，共 ${zabbixMappingPreview.groupList.length} 条，未映射 ${zabbixMappingPreview.unmatchedAreas.length} 条`
        );
      }
    })
    .finally(() => {
      loading.zabbixMappingPreview = false;
    });
};

const addAreaKeywordRule = () => {
  zabbixMappingForm.areaKeywordRules.push({
    keyword: "",
    areaName: ""
  });
};

const removeAreaKeywordRule = (idx: number) => {
  zabbixMappingForm.areaKeywordRules.splice(idx, 1);
};

const addTemplateModelRule = () => {
  zabbixMappingForm.templateModelRules.push({
    templateName: "",
    deviceModel: ""
  });
};

const removeTemplateModelRule = (idx: number) => {
  zabbixMappingForm.templateModelRules.splice(idx, 1);
};

const addCategoryGroupRule = () => {
  zabbixMappingForm.categoryGroupRules.push({
    zabbixCategory: "",
    deviceGroup: ""
  });
};

const removeCategoryGroupRule = (idx: number) => {
  zabbixMappingForm.categoryGroupRules.splice(idx, 1);
};

const saveZabbixMapping = () => {
  loading.zabbixMappingSave = true;
  baseService
    .put("/sys/config-center/zabbix/network-device-mapping", {
      zabbixName: zabbixMappingForm.zabbixName,
      selectedHostGroupIds: [...zabbixMappingForm.selectedHostGroupIds],
      templateModelRules: zabbixMappingForm.templateModelRules.map((item) => ({ ...item })),
      categoryGroupRules: zabbixMappingForm.categoryGroupRules.map((item) => ({ ...item })),
      areaKeywordRules: zabbixMappingForm.areaKeywordRules.map((item) => ({ ...item }))
    })
    .then(() => {
      ElMessage.success("设备映射配置已保存");
      previewZabbixMapping();
    })
    .finally(() => {
      loading.zabbixMappingSave = false;
    });
};

const syncZabbixNetworkHosts = () => {
  loading.zabbixMappingSync = true;
  baseService
    .post("/sys/config-center/zabbix/network-device-mapping/sync")
    .then((res) => {
      const data = res.data || {};
      if (Number(data.syncSuccess) === 0) {
        ElMessage.warning(data.message || "同步未执行");
        return;
      }
      ElMessage.success(
        `同步完成：拉取${data.totalFetched ?? 0}，新增${data.inserted ?? 0}，更新${data.updated ?? 0}，逻辑删除(禁用)${data.logicalDeleted ?? data.disabled ?? 0}，跳过${data.skipped ?? 0}`
      );
      if (Array.isArray(data.unmatchedAreas) && data.unmatchedAreas.length) {
        zabbixMappingPreview.unmatchedAreas = [...data.unmatchedAreas];
      }
    })
    .finally(() => {
      loading.zabbixMappingSync = false;
    });
};

const saveStorage = async () => {
  const valid = await validateForm(storageFormRef);
  if (!valid) {
    return;
  }
  loading.storage = true;
  baseService
    .put("/sys/config-center/storage", { ...storageForm, type: 4 })
    .then(() => {
      ElMessage.success("存储配置已保存");
    })
    .finally(() => {
      loading.storage = false;
    });
};

const deleteByUrl = async () => {
  const valid = await validateForm(deleteUrlFormRef);
  if (!valid) {
    return;
  }
  loading.deleteUrl = true;
  baseService
    .post("/sys/config-center/storage/delete-by-url", { url: deleteUrlForm.url })
    .then(() => {
      ElMessage.success("删除成功");
      deleteUrlForm.url = "";
    })
    .finally(() => {
      loading.deleteUrl = false;
    });
};

const testZabbix = async () => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) {
    return;
  }
  loading.zabbixTest = true;
  baseService
    .post("/sys/config-center/zabbix/test", { ...zabbixForm })
    .then(() => {
      tested.zabbix = true;
      ElMessage.success("Zabbix连接测试成功");
      checkZabbixVersion(false);
      loadZabbixMappingOptions();
    })
    .finally(() => {
      loading.zabbixTest = false;
    });
};

const checkZabbixVersion = async (showSuccess = true) => {
  const valid = await validateForm(zabbixFormRef);
  if (!valid) {
    return;
  }
  loading.zabbixVersion = true;
  baseService
    .post("/sys/config-center/zabbix/version", { ...zabbixForm })
    .then((res) => {
      Object.assign(zabbixVersion, {
        currentVersion: "",
        latestVersion: "",
        updateAvailable: null,
        upgradeUrl: "https://www.zabbix.com/download"
      }, res.data || {});
      if (showSuccess) {
        ElMessage.success("Zabbix版本检测完成");
      }
    })
    .finally(() => {
      loading.zabbixVersion = false;
    });
};

const openZabbixUpgradeUrl = () => {
  window.open(zabbixVersion.upgradeUrl || "https://www.zabbix.com/download", "_blank");
};

const saveZabbix = async () => {
  if (!tested.zabbix) {
    ElMessage.warning("请先测试连接");
    return;
  }
  const valid = await validateForm(zabbixFormRef);
  if (!valid) {
    return;
  }
  loading.zabbixSave = true;
  baseService
    .put("/sys/config-center/zabbix", { ...zabbixForm })
    .then(() => {
      ElMessage.success("Zabbix配置已保存");
      tested.zabbix = true;
      loadZabbix();
    })
    .finally(() => {
      loading.zabbixSave = false;
    });
};

const testAi = async () => {
  const valid = await validateForm(aiFormRef);
  if (!valid) {
    return;
  }
  loading.aiTest = true;
  baseService
    .post("/sys/config-center/ai/test", { ...aiForm })
    .then(() => {
      tested.ai = true;
      ElMessage.success("AI连接测试成功");
    })
    .finally(() => {
      loading.aiTest = false;
    });
};

const saveAi = async () => {
  if (!tested.ai) {
    ElMessage.warning("请先测试连接");
    return;
  }
  const valid = await validateForm(aiFormRef);
  if (!valid) {
    return;
  }
  loading.aiSave = true;
  baseService
    .put("/sys/config-center/ai", { ...aiForm })
    .then(() => {
      ElMessage.success("AI配置已保存");
      tested.ai = true;
      loadAi();
    })
    .finally(() => {
      loading.aiSave = false;
    });
};

onMounted(() => {
  loadStorage();
  loadZabbix();
  loadAi();
});
</script>

<style scoped>
.mod-sys__config-center {
  padding: 8px;
}

.config-shell {
  margin-bottom: 10px;
  border-radius: 14px;
  background: linear-gradient(135deg, #eff6ff 0%, #f8fafc 45%, #ffffff 100%);
  border: 1px solid #dbeafe;
  padding: 14px 16px;
}

.config-shell__title {
  font-size: 16px;
  font-weight: 700;
  color: #0f172a;
  line-height: 1.2;
}

.config-shell__desc {
  margin-top: 6px;
  color: #475569;
  font-size: 13px;
}

.config-tabs {
  border-radius: 14px;
  background: #fff;
  border: 1px solid #e5e7eb;
  padding: 10px 12px 14px;
}

.config-grid {
  display: grid;
  grid-template-columns: minmax(0, 1.6fr) minmax(260px, 1fr);
  gap: 12px;
}

.config-grid--storage {
  grid-template-columns: minmax(0, 1.8fr) minmax(280px, 1fr);
}

.stack-panels {
  display: grid;
  gap: 12px;
  align-content: start;
}

.panel-card {
  border-radius: 12px;
}

.panel-card__header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 8px;
  font-weight: 600;
  color: #0f172a;
}

.panel-card--muted {
  background: #f8fafc;
}

.panel-card--highlight {
  border: 1px solid #dbeafe;
  background: linear-gradient(180deg, #ffffff 0%, #f8fbff 100%);
}

.mapping-workbench {
  display: grid;
  grid-template-columns: minmax(280px, 330px) minmax(0, 1fr);
  gap: 12px;
  align-items: stretch;
}

.mapping-config-pane,
.mapping-preview-pane {
  display: grid;
  gap: 12px;
  height: 100%;
}

.mapping-config-card {
  position: sticky;
  top: 0;
}

.mapping-rules-card :deep(.el-card__body) {
  padding-top: 10px;
}

.mapping-metrics {
  display: grid;
  grid-template-columns: repeat(2, minmax(0, 1fr));
  gap: 8px;
  margin-bottom: 10px;
}

.mapping-metric {
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 8px 10px;
  background: #fff;
}

.mapping-metric__label {
  color: #64748b;
  font-size: 12px;
}

.mapping-metric__value {
  color: #0f172a;
  font-size: 18px;
  font-weight: 700;
  line-height: 1.2;
  margin-top: 2px;
}

.mapping-actions {
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
  margin-top: 12px;
}

.mapping-hostgroup {
  margin-top: 4px;
}

.mapping-hostgroup__title {
  font-size: 12px;
  font-weight: 700;
  color: #475569;
  margin-bottom: 6px;
}

.rule-empty {
  border-radius: 10px;
  border: 1px dashed #cbd5e1;
  background: #f8fafc;
  padding: 10px;
  color: #64748b;
  font-size: 12px;
}

.mapping-rule-row {
  display: grid;
  grid-template-columns: 1fr 1fr auto;
  gap: 8px;
  align-items: center;
}

.mapping-preview :deep(.el-table td),
.mapping-preview :deep(.el-table th) {
  padding: 6px 0;
}

.mapping-preview-pane :deep(.el-card) {
  height: 100%;
}

.mapping-preview-pane :deep(.el-card__body) {
  height: calc(100% - 2px);
  display: flex;
  flex-direction: column;
}

.preview-header-tags {
  display: flex;
  gap: 6px;
  font-size: 13px;
}

.preview-toolbar {
  display: grid;
  grid-template-columns: auto 1fr auto;
  gap: 8px;
  margin-bottom: 10px;
  flex-shrink: 0;
  font-size: 14px;
}

.mapping-preview {
  flex: 1;
  min-height: 0;
}

.mapping-preview :deep(.el-table) {
  font-size: 14px;
}

.mapping-preview :deep(.el-table th) {
  font-size: 14px;
}

.unmatched-list {
  margin-top: 8px;
  color: #b45309;
  font-size: 12px;
  line-height: 1.5;
}

.mapping-rule-tabs :deep(.el-tabs__header) {
  margin-bottom: 8px;
}

.mapping-rule-tabs :deep(.el-tabs__item) {
  font-size: 12px;
  font-weight: 600;
}

.mapping-rule-tabs :deep(.el-tabs__content) {
  height: 210px;
}

.mapping-rule-tabs :deep(.el-tab-pane) {
  height: 100%;
}

.mapping-rule-list {
  height: 100%;
  display: grid;
  grid-template-rows: minmax(0, 1fr) auto;
  gap: 8px;
}

.mapping-rule-scroll {
  min-height: 0;
  max-height: calc(3 * 40px + 2 * 8px);
  overflow: auto;
  padding-right: 2px;
}

.mapping-preview :deep(.preview-row--unmatched > td) {
  background: #fffbeb;
}

.config-form {
  max-width: 100%;
}

.config-form--compact :deep(.el-form-item__label) {
  width: 84px !important;
}

.action-row {
  margin-bottom: 0;
}

.action-row--wrap :deep(.el-form-item__content) {
  gap: 8px;
  flex-wrap: wrap;
}

.action-tip {
  margin-top: 6px;
  font-size: 13px;
  color: #64748b;
  line-height: 1.5;
}

.action-tip--ok {
  color: #15803d;
}

.kv-list {
  display: grid;
  gap: 10px;
}

.kv-item {
  display: flex;
  justify-content: space-between;
  gap: 12px;
  padding: 10px 12px;
  border-radius: 10px;
  background: #fff;
  border: 1px solid #e5e7eb;
}

.kv-item__label {
  color: #64748b;
}

.kv-item__value {
  color: #0f172a;
  font-weight: 600;
  text-align: right;
  word-break: break-word;
}

.side-actions {
  margin-top: 12px;
  display: flex;
  gap: 8px;
  flex-wrap: wrap;
}

.info-list {
  display: grid;
  gap: 8px;
}

.info-list__title {
  font-size: 13px;
  font-weight: 700;
  color: #334155;
}

.info-list__item {
  color: #475569;
  line-height: 1.55;
  font-size: 13px;
}

.info-list--inline {
  margin-top: 10px;
}

.hint {
  margin-left: 12px;
  color: #909399;
}

.hint.ok {
  color: #67c23a;
}

:deep(.config-tabs .el-tabs__header) {
  margin-bottom: 14px;
}

:deep(.config-tabs .el-tabs__nav-wrap::after) {
  background-color: #e5e7eb;
}

:deep(.config-tabs .el-tabs__item) {
  height: 38px;
  line-height: 38px;
  font-weight: 600;
}

@media (max-width: 960px) {
  .config-grid,
  .config-grid--storage {
    grid-template-columns: 1fr;
  }

  .mapping-workbench {
    grid-template-columns: 1fr;
  }

  .mapping-config-card {
    position: static;
  }

  .mapping-rules-card :deep(.el-card__body) {
    padding-top: 10px;
  }

  .config-tabs {
    padding: 8px;
  }

  .config-shell {
    padding: 12px;
  }

  .mapping-rule-row {
    grid-template-columns: 1fr;
  }

  .preview-toolbar {
    grid-template-columns: 1fr;
  }

  .mapping-metrics {
    grid-template-columns: 1fr 1fr;
  }

  .mapping-config-pane,
  .mapping-preview-pane {
    height: auto;
  }

  .mapping-preview-pane :deep(.el-card),
  .mapping-preview-pane :deep(.el-card__body) {
    height: auto;
  }

  .mapping-rule-tabs :deep(.el-tabs__content) {
    height: auto;
  }
}
</style>
