<template>
  <el-dialog
    v-model="visible"
    :title="dataForm.id ? '修改域名记录' : '新增域名记录'"
    width="980px"
    :close-on-click-modal="false"
    :close-on-press-escape="false"
  >
    <el-form ref="dataFormRef" :model="dataForm" :rules="rules" label-width="140px" class="domain-form">
      <el-tabs v-model="activeTab" class="domain-tabs" type="border-card">
        <el-tab-pane label="基础设置" name="base">
          <el-row :gutter="16" class="tab-pane-content">
            <el-col :span="12">
              <el-form-item label="项目名称" prop="projectName">
                <el-input v-model="dataForm.projectName" placeholder="项目名称"></el-input>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="域名" prop="domainName">
                <el-input v-model="dataForm.domainName" placeholder="example.com"></el-input>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="区域名称" prop="areaName">
                <ren-select
                  v-model="dataForm.areaName"
                  dict-type="area_name_type"
                  label-field="dictValue"
                  value-field="dictLabel"
                  placeholder="区域名称"
                ></ren-select>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="分组名称" prop="groupName">
                <ren-select
                  v-model="dataForm.groupName"
                  dict-type="server_host_group"
                  label-field="dictValue"
                  value-field="dictLabel"
                  placeholder="分组名称"
                ></ren-select>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="站点位置" prop="siteLocation">
                <ren-select
                  v-model="dataForm.siteLocation"
                  dict-type="base_site_location"
                  label-field="dictValue"
                  value-field="dictLabel"
                  placeholder="站点位置"
                ></ren-select>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="状态" prop="status">
                <el-select v-model="dataForm.status" placeholder="状态" style="width: 100%">
                  <el-option label="启用" :value="1"></el-option>
                  <el-option label="禁用" :value="0"></el-option>
                </el-select>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="项目负责人" prop="projectOwner">
                <el-input v-model="dataForm.projectOwner" placeholder="项目负责人"></el-input>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="申请时间" prop="applyTime">
                <el-date-picker
                  v-model="dataForm.applyTime"
                  type="datetime"
                  value-format="YYYY-MM-DD HH:mm:ss"
                  placeholder="申请时间"
                  style="width: 100%"
                ></el-date-picker>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="修改描述">
                <el-input v-model="dataForm.description" placeholder="本次修改的主要内容描述"></el-input>
              </el-form-item>
            </el-col>
            <el-col :span="12">
              <el-form-item label="访问地址" prop="apiUrl">
                <el-input v-model="dataForm.apiUrl" placeholder="https://grafana.leoch.net:8001/api"></el-input>
              </el-form-item>
            </el-col>
            <el-col :span="24">
              <el-form-item label="备注">
                <el-input v-model="dataForm.remark" type="textarea" :rows="2" placeholder="备注"></el-input>
              </el-form-item>
            </el-col>
          </el-row>
        </el-tab-pane>

        <el-tab-pane label="高级策略" name="strategy">
          <div class="delivery-section">
            <div class="delivery-section__header"><span>配置中心</span></div>
            <el-row :gutter="24">
              <el-col :span="8">
                <el-form-item label="开启深信服(AD)">
                  <el-switch v-model="dataForm.adEnabled" :active-value="1" :inactive-value="0"></el-switch>
                </el-form-item>
              </el-col>
              <el-col :span="8">
                <el-form-item label="开启内网配置">
                  <el-switch v-model="dataForm.internalEnabled" :active-value="1" :inactive-value="0"></el-switch>
                </el-form-item>
              </el-col>
              <el-col :span="8">
                <el-form-item label="开启公网配置">
                  <el-switch v-model="dataForm.externalEnabled" :active-value="1" :inactive-value="0"></el-switch>
                </el-form-item>
              </el-col>
            </el-row>
            <div style="margin-top: 16px; color: #64748b; font-size: 13px; line-height: 1.6;">
              <p v-if="dataForm.adEnabled === 1"><el-icon style="vertical-align: middle; margin-right: 4px;"><InfoFilled /></el-icon> 已开启 <b>应用交付(AD)</b>，将提供虚拟服务与负载均衡功能。</p>
              <p v-if="dataForm.internalEnabled === 1"><el-icon style="vertical-align: middle; margin-right: 4px;"><InfoFilled /></el-icon> 已开启 <b>内网配置</b>，将提供内网解析功能。</p>
              <p v-if="dataForm.externalEnabled === 1"><el-icon style="vertical-align: middle; margin-right: 4px;"><InfoFilled /></el-icon> 已开启 <b>公网配置</b>，将提供公网解析与防火墙映射功能。</p>
            </div>
          </div>
        </el-tab-pane>

        <el-tab-pane label="应用交付(AD)" name="delivery" v-if="dataForm.adEnabled === 1">
          <!-- 内网虚拟服务 -->
          <div v-if="dataForm.internalEnabled === 1" class="delivery-section">
            <div class="delivery-section__header">
              <span>{{ isDualAd ? '内网虚拟服务' : '虚拟服务' }}</span>
            </div>
            <el-row :gutter="16">
              <el-col :span="12">
                <el-form-item label="虚拟服务名称" prop="delivery.virtualServiceName">
                  <el-input v-model="dataForm.delivery.virtualServiceName" placeholder="虚拟服务名称"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item :label="isDualAd ? '内网虚拟服务IP' : '虚拟服务IP'" prop="delivery.virtualServiceIp">
                  <el-input v-model="dataForm.delivery.virtualServiceIp" placeholder="虚拟服务IP"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item :label="isDualAd ? '内网虚拟服务端口' : '虚拟服务端口'" prop="delivery.virtualServicePort">
                  <el-input-number v-model="dataForm.delivery.virtualServicePort" :min="1" :max="65535" style="width: 100%"></el-input-number>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item :label="isDualAd ? '内网虚拟服务协议' : '虚拟服务协议'" prop="delivery.virtualServiceProtocol">
                  <el-select v-model="dataForm.delivery.virtualServiceProtocol" placeholder="协议" style="width: 100%">
                    <el-option label="HTTP" value="HTTP"></el-option>
                    <el-option label="HTTPS" value="HTTPS"></el-option>
                    <el-option label="TCP" value="TCP"></el-option>
                  </el-select>
                </el-form-item>
              </el-col>
            </el-row>
          </div>

          <!-- 外网虚拟服务：仅公网配置开启时展示 -->
          <div v-if="dataForm.externalEnabled === 1" class="delivery-section">
            <div class="delivery-section__header">
              <span>外网虚拟服务</span>
            </div>
            <el-row :gutter="16">
              <el-col :span="12">
                <el-form-item label="虚拟服务名称" prop="delivery.externalVirtualServiceName">
                  <el-input v-model="dataForm.delivery.externalVirtualServiceName" placeholder="外网虚拟服务名称"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="外网虚拟服务IP" prop="delivery.externalVirtualServiceIp">
                  <el-input v-model="dataForm.delivery.externalVirtualServiceIp" placeholder="外网虚拟服务IP"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="外网虚拟服务端口" prop="delivery.externalVirtualServicePort">
                  <el-input-number v-model="dataForm.delivery.externalVirtualServicePort" :min="1" :max="65535" style="width: 100%"></el-input-number>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="外网虚拟服务协议" prop="delivery.externalVirtualServiceProtocol">
                  <el-select v-model="dataForm.delivery.externalVirtualServiceProtocol" placeholder="协议" style="width: 100%">
                    <el-option label="HTTP" value="HTTP"></el-option>
                    <el-option label="HTTPS" value="HTTPS"></el-option>
                    <el-option label="TCP" value="TCP"></el-option>
                  </el-select>
                </el-form-item>
              </el-col>
            </el-row>
          </div>

          <!-- 节点池（共享） -->
          <div class="delivery-section">
            <div class="delivery-section__header">
              <span>节点池{{ isDualAd ? '（共享）' : '' }}</span>
            </div>
            <el-row :gutter="16">
              <el-col :span="12">
                <el-form-item label="节点池名称" prop="delivery.poolName">
                  <el-input v-model="dataForm.delivery.poolName" placeholder="节点池名称"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="备注" prop="delivery.remark">
                  <el-input v-model="dataForm.delivery.remark" placeholder="应用交付备注"></el-input>
                </el-form-item>
              </el-col>
            </el-row>
            <div class="node-toolbar">
              <span class="node-toolbar__title">节点池明细</span>
              <el-button type="primary" plain size="small" @click="addNode">新增节点</el-button>
            </div>
            <el-table :data="dataForm.delivery.nodes" border size="small">
              <el-table-column label="节点IP" min-width="180">
                <template #default="{ row, $index }">
                  <el-form-item label-width="0" :prop="`delivery.nodes.${$index}.nodeIp`" :rules="[{ required: true, message: '必填且需为合法IP', pattern: /^((25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(25[0-5]|2[0-4]\d|[01]?\d\d?)$/, trigger: 'blur' }]" class="table-form-item" style="margin-bottom: 0;">
                    <el-input v-model="row.nodeIp" placeholder="节点IP"></el-input>
                  </el-form-item>
                </template>
              </el-table-column>
              <el-table-column label="节点端口" width="140">
                <template #default="{ row, $index }">
                  <el-form-item label-width="0" :prop="`delivery.nodes.${$index}.nodePort`" :rules="[{ required: true, message: '必填', trigger: 'blur' }]" class="table-form-item" style="margin-bottom: 0;">
                    <el-input-number v-model="row.nodePort" :min="1" :max="65535" style="width: 100%"></el-input-number>
                  </el-form-item>
                </template>
              </el-table-column>
              <el-table-column label="排序" width="120">
                <template #default="{ row }">
                  <el-input-number v-model="row.sort" :min="1" style="width: 100%"></el-input-number>
                </template>
              </el-table-column>
              <el-table-column label="备注" min-width="180">
                <template #default="{ row }">
                  <el-input v-model="row.remark" placeholder="备注"></el-input>
                </template>
              </el-table-column>
              <el-table-column label="操作" width="90" fixed="right">
                <template #default="{ $index }">
                  <el-button type="danger" link @click="removeNode($index)">删除</el-button>
                </template>
              </el-table-column>
            </el-table>
          </div>
        </el-tab-pane>

        <el-tab-pane label="DNS解析" name="dns" v-if="dataForm.internalEnabled === 1 || dataForm.externalEnabled === 1">
          <el-row :gutter="16" class="dns-config-row">
            <el-col :span="12" v-if="dataForm.internalEnabled === 1">
              <div class="delivery-section">
                <div class="delivery-section__header"><span>内网解析</span></div>
                <el-form-item label="解析链路">
                  <el-popover placement="top-start" :width="320" trigger="click" :content="internalResolveHint">
                    <template #reference>
                      <el-button link type="primary" :icon="QuestionFilled">点击查看链路说明</el-button>
                    </template>
                  </el-popover>
                </el-form-item>
                <el-form-item :label="dataForm.adEnabled === 1 ? '内部虚拟IP' : '目标IP'" prop="dnsInternal.dnsTargetIp">
                  <el-input
                    v-model="dataForm.dnsInternal.dnsTargetIp"
                    :placeholder="dataForm.adEnabled === 1 ? '深信服应用交付内部虚拟服务IP' : '目标IP'"
                  ></el-input>
                </el-form-item>
                <el-form-item label="备注">
                  <el-input v-model="dataForm.dnsInternal.remark" placeholder="备注"></el-input>
                </el-form-item>
              </div>
            </el-col>
            <el-col :span="12" v-if="dataForm.externalEnabled === 1">
              <div class="delivery-section">
                <div class="delivery-section__header"><span>公网解析</span></div>
                <el-form-item label="解析链路">
                  <el-popover placement="top-start" :width="400" trigger="click" :content="externalResolveHint">
                    <template #reference>
                      <el-button link type="primary" :icon="QuestionFilled">点击查看链路说明</el-button>
                    </template>
                  </el-popover>
                </el-form-item>
                <el-form-item :label="dataForm.adEnabled === 1 ? '公网解析IP' : '目标公网IP'" prop="dnsExternal.recordValue">
                  <el-input
                    v-model="dataForm.dnsExternal.recordValue"
                    :placeholder="dataForm.adEnabled === 1 ? '阿里云DNS解析公网IP' : '目标公网IP'"
                  ></el-input>
                </el-form-item>
                <el-form-item label="备注">
                  <el-input v-model="dataForm.dnsExternal.remark" placeholder="备注"></el-input>
                </el-form-item>
              </div>
            </el-col>
          </el-row>
        </el-tab-pane>

        <el-tab-pane label="防火墙映射" name="firewall" v-if="dataForm.adEnabled === 1 && dataForm.externalEnabled === 1">
          <div class="delivery-section">
            <div class="delivery-section__header"><span>NAT映射</span></div>
            <el-row :gutter="16">
              <el-col :span="12">
                <el-form-item label="外部映射IP" prop="firewallMapping.publicIp">
                  <el-input v-model="dataForm.firewallMapping.publicIp" placeholder="公网IP"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="外部端口" prop="firewallMapping.externalPort">
                  <el-input-number v-model="dataForm.firewallMapping.externalPort" :min="1" :max="65535" style="width: 100%"></el-input-number>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="内部映射IP" prop="firewallMapping.internalIp">
                  <el-input v-model="dataForm.firewallMapping.internalIp" placeholder="内部IP"></el-input>
                </el-form-item>
              </el-col>
              <el-col :span="12">
                <el-form-item label="内部端口" prop="firewallMapping.internalPort">
                  <el-input-number v-model="dataForm.firewallMapping.internalPort" :min="1" :max="65535" style="width: 100%"></el-input-number>
                </el-form-item>
              </el-col>
              <el-col :span="24">
                <el-form-item label="映射描述">
                  <el-input v-model="dataForm.firewallMapping.mappingDesc" placeholder="映射描述"></el-input>
                </el-form-item>
              </el-col>
            </el-row>
          </div>
        </el-tab-pane>
      </el-tabs>
    </el-form>

    <template #footer>
      <div class="dialog-footer">
        <el-button @click="visible = false">取消</el-button>
        <el-button type="primary" :loading="submitLoading" @click="submitHandle">确定</el-button>
      </div>
    </template>
  </el-dialog>
</template>

<script lang="ts" setup>
import { computed, reactive, ref, watch } from "vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";
import { InfoFilled, QuestionFilled } from "@element-plus/icons-vue";

const emit = defineEmits(["refreshDataList"]);

const visible = ref(false);
const activeTab = ref("base");
const dataFormRef = ref();
const submitLoading = ref(false);

const createNode = () => ({
  nodeIp: "",
  nodePort: 80,
  sort: 1,
  remark: ""
});

const createForm = () => ({
  id: "",
  projectName: "",
  domainName: "",
  areaName: "",
  groupName: "",
  siteLocation: "",
  status: 1,
  adEnabled: 0,
  internalEnabled: 1,
  externalEnabled: 0,
  description: "",
  projectOwner: "",
  applyTime: "",
  remark: "",
  apiUrl: "",
  delivery: {
    virtualServiceName: "",
    virtualServiceIp: "",
    virtualServicePort: 443,
    virtualServiceProtocol: "HTTPS",
    poolName: "",
    remark: "",
    externalVirtualServiceName: "",
    externalVirtualServiceIp: "",
    externalVirtualServicePort: 443,
    externalVirtualServiceProtocol: "HTTPS",
    nodes: [createNode()]
  },
  dnsInternal: {
    resolveMode: "DIRECT",
    dnsTargetIp: "",
    remark: ""
  },
  dnsExternal: {
    resolveMode: "DIRECT",
    recordValue: "",
    remark: ""
  },
  firewallMapping: {
    publicIp: "",
    externalPort: 80,
    internalIp: "",
    internalPort: 80,
    mappingDesc: ""
  }
});

const dataForm = reactive(createForm());

const rules = {
  projectName: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  domainName: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  areaName: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  groupName: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  siteLocation: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  status: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  projectOwner: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  applyTime: [{ required: true, message: "必填项不能为空", trigger: "change" }],
  apiUrl: [{
    validator: (_rule: any, value: string, callback: (error?: Error) => void) => {
      if (!value) {
        callback();
        return;
      }
      try {
        const url = new URL(value);
        const port = url.port ? Number(url.port) : undefined;
        if (!["http:", "https:"].includes(url.protocol)) {
          callback(new Error("仅支持http/https地址"));
          return;
        }
        if (port !== undefined && (!Number.isInteger(port) || port < 1 || port > 65535)) {
          callback(new Error("端口范围需在1-65535"));
          return;
        }
        callback();
      } catch (_error) {
        callback(new Error("请输入合法的完整URL"));
      }
    },
    trigger: "blur"
  }]
};

const internalResolveHint = computed(() => {
  if (dataForm.adEnabled === 1) {
    return "走AD时，内网DNS解析到深信服AD内网虚拟IP，再转发到节点池IP和端口。";
  }
  return "不走AD时，内网DNS直接解析到目标IP。";
});

const externalResolveHint = computed(() => {
  if (dataForm.adEnabled === 1) {
    return "走AD时，阿里云DNS解析到公网IP，经防火墙映射到深信服AD外网虚拟IP，再转发到节点池IP和端口。";
  }
  return "不走AD时，公网DNS直接解析到目标公网IP。";
});

const isDualAd = computed(() => {
  return dataForm.adEnabled === 1 && dataForm.internalEnabled === 1 && dataForm.externalEnabled === 1;
});

// 是否需要展示外网虚拟服务（AD开启 + 公网解析开启即可，不需要同时内网也开）
const showExternalVirtualService = computed(() => {
  return dataForm.adEnabled === 1 && dataForm.externalEnabled === 1;
});

watch(
  () => dataForm.adEnabled,
  (value) => {
    dataForm.dnsInternal.resolveMode = value === 1 ? "AD" : "DIRECT";
    dataForm.dnsExternal.resolveMode = value === 1 ? "AD" : "DIRECT";
    if (value === 1) {
      // 开启AD时默认打开内网解析
      dataForm.internalEnabled = 1;
    } else {
      dataForm.firewallMapping = createForm().firewallMapping;
    }
  }
);

watch(
  () => dataForm.internalEnabled,
  (value) => {
    if (value !== 1) {
      dataForm.dnsInternal = {
        ...createForm().dnsInternal,
        resolveMode: dataForm.adEnabled === 1 ? "AD" : "DIRECT"
      };
    }
  }
);

watch(
  () => dataForm.externalEnabled,
  (value) => {
    if (value !== 1) {
      dataForm.firewallMapping = createForm().firewallMapping;
      dataForm.dnsExternal = {
        ...createForm().dnsExternal,
        resolveMode: dataForm.adEnabled === 1 ? "AD" : "DIRECT"
      };
      // 清空外网虚拟服务字段
      dataForm.delivery.externalVirtualServiceName = "";
      dataForm.delivery.externalVirtualServiceIp = "";
      dataForm.delivery.externalVirtualServicePort = null;
      dataForm.delivery.externalVirtualServiceProtocol = "";
    }
  }
);

// 内网虚拟服务IP → 内网解析虚拟服务IP 联动
watch(
  () => dataForm.delivery.virtualServiceIp,
  (ip) => {
    if (dataForm.adEnabled === 1 && dataForm.internalEnabled === 1) {
      dataForm.dnsInternal.dnsTargetIp = ip;
    }
  }
);

// 外网虚拟服务IP/端口 → 防火墙NAT映射内部IP/端口 联动
watch(
  () => [dataForm.delivery.externalVirtualServiceIp, dataForm.delivery.externalVirtualServicePort],
  ([ip, port]) => {
    if (dataForm.adEnabled === 1 && dataForm.externalEnabled === 1 && ip) {
      dataForm.firewallMapping.internalIp = ip as string;
      dataForm.firewallMapping.internalPort = port as number;
    }
  }
);

// 公网解析记录值 → 防火墙NAT公网IP 联动
watch(
  () => dataForm.dnsExternal.recordValue,
  (recordValue) => {
    if (dataForm.adEnabled === 1 && dataForm.externalEnabled === 1) {
      dataForm.firewallMapping.publicIp = recordValue;
    }
  }
);

const resetForm = () => {
  Object.assign(dataForm, createForm());
  if (dataFormRef.value) {
    dataFormRef.value.clearValidate();
  }
};

const addNode = () => {
  dataForm.delivery.nodes.push(createNode());
};

const removeNode = (index: number) => {
  dataForm.delivery.nodes.splice(index, 1);
};

const normalizePayload = () => {
  const payload: any = {
    id: dataForm.id || undefined,
    projectName: dataForm.projectName,
    domainName: dataForm.domainName,
    areaName: dataForm.areaName,
    groupName: dataForm.groupName,
    siteLocation: dataForm.siteLocation,
    status: dataForm.status,
    adEnabled: dataForm.adEnabled,
    internalEnabled: dataForm.internalEnabled,
    externalEnabled: dataForm.externalEnabled,
    description: dataForm.description,
    projectOwner: dataForm.projectOwner,
    applyTime: dataForm.applyTime,
    remark: dataForm.remark,
    apiUrl: dataForm.apiUrl
  };
  if (dataForm.adEnabled === 1) {
    const deliveryPayload: any = {
      ...dataForm.delivery,
      nodes: dataForm.delivery.nodes.filter((item) => item.nodeIp && item.nodePort)
    };
    // 公网解析开启时才提交外网虚拟服务字段
    if (showExternalVirtualService.value) {
      deliveryPayload.externalVirtualServiceName = dataForm.delivery.externalVirtualServiceName || undefined;
      deliveryPayload.externalVirtualServiceIp = dataForm.delivery.externalVirtualServiceIp || undefined;
      deliveryPayload.externalVirtualServicePort = dataForm.delivery.externalVirtualServicePort || undefined;
      deliveryPayload.externalVirtualServiceProtocol = dataForm.delivery.externalVirtualServiceProtocol || undefined;
    } else {
      delete deliveryPayload.externalVirtualServiceName;
      delete deliveryPayload.externalVirtualServiceIp;
      delete deliveryPayload.externalVirtualServicePort;
      delete deliveryPayload.externalVirtualServiceProtocol;
    }
    payload.delivery = deliveryPayload;
  }
  if (dataForm.internalEnabled === 1) {
    payload.dnsInternal = {
      ...dataForm.dnsInternal,
      resolveMode: dataForm.adEnabled === 1 ? "AD" : "DIRECT"
    };
  }
  if (dataForm.externalEnabled === 1) {
    payload.dnsExternal = {
      ...dataForm.dnsExternal,
      resolveMode: dataForm.adEnabled === 1 ? "AD" : "DIRECT"
    };
  }
  if (dataForm.adEnabled === 1 && dataForm.externalEnabled === 1) {
    payload.firewallMapping = { ...dataForm.firewallMapping };
  }
  return payload;
};

const init = (id?: number) => {
  visible.value = true;
  activeTab.value = "base";
  resetForm();
  if (id) {
    baseService.get(`/ops/domain-record/${id}`).then((res) => {
      Object.assign(dataForm, createForm(), res.data);
      dataForm.delivery = Object.assign(createForm().delivery, res.data?.delivery || {}, {
        externalVirtualServiceName: res.data?.delivery?.externalVirtualServiceName || "",
        externalVirtualServiceIp: res.data?.delivery?.externalVirtualServiceIp || "",
        externalVirtualServicePort: res.data?.delivery?.externalVirtualServicePort || null,
        externalVirtualServiceProtocol: res.data?.delivery?.externalVirtualServiceProtocol || ""
      });
      dataForm.delivery.nodes = res.data?.delivery?.nodes?.length ? res.data.delivery.nodes : [createNode()];
      dataForm.dnsInternal = Object.assign(createForm().dnsInternal, res.data?.dnsInternal || {});
      dataForm.dnsExternal = Object.assign(createForm().dnsExternal, res.data?.dnsExternal || {});
      dataForm.firewallMapping = Object.assign(createForm().firewallMapping, res.data?.firewallMapping || {});
    });
  }
};

const submitHandle = () => {
    dataFormRef.value.validate((valid: boolean, fields: any) => {
    if (!valid) {
      if (fields) {
        const firstErrorField = Object.keys(fields)[0];
        if (firstErrorField === 'adEnabled' || firstErrorField === 'internalEnabled' || firstErrorField === 'externalEnabled') {
          activeTab.value = 'strategy';
        } else if (firstErrorField.startsWith('delivery.')) {
          activeTab.value = 'delivery';
        } else if (firstErrorField.startsWith('dnsInternal.') || firstErrorField.startsWith('dnsExternal.')) {
          activeTab.value = 'dns';
        } else if (firstErrorField.startsWith('firewallMapping.')) {
          activeTab.value = 'firewall';
        } else {
          activeTab.value = 'base';
        }
      }
      return;
    }
    submitLoading.value = true;
    const payload = normalizePayload();
    (!dataForm.id ? baseService.post : baseService.put)("/ops/domain-record", payload)
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

<style scoped>
.domain-form {
  padding: 0;
}

.domain-tabs {
  min-height: 520px;
  box-shadow: 0 4px 12px rgba(0, 0, 0, 0.05);
  border: 1px solid #e5e7eb;
  border-radius: 8px;
  overflow: hidden;
}

.domain-tabs :deep(.el-tabs__header) {
  background-color: #f9fafb;
  margin: 0;
}

.domain-tabs :deep(.el-tabs__content) {
  padding: 24px;
  max-height: 65vh;
  overflow-y: auto;
}

.tab-pane-content {
  margin-top: 8px;
}

.delivery-switch-bar {
  display: flex;
  flex-wrap: wrap;
  gap: 24px;
  padding: 16px 20px;
  margin-bottom: 24px;
  background: linear-gradient(135deg, #f8fafc 0%, #f1f5f9 100%);
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  align-items: center;
}

.delivery-switch-bar .switch-item {
  margin-bottom: 0;
}

.delivery-switch-bar .switch-item :deep(.el-form-item__label) {
  font-weight: 600;
  color: #1e293b;
  font-size: 14px;
}

.switch-readonly-hint {
  margin-left: 8px;
  font-size: 12px;
  color: #64748b;
  font-style: italic;
}

.delivery-section {
  padding: 20px;
  margin-bottom: 20px;
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  box-shadow: 0 1px 2px rgba(0, 0, 0, 0.03);
  transition: all 0.3s ease;
}

.delivery-section:hover {
  box-shadow: 0 4px 6px -1px rgba(0, 0, 0, 0.1), 0 2px 4px -1px rgba(0, 0, 0, 0.06);
  border-color: #cbd5e1;
}

.delivery-section__header {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 20px;
  padding-bottom: 12px;
  border-bottom: 1px solid #f1f5f9;
}

.delivery-section__header > span {
  font-size: 15px;
  font-weight: 700;
  color: #0f172a;
  position: relative;
  padding-left: 14px;
}

.delivery-section__header > span::before {
  content: "";
  position: absolute;
  left: 0;
  top: 50%;
  transform: translateY(-50%);
  width: 4px;
  height: 16px;
  background: linear-gradient(to bottom, #3b82f6, #2563eb);
  border-radius: 2px;
}

:deep(.table-form-item .el-form-item__error) {
  position: absolute;
  top: 100%;
  left: 0;
  padding-top: 2px;
  z-index: 10;
}

.sub-section-title {
  margin-bottom: 12px;
  font-size: 14px;
  font-weight: 600;
  color: #475569;
}

.node-toolbar {
  display: flex;
  align-items: center;
  justify-content: space-between;
  margin-bottom: 12px;
  padding: 0 4px;
}

.node-toolbar__title {
  font-size: 14px;
  font-weight: 600;
  color: #334155;
}

/* 优化输入框和下拉框宽度一致性 */
.delivery-section :deep(.el-input),
.delivery-section :deep(.el-select),
.delivery-section :deep(.el-input-number) {
  width: 100%;
}

/* 解析配置等高对齐 */
.dns-config-row {
  display: flex !important;
  flex-wrap: wrap;
}

.dns-config-row :deep(.el-col) {
  display: flex;
}

.dns-config-row .delivery-section {
  flex: 1;
  display: flex;
  flex-direction: column;
  width: 100%;
}
</style>
