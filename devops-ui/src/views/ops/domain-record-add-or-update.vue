<template>
  <el-dialog
    v-model="visible"
    :title="dataForm.id ? '修改域名记录' : '新增域名记录'"
    width="980px"
    :close-on-click-modal="false"
    :close-on-press-escape="false"
  >
    <el-form ref="dataFormRef" :model="dataForm" :rules="rules" label-width="120px" class="domain-form">
      <div class="form-section-title">基础信息</div>
      <el-row :gutter="16">
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
        <el-col :span="8">
          <el-form-item label="走应用交付" prop="adEnabled">
            <el-switch v-model="dataForm.adEnabled" :active-value="1" :inactive-value="0"></el-switch>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="启用内网解析" prop="internalEnabled">
            <el-switch v-model="dataForm.internalEnabled" :active-value="1" :inactive-value="0"></el-switch>
          </el-form-item>
        </el-col>
        <el-col :span="8">
          <el-form-item label="启用外网解析" prop="externalEnabled">
            <el-switch v-model="dataForm.externalEnabled" :active-value="1" :inactive-value="0"></el-switch>
          </el-form-item>
        </el-col>
        <el-col :span="12">
          <el-form-item label="外网地址">
            <el-input v-model="dataForm.externalAddress" placeholder="外网访问地址 / URL"></el-input>
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
          <el-form-item label="描述">
            <el-input v-model="dataForm.description" placeholder="描述"></el-input>
          </el-form-item>
        </el-col>
        <el-col :span="24">
          <el-form-item label="备注">
            <el-input v-model="dataForm.remark" type="textarea" :rows="2" placeholder="备注"></el-input>
          </el-form-item>
        </el-col>
      </el-row>

      <div class="form-section-title">应用交付</div>
      <div v-if="dataForm.adEnabled === 1">
        <el-row :gutter="16">
          <el-col :span="12">
            <el-form-item label="虚拟服务名称" prop="delivery.virtualServiceName">
              <el-input v-model="dataForm.delivery.virtualServiceName" placeholder="虚拟服务名称"></el-input>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="虚拟服务IP" prop="delivery.virtualServiceIp">
              <el-input v-model="dataForm.delivery.virtualServiceIp" placeholder="虚拟服务IP"></el-input>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="虚拟服务端口" prop="delivery.virtualServicePort">
              <el-input-number v-model="dataForm.delivery.virtualServicePort" :min="1" :max="65535" style="width: 100%"></el-input-number>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="虚拟服务协议" prop="delivery.virtualServiceProtocol">
              <el-select v-model="dataForm.delivery.virtualServiceProtocol" placeholder="协议" style="width: 100%">
                <el-option label="HTTP" value="HTTP"></el-option>
                <el-option label="HTTPS" value="HTTPS"></el-option>
                <el-option label="TCP" value="TCP"></el-option>
              </el-select>
            </el-form-item>
          </el-col>
          <el-col :span="8">
            <el-form-item label="负载策略" prop="delivery.loadStrategy">
              <el-input v-model="dataForm.delivery.loadStrategy" placeholder="负载策略"></el-input>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="节点池名称" prop="delivery.poolName">
              <el-input v-model="dataForm.delivery.poolName" placeholder="节点池名称"></el-input>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="应用交付备注">
              <el-input v-model="dataForm.delivery.remark" placeholder="应用交付备注"></el-input>
            </el-form-item>
          </el-col>
        </el-row>

        <div class="node-toolbar">
          <span class="node-toolbar__title">节点池明细</span>
          <el-button type="primary" plain @click="addNode">新增节点</el-button>
        </div>
        <el-table :data="dataForm.delivery.nodes" border>
          <el-table-column label="节点IP" min-width="180">
            <template #default="{ row }">
              <el-input v-model="row.nodeIp" placeholder="节点IP"></el-input>
            </template>
          </el-table-column>
          <el-table-column label="节点端口" width="140">
            <template #default="{ row }">
              <el-input-number v-model="row.nodePort" :min="1" :max="65535" style="width: 100%"></el-input-number>
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
      <el-empty v-else description="未启用应用交付"></el-empty>

      <div class="form-section-title">解析配置</div>
      <el-row :gutter="16">
        <el-col :span="12" v-if="dataForm.internalEnabled === 1">
          <div class="sub-section-title">内网解析</div>
          <el-form-item label="解析方式" prop="dnsInternal.resolveMode">
            <el-select v-model="dataForm.dnsInternal.resolveMode" placeholder="解析方式" style="width: 100%">
              <el-option label="AD" value="AD"></el-option>
              <el-option label="DIRECT" value="DIRECT"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item label="目标IP" prop="dnsInternal.dnsTargetIp">
            <el-input v-model="dataForm.dnsInternal.dnsTargetIp" placeholder="目标IP"></el-input>
          </el-form-item>
          <el-form-item label="备注">
            <el-input v-model="dataForm.dnsInternal.remark" placeholder="备注"></el-input>
          </el-form-item>
        </el-col>
        <el-col :span="12" v-if="dataForm.externalEnabled === 1">
          <div class="sub-section-title">外网解析</div>
          <el-form-item label="解析方式" prop="dnsExternal.resolveMode">
            <el-select v-model="dataForm.dnsExternal.resolveMode" placeholder="解析方式" style="width: 100%">
              <el-option label="AD" value="AD"></el-option>
              <el-option label="DIRECT" value="DIRECT"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item label="DNS记录值" prop="dnsExternal.recordValue">
            <el-input v-model="dataForm.dnsExternal.recordValue" placeholder="公网IP / 记录值"></el-input>
          </el-form-item>
          <el-form-item label="备注">
            <el-input v-model="dataForm.dnsExternal.remark" placeholder="备注"></el-input>
          </el-form-item>
        </el-col>
      </el-row>

      <template v-if="dataForm.adEnabled === 1 && dataForm.externalEnabled === 1">
        <div class="sub-section-title">防火墙映射</div>
        <el-row :gutter="16">
          <el-col :span="12">
            <el-form-item label="公网IP" prop="firewallMapping.publicIp">
              <el-input v-model="dataForm.firewallMapping.publicIp" placeholder="公网IP"></el-input>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="外部端口" prop="firewallMapping.externalPort">
              <el-input-number v-model="dataForm.firewallMapping.externalPort" :min="1" :max="65535" style="width: 100%"></el-input-number>
            </el-form-item>
          </el-col>
          <el-col :span="12">
            <el-form-item label="内部IP" prop="firewallMapping.internalIp">
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
      </template>
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
import { reactive, ref, watch } from "vue";
import baseService from "@/service/baseService";
import { ElMessage } from "element-plus";

const emit = defineEmits(["refreshDataList"]);

const visible = ref(false);
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
  adEnabled: 0,
  internalEnabled: 1,
  externalEnabled: 0,
  externalAddress: "",
  description: "",
  projectOwner: "",
  applyTime: "",
  remark: "",
  delivery: {
    virtualServiceName: "",
    virtualServiceIp: "",
    virtualServicePort: 80,
    virtualServiceProtocol: "HTTP",
    poolName: "",
    loadStrategy: "",
    remark: "",
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
  projectOwner: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  applyTime: [{ required: true, message: "必填项不能为空", trigger: "change" }]
};

watch(
  () => dataForm.adEnabled,
  (value) => {
    dataForm.dnsInternal.resolveMode = value === 1 ? "AD" : "DIRECT";
    dataForm.dnsExternal.resolveMode = value === 1 ? "AD" : "DIRECT";
    if (value !== 1) {
      dataForm.firewallMapping = createForm().firewallMapping;
    }
  }
);

watch(
  () => dataForm.externalEnabled,
  (value) => {
    if (value !== 1) {
      dataForm.firewallMapping = createForm().firewallMapping;
      dataForm.dnsExternal = createForm().dnsExternal;
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
    adEnabled: dataForm.adEnabled,
    internalEnabled: dataForm.internalEnabled,
    externalEnabled: dataForm.externalEnabled,
    externalAddress: dataForm.externalAddress,
    description: dataForm.description,
    projectOwner: dataForm.projectOwner,
    applyTime: dataForm.applyTime,
    remark: dataForm.remark
  };
  if (dataForm.adEnabled === 1) {
    payload.delivery = {
      ...dataForm.delivery,
      nodes: dataForm.delivery.nodes.filter((item) => item.nodeIp && item.nodePort)
    };
  }
  if (dataForm.internalEnabled === 1) {
    payload.dnsInternal = { ...dataForm.dnsInternal };
  }
  if (dataForm.externalEnabled === 1) {
    payload.dnsExternal = { ...dataForm.dnsExternal };
  }
  if (dataForm.adEnabled === 1 && dataForm.externalEnabled === 1) {
    payload.firewallMapping = { ...dataForm.firewallMapping };
  }
  return payload;
};

const init = (id?: number) => {
  visible.value = true;
  resetForm();
  if (id) {
    baseService.get(`/ops/domain-record/${id}`).then((res) => {
      Object.assign(dataForm, createForm(), res.data);
      dataForm.delivery = Object.assign(createForm().delivery, res.data?.delivery || {});
      dataForm.delivery.nodes = res.data?.delivery?.nodes?.length ? res.data.delivery.nodes : [createNode()];
      dataForm.dnsInternal = Object.assign(createForm().dnsInternal, res.data?.dnsInternal || {});
      dataForm.dnsExternal = Object.assign(createForm().dnsExternal, res.data?.dnsExternal || {});
      dataForm.firewallMapping = Object.assign(createForm().firewallMapping, res.data?.firewallMapping || {});
    });
  }
};

const submitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
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
  max-height: 70vh;
  overflow-y: auto;
  padding-right: 8px;
}

.form-section-title {
  margin: 8px 0 16px;
  font-size: 15px;
  font-weight: 600;
  color: #1f2937;
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
}

.node-toolbar__title {
  font-size: 14px;
  font-weight: 600;
}
</style>
