<template>
  <el-dialog
    v-model="visible"
    :title="!dataForm.id ? '新增定时任务' : '修改定时任务'"
    width="980px"
    :close-on-click-modal="false"
    :close-on-press-escape="false"
    class="schedule-dialog"
  >
    <el-form :model="dataForm" :rules="rules" ref="dataFormRef" label-width="100px">
      <div class="schedule-dialog__content">
        <section class="schedule-card">
          <div class="schedule-card__title">基础信息</div>
          <el-form-item prop="beanName" label="bean名称">
            <el-input v-model="dataForm.beanName" placeholder="spring bean名称, 如: testTask"></el-input>
          </el-form-item>
          <el-form-item prop="params" label="参数">
            <el-input v-model="dataForm.params" placeholder="参数"></el-input>
          </el-form-item>
          <el-form-item prop="remark" label="备注">
            <el-input v-model="dataForm.remark" type="textarea" :rows="3" placeholder="备注"></el-input>
          </el-form-item>
        </section>
        <section class="schedule-card">
          <div class="schedule-card__title">执行策略</div>
          <el-form-item prop="cronExpression">
            <cron-builder v-model="dataForm.cronExpression" />
          </el-form-item>
        </section>
      </div>
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
import CronBuilder from "@/components/cron-builder/index.vue";

const emit = defineEmits(["refreshDataList"]);

const visible = ref(false);
const dataFormRef = ref();
const submitLoading = ref(false);

const dataForm = reactive({
  id: "",
  beanName: "",
  params: "",
  cronExpression: "",
  remark: "",
  status: 0
});

const rules = ref({
  beanName: [{ required: true, message: "必填项不能为空", trigger: "blur" }],
  cronExpression: [{ required: true, message: "必填项不能为空", trigger: "change" }]
});

const init = (id?: number) => {
  visible.value = true;
  dataForm.id = "";

  if (dataFormRef.value) {
    dataFormRef.value.resetFields();
  }

  if (id) {
    getInfo(id);
  }
};

const getInfo = (id: number) => {
  baseService.get(`/sys/schedule/${id}`).then((res) => {
    Object.assign(dataForm, res.data);
  });
};

const dataFormSubmitHandle = () => {
  dataFormRef.value.validate((valid: boolean) => {
    if (!valid) {
      return false;
    }
    submitLoading.value = true;
    const fn = !dataForm.id ? baseService.post("/sys/schedule", dataForm) : baseService.put("/sys/schedule", dataForm);
    fn.then((res) => {
      ElMessage.success({
        message: "成功",
        duration: 500,
        onClose: () => {
          visible.value = false;
          emit("refreshDataList");
        }
      });
    }).finally(() => {
      submitLoading.value = false;
    });
  });
};

defineExpose({
  init
});
</script>

<style lang="less" scoped>
.schedule-dialog__content {
  display: grid;
  grid-template-columns: minmax(280px, 1fr) minmax(400px, 1.4fr);
  gap: 14px;
}

.schedule-card {
  padding: 16px;
  background: #ffffff;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
}

.schedule-card__title {
  margin-bottom: 14px;
  font-size: 14px;
  font-weight: 600;
  color: #0f172a;
}

.schedule-card :deep(.el-form-item) {
  margin-bottom: 16px;
}

.schedule-card :deep(.el-form-item:last-child) {
  margin-bottom: 0;
}

.schedule-card :deep(.el-form-item__label) {
  font-size: 13px;
}

// 右侧卡片的 cron-builder 不需要 form-item 的 label
.schedule-card:last-child :deep(.el-form-item__label) {
  display: none;
}

.schedule-card:last-child :deep(.el-form-item__content) {
  margin-left: 0 !important;
}

@media (max-width: 980px) {
  .schedule-dialog__content {
    grid-template-columns: 1fr;
  }
}
</style>
