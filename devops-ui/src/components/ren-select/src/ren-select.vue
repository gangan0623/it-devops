<template>
  <el-select v-model="value" @change="$emit('update:modelValue', $event)" :placeholder="placeholder" clearable>
    <el-option
      :label="data[labelField]"
      v-for="data in dataList"
      :key="data[valueField]"
      :value="data[valueField]"
    >
      {{ data[labelField] }}
    </el-option>
  </el-select>
</template>
<script lang="ts">
import {computed, defineComponent} from "vue";
import {getDictDataList} from "@/utils/utils";
import {useAppStore} from "@/store";

export default defineComponent({
  name: "RenSelect",
  props: {
    modelValue: [Number, String],
    dictType: String,
    placeholder: String,
    labelField: {
      type: String,
      default: "dictLabel"
    },
    valueField: {
      type: String,
      default: "dictValue"
    }
  },
  setup(props) {
    const store = useAppStore();
    return {
      value: computed(() => `${props.modelValue}`),
      dataList: getDictDataList(store.state.dicts, props.dictType)
    };
  }
});
</script>
