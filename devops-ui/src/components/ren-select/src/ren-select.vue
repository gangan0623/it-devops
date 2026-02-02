<template>
  <el-select v-model="value" :placeholder="placeholder" clearable>
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
  setup(props, { emit }) {
    const store = useAppStore();
    return {
      value: computed({
        get: () => (props.modelValue === undefined ? null : props.modelValue),
        set: (val: number | string | null | undefined) => {
          const next = val === undefined ? null : val;
          emit("update:modelValue", next);
        }
      }),
      dataList: getDictDataList(store.state.dicts, props.dictType)
    };
  }
});
</script>
