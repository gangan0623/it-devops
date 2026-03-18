import { nextTick, reactive, ref, type Ref } from "vue";

export const MESSAGE_DURATION = {
  success: 2000,
  info: 2000,
  warning: 3000,
  error: 0
} as const;

export const validateForm = (formRef: Ref<any>) =>
  new Promise<boolean>((resolve) => {
    formRef.value.validate((valid: boolean) => resolve(valid));
  });

export const useSavePulse = () => {
  const pulseSave = ref(false);

  const triggerSavePulse = () => {
    nextTick(() => {
      pulseSave.value = true;
      setTimeout(() => {
        pulseSave.value = false;
      }, 700);
    });
  };

  return {
    pulseSave,
    triggerSavePulse
  };
};

export const useLoadingState = <T extends Record<string, boolean>>(initialState: T) => {
  const loading = reactive({ ...initialState }) as T;

  const withLoading = async <K extends keyof T, R>(key: K, task: () => Promise<R>) => {
    loading[key] = true;
    try {
      return await task();
    } finally {
      loading[key] = false;
    }
  };

  return {
    loading,
    withLoading
  };
};

export const assignConfig = <T extends Record<string, any>>(target: T, defaults: Partial<T>, data?: Partial<T> | null) => {
  Object.assign(target, defaults, data || {});
};
