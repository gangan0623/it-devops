<template>
  <div class="mod-ops__devicebackup-record">
    <el-form :inline="true" :model="state.dataForm" @keyup.enter="queryList()" class="ops-toolbar">
      <div class="ops-toolbar__row">
        <div class="ops-toolbar__group ops-filters">
          <el-form-item>
            <el-input v-model="state.dataForm.name" class="query-input" placeholder="主机名" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-input v-model="state.dataForm.ip" class="query-input" placeholder="IP" clearable></el-input>
          </el-form-item>
          <el-form-item>
            <el-select v-model="state.dataForm.status" placeholder="状态" clearable>
              <el-option label="已完成" :value="1"></el-option>
              <el-option label="异常" :value="0"></el-option>
            </el-select>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" :loading="state.dataListLoading" @click="queryList()">查询</el-button>
          </el-form-item>
          <el-form-item>
            <el-button class="query-btn" @click="handleToolbarReset">重置</el-button>
          </el-form-item>
        </div>
        <div class="ops-toolbar__group ops-actions">
          <div class="record-stats">
            <span class="record-stats__item record-stats__item--ok">已完成 {{ successCount }}</span>
            <span class="record-stats__item record-stats__item--bad">异常 {{ failCount }}</span>
          </div>
          <el-button v-if="state.hasPermission('ops:network-device-backup-record:delete')" type="danger" @click="state.deleteHandle()">删除</el-button>
        </div>
      </div>
    </el-form>

    <el-table v-loading="state.dataListLoading" :data="state.dataList" border @selection-change="state.dataListSelectionChangeHandle" class="backup-record-table" style="width: 100%">
      <el-table-column type="selection" header-align="center" align="center" width="50"></el-table-column>
      <el-table-column prop="name" label="主机名" header-align="center" align="center"></el-table-column>
      <el-table-column prop="ip" label="IP" header-align="center" align="center"></el-table-column>
      <el-table-column prop="url" label="备份URL" header-align="center" align="center"></el-table-column>
      <el-table-column prop="lastBackupTime" label="最后备份时间" header-align="center" align="center"></el-table-column>
      <el-table-column prop="lastBackupStatus" label="最后备份状态" header-align="center" align="center">
        <template v-slot="scope">
          <el-tag v-if="scope.row.lastBackupStatus === 1" size="small" type="success">已完成</el-tag>
          <el-tag v-else size="small" type="danger">异常</el-tag>
        </template>
      </el-table-column>
      <el-table-column prop="backupNum" label="备份次数" header-align="center" align="center"></el-table-column>
      <el-table-column label="操作" fixed="right" header-align="center" align="center" width="240">
        <template v-slot="scope">
          <el-button v-if="state.hasPermission('ops:network-device-backup-record:preview')" type="primary" link @click="openPreview(scope.row.url)">预览</el-button>
          <el-button type="primary" link @click="downloadFile(scope.row.url)">下载</el-button>
          <el-button v-if="state.hasPermission('ops:network-device-backup-record:history')" type="primary" link @click="openHistory(scope.row)">历史</el-button>
          <el-button v-if="state.hasPermission('ops:network-device-backup-record:delete')" type="primary" link @click="state.deleteHandle(scope.row.id)">删除</el-button>
        </template>
      </el-table-column>
    </el-table>

    <el-pagination
      :current-page="state.page"
      :page-sizes="[10, 20, 50, 100]"
      :page-size="state.limit"
      :total="state.total"
      layout="total, sizes, prev, pager, next, jumper"
      @size-change="state.pageSizeChangeHandle"
      @current-change="state.pageCurrentChangeHandle"
    ></el-pagination>

    <el-dialog v-model="historyVisible" title="备份历史" width="960px" class="history-dialog">
      <div class="history-toolbar">
        <div class="history-toolbar__left">
          <div class="history-stats">
            <span class="history-stats__item history-stats__item--ok">已完成 {{ historySuccessCount }}</span>
            <span class="history-stats__item history-stats__item--bad">异常 {{ historyFailCount }}</span>
          </div>
          <el-date-picker v-model="historyDateRange" type="daterange" range-separator="至" start-placeholder="开始日期" end-placeholder="结束日期" value-format="YYYY-MM-DD" size="small" style="width: 260px" @change="historyPage = 1"></el-date-picker>
          <el-switch v-model="showOnlyFailed" inline-prompt active-text="仅异常" inactive-text="全部" @change="historyPage = 1"></el-switch>
        </div>
        <el-button v-if="state.hasPermission('ops:network-device-backup-record:diff')" type="primary" @click="handleDiff" :disabled="historySelections.length < 1 || historySelections.length > 2">对比</el-button>
      </div>
      <el-table v-loading="historyLoading" :data="pagedHistoryList" border @selection-change="handleHistorySelectionChange" max-height="420" style="width: 100%">
        <el-table-column type="selection" width="50" header-align="center" align="center"></el-table-column>
        <el-table-column prop="name" label="主机名" header-align="center" align="center"></el-table-column>
        <el-table-column prop="ip" label="IP" header-align="center" align="center"></el-table-column>
        <el-table-column prop="url" label="备份URL" header-align="center" align="center" show-overflow-tooltip></el-table-column>
        <el-table-column prop="backupTime" label="备份时间" header-align="center" align="center" width="180"></el-table-column>
        <el-table-column prop="backupStatus" label="备份状态" header-align="center" align="center" width="100">
          <template v-slot="scope">
            <el-tag v-if="scope.row.backupStatus === 1" size="small" type="success">已完成</el-tag>
            <el-tag v-else size="small" type="danger">异常</el-tag>
          </template>
        </el-table-column>
        <el-table-column label="操作" header-align="center" align="center" width="140">
          <template v-slot="scope">
            <el-button v-if="state.hasPermission('ops:network-device-backup-record:preview')" type="primary" link @click="openPreview(scope.row.url)">预览</el-button>
            <el-button type="primary" link @click="downloadFile(scope.row.url)">下载</el-button>
          </template>
        </el-table-column>
      </el-table>
      <el-pagination
        v-if="filteredHistoryList.length > historyPageSize"
        :current-page="historyPage"
        :page-size="historyPageSize"
        :page-sizes="[20, 50, 100]"
        :total="filteredHistoryList.length"
        layout="total, sizes, prev, pager, next"
        @size-change="(val: number) => { historyPageSize = val; historyPage = 1; }"
        @current-change="(val: number) => { historyPage = val; }"
        style="margin-top: 12px; justify-content: flex-end"
      ></el-pagination>
    </el-dialog>

    <el-dialog v-model="diffVisible" title="备份对比" width="100%" top="2vh" class="diff-dialog">
      <div class="diff-summary">
        <span class="diff-summary__mode">{{ diffModeText }}</span>
        <span class="diff-summary__total">共 {{ diffLines.length }} 行</span>
        <span class="diff-summary__same">未变 {{ diffSameCount }}</span>
        <span class="diff-summary__change">~ 修改 {{ diffChangeCount }}</span>
        <span class="diff-summary__add">+ 新增 {{ diffAddCount }}</span>
        <span class="diff-summary__del">- 删除 {{ diffDelCount }}</span>
        <div class="diff-summary__nav">
          <button type="button" class="diff-nav-btn" @click="jumpDiff(-1)" :disabled="diffAnchorCount === 0">上一处差异</button>
          <button type="button" class="diff-nav-btn" @click="jumpDiff(1)" :disabled="diffAnchorCount === 0">下一处差异</button>
          <span class="diff-summary__index" v-if="diffAnchorCount > 0">{{ activeDiffIndex + 1 }}/{{ diffAnchorCount }}</span>
        </div>
      </div>
      <div v-if="diffLoading" class="diff-loading">
        <div v-for="i in 10" :key="i" class="diff-loading__row"></div>
      </div>
      <div v-else class="diff-wrap">
        <div class="diff-grid">
          <div class="diff-side diff-side--left" ref="leftPanelRef" @scroll="onLeftScroll">
            <div class="diff-side__header">{{ diffLeftTitle }}</div>
            <div :style="{ height: totalDiffHeight + 'px', position: 'relative' }">
              <template v-for="(line, index) in displayDiffLines" :key="`left-${line._key || index}`">
                <div
                  v-if="index >= leftRange.start && index <= leftRange.end"
                  class="diff-line"
                  :class="line.collapsed ? 'diff-line--collapsed' : `diff-line--${line.type}`"
                  :data-diff-anchor="line.diffAnchor ? line.diffAnchor : null"
                  :data-diff-active="line.diffAnchor && activeDiffIndex === line.diffAnchor - 1 ? '1' : null"
                  :style="{ position: 'absolute', top: rowTops[index] + 'px', width: '100%' }"
                >
                  <template v-if="line.collapsed">
                    <button type="button" class="diff-line__fold" @click="expandCollapsed(line.groupId)">
                      展开 {{ line.hiddenCount }} 行未变内容
                    </button>
                  </template>
                  <template v-else>
                    <span class="diff-line__num">{{ line.leftLineNo || "" }}</span>
                    <span class="diff-line__prefix">{{ leftPrefix(line) }}</span>
                    <span class="diff-line__text" v-html="renderDiffHtml(line, 'left')"></span>
                  </template>
                </div>
              </template>
            </div>
          </div>
          <div class="diff-side diff-side--right" ref="rightPanelRef" @scroll="onRightScroll">
            <div class="diff-side__header">{{ diffRightTitle }}</div>
            <div :style="{ height: totalDiffHeight + 'px', position: 'relative' }">
              <template v-for="(line, index) in displayDiffLines" :key="`right-${line._key || index}`">
                <div
                  v-if="index >= rightRange.start && index <= rightRange.end"
                  class="diff-line"
                  :class="line.collapsed ? 'diff-line--collapsed' : `diff-line--${line.type}`"
                  :data-diff-anchor="line.diffAnchor ? line.diffAnchor : null"
                  :data-diff-active="line.diffAnchor && activeDiffIndex === line.diffAnchor - 1 ? '1' : null"
                  :style="{ position: 'absolute', top: rowTops[index] + 'px', width: '100%' }"
                >
                  <template v-if="line.collapsed">
                    <button type="button" class="diff-line__fold" @click="expandCollapsed(line.groupId)">
                      展开 {{ line.hiddenCount }} 行未变内容
                    </button>
                  </template>
                  <template v-else>
                    <span class="diff-line__num">{{ line.rightLineNo || "" }}</span>
                    <span class="diff-line__prefix">{{ rightPrefix(line) }}</span>
                    <span class="diff-line__text" v-html="renderDiffHtml(line, 'right')"></span>
                  </template>
                </div>
              </template>
            </div>
          </div>
        </div>
      </div>
      <div class="diff-scrollbar">
        <div class="diff-scrollbar__spacer"></div>
      </div>
    </el-dialog>

    <el-dialog v-model="previewVisible" title="备份预览" width="80%" top="6vh" class="preview-dialog">
      <div class="preview-wrap">
        <div class="preview-view">
          <pre>{{ previewContent }}</pre>
        </div>
      </div>
      <div class="preview-scrollbar">
        <div class="preview-scrollbar__spacer"></div>
      </div>
    </el-dialog>
  </div>
</template>

<script lang="ts" setup>
import useView from "@/hooks/useView";
import {computed, nextTick, reactive, ref, toRefs} from "vue";
import baseService from "@/service/baseService";
import app from "@/constants/app";
import {getToken} from "@/utils/cache";
import {ElMessage} from "element-plus";

const view = reactive({
  deleteIsBatch: true,
  getDataListURL: "/ops/network-device-backup-record/page",
  getDataListIsPage: true,
  deleteURL: "/ops/network-device-backup-record",
  dataForm: {
    name: "",
    ip: "",
    status: ""
  }
});

const state = reactive({ ...useView(view), ...toRefs(view) });
const successCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.lastBackupStatus) === 1).length);
const failCount = computed(() => (state.dataList || []).filter((item: any) => Number(item?.lastBackupStatus) === 0).length);

const historyVisible = ref(false);
const historyLoading = ref(false);
const historyList = ref<any[]>([]);
const historySelections = ref<any[]>([]);
const showOnlyFailed = ref(false);
const historyDateRange = ref<string[] | null>(null);
const historyPage = ref(1);
const historyPageSize = ref(20);
const diffVisible = ref(false);
const diffLoading = ref(false);
const ROW_HEIGHT = 26;
const COLLAPSED_HEIGHT = 36;
const VIRT_BUFFER = 20;

const leftScrollTop = ref(0);
const rightScrollTop = ref(0);
const leftPanelRef = ref<HTMLElement | null>(null);
const rightPanelRef = ref<HTMLElement | null>(null);
const diffLines = ref<any[]>([]);
const diffLeftTitle = ref("旧版本");
const diffRightTitle = ref("当前版本");
const diffAddCount = computed(() => diffLines.value.filter((l: any) => l.type === "add").length);
const diffDelCount = computed(() => diffLines.value.filter((l: any) => l.type === "del").length);
const diffSameCount = computed(() => diffLines.value.filter((l: any) => l.type === "same").length);
const diffChangeCount = computed(() => diffLines.value.filter((l: any) => l.type === "change").length);
const diffModeText = computed(() => `${diffLeftTitle.value} vs ${diffRightTitle.value}`);
const expandedCollapsedGroups = ref<number[]>([]);
const DIFF_CONTEXT_LINES = 3;
const DIFF_COLLAPSE_THRESHOLD = 12;
const activeDiffIndex = ref(0);
const currentIp = ref("");
const previewVisible = ref(false);
const previewContent = ref("");
const queryList = () => {
  state.getDataList();
};
const handleToolbarReset = () => {
  state.dataForm.name = "";
  state.dataForm.ip = "";
  state.dataForm.status = "";
  queryList();
};
const historySuccessCount = computed(() => (historyList.value || []).filter((item: any) => Number(item?.backupStatus) === 1).length);
const historyFailCount = computed(() => (historyList.value || []).filter((item: any) => Number(item?.backupStatus) === 0).length);
const filteredHistoryList = computed(() => {
  let list = historyList.value || [];
  if (showOnlyFailed.value) {
    list = list.filter((item: any) => Number(item?.backupStatus) === 0);
  }
  if (historyDateRange.value && historyDateRange.value.length === 2) {
    const [start, end] = historyDateRange.value;
    list = list.filter((item: any) => {
      const t = String(item?.backupTime || "").slice(0, 10);
      return t >= start && t <= end;
    });
  }
  return list;
});
const pagedHistoryList = computed(() => {
  const start = (historyPage.value - 1) * historyPageSize.value;
  return filteredHistoryList.value.slice(start, start + historyPageSize.value);
});
const displayDiffLines = computed(() => {
  const list = diffLines.value || [];
  const result: any[] = [];
  let sameStart = -1;
  let groupId = 0;
  let diffAnchor = 0;
  const flushSameBlock = (endExclusive: number) => {
    if (sameStart < 0) {
      return;
    }
    const block = list.slice(sameStart, endExclusive);
    if (block.length <= DIFF_COLLAPSE_THRESHOLD || expandedCollapsedGroups.value.includes(groupId)) {
      block.forEach((item: any, idx: number) => result.push({ ...item, _key: `same-${sameStart + idx}` }));
    } else {
      const head = block.slice(0, DIFF_CONTEXT_LINES);
      const tail = block.slice(-DIFF_CONTEXT_LINES);
      head.forEach((item: any, idx: number) => result.push({ ...item, _key: `same-head-${sameStart + idx}` }));
      result.push({
        collapsed: true,
        hiddenCount: block.length - head.length - tail.length,
        groupId,
        _key: `collapsed-${groupId}`
      });
      tail.forEach((item: any, idx: number) => result.push({ ...item, _key: `same-tail-${endExclusive - tail.length + idx}` }));
    }
    groupId++;
    sameStart = -1;
  };
  for (let i = 0; i < list.length; i++) {
    const item = list[i];
    if (item?.type === "same") {
      if (sameStart < 0) {
        sameStart = i;
      }
      continue;
    }
    flushSameBlock(i);
    diffAnchor++;
    result.push({ ...item, diffAnchor, _key: `diff-${i}` });
  }
  flushSameBlock(list.length);
  return result;
});
const diffAnchorCount = computed(() => displayDiffLines.value.filter((item: any) => item.diffAnchor).length);

const rowTops = computed(() => {
  const tops: number[] = [];
  let acc = 0;
  for (const line of displayDiffLines.value) {
    tops.push(acc);
    acc += line.collapsed ? COLLAPSED_HEIGHT : ROW_HEIGHT;
  }
  return tops;
});

const totalDiffHeight = computed(() => {
  const list = displayDiffLines.value;
  if (list.length === 0) return 0;
  const last = list[list.length - 1];
  return rowTops.value[list.length - 1] + (last.collapsed ? COLLAPSED_HEIGHT : ROW_HEIGHT);
});

const leftRange = computed(() => {
  const tops = rowTops.value;
  const list = displayDiffLines.value;
  if (list.length === 0) return { start: 0, end: 0 };
  const scrollTop = leftScrollTop.value;
  const containerHeight = 600;
  const targetTop = Math.max(0, scrollTop - VIRT_BUFFER * ROW_HEIGHT);
  let lo = 0, hi = tops.length - 1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (tops[mid] < targetTop) lo = mid + 1;
    else hi = mid;
  }
  const start = lo;
  const endTop = scrollTop + containerHeight + VIRT_BUFFER * ROW_HEIGHT;
  let end = start;
  while (end < tops.length - 1 && tops[end] <= endTop) end++;
  return { start, end };
});

const rightRange = computed(() => {
  const tops = rowTops.value;
  const list = displayDiffLines.value;
  if (list.length === 0) return { start: 0, end: 0 };
  const scrollTop = rightScrollTop.value;
  const containerHeight = 600;
  const targetTop = Math.max(0, scrollTop - VIRT_BUFFER * ROW_HEIGHT);
  let lo = 0, hi = tops.length - 1;
  while (lo < hi) {
    const mid = (lo + hi) >> 1;
    if (tops[mid] < targetTop) lo = mid + 1;
    else hi = mid;
  }
  const start = lo;
  const endTop = scrollTop + containerHeight + VIRT_BUFFER * ROW_HEIGHT;
  let end = start;
  while (end < tops.length - 1 && tops[end] <= endTop) end++;
  return { start, end };
});

const openHistory = (row: any) => {
  if (!row?.ip) {
    return;
  }
  currentIp.value = row.ip;
  historyVisible.value = true;
  historyLoading.value = true;
  historySelections.value = [];
  showOnlyFailed.value = false;
  historyDateRange.value = null;
  historyPage.value = 1;
  baseService
    .get("/ops/network-device-backup-record/history", { ip: row.ip, limit: 200 })
    .then((res) => {
      historyList.value = res.data || [];
    })
    .finally(() => {
      historyLoading.value = false;
    });
};

const handleHistorySelectionChange = (rows: any[]) => {
  historySelections.value = rows || [];
};

const handleDiff = () => {
  if (historySelections.value.length < 1 || historySelections.value.length > 2) {
    return ElMessage.warning("请选择一条或两条历史记录进行对比");
  }
  diffVisible.value = true;
  diffLines.value = [];
  expandedCollapsedGroups.value = [];
  activeDiffIndex.value = 0;
  diffLoading.value = true;

  if (historySelections.value.length === 1) {
    const [history] = historySelections.value;
    diffLeftTitle.value = formatHistoryTitle(history);
    diffRightTitle.value = "当前版本";
    baseService
      .get("/ops/network-device-backup-record/diff-current", { ip: currentIp.value, historyId: history.id })
      .then((res) => {
        diffLines.value = res.data || [];
        nextTick(() => { setupDiffSync(); scrollToFirstDiff(); });
      })
      .catch(() => ElMessage.error("加载对比数据失败"))
      .finally(() => { diffLoading.value = false; });
    return;
  }
  const [left, right] = [...historySelections.value].sort((a: any, b: any) => {
    return new Date(a?.backupTime || 0).getTime() - new Date(b?.backupTime || 0).getTime();
  });
  diffLeftTitle.value = formatHistoryTitle(left);
  diffRightTitle.value = formatHistoryTitle(right);
  baseService
    .get("/ops/network-device-backup-record/diff", { leftId: left.id, rightId: right.id })
    .then((res) => {
      diffLines.value = res.data || [];
      nextTick(() => { setupDiffSync(); scrollToFirstDiff(); });
    })
    .catch(() => ElMessage.error("加载对比数据失败"))
    .finally(() => { diffLoading.value = false; });
};

const formatHistoryTitle = (row: any) => {
  const backupTime = row?.backupTime || "-";
  return `历史版本 ${backupTime}`;
};

const leftPrefix = (line: any) => {
  if (line?.type === "del" || line?.type === "change") return "-";
  return "";
};

const rightPrefix = (line: any) => {
  if (line?.type === "add" || line?.type === "change") return "+";
  return "";
};

const renderDiffHtml = (line: any, side: "left" | "right") => {
  const content = String(side === "left" ? line?.leftContent || "" : line?.rightContent || "");
  if (line?.type !== "change") {
    return escapeHtml(content);
  }
  const left = String(line?.leftContent || "");
  const right = String(line?.rightContent || "");
  const [prefixLen, suffixLen] = diffBounds(left, right);
  const target = side === "left" ? left : right;
  const changedEnd = Math.max(prefixLen, target.length - suffixLen);
  const before = target.slice(0, prefixLen);
  const changed = target.slice(prefixLen, changedEnd);
  const after = target.slice(changedEnd);
  if (!changed) {
    return escapeHtml(target);
  }
  return `${escapeHtml(before)}<span class="diff-line__inline-change">${escapeHtml(changed)}</span>${escapeHtml(after)}`;
};

const diffBounds = (left: string, right: string) => {
  let prefixLen = 0;
  const minLen = Math.min(left.length, right.length);
  while (prefixLen < minLen && left[prefixLen] === right[prefixLen]) {
    prefixLen++;
  }
  let suffixLen = 0;
  const leftRemain = left.length - prefixLen;
  const rightRemain = right.length - prefixLen;
  const suffixMax = Math.min(leftRemain, rightRemain);
  while (
    suffixLen < suffixMax &&
    left[left.length - 1 - suffixLen] === right[right.length - 1 - suffixLen]
  ) {
    suffixLen++;
  }
  return [prefixLen, suffixLen];
};

const escapeHtml = (value: string) =>
  value
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")
    .replaceAll(">", "&gt;")
    .replaceAll("\"", "&quot;")
    .replaceAll("'", "&#39;");

const expandCollapsed = (groupId: number) => {
  if (!expandedCollapsedGroups.value.includes(groupId)) {
    expandedCollapsedGroups.value = [...expandedCollapsedGroups.value, groupId];
    nextTick(() => {
      setupDiffSync();
    });
  }
};

const jumpDiff = (step: number) => {
  const total = diffAnchorCount.value;
  if (total === 0) {
    return;
  }
  activeDiffIndex.value = (activeDiffIndex.value + step + total) % total;
  nextTick(() => {
    scrollToDiff(activeDiffIndex.value + 1);
  });
};

const scrollToDiff = (anchor: number) => {
  const left = document.querySelector(`.diff-side--left [data-diff-anchor="${anchor}"]`) as HTMLElement | null;
  const right = document.querySelector(`.diff-side--right [data-diff-anchor="${anchor}"]`) as HTMLElement | null;
  if (left) {
    left.scrollIntoView({ block: "center", behavior: "smooth" });
  }
  if (right) {
    right.scrollIntoView({ block: "center", behavior: "smooth" });
  }
};

const scrollToFirstDiff = () => {
  if (diffAnchorCount.value <= 0) {
    return;
  }
  activeDiffIndex.value = 0;
  scrollToDiff(1);
};

const openPreview = (url: string) => {
  if (!url) {
    return ElMessage.warning("URL为空");
  }
  previewVisible.value = true;
  previewContent.value = "加载中...";
  baseService
    .get("/ops/network-device-backup-record/preview", { url })
    .then((res) => {
      previewContent.value = res.data || "";
      nextTick(() => {
        setupPreviewSync();
      });
    })
    .catch(() => {
      previewContent.value = "加载失败";
    });
};

const downloadFile = (url: string) => {
  if (!url) {
    return ElMessage.warning("URL为空");
  }
  const token = getToken();
  const downloadUrl = `${app.api}/ops/network-device-backup-record/download?url=${encodeURIComponent(url)}&token=${encodeURIComponent(token)}`;
  window.open(downloadUrl, "_blank");
};

const setupDiffSync = () => {
const onLeftScroll = (e: Event) => {
  leftScrollTop.value = (e.target as HTMLElement).scrollTop;
};
const onRightScroll = (e: Event) => {
  rightScrollTop.value = (e.target as HTMLElement).scrollTop;
};
  const left = document.querySelector(".diff-side--left") as HTMLElement | null;
  const right = document.querySelector(".diff-side--right") as HTMLElement | null;
  const bar = document.querySelector(".diff-scrollbar") as HTMLElement | null;
  const spacer = document.querySelector(".diff-scrollbar__spacer") as HTMLElement | null;
  if (!left || !right || !bar || !spacer) {
    return;
  }
  const maxScroll = Math.max(left.scrollWidth, right.scrollWidth);
  spacer.style.width = `${maxScroll}px`;
  let syncing = false;
  const syncFromBar = () => {
    if (syncing) {
      return;
    }
    syncing = true;
    left.scrollLeft = bar.scrollLeft;
    right.scrollLeft = bar.scrollLeft;
    syncing = false;
  };
  const syncFromSide = (source: HTMLElement) => {
    if (syncing) {
      return;
    }
    syncing = true;
    bar.scrollLeft = source.scrollLeft;
    if (source === left) {
      right.scrollLeft = source.scrollLeft;
      right.scrollTop = source.scrollTop;
    } else {
      left.scrollLeft = source.scrollLeft;
      left.scrollTop = source.scrollTop;
    }
    syncing = false;
  };
  bar.onscroll = syncFromBar;
  left.onscroll = () => syncFromSide(left);
  right.onscroll = () => syncFromSide(right);
};

const setupPreviewSync = () => {
  const view = document.querySelector(".preview-view") as HTMLElement | null;
  const bar = document.querySelector(".preview-scrollbar") as HTMLElement | null;
  const spacer = document.querySelector(".preview-scrollbar__spacer") as HTMLElement | null;
  if (!view || !bar || !spacer) {
    return;
  }
  spacer.style.width = `${view.scrollWidth}px`;
  let syncing = false;
  const syncFromBar = () => {
    if (syncing) return;
    syncing = true;
    view.scrollLeft = bar.scrollLeft;
    syncing = false;
  };
  const syncFromView = () => {
    if (syncing) return;
    syncing = true;
    bar.scrollLeft = view.scrollLeft;
    syncing = false;
  };
  bar.onscroll = syncFromBar;
  view.onscroll = syncFromView;
};
</script>

<style lang="less" scoped>
/* 统计标签容器 */
.record-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

/* 统计标签基础样式 */
.record-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}

/* 表格单元格不换行 */
.mod-ops__devicebackup-record :deep(.el-table .cell) {
  white-space: nowrap;
}

/* 历史弹窗 */
.history-dialog :deep(.el-dialog__body) {
  max-height: 72vh;
  overflow-y: auto;
}

.history-toolbar {
  display: flex;
  justify-content: space-between;
  align-items: center;
  gap: 12px;
  margin-bottom: 14px;
  padding: 10px 14px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
}

.history-toolbar__left {
  display: flex;
  align-items: center;
  gap: 12px;
  flex-wrap: wrap;
}

.history-stats {
  display: flex;
  align-items: center;
  gap: 8px;
}

.history-stats__item {
  padding: 4px 10px;
  border-radius: 4px;
  font-size: 12px;
  font-weight: 500;
}

.history-stats__item--ok {
  color: #065f46;
  background: #ecfdf5;
  border: 1px solid #a7f3d0;
}

.history-stats__item--bad {
  color: #991b1b;
  background: #fef2f2;
  border: 1px solid #fecaca;
}

/* 差异统计摘要 */
.diff-summary {
  display: flex;
  align-items: center;
  gap: 14px;
  padding: 10px 14px;
  margin-bottom: 10px;
  background: #f8fafc;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  font-size: 12px;
  font-weight: 500;
}

.diff-summary__total {
  color: #475569;
}

.diff-summary__same {
  color: #64748b;
}

.diff-summary__add {
  color: #059669;
  padding: 2px 8px;
  background: #ecfdf5;
  border-radius: 4px;
}

.diff-summary__change {
  color: #b45309;
  padding: 2px 8px;
  background: #fffbeb;
  border-radius: 4px;
}

.diff-summary__del {
  color: #dc2626;
  padding: 2px 8px;
  background: #fef2f2;
  border-radius: 4px;
}

.diff-summary__nav {
  display: inline-flex;
  align-items: center;
  gap: 8px;
  margin-left: auto;
}

.diff-summary__index {
  color: #64748b;
}

.diff-nav-btn {
  border: 1px solid #cbd5e1;
  background: #fff;
  color: #334155;
  border-radius: 6px;
  padding: 4px 10px;
  font-size: 12px;
  cursor: pointer;
}

.diff-nav-btn:hover:not(:disabled) {
  border-color: #93c5fd;
  color: #1d4ed8;
}

.diff-nav-btn:disabled {
  cursor: not-allowed;
  opacity: 0.55;
}

/* 版本标题 */
.diff-side__header {
  position: sticky;
  top: 0;
  z-index: 1;
  padding: 6px 10px;
  font-size: 12px;
  font-weight: 600;
  color: #475569;
  background: #f1f5f9;
  border-bottom: 1px solid #e2e8f0;
}

/* 差异对比视图 */
.diff-wrap {
  max-height: 80vh;
  overflow: hidden;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  font-family: "SF Mono", Consolas, "Courier New", monospace;
  font-size: 12px;
  line-height: 1.6;
  background: #fff;
  position: relative;
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.diff-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  border-top: 1px solid #e2e8f0;
  overflow: hidden;
  flex: 1;
  min-height: 0;
}

.diff-side {
  overflow: auto;
  height: 100%;
}

.diff-side--left {
  border-right: 1px solid #e2e8f0;
}

.diff-line {
  display: grid;
  grid-template-columns: 50px 18px 1fr;
  gap: 8px;
  padding: 3px 10px;
  white-space: pre;
  border-bottom: 1px solid #f1f5f9;
}

.diff-line__num {
  color: #94a3b8;
  text-align: right;
  font-size: 11px;
}

.diff-line__prefix {
  text-align: center;
  color: #64748b;
}

.diff-line__text {
  display: inline-block;
  min-width: max-content;
}

.diff-line__text :deep(.diff-line__inline-change) {
  background: rgba(245, 158, 11, 0.22);
  border-radius: 2px;
}

.diff-line--collapsed {
  display: block;
  padding: 0;
  background: #f8fafc;
}

.diff-line__fold {
  width: 100%;
  padding: 8px 12px;
  border: 0;
  background: transparent;
  color: #2563eb;
  font-size: 12px;
  text-align: center;
  cursor: pointer;
}

.diff-line__fold:hover {
  background: #eff6ff;
}

.diff-line[data-diff-active="1"] {
  box-shadow: inset 0 0 0 1px rgba(59, 130, 246, 0.45);
}

.diff-scrollbar {
  height: 14px;
  overflow-x: auto;
  overflow-y: hidden;
  border-top: 1px solid #e2e8f0;
  background: #f8fafc;
}

.diff-scrollbar__spacer {
  height: 1px;
}

.diff-side--right .diff-line--add {
  background: #ecfdf5;
}

.diff-side--right .diff-line--add .diff-line__prefix {
  color: #10b981;
}

.diff-side--right .diff-line--change {
  background: #fffbeb;
}

.diff-side--right .diff-line--change .diff-line__prefix {
  color: #d97706;
}

.diff-side--left .diff-line--del {
  background: #fef2f2;
}

.diff-side--left .diff-line--del .diff-line__prefix {
  color: #ef4444;
}

.diff-side--left .diff-line--change {
  background: #fffbeb;
}

.diff-side--left .diff-line--change .diff-line__prefix {
  color: #d97706;
}

/* 预览视图 */
.preview-wrap {
  max-height: 80vh;
  overflow: hidden;
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  background: #fff;
  position: relative;
  flex: 1;
  min-height: 0;
  display: flex;
  flex-direction: column;
}

.preview-view {
  overflow: auto;
  padding: 14px;
  font-family: "SF Mono", Consolas, "Courier New", monospace;
  font-size: 12px;
  line-height: 1.6;
  white-space: pre;
  flex: 1;
  min-height: 0;
  background: #fafafa;
}

.preview-scrollbar {
  height: 14px;
  overflow-x: auto;
  overflow-y: hidden;
  border-top: 1px solid #e2e8f0;
  background: #f8fafc;
}

.preview-scrollbar__spacer {
  height: 1px;
}

/* 弹窗布局 */
.diff-dialog :deep(.el-dialog__body) {
  display: flex;
  flex-direction: column;
  height: calc(100vh - 180px);
  overflow: hidden;
}

.preview-dialog :deep(.el-dialog__body) {
  display: flex;
  flex-direction: column;
  height: calc(80vh - 140px);
  overflow: hidden;
}

/* loading 骨架屏 */
.diff-loading {
  border: 1px solid #e2e8f0;
  border-radius: 8px;
  padding: 12px 16px;
  background: #fff;
  flex: 1;
}

.diff-loading__row {
  height: 18px;
  border-radius: 4px;
  background: linear-gradient(90deg, #f1f5f9 25%, #e2e8f0 50%, #f1f5f9 75%);
  background-size: 200% 100%;
  animation: skeleton-shimmer 1.4s infinite;
  margin-bottom: 10px;
}

.diff-loading__row:nth-child(odd) {
  width: 85%;
}

.diff-loading__row:nth-child(even) {
  width: 70%;
}

@keyframes skeleton-shimmer {
  0% { background-position: 200% 0; }
  100% { background-position: -200% 0; }
}
</style>
