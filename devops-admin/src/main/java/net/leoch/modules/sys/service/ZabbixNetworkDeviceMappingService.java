package net.leoch.modules.sys.service;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.base.Constant;
import net.leoch.modules.ops.service.ZabbixClient;
import net.leoch.modules.ops.service.ZabbixConfigService;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.modules.sys.vo.req.ZabbixNetworkDeviceMappingReq;
import net.leoch.modules.sys.vo.req.ZabbixNetworkDeviceMappingPreviewReq;
import net.leoch.modules.sys.vo.rsp.DictDataRsp;
import net.leoch.modules.sys.vo.rsp.DictTypeRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixHostGroupRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingOptionsRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingPreviewRsp;
import net.leoch.modules.sys.vo.rsp.ZabbixNetworkDeviceMappingRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
@Service
@RequiredArgsConstructor
public class ZabbixNetworkDeviceMappingService {
    private static final Pattern GROUP_NAME_PATTERN = Pattern.compile("^(\\d+)\\.([^_]+)_(.+)$");

    private final ISysParamsService sysParamsService;
    private final ZabbixConfigService zabbixConfigService;
    private final ZabbixClient zabbixClient;
    private final ISysDictTypeService sysDictTypeService;

    public ZabbixNetworkDeviceMappingRsp getConfig() {
        ZabbixNetworkDeviceMappingRsp rsp = sysParamsService.getValueObject(
                Constant.ZABBIX_NETWORK_DEVICE_MAPPING_KEY, ZabbixNetworkDeviceMappingRsp.class);
        boolean initializeDefaults = false;
        if (rsp == null) {
            rsp = new ZabbixNetworkDeviceMappingRsp();
            initializeDefaults = true;
        }
        fillDefaults(rsp);
        normalizeMappingToDictLabel(rsp);
        if (initializeDefaults) {
            initDefaultAreaRules(rsp);
            normalizeMappingToDictLabel(rsp);
        }
        return rsp;
    }

    @Transactional(rollbackFor = Exception.class)
    public void saveConfig(ZabbixNetworkDeviceMappingReq req) {
        ZabbixNetworkDeviceMappingRsp rsp = new ZabbixNetworkDeviceMappingRsp();
        BeanUtil.copyProperties(req, rsp);
        fillDefaults(rsp);
        normalizeMappingToDictLabel(rsp);
        String json = JSONUtil.toJsonStr(rsp);
        int count = sysParamsService.updateValueByCode(Constant.ZABBIX_NETWORK_DEVICE_MAPPING_KEY, json);
        if (count > 0) {
            return;
        }
        SysParamsReq paramsReq = new SysParamsReq();
        paramsReq.setParamCode(Constant.ZABBIX_NETWORK_DEVICE_MAPPING_KEY);
        paramsReq.setParamValue(json);
        paramsReq.setRemark("Zabbix网络设备映射配置");
        sysParamsService.save(paramsReq);
    }

    public ZabbixNetworkDeviceMappingOptionsRsp getOptions() {
        ZabbixNetworkDeviceMappingOptionsRsp rsp = new ZabbixNetworkDeviceMappingOptionsRsp();
        var zabbixConfig = zabbixConfigService.getConfig();
        rsp.setZabbixName(StrUtil.blankToDefault(zabbixConfig.getName(), "Zabbix"));
        if (zabbixConfig.getTemplates() != null) {
            rsp.setTemplateOptions(new ArrayList<>(zabbixConfig.getTemplates()));
        }
        rsp.setHostGroupOptions(filterHostGroupsForNetwork(zabbixClient.getHostGroups(zabbixConfig)));

        Map<String, List<DictDataRsp>> dictMap = getDictMap();
        rsp.setAreaOptions(toDictData(dictMap.get("area_name_type")));
        rsp.setDeviceGroupOptions(toDictData(dictMap.get("network_device_group")));
        rsp.setDeviceModelOptions(toDictData(dictMap.get("network_device_model")));
        return rsp;
    }

    public ZabbixNetworkDeviceMappingPreviewRsp preview(ZabbixNetworkDeviceMappingPreviewReq req) {
        ZabbixNetworkDeviceMappingRsp config = getConfig();
        Set<String> selectedIds = new LinkedHashSet<>();
        if (req != null) {
            selectedIds.addAll(Optional.ofNullable(req.getSelectedHostGroupIds()).orElseGet(ArrayList::new));
        } else if (CollUtil.isNotEmpty(config.getSelectedHostGroupIds())) {
            selectedIds.addAll(config.getSelectedHostGroupIds());
        }

        Map<String, String> categoryMap = new LinkedHashMap<>();
        List<ZabbixNetworkDeviceMappingRsp.CategoryGroupRule> categoryRules =
                (req != null) ? req.getCategoryGroupRules() : config.getCategoryGroupRules();
        if (CollUtil.isNotEmpty(categoryRules)) {
            for (var rule : categoryRules) {
                if (rule == null || StrUtil.isBlank(rule.getZabbixCategory())) {
                    continue;
                }
                categoryMap.put(rule.getZabbixCategory().trim(), StrUtil.blankToDefault(rule.getDeviceGroup(), ""));
            }
        }
        Map<String, String> areaRuleMap = new LinkedHashMap<>();
        List<ZabbixNetworkDeviceMappingRsp.AreaKeywordRule> areaRules =
                (req != null) ? req.getAreaKeywordRules() : config.getAreaKeywordRules();
        if (CollUtil.isNotEmpty(areaRules)) {
            for (var rule : areaRules) {
                if (rule == null || StrUtil.isBlank(rule.getKeyword())) {
                    continue;
                }
                areaRuleMap.put(rule.getKeyword().trim(), StrUtil.blankToDefault(rule.getAreaName(), ""));
            }
        }
        Map<String, List<DictDataRsp>> dictMap = getDictMap();
        Set<String> areaDictLabels = new LinkedHashSet<>(toDictLabels(dictMap.get("area_name_type")));

        ZabbixNetworkDeviceMappingPreviewRsp rsp = new ZabbixNetworkDeviceMappingPreviewRsp();
        for (ZabbixHostGroupRsp item : getOptions().getHostGroupOptions()) {
            if (item == null || StrUtil.isBlank(item.getGroupId())) {
                continue;
            }
            if (!selectedIds.isEmpty() && !selectedIds.contains(item.getGroupId())) {
                continue;
            }
            ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem row = parseGroupPreview(item, categoryMap, areaRuleMap, areaDictLabels);
            rsp.getGroupList().add(row);
            if (!Integer.valueOf(1).equals(row.getMatched()) && StrUtil.isNotBlank(row.getRawAreaSegment())) {
                rsp.getUnmatchedAreas().add(row.getRawAreaSegment());
            }
        }
        rsp.setUnmatchedAreas(rsp.getUnmatchedAreas().stream().distinct().sorted().toList());
        return rsp;
    }

    private ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem parseGroupPreview(
            ZabbixHostGroupRsp item,
            Map<String, String> categoryMap,
            Map<String, String> areaRuleMap,
            Set<String> areaDictLabels) {
        ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem row = new ZabbixNetworkDeviceMappingPreviewRsp.GroupPreviewItem();
        row.setGroupId(item.getGroupId());
        row.setGroupName(item.getName());
        if (StrUtil.isBlank(item.getName())) {
            return row;
        }
        Matcher matcher = GROUP_NAME_PATTERN.matcher(item.getName().trim());
        if (!matcher.find()) {
            return row;
        }
        String category = matcher.group(2).trim();
        String rawArea = matcher.group(3).trim();
        String normalizedKeyword = normalizeAreaKeyword(rawArea);
        String mappedArea = resolveMappedArea(rawArea, normalizedKeyword, areaRuleMap);
        row.setZabbixCategory(category);
        row.setRawAreaSegment(rawArea);
        row.setNormalizedAreaKeyword(normalizedKeyword);
        row.setMappedDeviceGroup(StrUtil.blankToDefault(categoryMap.get(category), null));
        if (areaDictLabels.contains(mappedArea)) {
            row.setMatchedAreaName(mappedArea);
            row.setMatched(1);
        } else {
            row.setMatchedAreaName(mappedArea);
            row.setMatched(0);
        }
        return row;
    }

    private String resolveMappedArea(String rawArea, String normalizedKeyword, Map<String, String> areaRuleMap) {
        if (areaRuleMap == null || areaRuleMap.isEmpty()) {
            return normalizedKeyword;
        }
        // 优先精确匹配原始区域片段，例如“安徽新能源”
        if (StrUtil.isNotBlank(rawArea) && areaRuleMap.containsKey(rawArea)) {
            return StrUtil.blankToDefault(areaRuleMap.get(rawArea), normalizedKeyword);
        }
        // 次优先匹配归一化关键字，例如“安徽”
        if (StrUtil.isNotBlank(normalizedKeyword) && areaRuleMap.containsKey(normalizedKeyword)) {
            return StrUtil.blankToDefault(areaRuleMap.get(normalizedKeyword), normalizedKeyword);
        }
        // 最后兜底：原始区域包含某个配置关键字时，按最长关键字命中
        if (StrUtil.isNotBlank(rawArea)) {
            String matchedKeyword = areaRuleMap.keySet().stream()
                    .filter(StrUtil::isNotBlank)
                    .filter(rawArea::contains)
                    .max(Comparator.comparingInt(String::length))
                    .orElse(null);
            if (StrUtil.isNotBlank(matchedKeyword)) {
                return StrUtil.blankToDefault(areaRuleMap.get(matchedKeyword), normalizedKeyword);
            }
        }
        return normalizedKeyword;
    }

    private List<ZabbixHostGroupRsp> filterHostGroupsForNetwork(List<ZabbixHostGroupRsp> source) {
        if (CollUtil.isEmpty(source)) {
            return new ArrayList<>();
        }
        List<ZabbixHostGroupRsp> list = source.stream()
                .filter(Objects::nonNull)
                .filter(item -> StrUtil.isNotBlank(item.getName()))
                .filter(item -> GROUP_NAME_PATTERN.matcher(item.getName().trim()).find())
                .filter(item -> {
                    Matcher matcher = GROUP_NAME_PATTERN.matcher(item.getName().trim());
                    if (!matcher.find()) {
                        return false;
                    }
                    String category = matcher.group(2);
                    return "核心网络设备".equals(category) || "网络设备".equals(category);
                })
                .toList();
        return new ArrayList<>(list);
    }

    private Map<String, List<DictDataRsp>> getDictMap() {
        List<DictTypeRsp> all = sysDictTypeService.getAllList();
        Map<String, List<DictDataRsp>> map = new LinkedHashMap<>();
        if (all == null) {
            return map;
        }
        for (DictTypeRsp item : all) {
            if (item == null || StrUtil.isBlank(item.getDictType())) {
                continue;
            }
            map.put(item.getDictType(), item.getDataList() == null ? List.of() : item.getDataList());
        }
        return map;
    }

    private List<DictDataRsp> toDictData(List<DictDataRsp> dataList) {
        if (dataList == null) {
            return new ArrayList<>();
        }
        return dataList.stream()
                .filter(Objects::nonNull)
                .filter(it -> StrUtil.isNotBlank(it.getDictLabel()) && StrUtil.isNotBlank(it.getDictValue()))
                .toList();
    }

    private List<String> toDictLabels(List<DictDataRsp> dataList) {
        if (dataList == null) {
            return new ArrayList<>();
        }
        return dataList.stream()
                .filter(Objects::nonNull)
                .map(DictDataRsp::getDictLabel)
                .filter(StrUtil::isNotBlank)
                .distinct()
                .toList();
    }

    private String normalizeAreaKeyword(String rawArea) {
        if (StrUtil.isBlank(rawArea)) {
            return rawArea;
        }
        String value = rawArea.trim();
        // 先处理显式业务词
        if (value.contains("销售")) {
            return "销售";
        }
        // 区域关键词优先
        String[] areaKeywords = new String[]{"安徽", "江苏", "深圳", "肇庆", "海外", "东莞", "顺德", "新加坡", "马来西亚", "越南"};
        for (String keyword : areaKeywords) {
            if (value.contains(keyword)) {
                return keyword;
            }
        }
        return value;
    }

    private void fillDefaults(ZabbixNetworkDeviceMappingRsp rsp) {
        if (rsp == null) {
            return;
        }
        var zabbixConfig = zabbixConfigService.getConfig();
        if (StrUtil.isBlank(rsp.getZabbixName())) {
            rsp.setZabbixName(StrUtil.blankToDefault(zabbixConfig.getName(), "Zabbix"));
        }
        if (rsp.getSelectedHostGroupIds() == null) {
            rsp.setSelectedHostGroupIds(new ArrayList<>());
        }
        if (rsp.getTemplateModelRules() == null) {
            rsp.setTemplateModelRules(new ArrayList<>());
        }
        if (rsp.getCategoryGroupRules() == null) {
            rsp.setCategoryGroupRules(new ArrayList<>());
        }
        if (rsp.getAreaKeywordRules() == null) {
            rsp.setAreaKeywordRules(new ArrayList<>());
        }

        ensureTemplateRule(rsp, "001.飞塔监控模板", "飞塔");
        ensureTemplateRule(rsp, "002.华为监控模板", "华为");
        ensureTemplateRule(rsp, "003.华三监控模板", "华三");

        ensureCategoryRule(rsp, "核心网络设备", "核心网络设备");
        ensureCategoryRule(rsp, "网络设备", "普通网络设备");
    }

    private void normalizeMappingToDictLabel(ZabbixNetworkDeviceMappingRsp rsp) {
        Map<String, List<DictDataRsp>> dictMap = getDictMap();
        Map<String, String> areaByValue = buildValueToLabelMap(dictMap.get("area_name_type"));
        Map<String, String> groupByValue = buildValueToLabelMap(dictMap.get("network_device_group"));
        Map<String, String> modelByValue = buildValueToLabelMap(dictMap.get("network_device_model"));

        if (CollUtil.isNotEmpty(rsp.getAreaKeywordRules())) {
            for (var rule : rsp.getAreaKeywordRules()) {
                if (rule == null || StrUtil.isBlank(rule.getAreaName())) {
                    continue;
                }
                rule.setAreaName(normalizeDictLabel(rule.getAreaName(), areaByValue));
            }
        }
        if (CollUtil.isNotEmpty(rsp.getCategoryGroupRules())) {
            for (var rule : rsp.getCategoryGroupRules()) {
                if (rule == null || StrUtil.isBlank(rule.getDeviceGroup())) {
                    continue;
                }
                rule.setDeviceGroup(normalizeDictLabel(rule.getDeviceGroup(), groupByValue));
            }
        }
        if (CollUtil.isNotEmpty(rsp.getTemplateModelRules())) {
            for (var rule : rsp.getTemplateModelRules()) {
                if (rule == null || StrUtil.isBlank(rule.getDeviceModel())) {
                    continue;
                }
                rule.setDeviceModel(normalizeDictLabel(rule.getDeviceModel(), modelByValue));
            }
        }
    }

    private Map<String, String> buildValueToLabelMap(List<DictDataRsp> list) {
        Map<String, String> map = new LinkedHashMap<>();
        if (list == null) {
            return map;
        }
        for (DictDataRsp item : list) {
            if (item == null || StrUtil.isBlank(item.getDictLabel()) || StrUtil.isBlank(item.getDictValue())) {
                continue;
            }
            map.put(item.getDictValue(), item.getDictLabel());
        }
        return map;
    }

    private String normalizeDictLabel(String input, Map<String, String> valueToLabelMap) {
        if (StrUtil.isBlank(input)) {
            return input;
        }
        String text = input.trim();
        return valueToLabelMap.getOrDefault(text, text);
    }

    private void initDefaultAreaRules(ZabbixNetworkDeviceMappingRsp rsp) {
        ensureAreaRule(rsp, "安徽新能源", "安徽");
        ensureAreaRule(rsp, "安徽理士", "安徽");
        ensureAreaRule(rsp, "安徽", "安徽");
        ensureAreaRule(rsp, "江苏理士", "江苏");
        ensureAreaRule(rsp, "江苏新能源", "江苏");
        ensureAreaRule(rsp, "江苏", "江苏");
        ensureAreaRule(rsp, "深圳理士", "深圳");
        ensureAreaRule(rsp, "深圳", "深圳");
        ensureAreaRule(rsp, "肇庆理士", "肇庆");
        ensureAreaRule(rsp, "肇庆", "肇庆");
        ensureAreaRule(rsp, "海外", "海外");
        ensureAreaRule(rsp, "销售公司", "销售");
        ensureAreaRule(rsp, "销售", "销售");
    }

    private void ensureTemplateRule(ZabbixNetworkDeviceMappingRsp rsp, String template, String deviceModel) {
        boolean exists = rsp.getTemplateModelRules().stream().anyMatch(it -> it != null && template.equals(it.getTemplateName()));
        if (!exists) {
            ZabbixNetworkDeviceMappingRsp.TemplateModelRule rule = new ZabbixNetworkDeviceMappingRsp.TemplateModelRule();
            rule.setTemplateName(template);
            rule.setDeviceModel(deviceModel);
            rsp.getTemplateModelRules().add(rule);
        }
    }

    private void ensureCategoryRule(ZabbixNetworkDeviceMappingRsp rsp, String category, String deviceGroup) {
        boolean exists = rsp.getCategoryGroupRules().stream().anyMatch(it -> it != null && category.equals(it.getZabbixCategory()));
        if (!exists) {
            ZabbixNetworkDeviceMappingRsp.CategoryGroupRule rule = new ZabbixNetworkDeviceMappingRsp.CategoryGroupRule();
            rule.setZabbixCategory(category);
            rule.setDeviceGroup(deviceGroup);
            rsp.getCategoryGroupRules().add(rule);
        }
    }

    private void ensureAreaRule(ZabbixNetworkDeviceMappingRsp rsp, String keyword, String areaName) {
        boolean exists = rsp.getAreaKeywordRules().stream().anyMatch(it -> it != null && keyword.equals(it.getKeyword()));
        if (!exists) {
            ZabbixNetworkDeviceMappingRsp.AreaKeywordRule rule = new ZabbixNetworkDeviceMappingRsp.AreaKeywordRule();
            rule.setKeyword(keyword);
            rule.setAreaName(areaName);
            rsp.getAreaKeywordRules().add(rule);
        }
    }
}
