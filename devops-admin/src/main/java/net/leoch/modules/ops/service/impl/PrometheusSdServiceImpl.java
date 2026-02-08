package net.leoch.modules.ops.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.modules.ops.mapper.BusinessSystemMapper;
import net.leoch.modules.ops.mapper.LinuxHostMapper;
import net.leoch.modules.ops.mapper.WindowHostMapper;
import net.leoch.modules.ops.dto.PrometheusSdRequest;
import net.leoch.modules.ops.dto.PrometheusSdResponse;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.service.IPrometheusSdService;
import net.leoch.modules.sys.mapper.SysDictDataMapper;
import net.leoch.modules.sys.entity.DictData;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Prometheus SD 服务
 */
@Service
public class PrometheusSdServiceImpl implements IPrometheusSdService {

    private final LinuxHostMapper linuxHostMapper;
    private final WindowHostMapper windowHostMapper;
    private final BusinessSystemMapper businessSystemMapper;
    private final SysDictDataMapper sysDictDataMapper;

    public PrometheusSdServiceImpl(LinuxHostMapper linuxHostMapper, WindowHostMapper windowHostMapper, BusinessSystemMapper businessSystemMapper, SysDictDataMapper sysDictDataMapper) {
        this.linuxHostMapper = linuxHostMapper;
        this.windowHostMapper = windowHostMapper;
        this.businessSystemMapper = businessSystemMapper;
        this.sysDictDataMapper = sysDictDataMapper;
    }

    @Override
    public List<PrometheusSdResponse> linux(PrometheusSdRequest request) {
        String areaName = getAreaName(request);
        if (areaName == null) {
            return new ArrayList<>();
        }
        DictMaps dictMaps = loadDictMaps();
        List<LinuxHostEntity> list = linuxHostMapper.selectList(new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance, LinuxHostEntity::getName, LinuxHostEntity::getSiteLocation, LinuxHostEntity::getAreaName, LinuxHostEntity::getMenuName, LinuxHostEntity::getSubMenuName, LinuxHostEntity::getType)
                .eq(LinuxHostEntity::getStatus, 1)
                .eq(LinuxHostEntity::getAreaName, areaName)
        );
        return buildTargets(
                list,
                "linux",
                dictMaps,
                LinuxHostEntity::getInstance,
                LinuxHostEntity::getName,
                LinuxHostEntity::getSiteLocation,
                LinuxHostEntity::getAreaName,
                LinuxHostEntity::getMenuName,
                LinuxHostEntity::getSubMenuName,
                LinuxHostEntity::getType
        );
    }

    @Override
    public List<PrometheusSdResponse> windows(PrometheusSdRequest request) {
        String areaName = getAreaName(request);
        if (areaName == null) {
            return new ArrayList<>();
        }
        DictMaps dictMaps = loadDictMaps();
        List<WindowHostEntity> list = windowHostMapper.selectList(new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance, WindowHostEntity::getName, WindowHostEntity::getSiteLocation, WindowHostEntity::getAreaName, WindowHostEntity::getMenuName, WindowHostEntity::getSubMenuName, WindowHostEntity::getType)
                .eq(WindowHostEntity::getStatus, 1)
                .eq(WindowHostEntity::getAreaName, areaName)
        );
        return buildTargets(
                list,
                "windows",
                dictMaps,
                WindowHostEntity::getInstance,
                WindowHostEntity::getName,
                WindowHostEntity::getSiteLocation,
                WindowHostEntity::getAreaName,
                WindowHostEntity::getMenuName,
                WindowHostEntity::getSubMenuName,
                WindowHostEntity::getType
        );
    }

    @Override
    public List<PrometheusSdResponse> httpProbe(PrometheusSdRequest request) {
        String areaName = getAreaName(request);
        if (areaName == null) {
            return new ArrayList<>();
        }
        DictMaps dictMaps = loadDictMaps();
        List<BusinessSystemEntity> list = businessSystemMapper.selectList(new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName, BusinessSystemEntity::getSiteLocation, BusinessSystemEntity::getAreaName, BusinessSystemEntity::getMenuName, BusinessSystemEntity::getSubMenuName)
                .eq(BusinessSystemEntity::getStatus, 1)
                .eq(BusinessSystemEntity::getAreaName, areaName)
        );
        return buildTargets(
                list,
                "http_probe",
                dictMaps,
                BusinessSystemEntity::getInstance,
                BusinessSystemEntity::getName,
                BusinessSystemEntity::getSiteLocation,
                BusinessSystemEntity::getAreaName,
                BusinessSystemEntity::getMenuName,
                BusinessSystemEntity::getSubMenuName,
                item -> null
        );
    }

    private String getAreaName(PrometheusSdRequest request) {
        if (request == null || request.getArea() == null) {
            return null;
        }
        String areaName = request.getArea().trim();
        if (areaName.isEmpty()) {
            return null;
        }
        return areaName;
    }

    private DictMaps loadDictMaps() {
        return new DictMaps(
                getDictMapByType("area_name_type"),
                getDictMapByType("base_site_location"),
                getDictMapByType("server_host_group"),
                getDictMapByType("server_machine_type")
        );
    }

    private Map<String, String> getDictMapByType(String dictType) {
        List<DictData> dictDataList = sysDictDataMapper.getDictDataListByType(dictType);
        if (dictDataList == null || dictDataList.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, String> map = new HashMap<>();
        for (DictData data : dictDataList) {
            if (data.getDictLabel() != null && data.getDictValue() != null) {
                String key = data.getDictLabel().trim();
                if (!key.isEmpty()) {
                    map.put(key, data.getDictValue());
                    String lower = key.toLowerCase();
                    if (!lower.equals(key)) {
                        map.putIfAbsent(lower, data.getDictValue());
                    }
                }
            }
        }
        return map;
    }

    private String convertDictValue(Map<String, String> dictMap, String value) {
        if (value == null) {
            return null;
        }
        String key = value.trim();
        if (key.isEmpty()) {
            return value;
        }
        String label = dictMap.get(key);
        if (label == null) {
            label = dictMap.get(key.toLowerCase());
        }
        return label == null ? value : label;
    }

    private String resolveBaseSiteLocation(String siteLocation, String areaName, DictMaps dictMaps) {
        if (siteLocation != null && !siteLocation.isEmpty()) {
            return convertDictValue(dictMaps.baseSiteLocationMap, siteLocation);
        }
        return convertDictValue(dictMaps.areaNameMap, areaName);
    }

    private <T> List<PrometheusSdResponse> buildTargets(
            List<T> list,
            String type,
            DictMaps dictMaps,
            Function<T, String> instanceFn,
            Function<T, String> nameFn,
            Function<T, String> siteLocationFn,
            Function<T, String> areaNameFn,
            Function<T, String> menuNameFn,
            Function<T, String> subMenuNameFn,
            Function<T, String> machineTypeFn) {
        return toTargets(list.stream()
                .filter(item -> {
                    String instance = instanceFn.apply(item);
                    return instance != null && !instance.isEmpty();
                })
                .map(item -> buildTarget(
                        instanceFn.apply(item),
                        nameFn.apply(item),
                        resolveBaseSiteLocation(siteLocationFn.apply(item), areaNameFn.apply(item), dictMaps),
                        convertDictValue(dictMaps.areaNameMap, areaNameFn.apply(item)),
                        type,
                        convertDictValue(dictMaps.menuNameMap, menuNameFn.apply(item)),
                        subMenuNameFn.apply(item),
                        convertDictValue(dictMaps.machineTypeMap, machineTypeFn.apply(item))))
                .collect(Collectors.toList()));
    }

    private PrometheusSdResponse buildTarget(String instance, String name, String siteLocation, String areaName, String targetType, String menuName, String subMenuName, String machineType) {
        Map<String, Object> labels = new HashMap<>();
        labels.put("base_site_location", siteLocation);
        if (areaName != null && !areaName.isEmpty()) {
            labels.put("area_name", areaName);
        }
        if (name != null && !name.isEmpty()) {
            labels.put("name", name);
        }
        if (menuName != null && !menuName.isEmpty()) {
            labels.put("menu_name", menuName);
        }
        if (subMenuName != null && !subMenuName.isEmpty()) {
            labels.put("sub_menu_name", subMenuName);
        }
        labels.put("instance", resolveLabelInstance(instance, targetType));
        labels.put("target_type", targetType);
        labels.put("type", (machineType == null || machineType.isEmpty()) ? targetType : machineType);
        PrometheusSdResponse response = new PrometheusSdResponse();
        response.setTargets(Collections.singletonList(resolveTarget(instance, targetType)));
        response.setLabels(labels);
        return response;
    }

    private String resolveTarget(String instance, String type) {
        if (instance == null) {
            return null;
        }
        if (!"linux".equals(type) && !"windows".equals(type)) {
            return instance;
        }
        return instance.trim();
    }

    private String resolveLabelInstance(String instance, String type) {
        if (instance == null) {
            return null;
        }
        if (!"linux".equals(type) && !"windows".equals(type)) {
            return instance;
        }
        String trimmed = instance.trim();
        int colon = trimmed.indexOf(':');
        if (colon > 0) {
            return trimmed.substring(0, colon);
        }
        return trimmed;
    }

    private List<PrometheusSdResponse> toTargets(List<PrometheusSdResponse> groups) {
        return groups == null ? new ArrayList<>() : groups;
    }

    private record DictMaps(Map<String, String> areaNameMap, Map<String, String> baseSiteLocationMap,
                            Map<String, String> menuNameMap, Map<String, String> machineTypeMap) {
    }
}
