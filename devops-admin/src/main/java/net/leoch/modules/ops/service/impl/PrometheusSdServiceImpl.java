package net.leoch.modules.ops.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import net.leoch.modules.ops.dao.BusinessSystemDao;
import net.leoch.modules.ops.dao.LinuxHostDao;
import net.leoch.modules.ops.dao.WindowHostDao;
import net.leoch.modules.ops.dto.PrometheusSdRequest;
import net.leoch.modules.ops.dto.PrometheusSdResponse;
import net.leoch.modules.ops.entity.BusinessSystemEntity;
import net.leoch.modules.ops.entity.LinuxHostEntity;
import net.leoch.modules.ops.entity.WindowHostEntity;
import net.leoch.modules.ops.service.PrometheusSdService;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Prometheus SD 服务
 */
@Service
public class PrometheusSdServiceImpl implements PrometheusSdService {

    private static final Map<String, String> AREA_MAP = Map.of(
            "ah", "安徽理士",
            "js", "江苏理士",
            "sz", "深圳理士",
            "zq", "肇庆理士",
            "hw", "海外理士"
    );

    private final LinuxHostDao linuxHostDao;
    private final WindowHostDao windowHostDao;
    private final BusinessSystemDao businessSystemDao;

    public PrometheusSdServiceImpl(LinuxHostDao linuxHostDao, WindowHostDao windowHostDao, BusinessSystemDao businessSystemDao) {
        this.linuxHostDao = linuxHostDao;
        this.windowHostDao = windowHostDao;
        this.businessSystemDao = businessSystemDao;
    }

    @Override
    public List<PrometheusSdResponse> linux(PrometheusSdRequest request) {
        String siteLocation = getSiteLocation(request);
        if (siteLocation == null) {
            return new ArrayList<>();
        }
        List<LinuxHostEntity> list = linuxHostDao.selectList(new LambdaQueryWrapper<LinuxHostEntity>()
                .select(LinuxHostEntity::getInstance, LinuxHostEntity::getName, LinuxHostEntity::getSiteLocation, LinuxHostEntity::getAreaName, LinuxHostEntity::getMenuName, LinuxHostEntity::getSubMenuName)
                .eq(LinuxHostEntity::getStatus, 1)
                .eq(LinuxHostEntity::getSiteLocation, siteLocation)
        );
        return toTargets(list.stream()
                .filter(item -> item.getInstance() != null && !item.getInstance().isEmpty())
                .map(item -> buildTarget(item.getInstance(), item.getName(), siteLocation(item), "linux", item.getMenuName(), item.getSubMenuName()))
                .collect(Collectors.toList()));
    }

    @Override
    public List<PrometheusSdResponse> windows(PrometheusSdRequest request) {
        String siteLocation = getSiteLocation(request);
        if (siteLocation == null) {
            return new ArrayList<>();
        }
        List<WindowHostEntity> list = windowHostDao.selectList(new LambdaQueryWrapper<WindowHostEntity>()
                .select(WindowHostEntity::getInstance, WindowHostEntity::getName, WindowHostEntity::getSiteLocation, WindowHostEntity::getAreaName, WindowHostEntity::getMenuName, WindowHostEntity::getSubMenuName)
                .eq(WindowHostEntity::getStatus, 1)
                .eq(WindowHostEntity::getSiteLocation, siteLocation)
        );
        return toTargets(list.stream()
                .filter(item -> item.getInstance() != null && !item.getInstance().isEmpty())
                .map(item -> buildTarget(item.getInstance(), item.getName(), siteLocation(item), "windows", item.getMenuName(), item.getSubMenuName()))
                .collect(Collectors.toList()));
    }

    @Override
    public List<PrometheusSdResponse> httpProbe(PrometheusSdRequest request) {
        String siteLocation = getSiteLocation(request);
        if (siteLocation == null) {
            return new ArrayList<>();
        }
        List<BusinessSystemEntity> list = businessSystemDao.selectList(new LambdaQueryWrapper<BusinessSystemEntity>()
                .select(BusinessSystemEntity::getInstance, BusinessSystemEntity::getName, BusinessSystemEntity::getSiteLocation, BusinessSystemEntity::getAreaName, BusinessSystemEntity::getMenuName, BusinessSystemEntity::getSubMenuName)
                .eq(BusinessSystemEntity::getStatus, 1)
                .eq(BusinessSystemEntity::getSiteLocation, siteLocation)
        );
        return toTargets(list.stream()
                .filter(item -> item.getInstance() != null && !item.getInstance().isEmpty())
                .map(item -> buildTarget(item.getInstance(), item.getName(), siteLocation(item), "http_probe", item.getMenuName(), item.getSubMenuName()))
                .collect(Collectors.toList()));
    }

    private String getSiteLocation(PrometheusSdRequest request) {
        if (request == null || request.getArea() == null) {
            return null;
        }
        return AREA_MAP.get(request.getArea().trim().toLowerCase());
    }

    private PrometheusSdResponse buildTarget(String instance, String name, String siteLocation, String type, String menuName, String subMenuName) {
        Map<String, Object> labels = new HashMap<>();
        labels.put("site_location", siteLocation);
        if (name != null && !name.isEmpty()) {
            labels.put("name", name);
        }
        if (menuName != null && !menuName.isEmpty()) {
            labels.put("menu_name", menuName);
        }
        if (subMenuName != null && !subMenuName.isEmpty()) {
            labels.put("sub_menu_name", subMenuName);
        }
        labels.put("instance", resolveLabelInstance(instance, type));
        labels.put("type", type);
        PrometheusSdResponse response = new PrometheusSdResponse();
        response.setTargets(Collections.singletonList(resolveTarget(instance, type)));
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

    private String siteLocation(LinuxHostEntity entity) {
        if (entity == null) {
            return null;
        }
        if (entity.getSiteLocation() != null && !entity.getSiteLocation().isEmpty()) {
            return entity.getSiteLocation();
        }
        return entity.getAreaName();
    }

    private String siteLocation(WindowHostEntity entity) {
        if (entity == null) {
            return null;
        }
        if (entity.getSiteLocation() != null && !entity.getSiteLocation().isEmpty()) {
            return entity.getSiteLocation();
        }
        return entity.getAreaName();
    }

    private String siteLocation(BusinessSystemEntity entity) {
        if (entity == null) {
            return null;
        }
        if (entity.getSiteLocation() != null && !entity.getSiteLocation().isEmpty()) {
            return entity.getSiteLocation();
        }
        return entity.getAreaName();
    }

    private List<PrometheusSdResponse> toTargets(List<PrometheusSdResponse> groups) {
        return groups == null ? new ArrayList<>() : groups;
    }
}
