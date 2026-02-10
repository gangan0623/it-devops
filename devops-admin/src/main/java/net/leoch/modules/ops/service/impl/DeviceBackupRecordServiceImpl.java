package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.convert.ConvertUtils;
import net.leoch.modules.ops.mapper.DeviceBackupRecordMapper;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.*;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import net.leoch.modules.ops.service.IDeviceBackupHistoryService;
import net.leoch.modules.ops.service.IDeviceBackupRecordService;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Slf4j
@Service
public class DeviceBackupRecordServiceImpl extends ServiceImpl<DeviceBackupRecordMapper, DeviceBackupRecordEntity> implements IDeviceBackupRecordService {

    private static final Set<String> ALLOWED_DOWNLOAD_HOSTS = new HashSet<>();

    private final IDeviceBackupHistoryService deviceBackupHistoryService;

    public DeviceBackupRecordServiceImpl(IDeviceBackupHistoryService deviceBackupHistoryService) {
        this.deviceBackupHistoryService = deviceBackupHistoryService;
    }

    @Override
    public PageData<DeviceBackupRecordRsp> page(DeviceBackupRecordPageReq request) {
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupRecordEntity::getName, request.getName());
        wrapper.like(StrUtil.isNotBlank(request.getIp()), DeviceBackupRecordEntity::getIp, request.getIp());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupRecordEntity::getLastBackupStatus, request.getStatus());
        Page<DeviceBackupRecordEntity> page = request.buildPage();
        IPage<DeviceBackupRecordEntity> result = this.page(page, wrapper);
        List<DeviceBackupRecordRsp> list = ConvertUtils.sourceToTarget(result.getRecords(), DeviceBackupRecordRsp.class);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public DeviceBackupRecordRsp get(DeviceBackupRecordIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        DeviceBackupRecordEntity entity = this.getById(request.getId());
        return ConvertUtils.sourceToTarget(entity, DeviceBackupRecordRsp.class);
    }

    @Override
    public void delete(DeviceBackupRecordDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public List<DeviceBackupHistoryRsp> history(DeviceBackupRecordHistoryReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        return deviceBackupHistoryService.listByIp(request.getIp(), request.getLimit());
    }

    @Override
    public List<DeviceBackupDiffLineRsp> diff(DeviceBackupRecordDiffReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        List<Map<String, Object>> data = deviceBackupHistoryService.diffById(request.getLeftId(), request.getRightId());
        return toDiffLines(data);
    }

    @Override
    public List<DeviceBackupDiffLineRsp> diffCurrent(DeviceBackupRecordDiffCurrentReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        DeviceBackupRecordRsp current = getByIp(request.getIp());
        if (current == null || current.getUrl() == null) {
            throw new ServiceException("当前记录不存在或URL为空");
        }
        DeviceBackupHistoryRsp history = deviceBackupHistoryService.get(request.getHistoryId());
        if (history == null || history.getUrl() == null) {
            throw new ServiceException("历史记录不存在或URL为空");
        }
        List<Map<String, Object>> data = deviceBackupHistoryService.diffByUrls(history.getUrl(), current.getUrl());
        return toDiffLines(data);
    }

    @Override
    public String preview(DeviceBackupRecordPreviewReq request) {
        if (request == null) {
            return "";
        }
        return deviceBackupHistoryService.previewByUrl(request.getUrl());
    }

    @Override
    public void download(DeviceBackupRecordDownloadReq request, HttpServletResponse response) {
        if (request == null || request.getUrl() == null || request.getUrl().isBlank()) {
            response.setStatus(400);
            return;
        }
        validateDownloadUrl(request.getUrl());
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(request.getUrl()).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(15000);
            int code = connection.getResponseCode();
            if (code != 200) {
                response.setStatus(code);
                return;
            }
            String fileName = "backup.txt";
            int slash = request.getUrl().lastIndexOf('/');
            if (slash > -1 && slash + 1 < request.getUrl().length()) {
                fileName = request.getUrl().substring(slash + 1);
            }
            String encoded = URLEncoder.encode(fileName, StandardCharsets.UTF_8);
            response.setHeader("Content-Disposition", "attachment; filename*=UTF-8''" + encoded);
            response.setContentType("application/octet-stream");
            try (InputStream in = connection.getInputStream()) {
                in.transferTo(response.getOutputStream());
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("[备份下载] 下载失败, url: {}", request.getUrl(), e);
            response.setStatus(500);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private void validateDownloadUrl(String urlStr) {
        try {
            URI uri = new URI(urlStr);
            String scheme = uri.getScheme();
            if (scheme == null || (!scheme.equals("http") && !scheme.equals("https"))) {
                throw new ServiceException("仅支持HTTP/HTTPS协议下载");
            }
            String host = uri.getHost();
            if (host == null) {
                throw new ServiceException("下载URL无效");
            }
            if (host.equals("127.0.0.1") || host.equals("localhost") || host.startsWith("169.254.") || host.equals("0.0.0.0")) {
                throw new ServiceException("不允许访问内部地址");
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException("下载URL格式无效");
        }
    }

    @Override
    public void upsertRecord(String name, String ip, String url, boolean success) {
        if (StrUtil.isBlank(ip)) {
            return;
        }
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DeviceBackupRecordEntity::getIp, ip);
        DeviceBackupRecordEntity existing = this.getOne(wrapper);
        Date now = new Date();
        if (existing == null) {
            DeviceBackupRecordEntity entity = new DeviceBackupRecordEntity();
            entity.setName(name);
            entity.setIp(ip);
            entity.setUrl(url);
            entity.setLastBackupTime(now);
            entity.setLastBackupStatus(success ? 1 : 0);
            entity.setBackupNum(1);
            this.getBaseMapper().insert(entity);
            return;
        }
        existing.setName(name);
        if (StrUtil.isNotBlank(url)) {
            existing.setUrl(url);
        }
        existing.setLastBackupTime(now);
        existing.setLastBackupStatus(success ? 1 : 0);
        Integer num = existing.getBackupNum() == null ? 0 : existing.getBackupNum();
        existing.setBackupNum(num + 1);
        this.updateById(existing);
    }

    @Override
    public DeviceBackupRecordRsp getByIp(String ip) {
        if (StrUtil.isBlank(ip)) {
            return null;
        }
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DeviceBackupRecordEntity::getIp, ip).last("limit 1");
        DeviceBackupRecordEntity existing = this.getOne(wrapper);
        return ConvertUtils.sourceToTarget(existing, DeviceBackupRecordRsp.class);
    }

    private List<DeviceBackupDiffLineRsp> toDiffLines(List<Map<String, Object>> data) {
        List<DeviceBackupDiffLineRsp> list = new ArrayList<>();
        if (data == null || data.isEmpty()) {
            return list;
        }
        for (Map<String, Object> item : data) {
            DeviceBackupDiffLineRsp line = new DeviceBackupDiffLineRsp();
            Object type = item.get("type");
            line.setType(type == null ? "" : String.valueOf(type));
            line.setLeftLineNo(toInt(item.get("leftLineNo")));
            line.setRightLineNo(toInt(item.get("rightLineNo")));
            Object content = item.get("content");
            line.setContent(content == null ? "" : String.valueOf(content));
            list.add(line);
        }
        return list;
    }

    private Integer toInt(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Number) {
            return ((Number) value).intValue();
        }
        try {
            return Integer.parseInt(String.valueOf(value));
        } catch (Exception ignore) {
            return null;
        }
    }
}
