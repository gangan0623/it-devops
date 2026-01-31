package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import jakarta.servlet.http.HttpServletResponse;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.RenException;
import net.leoch.common.page.PageData;
import net.leoch.common.service.impl.CrudServiceImpl;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.ops.dao.DeviceBackupRecordDao;
import net.leoch.modules.ops.dto.*;
import net.leoch.modules.ops.entity.DeviceBackupRecordEntity;
import net.leoch.modules.ops.service.DeviceBackupHistoryService;
import net.leoch.modules.ops.service.DeviceBackupRecordService;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * 设备备份信息表
 *
 * @author Taohongqiang
 * @since 1.0.0 2026-01-29
 */
@Service
public class DeviceBackupRecordServiceImpl extends CrudServiceImpl<DeviceBackupRecordDao, DeviceBackupRecordEntity, DeviceBackupRecordDTO> implements DeviceBackupRecordService {

    private final DeviceBackupHistoryService deviceBackupHistoryService;

    public DeviceBackupRecordServiceImpl(DeviceBackupHistoryService deviceBackupHistoryService) {
        this.deviceBackupHistoryService = deviceBackupHistoryService;
    }

    @Override
    public QueryWrapper<DeviceBackupRecordEntity> getWrapper(Map<String, Object> params) {
        QueryWrapper<DeviceBackupRecordEntity> wrapper = new QueryWrapper<>();
        LambdaQueryWrapper<DeviceBackupRecordEntity> lambda = wrapper.lambda();
        String name = (String) params.get("name");
        String ip = (String) params.get("ip");
        String status = (String) params.get("status");
        lambda.like(StrUtil.isNotBlank(name), DeviceBackupRecordEntity::getName, name);
        lambda.like(StrUtil.isNotBlank(ip), DeviceBackupRecordEntity::getIp, ip);
        lambda.eq(StrUtil.isNotBlank(status), DeviceBackupRecordEntity::getLastBackupStatus, status);
        lambda.orderByDesc(DeviceBackupRecordEntity::getLastBackupTime);
        return wrapper;
    }

    @Override
    public PageData<DeviceBackupRecordDTO> page(DeviceBackupRecordPageRequest request) {
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getName()), DeviceBackupRecordEntity::getName, request.getName());
        wrapper.like(StrUtil.isNotBlank(request.getIp()), DeviceBackupRecordEntity::getIp, request.getIp());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), DeviceBackupRecordEntity::getLastBackupStatus, request.getStatus());
        Page<DeviceBackupRecordEntity> page = buildPage(request);
        IPage<DeviceBackupRecordEntity> result = baseDao.selectPage(page, wrapper);
        List<DeviceBackupRecordDTO> list = ConvertUtils.sourceToTarget(result.getRecords(), DeviceBackupRecordDTO.class);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public DeviceBackupRecordDTO get(DeviceBackupRecordIdRequest request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        DeviceBackupRecordEntity entity = baseDao.selectById(request.getId());
        return ConvertUtils.sourceToTarget(entity, DeviceBackupRecordDTO.class);
    }

    @Override
    public void delete(DeviceBackupRecordDeleteRequest request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        super.delete(request.getIds());
    }

    @Override
    public List<DeviceBackupHistoryDTO> history(DeviceBackupRecordHistoryRequest request) {
        if (request == null) {
            return new ArrayList<>();
        }
        return deviceBackupHistoryService.listByIp(request.getIp(), request.getLimit());
    }

    @Override
    public List<DeviceBackupDiffLineDTO> diff(DeviceBackupRecordDiffRequest request) {
        if (request == null) {
            return new ArrayList<>();
        }
        List<java.util.Map<String, Object>> data = deviceBackupHistoryService.diffById(request.getLeftId(), request.getRightId());
        return toDiffLines(data);
    }

    @Override
    public List<DeviceBackupDiffLineDTO> diffCurrent(DeviceBackupRecordDiffCurrentRequest request) {
        if (request == null) {
            return new ArrayList<>();
        }
        DeviceBackupRecordDTO current = getByIp(request.getIp());
        if (current == null || current.getUrl() == null) {
            throw new RenException("当前记录不存在或URL为空");
        }
        DeviceBackupHistoryDTO history = deviceBackupHistoryService.get(request.getHistoryId());
        if (history == null || history.getUrl() == null) {
            throw new RenException("历史记录不存在或URL为空");
        }
        List<java.util.Map<String, Object>> data = deviceBackupHistoryService.diffByUrls(history.getUrl(), current.getUrl());
        return toDiffLines(data);
    }

    @Override
    public String preview(DeviceBackupRecordPreviewRequest request) {
        if (request == null) {
            return "";
        }
        return deviceBackupHistoryService.previewByUrl(request.getUrl());
    }

    @Override
    public void download(DeviceBackupRecordDownloadRequest request, HttpServletResponse response) {
        if (request == null || request.getUrl() == null || request.getUrl().isBlank()) {
            response.setStatus(400);
            return;
        }
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
        } catch (Exception e) {
            response.setStatus(500);
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    @Override
    public void upsertRecord(String name, String ip, String url, boolean success) {
        if (StrUtil.isBlank(ip)) {
            return;
        }
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DeviceBackupRecordEntity::getIp, ip);
        DeviceBackupRecordEntity existing = baseDao.selectOne(wrapper);
        Date now = new Date();
        if (existing == null) {
            DeviceBackupRecordEntity entity = new DeviceBackupRecordEntity();
            entity.setName(name);
            entity.setIp(ip);
            entity.setUrl(url);
            entity.setLastBackupTime(now);
            entity.setLastBackupStatus(success ? 1 : 0);
            entity.setBackupNum(1);
            baseDao.insert(entity);
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
        baseDao.updateById(existing);
    }

    @Override
    public DeviceBackupRecordDTO getByIp(String ip) {
        if (StrUtil.isBlank(ip)) {
            return null;
        }
        LambdaQueryWrapper<DeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DeviceBackupRecordEntity::getIp, ip).last("limit 1");
        DeviceBackupRecordEntity existing = baseDao.selectOne(wrapper);
        return ConvertUtils.sourceToTarget(existing, DeviceBackupRecordDTO.class);
    }

    private Page<DeviceBackupRecordEntity> buildPage(DeviceBackupRecordPageRequest request) {
        long curPage = 1;
        long limit = 10;
        if (request != null) {
            if (StrUtil.isNotBlank(request.getPage())) {
                curPage = Long.parseLong(request.getPage());
            }
            if (StrUtil.isNotBlank(request.getLimit())) {
                limit = Long.parseLong(request.getLimit());
            }
        }
        Page<DeviceBackupRecordEntity> page = new Page<>(curPage, limit);
        if (request == null) {
            return page;
        }
        if (StrUtil.isNotBlank(request.getOrderField()) && StrUtil.isNotBlank(request.getOrder())) {
            if (Constant.ASC.equalsIgnoreCase(request.getOrder())) {
                page.addOrder(OrderItem.asc(request.getOrderField()));
            } else {
                page.addOrder(OrderItem.desc(request.getOrderField()));
            }
        }
        return page;
    }

    private List<DeviceBackupDiffLineDTO> toDiffLines(List<java.util.Map<String, Object>> data) {
        List<DeviceBackupDiffLineDTO> list = new ArrayList<>();
        if (data == null || data.isEmpty()) {
            return list;
        }
        for (java.util.Map<String, Object> item : data) {
            DeviceBackupDiffLineDTO line = new DeviceBackupDiffLineDTO();
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
