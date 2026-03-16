package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.exception.ServiceException;
import net.leoch.framework.config.ops.OnlineStatusConfig;
import net.leoch.modules.ops.entity.NetworkDeviceBackupRecordEntity;
import net.leoch.modules.ops.mapper.NetworkDeviceBackupRecordMapper;
import net.leoch.modules.ops.service.INetworkDeviceBackupHistoryService;
import net.leoch.modules.ops.service.INetworkDeviceBackupRecordService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupDiffLineRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupHistoryRsp;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupRecordRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
public class NetworkDeviceBackupRecordServiceImpl extends ServiceImpl<NetworkDeviceBackupRecordMapper, NetworkDeviceBackupRecordEntity> implements INetworkDeviceBackupRecordService {

    private static final Set<String> ALLOWED_DOWNLOAD_HOSTS = new HashSet<>();

    private final INetworkDeviceBackupHistoryService deviceBackupHistoryService;
    private final OnlineStatusConfig properties;

    public NetworkDeviceBackupRecordServiceImpl(INetworkDeviceBackupHistoryService deviceBackupHistoryService,
                                         OnlineStatusConfig properties) {
        this.deviceBackupHistoryService = deviceBackupHistoryService;
        this.properties = properties;
    }

    @Override
    public PageData<NetworkDeviceBackupRecordRsp> page(NetworkDeviceBackupRecordPageReq request) {
        LambdaQueryWrapper<NetworkDeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(request.getName()), NetworkDeviceBackupRecordEntity::getName, request.getName());
        wrapper.like(StrUtil.isNotBlank(request.getIp()), NetworkDeviceBackupRecordEntity::getIp, request.getIp());
        wrapper.eq(StrUtil.isNotBlank(request.getStatus()), NetworkDeviceBackupRecordEntity::getLastBackupStatus, request.getStatus());
        Page<NetworkDeviceBackupRecordEntity> page = request.buildPage();
        IPage<NetworkDeviceBackupRecordEntity> result = this.page(page, wrapper);
        List<NetworkDeviceBackupRecordRsp> list = BeanUtil.copyToList(result.getRecords(), NetworkDeviceBackupRecordRsp.class);
        return new PageData<>(list, result.getTotal());
    }

    @Override
    public NetworkDeviceBackupRecordRsp get(NetworkDeviceBackupRecordIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        NetworkDeviceBackupRecordEntity entity = this.getById(request.getId());
        return BeanUtil.copyProperties(entity, NetworkDeviceBackupRecordRsp.class);
    }

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void delete(NetworkDeviceBackupRecordDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            return;
        }
        this.removeByIds(Arrays.asList(request.getIds()));
    }

    @Override
    public List<NetworkDeviceBackupHistoryRsp> history(NetworkDeviceBackupRecordHistoryReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        return deviceBackupHistoryService.listByIp(request.getIp(), request.getLimit());
    }

    @Override
    public List<NetworkDeviceBackupDiffLineRsp> diff(NetworkDeviceBackupRecordDiffReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        List<Map<String, Object>> data = deviceBackupHistoryService.diffById(request.getLeftId(), request.getRightId());
        return toDiffLines(data);
    }

    @Override
    public List<NetworkDeviceBackupDiffLineRsp> diffCurrent(NetworkDeviceBackupRecordDiffCurrentReq request) {
        if (request == null) {
            return new ArrayList<>();
        }
        NetworkDeviceBackupRecordRsp current = getByIp(request.getIp());
        if (current == null || current.getUrl() == null) {
            throw new ServiceException("当前记录不存在或URL为空");
        }
        NetworkDeviceBackupHistoryRsp history = deviceBackupHistoryService.get(request.getHistoryId());
        if (history == null || history.getUrl() == null) {
            throw new ServiceException("历史记录不存在或URL为空");
        }
        List<Map<String, Object>> data = deviceBackupHistoryService.diffByUrls(history.getUrl(), current.getUrl());
        return toDiffLines(data);
    }

    @Override
    public String preview(NetworkDeviceBackupRecordPreviewReq request) {
        if (request == null) {
            return "";
        }
        return deviceBackupHistoryService.previewByUrl(request.getUrl());
    }

    @Override
    public void download(NetworkDeviceBackupRecordDownloadReq request, HttpServletResponse response) {
        if (request == null || request.getUrl() == null || request.getUrl().isBlank()) {
            log.warn("[备份下载] 请求参数无效, request={}", request);
            response.setStatus(400);
            return;
        }
        log.info("[备份下载] 开始下载, url={}", request.getUrl());
        validateDownloadUrl(request.getUrl());
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(request.getUrl()).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(properties.getBackup().getDownloadConnectTimeout());
            connection.setReadTimeout(properties.getBackup().getDownloadReadTimeout());
            int code = connection.getResponseCode();
            if (code != 200) {
                log.warn("[备份下载] HTTP响应异常, url={}, code={}", request.getUrl(), code);
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
            log.info("[备份下载] 下载成功, fileName={}", fileName);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.error("[备份下载] 下载失败, url={}", request.getUrl(), e);
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
            throw new ServiceException("下载URL格式无效", e);
        }
    }

    @Override
    public void upsertRecord(String name, String ip, String url, boolean success) {
        if (StrUtil.isBlank(ip)) {
            log.warn("[备份记录] upsert失败, ip为空");
            return;
        }
        log.debug("[备份记录] upsert备份记录, ip={}, success={}", ip, success);
        LambdaQueryWrapper<NetworkDeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(NetworkDeviceBackupRecordEntity::getIp, ip);
        NetworkDeviceBackupRecordEntity existing = this.getOne(wrapper);
        Date now = new Date();
        if (existing == null) {
            NetworkDeviceBackupRecordEntity entity = new NetworkDeviceBackupRecordEntity();
            entity.setName(name);
            entity.setIp(ip);
            entity.setUrl(url);
            entity.setLastBackupTime(now);
            entity.setLastBackupStatus(success ? 1 : 0);
            entity.setBackupNum(1);
            this.getBaseMapper().insert(entity);
            log.info("[备份记录] 新建备份记录, ip={}, name={}", ip, name);
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
        log.debug("[备份记录] 更新备份记录, ip={}, backupNum={}", ip, existing.getBackupNum());
    }

    @Override
    public NetworkDeviceBackupRecordRsp getByIp(String ip) {
        if (StrUtil.isBlank(ip)) {
            return null;
        }
        LambdaQueryWrapper<NetworkDeviceBackupRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(NetworkDeviceBackupRecordEntity::getIp, ip).last("limit 1");
        NetworkDeviceBackupRecordEntity existing = this.getOne(wrapper);
        return BeanUtil.copyProperties(existing, NetworkDeviceBackupRecordRsp.class);
    }

    private List<NetworkDeviceBackupDiffLineRsp> toDiffLines(List<Map<String, Object>> data) {
        if (data == null || data.isEmpty()) {
            return new ArrayList<>();
        }
        List<NetworkDeviceBackupDiffLineRsp> list = new ArrayList<>(data.size());
        int index = 0;
        while (index < data.size()) {
            Map<String, Object> item = data.get(index);
            String type = item.get("type") == null ? "" : String.valueOf(item.get("type"));
            if ("same".equals(type)) {
                list.add(buildSameLine(item));
                index++;
                continue;
            }
            if ("del".equals(type) || "add".equals(type)) {
                index = appendChangeBlock(data, index, list);
                continue;
            }
            list.add(buildSingleSideLine(type, item));
            index++;
        }
        return list;
    }

    private int appendChangeBlock(List<Map<String, Object>> data, int start, List<NetworkDeviceBackupDiffLineRsp> target) {
        List<Map<String, Object>> leftOnly = new ArrayList<>();
        List<Map<String, Object>> rightOnly = new ArrayList<>();
        int index = start;
        while (index < data.size()) {
            Map<String, Object> item = data.get(index);
            String type = item.get("type") == null ? "" : String.valueOf(item.get("type"));
            if ("del".equals(type)) {
                leftOnly.add(item);
                index++;
                continue;
            }
            if ("add".equals(type)) {
                rightOnly.add(item);
                index++;
                continue;
            }
            break;
        }
        int size = Math.max(leftOnly.size(), rightOnly.size());
        for (int i = 0; i < size; i++) {
            Map<String, Object> left = i < leftOnly.size() ? leftOnly.get(i) : null;
            Map<String, Object> right = i < rightOnly.size() ? rightOnly.get(i) : null;
            NetworkDeviceBackupDiffLineRsp line = new NetworkDeviceBackupDiffLineRsp();
            line.setLeftLineNo(left == null ? null : toInt(left.get("leftLineNo")));
            line.setRightLineNo(right == null ? null : toInt(right.get("rightLineNo")));
            line.setLeftContent(left == null ? "" : toText(left.get("content")));
            line.setRightContent(right == null ? "" : toText(right.get("content")));
            if (left != null && right != null && charOverlapRatio(line.getLeftContent(), line.getRightContent()) >= 0.3) {
                line.setType("change");
            } else if (left != null) {
                line.setType("del");
                line.setRightContent("");
                line.setRightLineNo(null);
            } else {
                line.setType("add");
                line.setLeftContent("");
                line.setLeftLineNo(null);
            }
            target.add(line);
        }
        return index;
    }

    /**
     * 计算两行字符重叠率：公共字符数 × 2 / (leftLen + rightLen)
     * 基于字符频率统计，O(n) 复杂度。
     * 用于判断两行是否足够相似，阈值 0.3 认为相关。
     */
    private double charOverlapRatio(String left, String right) {
        if (left == null || right == null) return 0.0;
        int totalLen = left.length() + right.length();
        if (totalLen == 0) return 1.0;
        Map<Character, Integer> freq = new HashMap<>();
        for (char c : left.toCharArray()) {
            freq.merge(c, 1, Integer::sum);
        }
        int common = 0;
        for (char c : right.toCharArray()) {
            int count = freq.getOrDefault(c, 0);
            if (count > 0) {
                common++;
                freq.put(c, count - 1);
            }
        }
        return common * 2.0 / totalLen;
    }

    private NetworkDeviceBackupDiffLineRsp buildSameLine(Map<String, Object> item) {
        NetworkDeviceBackupDiffLineRsp line = new NetworkDeviceBackupDiffLineRsp();
        line.setType("same");
        line.setLeftLineNo(toInt(item.get("leftLineNo")));
        line.setRightLineNo(toInt(item.get("rightLineNo")));
        String content = toText(item.get("content"));
        line.setLeftContent(content);
        line.setRightContent(content);
        return line;
    }

    private NetworkDeviceBackupDiffLineRsp buildSingleSideLine(String type, Map<String, Object> item) {
        NetworkDeviceBackupDiffLineRsp line = new NetworkDeviceBackupDiffLineRsp();
        line.setType(type);
        if ("del".equals(type)) {
            line.setLeftLineNo(toInt(item.get("leftLineNo")));
            line.setLeftContent(toText(item.get("content")));
            line.setRightContent("");
        } else {
            line.setRightLineNo(toInt(item.get("rightLineNo")));
            line.setRightContent(toText(item.get("content")));
            line.setLeftContent("");
        }
        return line;
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
        } catch (Exception e) {
            log.warn("[设备备份记录] 整数解析失败, value={}", value, e);
            return null;
        }
    }

    private String toText(Object value) {
        return value == null ? "" : String.valueOf(value);
    }
}
