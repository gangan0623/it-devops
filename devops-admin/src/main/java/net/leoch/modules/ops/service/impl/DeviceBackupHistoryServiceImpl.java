package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.ops.dao.DeviceBackupHistoryDao;
import net.leoch.modules.ops.dto.DeviceBackupHistoryDTO;
import net.leoch.modules.ops.entity.DeviceBackupHistoryEntity;
import net.leoch.modules.ops.service.DeviceBackupHistoryService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 设备备份历史表
 */
@Slf4j
@Service
public class DeviceBackupHistoryServiceImpl extends ServiceImpl<DeviceBackupHistoryDao, DeviceBackupHistoryEntity> implements DeviceBackupHistoryService {

    @Override
    public void saveHistory(String name, String ip, String url, Integer status) {
        if (StrUtil.isBlank(ip)) {
            return;
        }
        DeviceBackupHistoryEntity entity = new DeviceBackupHistoryEntity();
        entity.setName(name);
        entity.setIp(ip);
        entity.setUrl(url);
        entity.setBackupTime(new Date());
        entity.setBackupStatus(status);
        this.getBaseMapper().insert(entity);
    }

    @Override
    public List<DeviceBackupHistoryDTO> listByIp(String ip, Integer limit) {
        LambdaQueryWrapper<DeviceBackupHistoryEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(ip), DeviceBackupHistoryEntity::getIp, ip);
        wrapper.orderByDesc(DeviceBackupHistoryEntity::getBackupTime);
        if (limit != null && limit > 0) {
            wrapper.last("limit " + Math.min(limit, 200));
        }
        List<DeviceBackupHistoryEntity> list = this.list(wrapper);
        return ConvertUtils.sourceToTarget(list, DeviceBackupHistoryDTO.class);
    }

    @Override
    public DeviceBackupHistoryDTO get(Long id) {
        if (id == null) {
            return null;
        }
        DeviceBackupHistoryEntity entity = this.getById(id);
        return ConvertUtils.sourceToTarget(entity, DeviceBackupHistoryDTO.class);
    }

    @Override
    public List<Map<String, Object>> diffById(Long leftId, Long rightId) {
        if (leftId == null || rightId == null) {
            throw new ServiceException("请选择两条记录进行对比");
        }
        DeviceBackupHistoryEntity left = this.getById(leftId);
        DeviceBackupHistoryEntity right = this.getById(rightId);
        if (left == null || right == null) {
            throw new ServiceException("历史记录不存在");
        }
        List<String> leftLines = readLines(left.getUrl());
        List<String> rightLines = readLines(right.getUrl());
        return diffLines(leftLines, rightLines);
    }

    @Override
    public List<Map<String, Object>> diffByUrls(String leftUrl, String rightUrl) {
        List<String> leftLines = readLines(leftUrl);
        List<String> rightLines = readLines(rightUrl);
        return diffLines(leftLines, rightLines);
    }

    @Override
    public String previewByUrl(String url) {
        if (StrUtil.isBlank(url)) {
            return "";
        }
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(15000);
            int code = connection.getResponseCode();
            if (code != 200) {
                throw new ServiceException("备份文件读取失败，HTTP " + code);
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readAllBytes();
                return new String(bytes, StandardCharsets.UTF_8);
            }
        } catch (Exception e) {
            throw new ServiceException("备份文件读取失败: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private List<String> readLines(String url) {
        if (StrUtil.isBlank(url)) {
            return new ArrayList<>();
        }
        HttpURLConnection connection = null;
        try {
            connection = (HttpURLConnection) new URL(url).openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000);
            connection.setReadTimeout(15000);
            int code = connection.getResponseCode();
            if (code != 200) {
                throw new ServiceException("备份文件读取失败，HTTP " + code);
            }
            try (InputStream in = connection.getInputStream()) {
                byte[] bytes = in.readAllBytes();
                String content = new String(bytes, StandardCharsets.UTF_8);
                String[] lines = content.split("\\r?\\n", -1);
                List<String> list = new ArrayList<>();
                for (String line : lines) {
                    list.add(line);
                }
                return list;
            }
        } catch (Exception e) {
            throw new ServiceException("备份文件读取失败: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private List<Map<String, Object>> diffLines(List<String> left, List<String> right) {
        int m = left.size();
        int n = right.size();
        int[][] lcs = new int[m + 1][n + 1];
        for (int i = 1; i <= m; i++) {
            for (int j = 1; j <= n; j++) {
                if (left.get(i - 1).equals(right.get(j - 1))) {
                    lcs[i][j] = lcs[i - 1][j - 1] + 1;
                } else {
                    lcs[i][j] = Math.max(lcs[i - 1][j], lcs[i][j - 1]);
                }
            }
        }
        List<Map<String, Object>> result = new ArrayList<>();
        int i = m;
        int j = n;
        while (i > 0 || j > 0) {
            if (i > 0 && j > 0 && left.get(i - 1).equals(right.get(j - 1))) {
                Map<String, Object> line = new HashMap<>();
                line.put("type", "same");
                line.put("leftLineNo", i);
                line.put("rightLineNo", j);
                line.put("content", left.get(i - 1));
                result.add(line);
                i--;
                j--;
            } else if (j > 0 && (i == 0 || lcs[i][j - 1] >= lcs[i - 1][j])) {
                Map<String, Object> line = new HashMap<>();
                line.put("type", "add");
                line.put("leftLineNo", 0);
                line.put("rightLineNo", j);
                line.put("content", right.get(j - 1));
                result.add(line);
                j--;
            } else if (i > 0) {
                Map<String, Object> line = new HashMap<>();
                line.put("type", "del");
                line.put("leftLineNo", i);
                line.put("rightLineNo", 0);
                line.put("content", left.get(i - 1));
                result.add(line);
                i--;
            }
        }
        Collections.reverse(result);
        return result;
    }
}
