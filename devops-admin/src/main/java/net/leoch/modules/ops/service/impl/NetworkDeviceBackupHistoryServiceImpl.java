package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ServiceException;
import net.leoch.modules.ops.entity.NetworkDeviceBackupHistoryEntity;
import net.leoch.modules.ops.mapper.NetworkDeviceBackupHistoryMapper;
import net.leoch.modules.ops.service.INetworkDeviceBackupHistoryService;
import net.leoch.modules.ops.vo.rsp.NetworkDeviceBackupHistoryRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 设备备份历史表
 */
@Slf4j
@Service
public class NetworkDeviceBackupHistoryServiceImpl extends ServiceImpl<NetworkDeviceBackupHistoryMapper, NetworkDeviceBackupHistoryEntity> implements INetworkDeviceBackupHistoryService {

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveHistory(String name, String ip, String url, Integer status) {
        if (StrUtil.isBlank(ip)) {
            return;
        }
        NetworkDeviceBackupHistoryEntity entity = new NetworkDeviceBackupHistoryEntity();
        entity.setName(name);
        entity.setIp(ip);
        entity.setUrl(url);
        entity.setBackupTime(new Date());
        entity.setBackupStatus(status);
        this.getBaseMapper().insert(entity);
    }

    @Override
    public List<NetworkDeviceBackupHistoryRsp> listByIp(String ip, Integer limit) {
        LambdaQueryWrapper<NetworkDeviceBackupHistoryEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(StrUtil.isNotBlank(ip), NetworkDeviceBackupHistoryEntity::getIp, ip);
        wrapper.orderByDesc(NetworkDeviceBackupHistoryEntity::getBackupTime);
        if (limit != null && limit > 0) {
            wrapper.last("limit " + Math.min(limit, 200));
        }
        List<NetworkDeviceBackupHistoryEntity> list = this.list(wrapper);
        return BeanUtil.copyToList(list, NetworkDeviceBackupHistoryRsp.class);
    }

    @Override
    public NetworkDeviceBackupHistoryRsp get(Long id) {
        if (id == null) {
            return null;
        }
        NetworkDeviceBackupHistoryEntity entity = this.getById(id);
        return BeanUtil.copyProperties(entity, NetworkDeviceBackupHistoryRsp.class);
    }

    @Override
    public List<Map<String, Object>> diffById(Long leftId, Long rightId) {
        if (leftId == null || rightId == null) {
            throw new ServiceException("请选择两条记录进行对比");
        }
        NetworkDeviceBackupHistoryEntity left = this.getById(leftId);
        NetworkDeviceBackupHistoryEntity right = this.getById(rightId);
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
        validateReadUrl(url);
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
        validateReadUrl(url);
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

    private void validateReadUrl(String urlStr) {
        try {
            URI uri = new URI(urlStr);
            String scheme = uri.getScheme();
            if (scheme == null || (!scheme.equals("http") && !scheme.equals("https"))) {
                throw new ServiceException("仅支持HTTP/HTTPS协议读取");
            }
            String host = uri.getHost();
            if (host == null) {
                throw new ServiceException("备份URL无效");
            }
            if (host.equals("127.0.0.1") || host.equals("localhost") || host.startsWith("169.254.") || host.equals("0.0.0.0")) {
                throw new ServiceException("不允许访问内部地址");
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException("备份URL格式无效", e);
        }
    }

    private List<Map<String, Object>> diffLines(List<String> left, List<String> right) {
        List<String> leftRaw = pruneEdgeBlankLines(left);
        List<String> rightRaw = pruneEdgeBlankLines(right);
        // 行数保护
        if (leftRaw.size() > 20_000 || rightRaw.size() > 20_000) {
            throw new ServiceException("文件行数超过限制（20000行），无法对比");
        }
        List<String> leftNorm = normalizeLines(leftRaw);
        List<String> rightNorm = normalizeLines(rightRaw);
        return myersDiff(leftRaw, leftNorm, rightRaw, rightNorm);
    }

    /**
     * Myers diff 算法。
     * 时间 O(nd)，空间 O(n)。n=行数，d=编辑操作数。
     */
    private List<Map<String, Object>> myersDiff(
            List<String> leftRaw, List<String> leftNorm,
            List<String> rightRaw, List<String> rightNorm) {
        int m = leftNorm.size();
        int n = rightNorm.size();
        int max = m + n;
        if (max == 0) {
            return new ArrayList<>();
        }
        final int MAX_DIFF_STEPS = Math.min(max, 2000);
        // v[k] = 在对角线 k 上能到达的最远 x 坐标
        int[] v = new int[2 * max + 2];
        // trace 记录每步的 v 快照，用于回溯
        List<int[]> trace = new ArrayList<>();

        outer:
        for (int d = 0; d <= max; d++) {
            if (d > MAX_DIFF_STEPS) {
                // 差异过大，降级为全量替换
                List<Map<String, Object>> fallback = new ArrayList<>(m + n);
                for (int fi = 0; fi < m; fi++) {
                    Map<String, Object> line = new HashMap<>();
                    line.put("type", "del");
                    line.put("leftLineNo", fi + 1);
                    line.put("rightLineNo", 0);
                    line.put("content", leftRaw.get(fi));
                    fallback.add(line);
                }
                for (int fi = 0; fi < n; fi++) {
                    Map<String, Object> line = new HashMap<>();
                    line.put("type", "add");
                    line.put("leftLineNo", 0);
                    line.put("rightLineNo", fi + 1);
                    line.put("content", rightRaw.get(fi));
                    fallback.add(line);
                }
                return fallback;
            }
            trace.add(v.clone());
            for (int k = -d; k <= d; k += 2) {
                int x;
                if (k == -d || (k != d && v[k - 1 + max] < v[k + 1 + max])) {
                    x = v[k + 1 + max]; // 向下
                } else {
                    x = v[k - 1 + max] + 1; // 向右
                }
                int y = x - k;
                while (x < m && y < n && leftNorm.get(x).equals(rightNorm.get(y))) {
                    x++;
                    y++;
                }
                v[k + max] = x;
                if (x >= m && y >= n) {
                    trace.add(v.clone());
                    break outer;
                }
            }
        }

        // 回溯生成 edit script
        List<int[]> path = new ArrayList<>();
        int x = m, y = n;
        for (int d = trace.size() - 1; d >= 0; d--) {
            int[] vSnap = trace.get(d);
            int k = x - y;
            int prevK;
            if (d == 0) {
                prevK = k;
            } else if (k == -d || (k != d && vSnap[k - 1 + max] < vSnap[k + 1 + max])) {
                prevK = k + 1;
            } else {
                prevK = k - 1;
            }
            int[] prevSnap = trace.get(Math.max(d - 1, 0));
            int prevX = d == 0 ? 0 : prevSnap[prevK + max];
            int prevY = prevX - prevK;
            // snake: diagonal moves
            while (x > prevX && y > prevY) {
                path.add(new int[]{x - 1, y - 1, 0}); // 0 = same
                x--;
                y--;
            }
            if (d > 0) {
                if (x == prevX) {
                    path.add(new int[]{x, y - 1, 2}); // 2 = add (right side)
                    y--;
                } else {
                    path.add(new int[]{x - 1, y, 1}); // 1 = del (left side)
                    x--;
                }
            }
        }
        Collections.reverse(path);

        // 生成结果
        List<Map<String, Object>> result = new ArrayList<>(path.size());
        for (int[] step : path) {
            int xi = step[0];
            int yi = step[1];
            int op = step[2];
            Map<String, Object> line = new HashMap<>();
            if (op == 0) {
                line.put("type", "same");
                line.put("leftLineNo", xi + 1);
                line.put("rightLineNo", yi + 1);
                line.put("content", rightRaw.get(yi));
            } else if (op == 1) {
                line.put("type", "del");
                line.put("leftLineNo", xi + 1);
                line.put("rightLineNo", 0);
                line.put("content", leftRaw.get(xi));
            } else {
                line.put("type", "add");
                line.put("leftLineNo", 0);
                line.put("rightLineNo", yi + 1);
                line.put("content", rightRaw.get(yi));
            }
            result.add(line);
        }
        return result;
    }

    private List<String> normalizeLines(List<String> lines) {
        List<String> normalized = new ArrayList<>(lines.size());
        for (String line : lines) {
            normalized.add(normalizeLine(line));
        }
        return normalized;
    }

    private String normalizeLine(String line) {
        String value = line == null ? "" : line;
        if (!value.isEmpty() && value.charAt(0) == '\uFEFF') {
            value = value.substring(1);
        }
        int end = value.length();
        while (end > 0) {
            char ch = value.charAt(end - 1);
            if (ch == ' ' || ch == '\t' || ch == '\r') {
                end--;
                continue;
            }
            break;
        }
        return value.substring(0, end);
    }

    private List<String> pruneEdgeBlankLines(List<String> lines) {
        if (lines == null || lines.isEmpty()) {
            return new ArrayList<>();
        }
        int start = 0;
        int end = lines.size() - 1;
        while (start <= end && StrUtil.isBlank(normalizeLine(lines.get(start)))) {
            start++;
        }
        while (end >= start && StrUtil.isBlank(normalizeLine(lines.get(end)))) {
            end--;
        }
        if (start > end) {
            return new ArrayList<>();
        }
        return new ArrayList<>(lines.subList(start, end + 1));
    }
}
