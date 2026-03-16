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
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

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

}
