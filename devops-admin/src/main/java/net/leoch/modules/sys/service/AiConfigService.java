package net.leoch.modules.sys.service;

import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.base.Constant;
import net.leoch.common.exception.ServiceException;
import net.leoch.framework.config.ops.HttpTimeoutConfig;
import net.leoch.modules.sys.vo.req.SysAiConfigReq;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.modules.sys.vo.rsp.SysAiConfigRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

/**
 * AI配置（统一走sys_params）
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class AiConfigService {
    private final ISysParamsService sysParamsService;
    private final HttpTimeoutConfig httpTimeoutConfig;

    public SysAiConfigRsp getConfig() {
        SysAiConfigRsp rsp = sysParamsService.getValueObject(Constant.AI_CONFIG_KEY, SysAiConfigRsp.class);
        if (rsp == null) {
            rsp = new SysAiConfigRsp();
        }
        if (rsp.getStatus() == null) {
            rsp.setStatus(0);
        }
        return rsp;
    }

    @Transactional(rollbackFor = Exception.class)
    public void saveConfig(SysAiConfigReq req) {
        validateReq(req);
        testConnection(req);
        String json = JSONUtil.toJsonStr(req);
        int count = sysParamsService.updateValueByCode(Constant.AI_CONFIG_KEY, json);
        if (count > 0) {
            return;
        }
        SysParamsReq paramsReq = new SysParamsReq();
        paramsReq.setParamCode(Constant.AI_CONFIG_KEY);
        paramsReq.setParamValue(json);
        paramsReq.setRemark("AI配置");
        sysParamsService.save(paramsReq);
    }

    public void testConnection(SysAiConfigReq req) {
        validateReq(req);

        HttpURLConnection connection = null;
        try {
            String baseUrl = normalizeBaseUrl(req.getBaseUrl());
            URL url = new URL(baseUrl + "/v1/models");
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(httpTimeoutConfig.getConnectTimeout());
            connection.setReadTimeout(httpTimeoutConfig.getReadTimeout());
            connection.setRequestProperty("Authorization", req.getApiKey().trim());
            connection.setRequestProperty("Content-Type", "application/json");
            int code = connection.getResponseCode();
            InputStream in = code >= 200 && code < 300 ? connection.getInputStream() : connection.getErrorStream();
            String body = in != null ? new String(in.readAllBytes(), StandardCharsets.UTF_8) : "";
            if (code < 200 || code >= 300) {
                log.warn("[AI配置] 测试连接失败, code={}, body={}", code, body);
                throw new ServiceException("AI连接测试失败，HTTP状态码: " + code);
            }
            ensureModelExists(req.getModel(), body);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.warn("[AI配置] 测试连接异常, baseUrl={}", req.getBaseUrl(), e);
            throw new ServiceException("AI连接测试失败");
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private void validateReq(SysAiConfigReq req) {
        if (req == null) {
            throw new ServiceException("AI配置不能为空");
        }
        if (StrUtil.isBlank(req.getBaseUrl()) || StrUtil.isBlank(req.getApiKey()) || StrUtil.isBlank(req.getModel())) {
            throw new ServiceException("AI配置缺少必填项");
        }
        if (req.getStatus() == null) {
            req.setStatus(0);
        }
    }

    private String normalizeBaseUrl(String baseUrl) {
        String value = (baseUrl == null ? "" : baseUrl.trim());
        while (value.endsWith("/")) {
            value = value.substring(0, value.length() - 1);
        }
        return value;
    }

    private void ensureModelExists(String model, String body) {
        if (StrUtil.isBlank(model)) {
            throw new ServiceException("Model不能为空");
        }
        if (StrUtil.isBlank(body) || !JSONUtil.isTypeJSON(body)) {
            throw new ServiceException("AI连接测试失败，模型列表返回格式异常");
        }
        JSONObject json = JSONUtil.parseObj(body);
        Object dataObj = json.get("data");
        if (!(dataObj instanceof JSONArray data)) {
            throw new ServiceException("AI连接测试失败，未返回模型列表");
        }
        String target = model.trim();
        boolean exists = data.stream().anyMatch(item -> {
            if (!(item instanceof JSONObject obj)) {
                return false;
            }
            return target.equals(String.valueOf(obj.get("id")));
        });
        if (!exists) {
            throw new ServiceException("模型不存在或当前API Key无权限访问该模型: " + target);
        }
    }
}
