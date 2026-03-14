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
import java.io.OutputStream;
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
    private static final int AI_CONNECT_TIMEOUT_MS = 60_000;
    private static final int AI_READ_TIMEOUT_MS = 300_000;

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
        try {
            String responseText = invokeText(req.getBaseUrl(), req.getApiKey(), req.getModel(),
                    "You are a helpful assistant.", "请只回复：ok", 32);
            if (StrUtil.isBlank(responseText)) {
                throw new ServiceException("AI连接测试失败，未返回文本内容");
            }
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            log.warn("[AI配置] 测试连接异常, baseUrl={}", req.getBaseUrl(), e);
            throw new ServiceException("AI连接测试失败");
        }
    }

    public String invokeText(SysAiConfigRsp config, String instructions, String input, Integer maxOutputTokens) {
        if (config == null) {
            throw new ServiceException("AI配置不能为空");
        }
        return invokeText(config.getBaseUrl(), config.getApiKey(), config.getModel(), instructions, input, maxOutputTokens);
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

    private String buildApiUrl(String baseUrl, String path) {
        String value = normalizeBaseUrl(baseUrl);
        if (value.endsWith("/v1") && path.startsWith("/v1/")) {
            return value + path.substring(3);
        }
        return value + path;
    }

    private String resolveAuthorizationHeader(String apiKey) {
        String value = StrUtil.trim(apiKey);
        if (StrUtil.startWithIgnoreCase(value, "Bearer ")) {
            return value;
        }
        return "Bearer " + value;
    }

    private String invokeText(String baseUrl, String apiKey, String model,
                              String instructions, String input, Integer maxOutputTokens) {
        HttpURLConnection connection = null;
        try {
            URL url = new URL(buildApiUrl(baseUrl, "/v1/responses"));
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setDoOutput(true);
            connection.setConnectTimeout(Math.max(httpTimeoutConfig.getConnectTimeout(), AI_CONNECT_TIMEOUT_MS));
            connection.setReadTimeout(Math.max(httpTimeoutConfig.getReadTimeout(), AI_READ_TIMEOUT_MS));
            connection.setRequestProperty("Authorization", resolveAuthorizationHeader(apiKey));
            connection.setRequestProperty("Content-Type", "application/json");

            JSONObject body = new JSONObject();
            body.set("model", StrUtil.trim(model));
            body.set("input", StrUtil.blankToDefault(input, ""));
            if (StrUtil.isNotBlank(instructions)) {
                body.set("instructions", instructions);
            }
            if (maxOutputTokens != null && maxOutputTokens > 0) {
                body.set("max_output_tokens", maxOutputTokens);
            }

            try (OutputStream os = connection.getOutputStream()) {
                os.write(body.toString().getBytes(StandardCharsets.UTF_8));
            }

            int code = connection.getResponseCode();
            InputStream rawIn = code >= 200 && code < 300 ? connection.getInputStream() : connection.getErrorStream();
            String response;
            try (InputStream in = rawIn) {
                response = in != null ? new String(in.readAllBytes(), StandardCharsets.UTF_8) : "";
            }
            if (code < 200 || code >= 300) {
                log.warn("[AI配置] AI调用失败, code={}, body={}", code, response);
                throw new ServiceException("AI接口调用失败，HTTP状态码: " + code);
            }
            return extractOutputText(response);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException("AI接口调用异常: " + e.getMessage());
        } finally {
            if (connection != null) {
                connection.disconnect();
            }
        }
    }

    private String extractOutputText(String body) {
        if (StrUtil.isBlank(body) || !JSONUtil.isTypeJSON(body)) {
            throw new ServiceException("AI返回格式异常");
        }
        JSONObject json = JSONUtil.parseObj(body);
        JSONArray output = json.getJSONArray("output");
        if (output == null || output.isEmpty()) {
            throw new ServiceException("AI返回内容为空");
        }
        StringBuilder text = new StringBuilder();
        for (Object item : output) {
            if (!(item instanceof JSONObject message)) {
                continue;
            }
            JSONArray content = message.getJSONArray("content");
            if (content == null || content.isEmpty()) {
                continue;
            }
            for (Object contentItem : content) {
                if (!(contentItem instanceof JSONObject part)) {
                    continue;
                }
                if (!"output_text".equals(part.getStr("type"))) {
                    continue;
                }
                String value = part.getStr("text");
                if (StrUtil.isNotBlank(value)) {
                    if (!text.isEmpty()) {
                        text.append('\n');
                    }
                    text.append(value.trim());
                }
            }
        }
        if (text.isEmpty()) {
            throw new ServiceException("AI返回文本为空");
        }
        return text.toString();
    }
}
