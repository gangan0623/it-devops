package net.leoch.common.utils.ops;

import cn.hutool.core.lang.TypeReference;
import cn.hutool.json.JSONUtil;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;

@Slf4j
public final class AlertJsonUtils {
    private static final ObjectMapper LENIENT_MAPPER = new ObjectMapper()
        .configure(JsonReadFeature.ALLOW_UNESCAPED_CONTROL_CHARS.mappedFeature(), true)
        .configure(JsonReadFeature.ALLOW_BACKSLASH_ESCAPING_ANY_CHARACTER.mappedFeature(), true);

    private AlertJsonUtils() {
    }

    public static Map<String, Object> parsePayload(String rawJson) {
        if (rawJson == null || rawJson.isEmpty()) {
            return new HashMap<>();
        }
        try {
            return LENIENT_MAPPER.readValue(rawJson, new com.fasterxml.jackson.core.type.TypeReference<Map<String, Object>>() {});
        } catch (Exception e) {
            log.warn("[告警JSON] Jackson解析失败，使用Hutool重试, jsonLength={}", rawJson.length(), e);
            return JSONUtil.toBean(rawJson, new TypeReference<Map<String, Object>>() {}, false);
        }
    }
}
