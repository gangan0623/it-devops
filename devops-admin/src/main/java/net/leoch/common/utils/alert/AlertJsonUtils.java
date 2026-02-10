package net.leoch.common.utils.alert;

import cn.hutool.core.lang.TypeReference;
import com.fasterxml.jackson.core.json.JsonReadFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import cn.hutool.json.JSONUtil;

import java.util.HashMap;
import java.util.Map;

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
        } catch (Exception ignore) {
            return JSONUtil.toBean(rawJson, new TypeReference<Map<String, Object>>() {});
        }
    }
}
