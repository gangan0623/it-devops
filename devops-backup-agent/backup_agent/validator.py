import logging


class ConfigValidator:
    class DeviceType:
        HUAWEI = "HUAWEI"
        H3C = "H3C"
        FORTI = "FORTI"
        RGOS = "RGOS"
        GENERIC = "GENERIC"

    @staticmethod
    def is_valid(config, device_type):
        if config is None or not str(config).strip() or str(config).strip() == "null":
            logging.debug("config validation failed: empty")
            return False

        min_len = ConfigValidator._min_len(device_type)
        if len(config) < min_len:
            logging.debug("config validation failed: length %s < %s", len(config), min_len)
            return False

        return ConfigValidator._has_keywords(config.lower(), device_type)

    @staticmethod
    def _min_len(device_type):
        if device_type in (ConfigValidator.DeviceType.HUAWEI, ConfigValidator.DeviceType.H3C):
            return 200
        if device_type == ConfigValidator.DeviceType.FORTI:
            return 150
        if device_type == ConfigValidator.DeviceType.RGOS:
            return 150
        return 100

    @staticmethod
    def _has_keywords(config, device_type):
        if device_type == ConfigValidator.DeviceType.HUAWEI:
            return ConfigValidator._has_huawei_keywords(config)
        if device_type == ConfigValidator.DeviceType.H3C:
            return ConfigValidator._has_h3c_keywords(config)
        if device_type == ConfigValidator.DeviceType.FORTI:
            return ConfigValidator._has_forti_keywords(config)
        if device_type == ConfigValidator.DeviceType.RGOS:
            return ConfigValidator._has_rgos_keywords(config)
        return ConfigValidator._has_generic_keywords(config)

    @staticmethod
    def _has_huawei_keywords(config):
        has = any(k in config for k in [
            "interface", "vlan", "router", "system-view", "version", "sysname", "huawei", "vrp"
        ])
        if not has:
            logging.debug("huawei config validation failed: keywords missing")
        return has

    @staticmethod
    def _has_h3c_keywords(config):
        has = any(k in config for k in [
            "interface", "vlan", "router", "system-view", "version", "sysname", "h3c", "comware"
        ])
        if not has:
            logging.debug("h3c config validation failed: keywords missing")
        return has

    @staticmethod
    def _has_forti_keywords(config):
        has = any(k in config for k in [
            "config", "set", "edit", "policy", "firewall", "fortigate", "fortios",
            "system", "interface", "vdom", "status", "hardware", "ha", "version"
        ])
        if not has:
            logging.debug("forti config validation failed: keywords missing")
        return has

    @staticmethod
    def _has_rgos_keywords(config):
        has = any(k in config for k in [
            "ruijie", "rgos", "show running-config", "interface", "vlan", "version"
        ])
        if not has:
            logging.debug("rgos config validation failed: keywords missing")
        return has

    @staticmethod
    def _has_generic_keywords(config):
        has = any(k in config for k in [
            "config", "interface", "system", "version", "set", "show", "display"
        ])
        if not has:
            logging.debug("generic config validation failed: keywords missing")
        return has
