import yaml


def load_config(path):
    with open(path, "r", encoding="utf-8") as f:
        data = yaml.safe_load(f)
    if not isinstance(data, dict) or "minio" not in data:
        raise ValueError("config file must contain 'minio' section")
    minio_cfg = data.get("minio") or {}
    required = ["endpoint", "access-key", "secret-key", "bucket-name", "backup-path", "public-url"]
    missing = [k for k in required if k not in minio_cfg]
    if missing:
        raise ValueError("minio config missing: " + ", ".join(missing))
    callback_url = data.get("callback-url") or data.get("callback_url")
    return {
        "minio": minio_cfg,
        "callback_url": callback_url,
    }
