import io
import logging
import re
from datetime import datetime
from urllib.parse import urlparse

from minio import Minio
from minio.error import S3Error


class MinioUploader:
    def __init__(self, cfg):
        self._cfg = cfg
        endpoint = cfg.get("endpoint")
        secure = False
        if endpoint.startswith("http://") or endpoint.startswith("https://"):
            parsed = urlparse(endpoint)
            secure = parsed.scheme == "https"
            endpoint = parsed.netloc
        self._client = Minio(
            endpoint,
            access_key=cfg.get("access-key"),
            secret_key=cfg.get("secret-key"),
            secure=secure,
        )
        self._bucket = cfg.get("bucket-name")
        self._backup_path = cfg.get("backup-path")
        self._public_url = cfg.get("public-url")

    def ensure_bucket(self):
        try:
            if not self._client.bucket_exists(self._bucket):
                self._client.make_bucket(self._bucket)
                logging.info("created bucket: %s", self._bucket)
        except S3Error as e:
            raise RuntimeError(f"MinIO bucket operation failed: {e}")

    def upload_config(self, hostname, ip, content):
        if content is None or not str(content).strip():
            raise ValueError("config content is empty")
        self.ensure_bucket()

        clean_ip = self._clean_filename(ip)
        now = datetime.now()
        date_folder = now.strftime("%Y-%m-%d")
        time_part = now.strftime("%H-%M-%S")
        filename = f"{clean_ip}_{time_part}.txt"
        object_name = self._build_object_path(f"backup/{date_folder}/{filename}")

        data = content.encode("utf-8")
        data_stream = io.BytesIO(data)
        logging.debug("MinIO object path: %s", object_name)

        self._client.put_object(
            self._bucket,
            object_name,
            data_stream,
            length=len(data),
            content_type="text/plain; charset=utf-8",
        )

        return self._build_public_url(object_name)

    def _clean_filename(self, value):
        if value is None:
            return "unknown"
        return re.sub(r"[^a-zA-Z0-9._-]", "_", str(value))

    def _build_object_path(self, filename):
        backup_path = self._backup_path or ""
        if not backup_path or backup_path == "/":
            return filename
        backup_path = re.sub(r"^/+", "", backup_path)
        backup_path = re.sub(r"/+$", "", backup_path)
        return f"{backup_path}/{filename}"

    def _build_public_url(self, object_name):
        public_url = self._public_url
        if not public_url.endswith("/"):
            public_url += "/"
        return f"{public_url}{self._bucket}/{object_name}"
