import argparse
import json
import logging
import threading
from concurrent.futures import ThreadPoolExecutor, as_completed

import encodings.idna  # ensure codec bundled for frozen builds
import uvicorn
from fastapi import FastAPI, Header, HTTPException, Request
from urllib.request import Request as UrlRequest, urlopen
from urllib.error import URLError, HTTPError

try:
    from backup_agent.config import load_config
    from backup_agent.minio_client import MinioUploader
    from backup_agent.ssh_client import SshService
except Exception:
    from config import load_config
    from minio_client import MinioUploader
    from ssh_client import SshService


def _setup_logging(log_file):
    handlers = [logging.StreamHandler()]
    if log_file:
        handlers.append(logging.FileHandler(log_file))
    logging.basicConfig(
        level=logging.INFO,
        format="%(asctime)s %(levelname)s %(message)s",
        handlers=handlers,
    )


def _parse_args():
    parser = argparse.ArgumentParser(description="Backup agent")
    parser.add_argument("--config-file", required=True, help="MinIO config yaml file")
    parser.add_argument("--token", required=True, help="agent token")
    parser.add_argument("--callback-url", default=None, help="callback url")
    parser.add_argument("--log-file", default=None, help="log file path")
    parser.add_argument("--host", default="0.0.0.0", help="listen host")
    parser.add_argument("--port", default=8080, type=int, help="listen port")
    return parser.parse_args()


def create_app(minio_cfg, agent_token, callback_url=None):
    app = FastAPI()
    uploader = MinioUploader(minio_cfg)
    ssh_service = SshService()
    executor = ThreadPoolExecutor(max_workers=20)

    app.state.agent_token = agent_token
    app.state.callback_url = callback_url

    @app.get("/healthz")
    async def healthz():
        return {"status": "ok"}

    @app.post("/backup")
    async def backup(request: Request, agent_token: str = Header(None)):
        if agent_token is None:
            raise HTTPException(status_code=401, detail="missing agent-token")
        if agent_token != app.state.agent_token:
            raise HTTPException(status_code=403, detail="invalid token")

        try:
            payload = await request.json()
        except json.JSONDecodeError:
            raise HTTPException(status_code=400, detail="invalid json")

        if not isinstance(payload, list):
            raise HTTPException(status_code=400, detail="payload must be a list")

        threading.Thread(
            target=_process_backup_and_callback,
            args=(payload, ssh_service, uploader, app.state.callback_url, app.state.agent_token, executor),
            daemon=True,
        ).start()
        return {"status": "accepted"}

    return app


def _backup_single(item, ssh_service, uploader):
    index = item.get("_index", 0)
    instance = str(item.get("instance", ""))
    name = str(item.get("name", ""))
    username = str(item.get("username", ""))
    password = str(item.get("password", ""))
    model = str(item.get("model", "")).lower()

    if not instance or not username or not password or not model:
        return {"instance": instance, "url": "", "_index": index}

    try:
        if model in ("huawei", "vrp"):
            config = ssh_service.get_huawei_config(instance, username, password)
        elif model in ("h3c", "comware"):
            config = ssh_service.get_h3c_config(instance, username, password)
        elif model in ("forti", "fortios"):
            config = ssh_service.get_forti_config(instance, username, password)
        elif model in ("ruijie", "rgos"):
            config = ssh_service.get_rgos_config(instance, username, password)
        else:
            logging.warning("unsupported model: %s", model)
            return {"instance": instance, "url": "", "_index": index}

        if not config or not str(config).strip():
            return {"instance": instance, "url": "", "_index": index}

        hostname = name if name else instance
        url = uploader.upload_config(hostname, instance, config)
        return {"instance": instance, "url": url, "_index": index}
    except Exception as e:
        logging.error("backup failed for %s: %s", instance, e)
        return {"instance": instance, "url": "", "_index": index}

def _process_backup_and_callback(payload, ssh_service, uploader, callback_url, agent_token, executor):
    futures = []
    results = []
    for idx, item in enumerate(payload):
        if isinstance(item, dict):
            item = dict(item)
            item["_index"] = idx
            futures.append(executor.submit(_backup_single, item, ssh_service, uploader))
        else:
            results.append({"instance": "", "url": "", "_index": idx})

    for fut in as_completed(futures):
        results.append(fut.result())

    ordered = sorted(results, key=lambda x: x.get("_index", 0))
    final_result = [{"instance": r["instance"], "url": r["url"]} for r in ordered]
    if callback_url:
        _post_callback(callback_url, agent_token, final_result)
    else:
        logging.warning("callback_url not configured, skipping callback")


def _post_callback(callback_url, agent_token, result_list):
    try:
        body = json.dumps(result_list).encode("utf-8")
        req = UrlRequest(
            callback_url,
            data=body,
            method="POST",
            headers={
                "Content-Type": "application/json",
                "agent-token": agent_token,
            },
        )
        with urlopen(req, timeout=10) as resp:
            logging.info("callback status: %s", resp.status)
    except HTTPError as e:
        logging.error("callback http error: %s", e)
    except URLError as e:
        logging.error("callback url error: %s", e)
    except Exception as e:
        logging.error("callback failed: %s", e)

def main():
    args = _parse_args()
    _setup_logging(args.log_file)
    cfg = load_config(args.config_file)
    minio_cfg = cfg.get("minio") or {}
    callback_url = args.callback_url or cfg.get("callback_url")
    app = create_app(minio_cfg, args.token, callback_url)
    uvicorn.run(app, host=args.host, port=args.port, log_level="info")


if __name__ == "__main__":
    main()
