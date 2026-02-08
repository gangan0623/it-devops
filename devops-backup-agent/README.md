# devops-backup-agent

网络设备备份代理。通过 SSH 拉取配置并上传到 MinIO，可选回调给后端。

## 系统架构（与后端协作）
- 入口：HTTP API（`/backup`）接收后端下发的设备清单
- 执行：按设备型号选择适配器，通过 SSH 拉取配置
- 存储：上传到对象存储（默认 MinIO，S3 协议）
- 回调：将结果（设备 + URL）回调到 `devops-admin` 的备份回调接口

与后端模块关系：
- `net.leoch.modules.ops`：设备/备份配置、备份记录、回调接收与状态更新
- `net.leoch.modules.oss`：对象存储配置与访问地址管理（可选由后端统一）

简化流程：
```
devops-admin -> /backup -> devops-backup-agent -> MinIO
devops-backup-agent --(callback)--> devops-admin
```

## 功能概览
- 设备配置备份（多厂商型号）
- 上传到 MinIO 并返回可访问 URL
- 支持回调通知结果

## 依赖
- Python 3.12
- pip

## 安装与运行（开发方式）
```bash
pip install -r requirements.txt
python -m backup_agent.main \
  --config-file config.yaml \
  --token <AGENT_TOKEN> \
  --callback-url <CALLBACK_URL> \
  --log-file agent.log \
  --host 0.0.0.0 \
  --port 8120
```

## 二进制构建
```bash
./build.sh
```
构建完成后产物：`dist/backup-agent`

## 配置文件示例
`config.yaml`
```yaml
minio:
  endpoint: https://<MINIO_ENDPOINT>
  access-key: <ACCESS_KEY>
  secret-key: <SECRET_KEY>
  bucket-name: devops
  backup-path: /switch
  public-url: https://<PUBLIC_URL>
```

## API
- 健康检查：`GET /healthz`
- 触发备份（异步）：`POST /backup`
  - Header：`agent-token: <AGENT_TOKEN>`
  - Body：设备列表（JSON 数组）
```json
[
  {
    "instance": "10.0.0.1",
    "name": "core-switch-01",
    "username": "admin",
    "password": "******",
    "model": "vrp"
  }
]
```

## 回调
若启动参数提供 `--callback-url`，完成后会以 POST 回调：
- Header：`agent-token: <AGENT_TOKEN>`
- Body：
```json
[
  {"instance":"10.0.0.1","url":"https://<PUBLIC_URL>/devops/switch/backup/2026-01-31/10.0.0.1_12-00-00.txt"}
]
```

## 支持的设备型号
- `vrp` / `huawei`
- `comware` / `h3c`
- `fortios` / `forti`
- `ruijie` / `rgos`

## systemd 示例
`/etc/systemd/system/backup-agent.service`
```ini
[Unit]
Description=Backup Agent
After=network.target

[Service]
Type=simple
WorkingDirectory=/data/backup-agent
ExecStart=/data/backup-agent/backup-agent --config-file /data/backup-agent/config.yaml --token <AGENT_TOKEN> --callback-url <CALLBACK_URL> --log-file /data/backup-agent/agent.log --host 0.0.0.0 --port 8120
Restart=always
RestartSec=3
LimitNOFILE=65535

[Install]
WantedBy=multi-user.target
```

启用：
```bash
systemctl daemon-reload
systemctl enable --now backup-agent
systemctl status --no-pager -l backup-agent
```
