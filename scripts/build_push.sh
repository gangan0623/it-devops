#!/usr/bin/env bash
set -euo pipefail

# Usage: ./scripts/build_push.sh <version>
VERSION="${1:-}"
if [[ -z "${VERSION}" ]]; then
  echo "Usage: $0 <version>"
  exit 1
fi

REGISTRY_HOST="${REGISTRY_HOST:-crpi-3b3nup7gk3gkey6i.cn-hangzhou.personal.cr.aliyuncs.com}"
NAMESPACE="${NAMESPACE:-it-devops}"
PROJECT="${PROJECT:-it-devops}"

ADMIN_LOCAL_IMAGE="${PROJECT}-devops-admin"
NGINX_LOCAL_IMAGE="${PROJECT}-nginx"
MYSQL_LOCAL_IMAGE="${PROJECT}-mysql-init"

ADMIN_REMOTE_IMAGE="${REGISTRY_HOST}/${NAMESPACE}/it-devops-devops-admin:${VERSION}"
NGINX_REMOTE_IMAGE="${REGISTRY_HOST}/${NAMESPACE}/it-devops-nginx:${VERSION}"
MYSQL_REMOTE_IMAGE="${REGISTRY_HOST}/${NAMESPACE}/it-devops-mysql:${VERSION}"
REDIS_REMOTE_IMAGE="${REGISTRY_HOST}/${NAMESPACE}/it-devops-redis:7.4.7"

# Build app images via compose
DOCKER_BUILDKIT=1 docker compose build devops-admin nginx

# Build mysql image with init SQL (data will be initialized on first run)
DOCKER_BUILDKIT=1 docker build -f docker/mysql-image/Dockerfile -t "${MYSQL_LOCAL_IMAGE}" .

# Tag images for registry
docker tag "${ADMIN_LOCAL_IMAGE}" "${ADMIN_REMOTE_IMAGE}"
docker tag "${NGINX_LOCAL_IMAGE}" "${NGINX_REMOTE_IMAGE}"
docker tag "${MYSQL_LOCAL_IMAGE}" "${MYSQL_REMOTE_IMAGE}"
docker tag "redis:7.4.7" "${REDIS_REMOTE_IMAGE}"

# Push images
docker push "${ADMIN_REMOTE_IMAGE}"
docker push "${NGINX_REMOTE_IMAGE}"
docker push "${MYSQL_REMOTE_IMAGE}"
docker push "${REDIS_REMOTE_IMAGE}"

echo "Pushed:"
echo "  ${ADMIN_REMOTE_IMAGE}"
echo "  ${NGINX_REMOTE_IMAGE}"
echo "  ${MYSQL_REMOTE_IMAGE}"
echo "  ${REDIS_REMOTE_IMAGE}"
