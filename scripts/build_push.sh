#!/usr/bin/env bash
set -euo pipefail

# Usage: ./scripts/build_push.sh <version>
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"
cd "${REPO_ROOT}"
VERSION="${1:-}"
if [[ -z "${VERSION}" ]]; then
  echo "Usage: $0 <version>"
  exit 1
fi

DOCKERHUB_NAMESPACE="${DOCKERHUB_NAMESPACE:-taohongqiang}"
PROJECT="${PROJECT:-it-devops}"

ADMIN_LOCAL_IMAGE="${PROJECT}-devops-admin"
NGINX_LOCAL_IMAGE="${PROJECT}-nginx"
MYSQL_LOCAL_IMAGE="${PROJECT}-mysql-init"

ADMIN_REMOTE_IMAGE="${DOCKERHUB_NAMESPACE}/it-devops-admin:${VERSION}"
NGINX_REMOTE_IMAGE="${DOCKERHUB_NAMESPACE}/it-devops-ui:${VERSION}"
MYSQL_REMOTE_IMAGE="${DOCKERHUB_NAMESPACE}/it-devops-mysql:${VERSION}"
REDIS_REMOTE_IMAGE="${DOCKERHUB_NAMESPACE}/it-devops-redis:${VERSION}"

# Build app images via compose
DOCKER_BUILDKIT=1 docker compose build devops-admin nginx

# Build mysql image with init SQL (data will be initialized on first run)
DOCKER_BUILDKIT=1 docker build -f docker/mysql-image/Dockerfile -t "${MYSQL_LOCAL_IMAGE}" .

# Tag images for registry (version + latest)
ADMIN_REMOTE_LATEST="${DOCKERHUB_NAMESPACE}/it-devops-admin:latest"
NGINX_REMOTE_LATEST="${DOCKERHUB_NAMESPACE}/it-devops-ui:latest"
MYSQL_REMOTE_LATEST="${DOCKERHUB_NAMESPACE}/it-devops-mysql:latest"
REDIS_REMOTE_LATEST="${DOCKERHUB_NAMESPACE}/it-devops-redis:latest"

docker tag "${ADMIN_LOCAL_IMAGE}" "${ADMIN_REMOTE_IMAGE}"
docker tag "${ADMIN_LOCAL_IMAGE}" "${ADMIN_REMOTE_LATEST}"
docker tag "${NGINX_LOCAL_IMAGE}" "${NGINX_REMOTE_IMAGE}"
docker tag "${NGINX_LOCAL_IMAGE}" "${NGINX_REMOTE_LATEST}"
docker tag "${MYSQL_LOCAL_IMAGE}" "${MYSQL_REMOTE_IMAGE}"
docker tag "${MYSQL_LOCAL_IMAGE}" "${MYSQL_REMOTE_LATEST}"
docker tag "redis:7.4.7" "${REDIS_REMOTE_IMAGE}"
docker tag "redis:7.4.7" "${REDIS_REMOTE_LATEST}"

# Push images
docker push "${ADMIN_REMOTE_IMAGE}"
docker push "${ADMIN_REMOTE_LATEST}"
docker push "${NGINX_REMOTE_IMAGE}"
docker push "${NGINX_REMOTE_LATEST}"
docker push "${MYSQL_REMOTE_IMAGE}"
docker push "${MYSQL_REMOTE_LATEST}"
docker push "${REDIS_REMOTE_IMAGE}"
docker push "${REDIS_REMOTE_LATEST}"

echo "Pushed:"
echo "  ${ADMIN_REMOTE_IMAGE}"
echo "  ${ADMIN_REMOTE_LATEST}"
echo "  ${NGINX_REMOTE_IMAGE}"
echo "  ${NGINX_REMOTE_LATEST}"
echo "  ${MYSQL_REMOTE_IMAGE}"
echo "  ${MYSQL_REMOTE_LATEST}"
echo "  ${REDIS_REMOTE_IMAGE}"
echo "  ${REDIS_REMOTE_LATEST}"
