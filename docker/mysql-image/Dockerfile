FROM mysql:8.0
ENV TZ=Asia/Shanghai

# Initialize schema/data on first container start.
COPY devops-admin/db/mysql.sql /docker-entrypoint-initdb.d/init.sql
COPY docker/mysql-image/my.cnf /etc/mysql/conf.d/timezone.cnf
