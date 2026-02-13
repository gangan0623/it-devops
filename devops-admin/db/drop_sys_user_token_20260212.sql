-- 清理 Shiro + OAuth2 遗留表
-- 迁移到 sa-token 后，sys_user_token 表不再使用
-- sa-token 通过 Redis 管理 session，无需数据库表

DROP TABLE IF EXISTS `sys_user_token`;
