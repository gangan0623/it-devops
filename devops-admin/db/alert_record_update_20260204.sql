ALTER TABLE `tb_alert_record`
    ADD COLUMN IF NOT EXISTS `closed` tinyint DEFAULT 0 COMMENT '是否关闭 0否 1是' AFTER `raw_json`,
    ADD COLUMN IF NOT EXISTS `suppressed_until` datetime DEFAULT NULL COMMENT '抑制截止时间' AFTER `closed`;

CREATE TABLE IF NOT EXISTS `tb_alert_record_action` (
  `id` bigint NOT NULL,
  `record_id` bigint NOT NULL COMMENT '告警记录ID',
  `action` varchar(100) DEFAULT NULL COMMENT '操作动作',
  `message` varchar(500) DEFAULT NULL COMMENT '消息',
  `details` text COMMENT '附加详情',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_record_id` (`record_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警记录操作历史';

CREATE TABLE IF NOT EXISTS `tb_alert_notify_log` (
  `id` bigint NOT NULL,
  `record_id` bigint DEFAULT NULL COMMENT '告警记录ID',
  `alert_name` varchar(200) DEFAULT NULL COMMENT '告警名',
  `instance` varchar(200) DEFAULT NULL COMMENT '实例',
  `severity` varchar(50) DEFAULT NULL COMMENT '严重性',
  `media_name` varchar(100) DEFAULT NULL COMMENT '媒介名称',
  `receivers` varchar(1000) DEFAULT NULL COMMENT '接收人',
  `send_status` tinyint DEFAULT NULL COMMENT '发送状态 1成功 0失败',
  `error_message` varchar(500) DEFAULT NULL COMMENT '失败原因',
  `send_time` datetime DEFAULT NULL COMMENT '发送时间',
  PRIMARY KEY (`id`),
  KEY `idx_record_send_time` (`record_id`,`send_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警发送日志';
