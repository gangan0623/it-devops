USE `devops`;

START TRANSACTION;

-- 域名记录主表
CREATE TABLE IF NOT EXISTS `tb_domain_record` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `project_name` varchar(100) NOT NULL COMMENT '项目名称',
  `domain_name` varchar(255) NOT NULL COMMENT '域名',
  `area_name` varchar(50) DEFAULT NULL COMMENT '区域名称',
  `group_name` varchar(50) DEFAULT NULL COMMENT '分组名称',
  `ad_enabled` tinyint NOT NULL DEFAULT 0 COMMENT '是否走应用交付 0否 1是',
  `internal_enabled` tinyint NOT NULL DEFAULT 0 COMMENT '是否启用内网解析 0否 1是',
  `external_enabled` tinyint NOT NULL DEFAULT 0 COMMENT '是否启用外网解析 0否 1是',
  `description` varchar(500) DEFAULT NULL COMMENT '描述',
  `project_owner` varchar(100) NOT NULL COMMENT '项目负责人',
  `apply_time` datetime NOT NULL COMMENT '申请时间',
  `remark` varchar(500) DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_domain_name` (`domain_name`),
  KEY `idx_area_name` (`area_name`),
  KEY `idx_group_name` (`group_name`),
  KEY `idx_project_name` (`project_name`),
  KEY `idx_project_owner` (`project_owner`),
  KEY `idx_apply_time` (`apply_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名记录主表';

-- 应用交付表
CREATE TABLE IF NOT EXISTS `tb_domain_delivery` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_record_id` bigint NOT NULL COMMENT '域名记录ID',
  `virtual_service_name` varchar(200) DEFAULT NULL COMMENT '虚拟服务名称',
  `virtual_service_ip` varchar(64) DEFAULT NULL COMMENT '虚拟服务IP',
  `virtual_service_port` int DEFAULT NULL COMMENT '虚拟服务端口',
  `virtual_service_protocol` varchar(20) DEFAULT NULL COMMENT '虚拟服务协议',
  `pool_name` varchar(200) DEFAULT NULL COMMENT '节点池名称',
  `remark` varchar(500) DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_domain_record_id` (`domain_record_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名应用交付表';

-- 节点池明细表
CREATE TABLE IF NOT EXISTS `tb_domain_delivery_node` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_delivery_id` bigint NOT NULL COMMENT '应用交付ID',
  `node_ip` varchar(64) NOT NULL COMMENT '节点IP',
  `node_port` int NOT NULL COMMENT '节点端口',
  `sort` int DEFAULT NULL COMMENT '排序',
  `remark` varchar(500) DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  KEY `idx_domain_delivery_id` (`domain_delivery_id`),
  KEY `idx_sort` (`sort`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名应用交付节点明细表';

-- 内网解析表
CREATE TABLE IF NOT EXISTS `tb_domain_dns_internal` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_record_id` bigint NOT NULL COMMENT '域名记录ID',
  `resolve_mode` varchar(20) DEFAULT NULL COMMENT '解析方式',
  `dns_target_ip` varchar(64) DEFAULT NULL COMMENT '解析目标IP',
  `remark` varchar(500) DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_domain_record_id` (`domain_record_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名内网解析表';

-- 外网解析表
CREATE TABLE IF NOT EXISTS `tb_domain_dns_external` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_record_id` bigint NOT NULL COMMENT '域名记录ID',
  `resolve_mode` varchar(20) DEFAULT NULL COMMENT '解析方式',
  `record_value` varchar(255) DEFAULT NULL COMMENT 'DNS记录值',
  `remark` varchar(500) DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_domain_record_id` (`domain_record_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名外网解析表';

-- 防火墙映射表
CREATE TABLE IF NOT EXISTS `tb_domain_firewall_mapping` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_record_id` bigint NOT NULL COMMENT '域名记录ID',
  `public_ip` varchar(64) DEFAULT NULL COMMENT '公网IP',
  `external_port` int DEFAULT NULL COMMENT '外部端口',
  `internal_ip` varchar(64) DEFAULT NULL COMMENT '内部IP',
  `internal_port` int DEFAULT NULL COMMENT '内部端口',
  `mapping_desc` varchar(500) DEFAULT NULL COMMENT '映射描述',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `uk_domain_record_id` (`domain_record_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名防火墙映射表';

-- 操作历史主表
CREATE TABLE IF NOT EXISTS `tb_domain_record_history` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `domain_record_id` bigint NOT NULL COMMENT '域名记录ID',
  `operation_type` varchar(20) NOT NULL COMMENT '操作类型',
  `operator_id` bigint DEFAULT NULL COMMENT '操作人ID',
  `operator_name` varchar(100) DEFAULT NULL COMMENT '操作人名称',
  `operation_time` datetime NOT NULL COMMENT '操作时间',
  `operation_summary` varchar(1000) NOT NULL COMMENT '操作摘要',
  `snapshot_before` longtext COMMENT '修改前快照',
  `snapshot_after` longtext COMMENT '修改后快照',
  PRIMARY KEY (`id`),
  KEY `idx_domain_record_id` (`domain_record_id`),
  KEY `idx_operation_time` (`operation_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名记录操作历史表';

-- 操作历史差异明细表
CREATE TABLE IF NOT EXISTS `tb_domain_record_history_detail` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `history_id` bigint NOT NULL COMMENT '历史记录ID',
  `field_code` varchar(100) NOT NULL COMMENT '字段编码',
  `field_name` varchar(100) NOT NULL COMMENT '字段名称',
  `before_value` longtext COMMENT '修改前值',
  `after_value` longtext COMMENT '修改后值',
  PRIMARY KEY (`id`),
  KEY `idx_history_id` (`history_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='域名记录历史差异明细表';

DELETE FROM `sys_menu`
WHERE `id` IN (
  2026032600010000001,
  2026032600010000002,
  2026032600010000003,
  2026032600010000004,
  2026032600010000005,
  2026032600010000006,
  2026032600010000007,
  2026032600010000008,
  2026032600010000009
);

INSERT INTO `sys_menu` VALUES
  (2026032600010000001, 0, '域名管理', NULL, NULL, 0, 'icon-file-text', 5, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000002, 2026032600010000001, '域名记录管理', 'ops/domain-record', NULL, 0, 'icon-fileprotect', 0, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000003, 2026032600010000002, '查看', NULL, 'ops:domain-record:page,ops:domain-record:info', 1, NULL, 0, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000004, 2026032600010000002, '新增', NULL, 'ops:domain-record:save', 1, NULL, 1, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000005, 2026032600010000002, '修改', NULL, 'ops:domain-record:update', 1, NULL, 2, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000006, 2026032600010000002, '删除', NULL, 'ops:domain-record:delete', 1, NULL, 3, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000007, 2026032600010000001, '域名操作记录', 'ops/domain-record-history', NULL, 0, 'icon-filedone', 1, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000008, 2026032600010000007, '查看', NULL, 'ops:domain-record:history', 1, NULL, 0, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00'),
  (2026032600010000009, 2026032600010000002, '操作记录', NULL, 'ops:domain-record:history', 1, NULL, 4, 1067246875800000001, '2026-03-26 09:00:00', 1067246875800000001, '2026-03-26 09:00:00');

COMMIT;
