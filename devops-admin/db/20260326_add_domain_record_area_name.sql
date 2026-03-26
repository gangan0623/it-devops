USE `devops`;

START TRANSACTION;

ALTER TABLE `tb_domain_record`
  ADD COLUMN `area_name` varchar(50) DEFAULT NULL COMMENT '区域名称' AFTER `domain_name`;

ALTER TABLE `tb_domain_record`
  ADD KEY `idx_area_name` (`area_name`);

COMMIT;
