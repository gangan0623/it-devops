INSERT INTO `sys_menu` (`id`, `pid`, `name`, `url`, `permissions`, `menu_type`, `icon`, `sort`, `creator`, `create_date`, `updater`, `update_date`)
SELECT 1900000000000000105, 0, '问题', 'alert/problem', NULL, 0, 'icon-notification', 2, 1067246875800000001, NOW(), 1067246875800000001, NOW()
WHERE NOT EXISTS (SELECT 1 FROM `sys_menu` WHERE `id` = 1900000000000000105);

INSERT INTO `sys_menu` (`id`, `pid`, `name`, `url`, `permissions`, `menu_type`, `icon`, `sort`, `creator`, `create_date`, `updater`, `update_date`)
SELECT 1900000000000000601, 1900000000000000105, '查看', NULL, 'alert:problem:page,alert:record:info', 1, NULL, 0, 1067246875800000001, NOW(), 1067246875800000001, NOW()
WHERE NOT EXISTS (SELECT 1 FROM `sys_menu` WHERE `id` = 1900000000000000601);

INSERT INTO `sys_role_menu` (`id`, `role_id`, `menu_id`, `creator`, `create_date`)
SELECT 2900000000000000001, 2016354427531419649, 1900000000000000105, 1067246875800000001, NOW()
WHERE NOT EXISTS (
    SELECT 1 FROM `sys_role_menu` WHERE `role_id` = 2016354427531419649 AND `menu_id` = 1900000000000000105
);

INSERT INTO `sys_role_menu` (`id`, `role_id`, `menu_id`, `creator`, `create_date`)
SELECT 2900000000000000002, 2016354427531419649, 1900000000000000601, 1067246875800000001, NOW()
WHERE NOT EXISTS (
    SELECT 1 FROM `sys_role_menu` WHERE `role_id` = 2016354427531419649 AND `menu_id` = 1900000000000000601
);
