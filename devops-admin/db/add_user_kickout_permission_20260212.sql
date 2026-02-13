-- 添加用户强制下线权限
-- 需要在 sys_menu 表中添加按钮权限

-- 查找用户管理菜单的ID（假设用户管理菜单已存在）
-- 请根据实际情况调整 pid（父菜单ID）

INSERT INTO `sys_menu` (`id`, `pid`, `name`, `url`, `permissions`, `menu_type`, `icon`, `sort`, `creator`, `create_date`, `updater`, `update_date`)
SELECT
    (SELECT IFNULL(MAX(id), 0) + 1 FROM sys_menu sm),
    (SELECT id FROM sys_menu WHERE permissions = 'sys:user:page' LIMIT 1),
    '强制下线',
    NULL,
    'sys:user:kickout',
    1,
    NULL,
    6,
    1067246875800000001,
    NOW(),
    1067246875800000001,
    NOW()
FROM DUAL
WHERE NOT EXISTS (SELECT 1 FROM sys_menu WHERE permissions = 'sys:user:kickout');
