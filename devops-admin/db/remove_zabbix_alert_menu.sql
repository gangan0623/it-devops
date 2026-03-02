-- Remove Zabbix alert menus and related role mappings.
-- Run in target environment DB after deploying code changes.

START TRANSACTION;

-- 1) collect zabbix menus that should be removed from DB routing
-- keep the feature, but route/menu is now fixed in frontend workbench
DROP TEMPORARY TABLE IF EXISTS tmp_zabbix_menu_ids;
CREATE TEMPORARY TABLE tmp_zabbix_menu_ids AS
SELECT id
FROM sys_menu
WHERE url IN ('alert/zabbix/event', 'alert/zabbix/report');

-- 2) include child button menus recursively (1-level child is enough for this menu model)
INSERT INTO tmp_zabbix_menu_ids (id)
SELECT m.id
FROM sys_menu m
JOIN tmp_zabbix_menu_ids t ON m.pid = t.id
WHERE NOT EXISTS (SELECT 1 FROM tmp_zabbix_menu_ids x WHERE x.id = m.id);

-- 3) remove role-menu relations first
DELETE FROM sys_role_menu
WHERE menu_id IN (SELECT id FROM tmp_zabbix_menu_ids);

-- 4) remove menus
DELETE FROM sys_menu
WHERE id IN (SELECT id FROM tmp_zabbix_menu_ids);

DROP TEMPORARY TABLE IF EXISTS tmp_zabbix_menu_ids;

COMMIT;
