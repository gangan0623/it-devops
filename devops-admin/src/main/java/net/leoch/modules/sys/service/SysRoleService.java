

package net.leoch.modules.sys.service;


import net.leoch.common.page.PageData;
import net.leoch.common.service.BaseService;
import net.leoch.modules.sys.dto.SysRoleDTO;
import net.leoch.modules.sys.entity.SysRoleEntity;

import java.util.List;
import java.util.Map;


/**
 * 角色
 * 
 * @author Taohongqiang
 */
public interface SysRoleService extends BaseService<SysRoleEntity> {

	PageData<SysRoleDTO> page(Map<String, Object> params);

	List<SysRoleDTO> list(Map<String, Object> params);

	SysRoleDTO get(Long id);

	void save(SysRoleDTO dto);

	void update(SysRoleDTO dto);

	void delete(Long[] ids);

}
