package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.dto.SysRoleDTO;
import net.leoch.modules.sys.dto.SysRolePageRequest;
import net.leoch.modules.sys.entity.SysRoleEntity;

import java.util.List;


/**
 * 角色
 *
 * @author Taohongqiang
 */
public interface ISysRoleService extends IService<SysRoleEntity> {

	PageData<SysRoleDTO> page(SysRolePageRequest request);

	List<SysRoleDTO> list(SysRolePageRequest request);

	SysRoleDTO get(Long id);

	void save(SysRoleDTO dto);

	void update(SysRoleDTO dto);

	void delete(Long[] ids);

}
