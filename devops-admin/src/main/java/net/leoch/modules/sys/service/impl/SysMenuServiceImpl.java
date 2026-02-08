package net.leoch.modules.sys.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.TreeUtils;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.dao.SysMenuDao;
import net.leoch.modules.sys.dto.SysMenuDTO;
import net.leoch.modules.sys.entity.SysMenuEntity;
import net.leoch.modules.sys.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.SysMenuService;
import net.leoch.modules.sys.service.SysRoleMenuService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Slf4j
@Service
@AllArgsConstructor
public class SysMenuServiceImpl extends ServiceImpl<SysMenuDao, SysMenuEntity> implements SysMenuService {
    private final SysRoleMenuService sysRoleMenuService;

    @Override
    public SysMenuDTO get(Long id) {
        SysMenuEntity entity = this.getBaseMapper().getById(id);

        SysMenuDTO dto = ConvertUtils.sourceToTarget(entity, SysMenuDTO.class);

        return dto;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysMenuDTO dto) {
        SysMenuEntity entity = ConvertUtils.sourceToTarget(dto, SysMenuEntity.class);

        //保存菜单
        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysMenuDTO dto) {
        SysMenuEntity entity = ConvertUtils.sourceToTarget(dto, SysMenuEntity.class);

        //上级菜单不能为自身
        if (entity.getId().equals(entity.getPid())) {
            throw new ServiceException(ErrorCode.SUPERIOR_MENU_ERROR);
        }

        //更新菜单
        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long id) {
        //删除菜单
        this.removeById(id);

        //删除角色菜单关系
        sysRoleMenuService.deleteByMenuId(id);
    }

    @Override
    public List<SysMenuDTO> getAllMenuList(Integer menuType) {
        List<SysMenuEntity> menuList = this.getBaseMapper().getMenuList(menuType);

        List<SysMenuDTO> dtoList = ConvertUtils.sourceToTarget(menuList, SysMenuDTO.class);

        return TreeUtils.build(dtoList, Constant.MENU_ROOT);
    }

    @Override
    public List<SysMenuDTO> getUserMenuList(UserDetail user, Integer menuType) {
        List<SysMenuEntity> menuList;

        //系统管理员，拥有最高权限
        if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
            menuList = this.getBaseMapper().getMenuList(menuType);
        } else {
            menuList = this.getBaseMapper().getUserMenuList(user.getId(), menuType);
        }

        List<SysMenuDTO> dtoList = ConvertUtils.sourceToTarget(menuList, SysMenuDTO.class);

        return TreeUtils.build(dtoList);
    }

    @Override
    public List<SysMenuDTO> getListPid(Long pid) {
        List<SysMenuEntity> menuList = this.getBaseMapper().getListPid(pid);

        return ConvertUtils.sourceToTarget(menuList, SysMenuDTO.class);
    }

}
