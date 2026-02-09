package net.leoch.modules.sys.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.core.base.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.TreeUtils;
import net.leoch.common.validator.AssertUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.modules.security.service.ISecurityService;
import net.leoch.common.security.user.SecurityUser;
import net.leoch.common.security.user.UserDetail;
import net.leoch.modules.sys.mapper.SysMenuMapper;
import net.leoch.modules.sys.vo.req.SysMenuReq;
import net.leoch.modules.sys.vo.rsp.SysMenuRsp;
import net.leoch.modules.sys.entity.SysMenuEntity;
import net.leoch.common.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.ISysMenuService;
import net.leoch.modules.sys.service.ISysRoleMenuService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.Set;

@Slf4j
@Service
@RequiredArgsConstructor
public class SysMenuServiceImpl extends ServiceImpl<SysMenuMapper, SysMenuEntity> implements ISysMenuService {
    private final ISysRoleMenuService sysRoleMenuService;
    private final ISecurityService securityService;

    @Override
    public SysMenuRsp get(Long id) {
        SysMenuEntity entity = this.getBaseMapper().getById(id);

        SysMenuRsp dto = ConvertUtils.sourceToTarget(entity, SysMenuRsp.class);

        return dto;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysMenuReq dto) {
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);
        SysMenuEntity entity = ConvertUtils.sourceToTarget(dto, SysMenuEntity.class);

        //保存菜单
        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysMenuReq dto) {
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);
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
    public List<SysMenuRsp> getAllMenuList(Integer menuType) {
        List<SysMenuEntity> menuList = this.getBaseMapper().getMenuList(menuType);

        List<SysMenuRsp> dtoList = ConvertUtils.sourceToTarget(menuList, SysMenuRsp.class);

        return TreeUtils.build(dtoList, Constant.MENU_ROOT);
    }

    @Override
    public List<SysMenuRsp> getUserMenuList(UserDetail user, Integer menuType) {
        List<SysMenuEntity> menuList;

        //系统管理员，拥有最高权限
        if (user.getSuperAdmin() == SuperAdminEnum.YES.value()) {
            menuList = this.getBaseMapper().getMenuList(menuType);
        } else {
            menuList = this.getBaseMapper().getUserMenuList(user.getId(), menuType);
        }

        List<SysMenuRsp> dtoList = ConvertUtils.sourceToTarget(menuList, SysMenuRsp.class);

        return TreeUtils.build(dtoList);
    }

    @Override
    public List<SysMenuRsp> getListPid(Long pid) {
        List<SysMenuEntity> menuList = this.getBaseMapper().getListPid(pid);

        return ConvertUtils.sourceToTarget(menuList, SysMenuRsp.class);
    }

    @Override
    public List<SysMenuRsp> getCurrentUserMenuList(Integer menuType) {
        UserDetail user = SecurityUser.getUser();
        return getUserMenuList(user, menuType);
    }

    @Override
    public Set<String> getCurrentUserPermissions() {
        UserDetail user = SecurityUser.getUser();
        return securityService.getUserPermissions(user);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteWithChildCheck(Long id) {
        AssertUtils.isNull(id, "id");
        List<SysMenuRsp> children = this.getListPid(id);
        if (!children.isEmpty()) {
            throw new ServiceException(ErrorCode.SUB_MENU_EXIST);
        }
        this.delete(id);
    }

}
