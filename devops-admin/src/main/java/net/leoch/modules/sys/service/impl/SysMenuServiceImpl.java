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
import net.leoch.modules.sys.mapper.SysMenuMapper;
import net.leoch.modules.sys.vo.rsp.SysMenuRsp;
import net.leoch.modules.sys.entity.SysMenuEntity;
import net.leoch.modules.sys.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.ISysMenuService;
import net.leoch.modules.sys.service.ISysRoleMenuService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Slf4j
@Service
@AllArgsConstructor
public class SysMenuServiceImpl extends ServiceImpl<SysMenuMapper, SysMenuEntity> implements ISysMenuService {
    private final ISysRoleMenuService sysRoleMenuService;

    @Override
    public SysMenuRsp get(Long id) {
        SysMenuEntity entity = this.getBaseMapper().getById(id);

        SysMenuRsp dto = ConvertUtils.sourceToTarget(entity, SysMenuRsp.class);

        return dto;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysMenuRsp dto) {
        SysMenuEntity entity = ConvertUtils.sourceToTarget(dto, SysMenuEntity.class);

        //保存菜单
        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysMenuRsp dto) {
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

}
