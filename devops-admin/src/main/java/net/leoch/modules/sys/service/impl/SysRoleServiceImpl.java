package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.support.utils.ConvertUtils;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.security.user.SecurityUser;
import net.leoch.common.security.user.UserDetail;
import net.leoch.modules.sys.mapper.SysRoleMapper;
import net.leoch.modules.sys.vo.req.SysRolePageReq;
import net.leoch.modules.sys.vo.req.SysRoleReq;
import net.leoch.modules.sys.vo.rsp.SysRoleRsp;
import net.leoch.modules.sys.entity.SysRoleEntity;
import net.leoch.common.core.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.ISysDeptService;
import net.leoch.modules.sys.service.ISysRoleDataScopeService;
import net.leoch.modules.sys.service.ISysRoleMenuService;
import net.leoch.modules.sys.service.ISysRoleService;
import net.leoch.modules.sys.service.ISysRoleUserService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * 角色
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SysRoleServiceImpl extends ServiceImpl<SysRoleMapper, SysRoleEntity> implements ISysRoleService {
    private final ISysRoleMenuService sysRoleMenuService;
    private final ISysRoleDataScopeService sysRoleDataScopeService;
    private final ISysRoleUserService sysRoleUserService;
    private final ISysDeptService sysDeptService;

    @Override
    public PageData<SysRoleRsp> page(SysRolePageReq request) {
        IPage<SysRoleEntity> page = this.page(
                request.buildPage(),
                getWrapper(request)
        );

        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysRoleRsp.class), page.getTotal());
    }

    @Override
    public List<SysRoleRsp> list(SysRolePageReq request) {
        List<SysRoleEntity> entityList = this.list(getWrapper(request));

        return ConvertUtils.sourceToTarget(entityList, SysRoleRsp.class);
    }

    private QueryWrapper<SysRoleEntity> getWrapper(SysRolePageReq request) {
        String name = request.getName();

        QueryWrapper<SysRoleEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(name), "name", name);

        //普通管理员，只能查询所属部门及子部门的数据
        UserDetail user = SecurityUser.getUser();
        if (user.getSuperAdmin() == SuperAdminEnum.NO.value()) {
            List<Long> deptIdList = sysDeptService.getSubDeptIdList(user.getDeptId());
            wrapper.in(deptIdList != null, "dept_id", deptIdList);
        }

        return wrapper;
    }

    @Override
    public SysRoleRsp get(Long id) {
        SysRoleEntity entity = this.getById(id);

        return ConvertUtils.sourceToTarget(entity, SysRoleRsp.class);
    }

    @Override
    public SysRoleRsp getWithMenuAndDataScope(Long id) {
        SysRoleRsp role = this.get(id);
        if (role != null) {
            role.setMenuIdList(sysRoleMenuService.getMenuIdList(id));
            role.setDeptIdList(sysRoleDataScopeService.getDeptIdList(id));
        }
        return role;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysRoleReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        SysRoleEntity entity = ConvertUtils.sourceToTarget(dto, SysRoleEntity.class);

        //保存角色
        this.save(entity);

        //保存角色菜单关系
        sysRoleMenuService.saveOrUpdate(entity.getId(), dto.getMenuIdList());

        //保存角色数据权限关系
        sysRoleDataScopeService.saveOrUpdate(entity.getId(), dto.getDeptIdList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysRoleReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysRoleEntity entity = ConvertUtils.sourceToTarget(dto, SysRoleEntity.class);

        //更新角色
        this.updateById(entity);

        //更新角色菜单关系
        sysRoleMenuService.saveOrUpdate(entity.getId(), dto.getMenuIdList());

        //更新角色数据权限关系
        sysRoleDataScopeService.saveOrUpdate(entity.getId(), dto.getDeptIdList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        //删除角色
        this.removeByIds(Arrays.asList(ids));

        //删除角色用户关系
        sysRoleUserService.deleteByRoleIds(ids);

        //删除角色菜单关系
        sysRoleMenuService.deleteByRoleIds(ids);

        //删除角色数据权限关系
        sysRoleDataScopeService.deleteByRoleIds(ids);
    }

}
