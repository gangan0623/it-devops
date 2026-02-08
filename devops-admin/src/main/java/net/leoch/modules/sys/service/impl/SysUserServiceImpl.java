package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.security.password.PasswordUtils;
import net.leoch.modules.security.user.SecurityUser;
import net.leoch.modules.security.user.UserDetail;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.vo.req.PasswordReq;
import net.leoch.modules.sys.vo.rsp.SysUserRsp;
import net.leoch.modules.sys.vo.req.SysUserPageReq;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.ISysDeptService;
import net.leoch.modules.sys.service.ISysRoleUserService;
import net.leoch.modules.sys.service.ISysUserService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


/**
 * 系统用户
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
@AllArgsConstructor
public class SysUserServiceImpl extends ServiceImpl<SysUserMapper, SysUserEntity> implements ISysUserService {
    private final ISysRoleUserService sysRoleUserService;
    private final ISysDeptService sysDeptService;

    @Override
    public PageData<SysUserRsp> page(SysUserPageReq request) {
        Map<String, Object> params = new HashMap<>();

        //转换成like
        if (StrUtil.isNotBlank(request.getUsername())) {
            params.put("username", "%" + request.getUsername() + "%");
        }
        if (StrUtil.isNotBlank(request.getDeptId())) {
            params.put("deptId", request.getDeptId());
        }

        //分页
        IPage<SysUserEntity> page = request.buildPage();
        params.put("page", page);

        //普通管理员，只能查询所属部门及子部门的数据
        UserDetail user = SecurityUser.getUser();
        if (user.getSuperAdmin() == SuperAdminEnum.NO.value()) {
            params.put("deptIdList", sysDeptService.getSubDeptIdList(user.getDeptId()));
        }

        //查询
        List<SysUserEntity> list = this.getBaseMapper().getList(params);

        return new PageData<>(ConvertUtils.sourceToTarget(list, SysUserRsp.class), page.getTotal());
    }

    @Override
    public List<SysUserRsp> list(SysUserPageReq request) {
        Map<String, Object> params = new HashMap<>();

        if (StrUtil.isNotBlank(request.getUsername())) {
            params.put("username", "%" + request.getUsername() + "%");
        }
        if (StrUtil.isNotBlank(request.getDeptId())) {
            params.put("deptId", request.getDeptId());
        }

        //普通管理员，只能查询所属部门及子部门的数据
        UserDetail user = SecurityUser.getUser();
        if (user.getSuperAdmin() == SuperAdminEnum.NO.value()) {
            params.put("deptIdList", sysDeptService.getSubDeptIdList(user.getDeptId()));
        }

        List<SysUserEntity> entityList = this.getBaseMapper().getList(params);

        return ConvertUtils.sourceToTarget(entityList, SysUserRsp.class);
    }

    @Override
    public SysUserRsp get(Long id) {
        SysUserEntity entity = this.getBaseMapper().getById(id);

        return ConvertUtils.sourceToTarget(entity, SysUserRsp.class);
    }

    @Override
    public SysUserRsp getWithRoles(Long id) {
        SysUserRsp user = this.get(id);
        if (user != null) {
            //用户角色列表
            List<Long> roleIdList = sysRoleUserService.getRoleIdList(id);
            user.setRoleIdList(roleIdList);
        }
        return user;
    }

    @Override
    public SysUserRsp getByUsername(String username) {
        SysUserEntity entity = this.getBaseMapper().getByUsername(username);
        return ConvertUtils.sourceToTarget(entity, SysUserRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysUserRsp dto) {
        SysUserEntity entity = ConvertUtils.sourceToTarget(dto, SysUserEntity.class);

        //密码加密
        String password = PasswordUtils.encode(entity.getPassword());
        entity.setPassword(password);

        //保存用户
        entity.setSuperAdmin(SuperAdminEnum.NO.value());
        this.save(entity);

        //保存角色用户关系
        sysRoleUserService.saveOrUpdate(entity.getId(), dto.getRoleIdList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysUserRsp dto) {
        SysUserEntity entity = ConvertUtils.sourceToTarget(dto, SysUserEntity.class);

        //密码加密
        if (StrUtil.isBlank(dto.getPassword())) {
            entity.setPassword(null);
        } else {
            String password = PasswordUtils.encode(entity.getPassword());
            entity.setPassword(password);
        }

        //更新用户
        this.updateById(entity);

        //更新角色用户关系
        sysRoleUserService.saveOrUpdate(entity.getId(), dto.getRoleIdList());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        //删除用户
        this.removeByIds(Arrays.asList(ids));

        //删除角色用户关系
        sysRoleUserService.deleteByUserIds(ids);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updatePassword(Long id, String newPassword) {
        newPassword = PasswordUtils.encode(newPassword);

        this.getBaseMapper().updatePassword(id, newPassword);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void changePassword(PasswordReq dto) {
        log.info("[用户密码] 修改密码请求");

        UserDetail user = SecurityUser.getUser();

        //原密码不正确
        if (!PasswordUtils.matches(dto.getPassword(), user.getPassword())) {
            log.warn("[用户密码] 原密码错误, userId={}", user.getId());
            throw new ServiceException(ErrorCode.PASSWORD_ERROR);
        }

        this.updatePassword(user.getId(), dto.getNewPassword());
        log.info("[用户密码] 修改密码成功, userId={}", user.getId());
    }

    @Override
    public int getCountByDeptId(Long deptId) {
        return this.getBaseMapper().getCountByDeptId(deptId);
    }

    @Override
    public List<Long> getUserIdListByDeptId(List<Long> deptIdList) {
        return this.getBaseMapper().getUserIdListByDeptId(deptIdList);
    }

}
