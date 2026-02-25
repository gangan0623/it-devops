package net.leoch.modules.sys.service.impl;

import cn.dev33.satoken.stp.StpUtil;
import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.enums.SuperAdminEnum;
import net.leoch.common.enums.UserStatusEnum;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.integration.security.UserDetail;
import net.leoch.common.utils.security.PasswordUtils;
import net.leoch.modules.sys.entity.SysUserEntity;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.service.ISysRoleUserService;
import net.leoch.modules.sys.service.ISysUserService;
import net.leoch.modules.sys.vo.req.PasswordReq;
import net.leoch.modules.sys.vo.req.SysUserPageReq;
import net.leoch.modules.sys.vo.req.SysUserReq;
import net.leoch.modules.sys.vo.rsp.SysUserRsp;
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
@RequiredArgsConstructor
public class SysUserServiceImpl extends ServiceImpl<SysUserMapper, SysUserEntity> implements ISysUserService {
    private final ISysRoleUserService sysRoleUserService;

    @Override
    public PageData<SysUserRsp> page(SysUserPageReq request) {
        Map<String, Object> params = new HashMap<>();

        //转换成like
        if (StrUtil.isNotBlank(request.getUsername())) {
            params.put("username", "%" + request.getUsername() + "%");
        }
        //分页
        IPage<SysUserEntity> page = request.buildPage();
        params.put("page", page);

        //查询
        List<SysUserEntity> list = this.getBaseMapper().getList(params);

        return new PageData<>(BeanUtil.copyToList(list, SysUserRsp.class), page.getTotal());
    }

    @Override
    public List<SysUserRsp> list(SysUserPageReq request) {
        Map<String, Object> params = new HashMap<>();

        if (StrUtil.isNotBlank(request.getUsername())) {
            params.put("username", "%" + request.getUsername() + "%");
        }
        List<SysUserEntity> entityList = this.getBaseMapper().getList(params);

        return BeanUtil.copyToList(entityList, SysUserRsp.class);
    }

    @Override
    public SysUserRsp get(Long id) {
        SysUserEntity entity = this.getBaseMapper().getById(id);

        return BeanUtil.copyProperties(entity, SysUserRsp.class);
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
        return BeanUtil.copyProperties(entity, SysUserRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysUserReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        SysUserEntity entity = BeanUtil.copyProperties(dto, SysUserEntity.class);

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
    public void update(SysUserReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysUserEntity entity = BeanUtil.copyProperties(dto, SysUserEntity.class);

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

        //用户被禁用时，强制下线
        if (dto.getStatus() != null && dto.getStatus() == UserStatusEnum.DISABLE.value()) {
            kickout(new Long[]{entity.getId()});
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");

        //强制下线被删除的用户
        kickout(ids);

        //删除用户
        this.removeByIds(Arrays.asList(ids));

        //删除角色用户关系
        sysRoleUserService.deleteByUserIds(ids);
    }

    @Override
    public void kickout(Long[] ids) {
        if (ids == null || ids.length == 0) {
            return;
        }
        for (Long userId : ids) {
            try {
                StpUtil.kickout(userId);
                log.info("[用户管理] 强制下线, userId={}", userId);
            } catch (Exception e) {
                log.warn("[用户管理] 强制下线失败, userId={}", userId, e);
            }
        }
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
    public SysUserRsp getCurrentUserInfo() {
        return BeanUtil.copyProperties(SecurityUser.getUser(), SysUserRsp.class);
    }

}
