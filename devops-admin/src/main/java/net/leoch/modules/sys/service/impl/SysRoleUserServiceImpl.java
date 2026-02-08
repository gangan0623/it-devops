package net.leoch.modules.sys.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.sys.mapper.SysRoleUserMapper;
import net.leoch.modules.sys.entity.SysRoleUserEntity;
import net.leoch.modules.sys.service.SysRoleUserService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 角色用户关系
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Slf4j
@Service
public class SysRoleUserServiceImpl extends ServiceImpl<SysRoleUserMapper, SysRoleUserEntity> implements SysRoleUserService {

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOrUpdate(Long userId, List<Long> roleIdList) {
        //先删除角色用户关系
        deleteByUserIds(new Long[]{userId});

        //用户没有一个角色权限的情况
        if(CollUtil.isEmpty(roleIdList)){
            return ;
        }

        //保存角色用户关系
        List<SysRoleUserEntity> entityList = new ArrayList<>();
        for(Long roleId : roleIdList){
            SysRoleUserEntity sysRoleUserEntity = new SysRoleUserEntity();
            sysRoleUserEntity.setUserId(userId);
            sysRoleUserEntity.setRoleId(roleId);
            entityList.add(sysRoleUserEntity);
        }
        this.saveBatch(entityList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteByRoleIds(Long[] roleIds) {
        this.getBaseMapper().deleteByRoleIds(roleIds);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteByUserIds(Long[] userIds) {
        this.getBaseMapper().deleteByUserIds(userIds);
    }

    @Override
    public List<Long> getRoleIdList(Long userId) {

        return this.getBaseMapper().getRoleIdList(userId);
    }
}
