package net.leoch.modules.sys.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.modules.sys.mapper.SysRoleDataScopeMapper;
import net.leoch.modules.sys.entity.SysRoleDataScopeEntity;
import net.leoch.modules.sys.service.ISysRoleDataScopeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 角色数据权限
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Slf4j
@Service
public class SysRoleDataScopeServiceImpl extends ServiceImpl<SysRoleDataScopeMapper, SysRoleDataScopeEntity>
        implements ISysRoleDataScopeService {

    @Override
    public List<Long> getDeptIdList(Long roleId) {
        return this.getBaseMapper().getDeptIdList(roleId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveOrUpdate(Long roleId, List<Long> deptIdList) {
        //先删除角色数据权限关系
        deleteByRoleIds(new Long[]{roleId});

        //角色没有一个数据权限的情况
        if(CollUtil.isEmpty(deptIdList)){
            return ;
        }

        //保存角色数据权限关系
        List<SysRoleDataScopeEntity> entityList = new ArrayList<>();
        for(Long deptId : deptIdList){
            SysRoleDataScopeEntity sysRoleDataScopeEntity = new SysRoleDataScopeEntity();
            sysRoleDataScopeEntity.setDeptId(deptId);
            sysRoleDataScopeEntity.setRoleId(roleId);
            entityList.add(sysRoleDataScopeEntity);
        }
        this.saveBatch(entityList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void deleteByRoleIds(Long[] roleIds) {
        this.getBaseMapper().deleteByRoleIds(roleIds);
    }
}
