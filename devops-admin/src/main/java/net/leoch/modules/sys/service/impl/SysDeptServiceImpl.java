package net.leoch.modules.sys.service.impl;

import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.qiniu.util.StringUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.constant.Constant;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.TreeUtils;
import net.leoch.common.validator.AssertUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.common.security.user.SecurityUser;
import net.leoch.common.security.user.UserDetail;
import net.leoch.modules.sys.mapper.SysDeptMapper;
import net.leoch.modules.sys.mapper.SysUserMapper;
import net.leoch.modules.sys.vo.req.SysDeptReq;
import net.leoch.modules.sys.vo.rsp.SysDeptRsp;
import net.leoch.modules.sys.entity.SysDeptEntity;
import net.leoch.common.enums.SuperAdminEnum;
import net.leoch.modules.sys.service.ISysDeptService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


@Slf4j
@Service
@RequiredArgsConstructor
public class SysDeptServiceImpl extends ServiceImpl<SysDeptMapper, SysDeptEntity> implements ISysDeptService {
    private final SysUserMapper sysUserMapper;

    @Override
    public List<SysDeptRsp> list(Map<String, Object> params) {
        //普通管理员，只能查询所属部门及子部门的数据
        UserDetail user = SecurityUser.getUser();
        if (user.getSuperAdmin() == SuperAdminEnum.NO.value()) {
            params.put("deptIdList", getSubDeptIdList(user.getDeptId()));
        }

        //查询部门列表
        List<SysDeptEntity> entityList = this.getBaseMapper().getList(params);

        List<SysDeptRsp> dtoList = ConvertUtils.sourceToTarget(entityList, SysDeptRsp.class);

        return TreeUtils.build(dtoList);
    }

    @Override
    public SysDeptRsp get(Long id) {
        //超级管理员，部门ID为null
        if (id == null) {
            return null;
        }

        SysDeptEntity entity = this.getBaseMapper().getById(id);

        return ConvertUtils.sourceToTarget(entity, SysDeptRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysDeptReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        SysDeptEntity entity = ConvertUtils.sourceToTarget(dto, SysDeptEntity.class);

        entity.setPids(getPidList(entity.getPid()));
        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysDeptReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysDeptEntity entity = ConvertUtils.sourceToTarget(dto, SysDeptEntity.class);

        //上级部门不能为自身
        if (entity.getId().equals(entity.getPid())) {
            throw new ServiceException(ErrorCode.SUPERIOR_DEPT_ERROR);
        }

        //上级部门不能为下级部门
        List<Long> subDeptList = getSubDeptIdList(entity.getId());
        if (subDeptList.contains(entity.getPid())) {
            throw new ServiceException(ErrorCode.SUPERIOR_DEPT_ERROR);
        }

        entity.setPids(getPidList(entity.getPid()));
        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long id) {
        AssertUtils.isNull(id, "id");
        //判断是否有子部门
        List<Long> subList = getSubDeptIdList(id);
        if (subList.size() > 1) {
            throw new ServiceException(ErrorCode.DEPT_SUB_DELETE_ERROR);
        }

        //判断部门下面是否有用户
        int count = sysUserMapper.getCountByDeptId(id);
        if (count > 0) {
            throw new ServiceException(ErrorCode.DEPT_USER_DELETE_ERROR);
        }

        //删除
        this.removeById(id);
    }

    @Override
    public List<Long> getSubDeptIdList(Long id) {
        List<Long> deptIdList = this.getBaseMapper().getSubDeptIdList("%" + id + "%");
        deptIdList.add(id);

        return deptIdList;
    }

    /**
     * 获取所有上级部门ID
     *
     * @param pid 上级ID
     */
    private String getPidList(Long pid) {
        //顶级部门，无上级部门
        if (Constant.DEPT_ROOT.equals(pid)) {
            return Constant.DEPT_ROOT + "";
        }

        //所有部门的id、pid列表
        List<SysDeptEntity> deptList = this.getBaseMapper().getIdAndPidList();

        //list转map
        Map<Long, SysDeptEntity> map = new HashMap<>(deptList.size());
        for (SysDeptEntity entity : deptList) {
            map.put(entity.getId(), entity);
        }

        //递归查询所有上级部门ID列表
        List<Long> pidList = new ArrayList<>();
        getPidTree(pid, map, pidList);

        return StringUtils.join(pidList, ",");
    }

    private void getPidTree(Long pid, Map<Long, SysDeptEntity> map, List<Long> pidList) {
        //顶级部门，无上级部门
        if (Constant.DEPT_ROOT.equals(pid)) {
            return;
        }

        //上级部门存在
        SysDeptEntity parent = map.get(pid);
        if (parent != null) {
            getPidTree(parent.getPid(), map, pidList);
        }

        pidList.add(pid);
    }
}
