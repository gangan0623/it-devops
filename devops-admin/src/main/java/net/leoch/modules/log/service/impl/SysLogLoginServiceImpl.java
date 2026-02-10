package net.leoch.modules.log.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.convert.ConvertUtils;
import net.leoch.modules.log.mapper.SysLogLoginMapper;
import net.leoch.modules.log.vo.rsp.SysLogLoginRsp;
import net.leoch.modules.log.vo.req.SysLogLoginPageReq;
import net.leoch.modules.log.entity.SysLogLoginEntity;
import net.leoch.modules.log.service.ISysLogLoginService;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogLoginServiceImpl extends ServiceImpl<SysLogLoginMapper, SysLogLoginEntity> implements ISysLogLoginService {

    @Override
    public PageData<SysLogLoginRsp> page(SysLogLoginPageReq request) {
        IPage<SysLogLoginEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogLoginEntity>()
                .eq(StrUtil.isNotBlank(request.getStatus()), SysLogLoginEntity::getStatus, request.getStatus())
                .like(StrUtil.isNotBlank(request.getCreatorName()), SysLogLoginEntity::getCreatorName, request.getCreatorName())
                .orderByDesc(SysLogLoginEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysLogLoginRsp.class), page.getTotal());
    }

    @Override
    public List<SysLogLoginRsp> list(SysLogLoginPageReq request) {
        List<SysLogLoginEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogLoginEntity>()
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), SysLogLoginEntity::getStatus, request != null ? request.getStatus() : null)
                .like(request != null && StrUtil.isNotBlank(request.getCreatorName()), SysLogLoginEntity::getCreatorName, request != null ? request.getCreatorName() : null)
                .orderByDesc(SysLogLoginEntity::getCreateDate)
        );
        return ConvertUtils.sourceToTarget(entityList, SysLogLoginRsp.class);
    }
}
