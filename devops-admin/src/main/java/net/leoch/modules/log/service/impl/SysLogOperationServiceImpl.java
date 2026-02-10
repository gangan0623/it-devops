package net.leoch.modules.log.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.modules.log.mapper.SysLogOperationMapper;
import net.leoch.modules.log.vo.rsp.SysLogOperationRsp;
import net.leoch.modules.log.vo.req.SysLogOperationPageReq;
import net.leoch.modules.log.entity.SysLogOperationEntity;
import net.leoch.modules.log.service.ISysLogOperationService;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogOperationServiceImpl extends ServiceImpl<SysLogOperationMapper, SysLogOperationEntity> implements ISysLogOperationService {

    @Override
    public PageData<SysLogOperationRsp> page(SysLogOperationPageReq request) {
        IPage<SysLogOperationEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogOperationEntity>()
                .eq(StrUtil.isNotBlank(request.getStatus()), SysLogOperationEntity::getStatus, request.getStatus())
                .orderByDesc(SysLogOperationEntity::getCreateDate)
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), SysLogOperationRsp.class), page.getTotal());
    }

    @Override
    public List<SysLogOperationRsp> list(SysLogOperationPageReq request) {
        List<SysLogOperationEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogOperationEntity>()
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), SysLogOperationEntity::getStatus, request != null ? request.getStatus() : null)
                .orderByDesc(SysLogOperationEntity::getCreateDate)
        );
        return BeanUtil.copyToList(entityList, SysLogOperationRsp.class);
    }
}
