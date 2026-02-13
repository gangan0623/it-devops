package net.leoch.modules.log.service.impl;

import cn.hutool.core.bean.BeanUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.log.entity.SysLogErrorEntity;
import net.leoch.modules.log.mapper.SysLogErrorMapper;
import net.leoch.modules.log.service.ISysLogErrorService;
import net.leoch.modules.log.vo.req.SysLogErrorPageReq;
import net.leoch.modules.log.vo.rsp.SysLogErrorRsp;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogErrorServiceImpl extends ServiceImpl<SysLogErrorMapper, SysLogErrorEntity> implements ISysLogErrorService {

    @Override
    public PageData<SysLogErrorRsp> page(SysLogErrorPageReq request) {
        IPage<SysLogErrorEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogErrorEntity>()
                .orderByDesc(SysLogErrorEntity::getCreateDate)
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), SysLogErrorRsp.class), page.getTotal());
    }

    @Override
    public List<SysLogErrorRsp> list(SysLogErrorPageReq request) {
        List<SysLogErrorEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogErrorEntity>()
                .orderByDesc(SysLogErrorEntity::getCreateDate)
        );
        return BeanUtil.copyToList(entityList, SysLogErrorRsp.class);
    }
}
