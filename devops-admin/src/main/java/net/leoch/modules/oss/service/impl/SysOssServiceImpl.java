package net.leoch.modules.oss.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.modules.oss.dao.SysOssDao;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.service.SysOssService;
import net.leoch.modules.sys.dto.SysOssPageRequest;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class SysOssServiceImpl extends ServiceImpl<SysOssDao, SysOssEntity> implements SysOssService {

    @Override
    public PageData<SysOssEntity> page(SysOssPageRequest request) {
        IPage<SysOssEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysOssEntity>()
                .orderByDesc(SysOssEntity::getCreateDate)
        );
        return new PageData<>(page.getRecords(), page.getTotal());
    }
}
