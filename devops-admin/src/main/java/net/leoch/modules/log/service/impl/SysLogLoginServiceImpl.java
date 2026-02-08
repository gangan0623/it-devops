package net.leoch.modules.log.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.log.dao.SysLogLoginDao;
import net.leoch.modules.log.dto.SysLogLoginDTO;
import net.leoch.modules.log.dto.SysLogLoginPageRequest;
import net.leoch.modules.log.entity.SysLogLoginEntity;
import net.leoch.modules.log.service.SysLogLoginService;
import org.springframework.stereotype.Service;

import java.util.List;

@Slf4j
@Service
public class SysLogLoginServiceImpl extends ServiceImpl<SysLogLoginDao, SysLogLoginEntity> implements SysLogLoginService {

    @Override
    public PageData<SysLogLoginDTO> page(SysLogLoginPageRequest request) {
        IPage<SysLogLoginEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysLogLoginEntity>()
                .eq(StrUtil.isNotBlank(request.getStatus()), SysLogLoginEntity::getStatus, request.getStatus())
                .like(StrUtil.isNotBlank(request.getCreatorName()), SysLogLoginEntity::getCreatorName, request.getCreatorName())
                .orderByDesc(SysLogLoginEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysLogLoginDTO.class), page.getTotal());
    }

    @Override
    public List<SysLogLoginDTO> list(SysLogLoginPageRequest request) {
        List<SysLogLoginEntity> entityList = this.list(
            new LambdaQueryWrapper<SysLogLoginEntity>()
                .eq(request != null && StrUtil.isNotBlank(request.getStatus()), SysLogLoginEntity::getStatus, request != null ? request.getStatus() : null)
                .like(request != null && StrUtil.isNotBlank(request.getCreatorName()), SysLogLoginEntity::getCreatorName, request != null ? request.getCreatorName() : null)
                .orderByDesc(SysLogLoginEntity::getCreateDate)
        );
        return ConvertUtils.sourceToTarget(entityList, SysLogLoginDTO.class);
    }
}
