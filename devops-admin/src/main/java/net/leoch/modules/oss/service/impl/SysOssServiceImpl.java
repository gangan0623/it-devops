package net.leoch.modules.oss.service.impl;

import cn.hutool.core.io.file.FileNameUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.storage.OSSFactory;
import net.leoch.modules.oss.mapper.SysOssMapper;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.vo.rsp.SysOssRsp;
import net.leoch.modules.oss.service.ISysOssService;
import net.leoch.modules.sys.vo.req.SysOssPageReq;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Service
public class SysOssServiceImpl extends ServiceImpl<SysOssMapper, SysOssEntity> implements ISysOssService {

    @Override
    public PageData<SysOssRsp> page(SysOssPageReq request) {
        IPage<SysOssEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysOssEntity>()
                .orderByDesc(SysOssEntity::getCreateDate)
        );
        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysOssRsp.class), page.getTotal());
    }

    @Override
    public Map<String, Object> upload(MultipartFile file) throws Exception {
        if (file.isEmpty()) {
            throw new ServiceException("上传文件不能为空");
        }
        String suffix = FileNameUtil.getSuffix(file.getOriginalFilename());
        String url = OSSFactory.build().uploadSuffix(file.getBytes(), suffix);

        SysOssEntity ossEntity = new SysOssEntity();
        ossEntity.setUrl(url);
        ossEntity.setCreateDate(new Date());
        this.save(ossEntity);

        Map<String, Object> data = new HashMap<>(1);
        data.put("src", url);
        return data;
    }

    @Override
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }
}
