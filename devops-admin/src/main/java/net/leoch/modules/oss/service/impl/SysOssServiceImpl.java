package net.leoch.modules.oss.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.io.file.FileNameUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.storage.OSSFactory;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.mapper.SysOssMapper;
import net.leoch.modules.oss.service.ISysOssService;
import net.leoch.modules.oss.vo.rsp.SysOssRsp;
import net.leoch.modules.sys.vo.req.SysOssPageReq;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.*;

import cn.hutool.core.util.StrUtil;

@Slf4j
@Service
public class SysOssServiceImpl extends ServiceImpl<SysOssMapper, SysOssEntity> implements ISysOssService {

    /**
     * 允许上传的文件类型白名单（后缀名）
     */
    private static final Set<String> ALLOWED_FILE_TYPES = Set.of(
            // 图片
            "jpg", "jpeg", "png", "gif", "bmp", "webp", "svg",
            // 文档
            "pdf", "doc", "docx", "xls", "xlsx", "ppt", "pptx", "txt", "csv",
            // 压缩包
            "zip", "rar", "7z", "tar", "gz",
            // 其他
            "json", "xml", "log"
    );

    /**
     * 文件大小限制（默认50MB）
     */
    private static final long MAX_FILE_SIZE = 50 * 1024 * 1024;

    @Override
    public PageData<SysOssRsp> page(SysOssPageReq request) {
        IPage<SysOssEntity> page = this.page(request.buildPage(),
            new LambdaQueryWrapper<SysOssEntity>()
                .orderByDesc(SysOssEntity::getCreateDate)
        );
        return new PageData<>(BeanUtil.copyToList(page.getRecords(), SysOssRsp.class), page.getTotal());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Map<String, Object> upload(MultipartFile file) throws Exception {
        String originalFilename = file.getOriginalFilename();
        long fileSize = file.getSize();

        log.info("[文件上传] 开始上传, 原始文件名={}, 大小={}KB", originalFilename, fileSize / 1024);

        // 1. 验证文件不为空
        if (file.isEmpty()) {
            log.warn("[文件上传] 上传文件为空");
            throw new ServiceException("上传文件不能为空");
        }

        // 2. 验证文件名
        if (StrUtil.isBlank(originalFilename)) {
            log.warn("[文件上传] 文件名为空");
            throw new ServiceException("文件名不能为空");
        }

        // 3. 验证文件大小
        if (fileSize > MAX_FILE_SIZE) {
            log.warn("[文件上传] 文件过大, 文件名={}, 大小={}MB, 限制={}MB",
                originalFilename, fileSize / 1024 / 1024, MAX_FILE_SIZE / 1024 / 1024);
            throw new ServiceException("文件大小超过限制，最大允许" + (MAX_FILE_SIZE / 1024 / 1024) + "MB");
        }

        // 4. 提取并验证文件后缀
        String suffix = FileNameUtil.getSuffix(originalFilename);
        if (StrUtil.isBlank(suffix)) {
            log.warn("[文件上传] 文件无后缀名, 文件名={}", originalFilename);
            throw new ServiceException("文件必须有扩展名");
        }

        // 5. 验证文件类型是否在白名单中
        String lowerSuffix = suffix.toLowerCase();
        if (!ALLOWED_FILE_TYPES.contains(lowerSuffix)) {
            log.warn("[文件上传] 文件类型不允许, 文件名={}, 后缀={}", originalFilename, suffix);
            throw new ServiceException("不支持的文件类型: " + suffix);
        }

        // 6. 执行上传
        String url = OSSFactory.build().uploadSuffix(file.getBytes(), lowerSuffix);
        log.info("[文件上传] 上传成功, 原始文件名={}, URL={}", originalFilename, url);

        // 7. 保存上传记录
        SysOssEntity ossEntity = new SysOssEntity();
        ossEntity.setUrl(url);
        ossEntity.setCreateDate(new Date());
        this.save(ossEntity);

        Map<String, Object> data = new HashMap<>(1);
        data.put("src", url);
        return data;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        log.info("[SysOss] 开始删除, ids={}", Arrays.toString(ids));
        AssertUtils.isArrayEmpty(ids, "id");
        this.removeByIds(Arrays.asList(ids));
    }
}
