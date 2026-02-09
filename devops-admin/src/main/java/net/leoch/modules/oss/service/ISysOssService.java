package net.leoch.modules.oss.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.vo.rsp.SysOssRsp;
import net.leoch.modules.sys.vo.req.SysOssPageReq;
import org.springframework.web.multipart.MultipartFile;

import java.util.Map;

public interface ISysOssService extends IService<SysOssEntity> {
    PageData<SysOssRsp> page(SysOssPageReq request);

    /**
     * 上传文件并保存记录
     * @param file 上传文件
     * @return 包含 src(文件URL) 的结果
     */
    Map<String, Object> upload(MultipartFile file) throws Exception;
}
