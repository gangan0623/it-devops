package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.vo.rsp.SysDictDataRsp;
import net.leoch.modules.sys.vo.req.SysDictDataPageReq;
import net.leoch.modules.sys.entity.SysDictDataEntity;

/**
 * 数据字典
 *
 * @author Taohongqiang
 */
public interface ISysDictDataService extends IService<SysDictDataEntity> {

    PageData<SysDictDataRsp> page(SysDictDataPageReq request);

    SysDictDataRsp get(Long id);

    void save(SysDictDataRsp dto);

    void update(SysDictDataRsp dto);

    void delete(Long[] ids);

}
