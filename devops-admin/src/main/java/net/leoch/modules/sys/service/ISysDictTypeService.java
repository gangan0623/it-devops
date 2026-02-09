package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.page.PageData;
import net.leoch.modules.sys.vo.req.SysDictTypePageReq;
import net.leoch.modules.sys.vo.req.SysDictTypeReq;
import net.leoch.modules.sys.vo.rsp.SysDictTypeRsp;
import net.leoch.modules.sys.vo.rsp.DictTypeRsp;
import net.leoch.modules.sys.entity.SysDictTypeEntity;

import java.util.List;

/**
 * 数据字典
 *
 * @author Taohongqiang
 */
public interface ISysDictTypeService extends IService<SysDictTypeEntity> {

    PageData<SysDictTypeRsp> page(SysDictTypePageReq request);

    SysDictTypeRsp get(Long id);

    void save(SysDictTypeReq dto);

    void update(SysDictTypeReq dto);

    void delete(Long[] ids);

    /**
     * 获取所有字典
     */
    List<DictTypeRsp> getAllList();

}
