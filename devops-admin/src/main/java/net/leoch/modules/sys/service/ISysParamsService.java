package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.sys.entity.SysParamsEntity;
import net.leoch.modules.sys.vo.req.SysParamsPageReq;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.modules.sys.vo.rsp.SysParamsRsp;

import java.util.List;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public interface ISysParamsService extends IService<SysParamsEntity> {

    PageData<SysParamsRsp> page(SysParamsPageReq request);

    List<SysParamsRsp> list(SysParamsPageReq request);

    SysParamsRsp get(Long id);

    void save(SysParamsReq dto);

    void update(SysParamsReq dto);

    void delete(Long[] ids);

    /**
     * 根据参数编码，获取参数的value值
     *
     * @param paramCode  参数编码
     */
    String getValue(String paramCode);

    /**
     * 根据参数编码，获取value的Object对象
     * @param paramCode  参数编码
     * @param clazz  Object对象
     */
    <T> T getValueObject(String paramCode, Class<T> clazz);

    /**
     * 根据参数编码，更新value
     * @param paramCode  参数编码
     * @param paramValue  参数值
     */
    int updateValueByCode(String paramCode, String paramValue);
}
