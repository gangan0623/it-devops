package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.utils.JsonUtils;
import net.leoch.common.validator.AssertUtils;
import net.leoch.common.validator.ValidatorUtils;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;
import net.leoch.modules.sys.mapper.SysParamsMapper;
import net.leoch.modules.sys.vo.req.SysParamsPageReq;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.modules.sys.vo.rsp.SysParamsRsp;
import net.leoch.modules.sys.entity.SysParamsEntity;
import net.leoch.modules.sys.redis.SysParamsRedis;
import net.leoch.modules.sys.service.ISysParamsService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * 参数管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SysParamsServiceImpl extends ServiceImpl<SysParamsMapper, SysParamsEntity> implements ISysParamsService {
    private final SysParamsRedis sysParamsRedis;

    @Override
    public PageData<SysParamsRsp> page(SysParamsPageReq request) {
        IPage<SysParamsEntity> page = this.page(
                request.<SysParamsEntity>buildPage().addOrder(
                    com.baomidou.mybatisplus.core.metadata.OrderItem.desc("create_date")
                ),
                getWrapper(request)
        );

        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysParamsRsp.class), page.getTotal());
    }

    @Override
    public List<SysParamsRsp> list(SysParamsPageReq request) {
        List<SysParamsEntity> entityList = this.list(getWrapper(request));

        return ConvertUtils.sourceToTarget(entityList, SysParamsRsp.class);
    }

    private QueryWrapper<SysParamsEntity> getWrapper(SysParamsPageReq request) {
        String paramCode = request.getParamCode();

        QueryWrapper<SysParamsEntity> wrapper = new QueryWrapper<>();
        wrapper.eq("param_type", 1);
        wrapper.like(StrUtil.isNotBlank(paramCode), "param_code", paramCode);

        return wrapper;
    }

    @Override
    public SysParamsRsp get(Long id) {
        SysParamsEntity entity = this.getById(id);

        return ConvertUtils.sourceToTarget(entity, SysParamsRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysParamsReq dto) {
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        SysParamsEntity entity = ConvertUtils.sourceToTarget(dto, SysParamsEntity.class);
        this.save(entity);

        sysParamsRedis.set(entity.getParamCode(), entity.getParamValue());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysParamsReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysParamsEntity entity = ConvertUtils.sourceToTarget(dto, SysParamsEntity.class);
        this.updateById(entity);

        sysParamsRedis.set(entity.getParamCode(), entity.getParamValue());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        //删除Redis数据
        List<String> paramCodeList = this.getBaseMapper().getParamCodeList(ids);
        String[] paramCodes = paramCodeList.toArray(new String[paramCodeList.size()]);
        sysParamsRedis.delete(paramCodes);

        //删除
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public String getValue(String paramCode) {
        String paramValue = sysParamsRedis.get(paramCode);
        if (paramValue == null) {
            paramValue = this.getBaseMapper().getValueByCode(paramCode);

            sysParamsRedis.set(paramCode, paramValue);
        }
        return paramValue;
    }

    @Override
    public <T> T getValueObject(String paramCode, Class<T> clazz) {
        String paramValue = getValue(paramCode);
        if (StrUtil.isNotBlank(paramValue)) {
            return JsonUtils.parseObject(paramValue, clazz);
        }

        try {
            return clazz.newInstance();
        } catch (Exception e) {
            throw new ServiceException(ErrorCode.PARAMS_GET_ERROR);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int updateValueByCode(String paramCode, String paramValue) {
        int count = this.getBaseMapper().updateValueByCode(paramCode, paramValue);
        sysParamsRedis.set(paramCode, paramValue);
        return count;
    }

}
