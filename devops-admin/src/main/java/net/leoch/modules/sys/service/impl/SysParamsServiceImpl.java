package net.leoch.modules.sys.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.exception.ErrorCode;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.redis.SysParamsRedis;
import net.leoch.modules.sys.entity.SysParamsEntity;
import net.leoch.modules.sys.mapper.SysParamsMapper;
import net.leoch.modules.sys.service.ISysParamsService;
import net.leoch.modules.sys.vo.req.SysParamsPageReq;
import net.leoch.modules.sys.vo.req.SysParamsReq;
import net.leoch.modules.sys.vo.rsp.SysParamsRsp;
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

        return new PageData<>(BeanUtil.copyToList(page.getRecords(), SysParamsRsp.class), page.getTotal());
    }

    @Override
    public List<SysParamsRsp> list(SysParamsPageReq request) {
        List<SysParamsEntity> entityList = this.list(getWrapper(request));

        return BeanUtil.copyToList(entityList, SysParamsRsp.class);
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
        if (id == null) {
            log.warn("[参数管理] 查询参数失败, id为null");
            return null;
        }
        SysParamsEntity entity = this.getById(id);
        if (entity == null) {
            log.warn("[参数管理] 查询参数失败, 参数不存在, id={}", id);
            return null;
        }

        return BeanUtil.copyProperties(entity, SysParamsRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysParamsReq dto) {
        if (dto == null) {
            log.warn("[参数管理] 保存参数失败, dto为null");
            throw new ServiceException("参数不能为空");
        }
        ValidatorUtils.validateEntity(dto, AddGroup.class, DefaultGroup.class);
        SysParamsEntity entity = BeanUtil.copyProperties(dto, SysParamsEntity.class);
        this.save(entity);
        log.info("[参数管理] 保存参数成功, paramCode={}", entity.getParamCode());

        sysParamsRedis.set(entity.getParamCode(), entity.getParamValue());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysParamsReq dto) {
        if (dto == null) {
            log.warn("[参数管理] 更新参数失败, dto为null");
            throw new ServiceException("参数不能为空");
        }
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysParamsEntity entity = BeanUtil.copyProperties(dto, SysParamsEntity.class);
        this.updateById(entity);
        log.info("[参数管理] 更新参数成功, paramCode={}", entity.getParamCode());

        sysParamsRedis.set(entity.getParamCode(), entity.getParamValue());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        log.info("[参数管理] 开始删除参数, count={}", ids.length);
        //删除Redis数据
        List<String> paramCodeList = this.getBaseMapper().getParamCodeList(ids);
        if (paramCodeList != null && !paramCodeList.isEmpty()) {
            String[] paramCodes = paramCodeList.toArray(new String[0]);
            sysParamsRedis.delete(paramCodes);
            log.debug("[参数管理] 删除Redis缓存, count={}", paramCodes.length);
        }

        //删除
        this.removeByIds(Arrays.asList(ids));
        log.info("[参数管理] 删除参数成功, count={}", ids.length);
    }

    @Override
    public String getValue(String paramCode) {
        if (StrUtil.isBlank(paramCode)) {
            log.warn("[参数管理] 获取参数值失败, paramCode为空");
            return null;
        }
        String paramValue = sysParamsRedis.get(paramCode);
        if (paramValue == null) {
            paramValue = this.getBaseMapper().getValueByCode(paramCode);
            if (paramValue != null) {
                sysParamsRedis.set(paramCode, paramValue);
                log.debug("[参数管理] 从数据库加载参数并缓存, paramCode={}", paramCode);
            }
        }
        return paramValue;
    }

    @Override
    public <T> T getValueObject(String paramCode, Class<T> clazz) {
        if (StrUtil.isBlank(paramCode) || clazz == null) {
            log.warn("[参数管理] 获取参数对象失败, paramCode={}, clazz={}", paramCode, clazz);
            try {
                return clazz != null ? clazz.getDeclaredConstructor().newInstance() : null;
            } catch (Exception e) {
                log.error("[参数管理] 实例化默认对象失败, clazz={}", clazz, e);
                throw new ServiceException(ErrorCode.PARAMS_GET_ERROR);
            }
        }
        String paramValue = getValue(paramCode);
        if (StrUtil.isNotBlank(paramValue)) {
            try {
                return JSONUtil.toBean(paramValue, clazz);
            } catch (Exception e) {
                log.error("[参数管理] JSON解析失败, paramCode={}, paramValue={}, clazz={}", paramCode, paramValue, clazz, e);
                throw new ServiceException(ErrorCode.PARAMS_GET_ERROR);
            }
        }

        try {
            return clazz.getDeclaredConstructor().newInstance();
        } catch (Exception e) {
            log.error("[参数管理] 实例化默认对象失败, clazz={}", clazz, e);
            throw new ServiceException(ErrorCode.PARAMS_GET_ERROR);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public int updateValueByCode(String paramCode, String paramValue) {
        if (StrUtil.isBlank(paramCode)) {
            log.warn("[参数管理] 按code更新参数失败, paramCode为空");
            return 0;
        }
        log.info("[参数管理] 按code更新参数, paramCode={}", paramCode);
        int count = this.getBaseMapper().updateValueByCode(paramCode, paramValue);
        if (count > 0) {
            sysParamsRedis.set(paramCode, paramValue);
            log.debug("[参数管理] 更新Redis缓存, paramCode={}", paramCode);
        }
        return count;
    }

}
