package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.modules.sys.mapper.SysDictDataMapper;
import net.leoch.modules.sys.vo.req.SysDictDataPageReq;
import net.leoch.modules.sys.vo.req.SysDictDataReq;
import net.leoch.modules.sys.vo.rsp.SysDictDataRsp;
import net.leoch.modules.sys.entity.SysDictDataEntity;
import net.leoch.modules.sys.service.ISysDictDataService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
public class SysDictDataServiceImpl extends ServiceImpl<SysDictDataMapper, SysDictDataEntity> implements ISysDictDataService {

    @Override
    public PageData<SysDictDataRsp> page(SysDictDataPageReq request) {
        IPage<SysDictDataEntity> page = this.page(
            request.<SysDictDataEntity>buildPage().addOrder(
                com.baomidou.mybatisplus.core.metadata.OrderItem.asc("sort")
            ),
            getWrapper(request)
        );

        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysDictDataRsp.class), page.getTotal());
    }

    private QueryWrapper<SysDictDataEntity> getWrapper(SysDictDataPageReq request) {
        Long dictTypeId = Long.parseLong(request.getDictTypeId());
        String dictLabel = request.getDictLabel();
        String dictValue = request.getDictValue();

        QueryWrapper<SysDictDataEntity> wrapper = new QueryWrapper<>();
        wrapper.eq("dict_type_id", dictTypeId);
        wrapper.like(StrUtil.isNotBlank(dictLabel), "dict_label", dictLabel);
        wrapper.like(StrUtil.isNotBlank(dictValue), "dict_value", dictValue);

        return wrapper;
    }

    @Override
    public SysDictDataRsp get(Long id) {
        SysDictDataEntity entity = this.getById(id);

        return ConvertUtils.sourceToTarget(entity, SysDictDataRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysDictDataReq dto) {
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);
        SysDictDataEntity entity = ConvertUtils.sourceToTarget(dto, SysDictDataEntity.class);

        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysDictDataReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysDictDataEntity entity = ConvertUtils.sourceToTarget(dto, SysDictDataEntity.class);

        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        //删除
        this.removeByIds(Arrays.asList(ids));
    }

}
