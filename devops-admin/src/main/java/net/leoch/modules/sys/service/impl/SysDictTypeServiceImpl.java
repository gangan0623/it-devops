package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import cn.hutool.core.bean.BeanUtil;
import net.leoch.common.data.validator.AssertUtils;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.modules.sys.mapper.SysDictDataMapper;
import net.leoch.modules.sys.mapper.SysDictTypeMapper;
import net.leoch.modules.sys.vo.req.SysDictTypePageReq;
import net.leoch.modules.sys.vo.req.SysDictTypeReq;
import net.leoch.modules.sys.vo.rsp.DictDataRsp;
import net.leoch.modules.sys.vo.rsp.DictTypeRsp;
import net.leoch.modules.sys.vo.rsp.SysDictTypeRsp;
import net.leoch.modules.sys.entity.SysDictTypeEntity;
import net.leoch.modules.sys.service.ISysDictTypeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class SysDictTypeServiceImpl extends ServiceImpl<SysDictTypeMapper, SysDictTypeEntity> implements ISysDictTypeService {
    private final SysDictDataMapper sysDictDataMapper;

    @Override
    public PageData<SysDictTypeRsp> page(SysDictTypePageReq request) {
        IPage<SysDictTypeEntity> page = this.page(
                request.<SysDictTypeEntity>buildPage().addOrder(
                    com.baomidou.mybatisplus.core.metadata.OrderItem.asc("sort")
                ),
                getWrapper(request)
        );

        return new PageData<>(BeanUtil.copyToList(page.getRecords(), SysDictTypeRsp.class), page.getTotal());
    }

    private QueryWrapper<SysDictTypeEntity> getWrapper(SysDictTypePageReq request) {
        String dictType = request.getDictType();
        String dictName = request.getDictName();

        QueryWrapper<SysDictTypeEntity> wrapper = new QueryWrapper<>();
        wrapper.like(StrUtil.isNotBlank(dictType), "dict_type", dictType);
        wrapper.like(StrUtil.isNotBlank(dictName), "dict_name", dictName);

        return wrapper;
    }

    @Override
    public SysDictTypeRsp get(Long id) {
        SysDictTypeEntity entity = this.getById(id);

        return BeanUtil.copyProperties(entity, SysDictTypeRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysDictTypeReq dto) {
        ValidatorUtils.validateEntity(dto, DefaultGroup.class);
        SysDictTypeEntity entity = BeanUtil.copyProperties(dto, SysDictTypeEntity.class);

        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysDictTypeReq dto) {
        ValidatorUtils.validateEntity(dto, UpdateGroup.class, DefaultGroup.class);
        SysDictTypeEntity entity = BeanUtil.copyProperties(dto, SysDictTypeEntity.class);

        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        AssertUtils.isArrayEmpty(ids, "id");
        //删除
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public List<DictTypeRsp> getAllList() {
        List<DictTypeRsp> typeList = this.getBaseMapper().getDictTypeList();
        List<DictDataRsp> dataList = sysDictDataMapper.getDictDataList();

        Map<Long, List<DictDataRsp>> dataMap = dataList.stream()
                .collect(Collectors.groupingBy(DictDataRsp::getDictTypeId));

        for (DictTypeRsp type : typeList) {
            List<DictDataRsp> items = dataMap.get(type.getId());
            if (items != null) {
                type.setDataList(items);
            }
        }
        return typeList;
    }

}
