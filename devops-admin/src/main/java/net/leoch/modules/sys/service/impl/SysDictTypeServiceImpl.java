package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.sys.mapper.SysDictDataMapper;
import net.leoch.modules.sys.mapper.SysDictTypeMapper;
import net.leoch.modules.sys.vo.rsp.SysDictTypeRsp;
import net.leoch.modules.sys.vo.req.SysDictTypePageReq;
import net.leoch.modules.sys.entity.DictData;
import net.leoch.modules.sys.entity.DictType;
import net.leoch.modules.sys.entity.SysDictTypeEntity;
import net.leoch.modules.sys.service.ISysDictTypeService;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * 字典类型
 *
 * @author Taohongqiang
 */
@Slf4j
@Service
@AllArgsConstructor
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

        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysDictTypeRsp.class), page.getTotal());
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

        return ConvertUtils.sourceToTarget(entity, SysDictTypeRsp.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysDictTypeRsp dto) {
        SysDictTypeEntity entity = ConvertUtils.sourceToTarget(dto, SysDictTypeEntity.class);

        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysDictTypeRsp dto) {
        SysDictTypeEntity entity = ConvertUtils.sourceToTarget(dto, SysDictTypeEntity.class);

        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        //删除
        this.removeByIds(Arrays.asList(ids));
    }

    @Override
    public List<DictType> getAllList() {
        List<DictType> typeList = this.getBaseMapper().getDictTypeList();
        List<DictData> dataList = sysDictDataMapper.getDictDataList();
        for (DictType type : typeList) {
            for (DictData data : dataList) {
                if (type.getId().equals(data.getDictTypeId())) {
                    type.getDataList().add(data);
                }
            }
        }
        return typeList;
    }

}
