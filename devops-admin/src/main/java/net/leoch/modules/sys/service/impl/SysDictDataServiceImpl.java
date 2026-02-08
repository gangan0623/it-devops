package net.leoch.modules.sys.service.impl;

import cn.hutool.core.util.StrUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.page.PageData;
import net.leoch.common.utils.ConvertUtils;
import net.leoch.modules.sys.dao.SysDictDataDao;
import net.leoch.modules.sys.dto.SysDictDataDTO;
import net.leoch.modules.sys.dto.SysDictDataPageRequest;
import net.leoch.modules.sys.entity.SysDictDataEntity;
import net.leoch.modules.sys.service.SysDictDataService;
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
public class SysDictDataServiceImpl extends ServiceImpl<SysDictDataDao, SysDictDataEntity> implements SysDictDataService {

    @Override
    public PageData<SysDictDataDTO> page(SysDictDataPageRequest request) {
        IPage<SysDictDataEntity> page = this.page(
            request.<SysDictDataEntity>buildPage().addOrder(
                com.baomidou.mybatisplus.core.metadata.OrderItem.asc("sort")
            ),
            getWrapper(request)
        );

        return new PageData<>(ConvertUtils.sourceToTarget(page.getRecords(), SysDictDataDTO.class), page.getTotal());
    }

    private QueryWrapper<SysDictDataEntity> getWrapper(SysDictDataPageRequest request) {
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
    public SysDictDataDTO get(Long id) {
        SysDictDataEntity entity = this.getById(id);

        return ConvertUtils.sourceToTarget(entity, SysDictDataDTO.class);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(SysDictDataDTO dto) {
        SysDictDataEntity entity = ConvertUtils.sourceToTarget(dto, SysDictDataEntity.class);

        this.save(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(SysDictDataDTO dto) {
        SysDictDataEntity entity = ConvertUtils.sourceToTarget(dto, SysDictDataEntity.class);

        this.updateById(entity);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(Long[] ids) {
        //删除
        this.removeByIds(Arrays.asList(ids));
    }

}
