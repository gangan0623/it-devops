

package net.leoch.modules.oss.service.impl;

import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import net.leoch.common.constant.Constant;
import net.leoch.common.page.PageData;
import net.leoch.common.service.impl.BaseServiceImpl;
import net.leoch.modules.oss.dao.SysOssDao;
import net.leoch.modules.oss.entity.SysOssEntity;
import net.leoch.modules.oss.service.SysOssService;
import org.springframework.stereotype.Service;

import java.util.Map;


@Service
public class SysOssServiceImpl extends BaseServiceImpl<SysOssDao, SysOssEntity> implements SysOssService {

	@Override
	public PageData<SysOssEntity> page(Map<String, Object> params) {
		IPage<SysOssEntity> page = baseDao.selectPage(
			getPage(params, Constant.CREATE_DATE, false),
			new QueryWrapper<>()
		);
		return getPageData(page, SysOssEntity.class);
	}
}
