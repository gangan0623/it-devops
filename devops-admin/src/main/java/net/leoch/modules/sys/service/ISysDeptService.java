package net.leoch.modules.sys.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.modules.sys.entity.SysDeptEntity;
import net.leoch.modules.sys.vo.req.SysDeptReq;
import net.leoch.modules.sys.vo.rsp.SysDeptRsp;

import java.util.List;
import java.util.Map;

/**
 * 部门管理
 *
 * @author Taohongqiang
 */
public interface ISysDeptService extends IService<SysDeptEntity> {

	List<SysDeptRsp> list(Map<String, Object> params);

	SysDeptRsp get(Long id);

	void save(SysDeptReq dto);

	void update(SysDeptReq dto);

	void delete(Long id);

	/**
	 * 根据部门ID，获取本部门及子部门ID列表
	 * @param id   部门ID
	 */
	List<Long> getSubDeptIdList(Long id);
}
