

package net.leoch.modules.sys.entity;

import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.entity.BaseEntity;

/**
 * 角色数据权限
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Data
@EqualsAndHashCode(callSuper=false)
@TableName("sys_role_data_scope")
public class SysRoleDataScopeEntity extends BaseEntity {
	private static final long serialVersionUID = 1L;

	/**
	 * 角色ID
	 */
	private Long roleId;
	/**
	 * 部门ID
	 */
	private Long deptId;

}