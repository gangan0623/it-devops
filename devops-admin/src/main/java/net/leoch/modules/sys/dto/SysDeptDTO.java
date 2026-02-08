

package net.leoch.modules.sys.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Null;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.utils.TreeNode;
import net.leoch.common.validator.group.AddGroup;
import net.leoch.common.validator.group.DefaultGroup;
import net.leoch.common.validator.group.UpdateGroup;

import java.io.Serializable;
import java.util.Date;

/**
 * 部门管理
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(title = "部门管理")
public class SysDeptDTO extends TreeNode implements Serializable {
    private static final long serialVersionUID = 1L;

	@Schema(title = "id")
	@Null(message="{id.null}", groups = AddGroup.class)
	@NotNull(message="{id.require}", groups = UpdateGroup.class)
	private Long id;

	@Schema(title = "上级ID")
	@NotNull(message="{sysdept.pid.require}", groups = DefaultGroup.class)
	private Long pid;

	@Schema(title = "部门名称")
	@NotBlank(message="{sysdept.name.require}", groups = DefaultGroup.class)
	private String name;

	@Schema(title = "排序")
	@Min(value = 0, message = "{sort.number}", groups = DefaultGroup.class)
	private Integer sort;

	@Schema(title = "创建时间")
	@JsonProperty(access = JsonProperty.Access.READ_ONLY)
	private Date createDate;

	@Schema(title = "上级部门名称")
	private String parentName;

	@Override
	public Long getId() {
		return id;
	}

	@Override
	public void setId(Long id) {
		this.id = id;
	}

	@Override
	public Long getPid() {
		return pid;
	}

	@Override
	public void setPid(Long pid) {
		this.pid = pid;
	}
}
