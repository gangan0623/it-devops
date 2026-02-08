

package net.leoch.modules.security.user;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 登录用户信息
 *
 * @author Taohongqiang
 */
@Data
public class UserDetail implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;

    private Long id;
    private String username;
    private String realName;
    private String headUrl;
    private Integer gender;
    private String email;
    private String mobile;
    private Long deptId;
    @JsonIgnore
    private transient String password;
    private Integer status;
    private Integer superAdmin;
    /**
     * 部门数据权限
     */
    private List<Long> deptIdList;

}