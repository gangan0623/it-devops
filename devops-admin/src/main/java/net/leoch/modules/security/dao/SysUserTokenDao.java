

package net.leoch.modules.security.dao;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import net.leoch.modules.security.entity.SysUserTokenEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;

/**
 * 系统用户Token
 * 
 * @author Taohongqiang
 */
@Mapper
public interface SysUserTokenDao extends BaseMapper<SysUserTokenEntity> {

    SysUserTokenEntity getByToken(String token);

    SysUserTokenEntity getByUserId(Long userId);

    void updateToken(@Param("userId") Long userId, @Param("token") String token);
}
