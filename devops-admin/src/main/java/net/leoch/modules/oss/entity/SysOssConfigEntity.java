package net.leoch.modules.oss.entity;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;
import lombok.EqualsAndHashCode;
import net.leoch.common.entity.BaseEntity;

import java.util.Date;

/**
 * 云存储配置
 *
 * @author Taohongqiang
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("sys_oss_config")
public class SysOssConfigEntity extends BaseEntity  {
    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 配置JSON
     */
    private String configJson;

    /**
     * 更新者
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Long updater;

    /**
     * 更新时间
     */
    @TableField(fill = FieldFill.INSERT_UPDATE)
    private Date updateDate;
}
