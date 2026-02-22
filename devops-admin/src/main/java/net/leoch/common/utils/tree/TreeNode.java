

package net.leoch.common.utils.tree;

import lombok.Data;

import java.io.Serial;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 树节点，所有需要实现树节点的，都需要继承该类
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
 @Data
public class TreeNode<T> implements Serializable  {
    @Serial
    private static final long serialVersionUID = 1L;
    /**
     * 主键
     */
    private Long id;
    /**
     * 上级ID
     */
    private Long pid;
    /**
     * 子节点列表
     */
    private List<T> children = new ArrayList<>();
}