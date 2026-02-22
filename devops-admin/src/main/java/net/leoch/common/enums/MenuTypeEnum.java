

package net.leoch.common.enums;

/**
 * 菜单类型枚举
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public enum MenuTypeEnum {
    /**
     * 菜单
     */
    MENU(0),
    /**
     * 按钮
     */
    BUTTON(1);

    private final int value;

    MenuTypeEnum(int value) {
        this.value = value;
    }

    public int value() {
        return this.value;
    }
}
