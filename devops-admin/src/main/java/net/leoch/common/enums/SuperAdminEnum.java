

package net.leoch.common.enums;

/**
 * 超级管理员枚举
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public enum SuperAdminEnum {
    YES(1),
    NO(0);

    private final int value;

    SuperAdminEnum(int value) {
        this.value = value;
    }

    public int value() {
        return this.value;
    }
}