package net.leoch.modules.ops.dto;

import java.io.Serializable;

/**
 * 备份回调结果项
 */
public class BackupCallbackItem implements Serializable {
    private static final long serialVersionUID = 1L;

    private String instance;
    private String url;

    public String getInstance() {
        return instance;
    }

    public void setInstance(String instance) {
        this.instance = instance;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }
}
