

package net.leoch.common.support.exception;


import lombok.Getter;
import lombok.Setter;
import net.leoch.common.support.utils.MessageUtils;

import java.io.Serial;

/**
 * 自定义异常
 *
 * @author Taohongqiang
 */
@Setter
@Getter
public class ServiceException extends RuntimeException {
	@Serial
    private static final long serialVersionUID = 1L;

    private int code;
	private String msg;

	public ServiceException(int code) {
		this.code = code;
		this.msg = MessageUtils.getMessage(code);
	}

	public ServiceException(int code, String... params) {
		this.code = code;
		this.msg = MessageUtils.getMessage(code, params);
	}

	public ServiceException(int code, Throwable e) {
		super(e);
		this.code = code;
		this.msg = MessageUtils.getMessage(code);
	}

	public ServiceException(int code, Throwable e, String... params) {
		super(e);
		this.code = code;
		this.msg = MessageUtils.getMessage(code, params);
	}

	public ServiceException(String msg) {
		super(msg);
		this.code = ErrorCode.INTERNAL_SERVER_ERROR;
		this.msg = msg;
	}

	public ServiceException(String msg, Throwable e) {
		super(msg, e);
		this.code = ErrorCode.INTERNAL_SERVER_ERROR;
		this.msg = msg;
	}

}