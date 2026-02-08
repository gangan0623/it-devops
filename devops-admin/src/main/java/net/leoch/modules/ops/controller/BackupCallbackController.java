package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.dto.BackupCallbackItem;
import net.leoch.modules.ops.dto.BackupCallbackRequest;
import net.leoch.modules.ops.service.BackupCallbackService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 备份回调接口
 */
@RestController
@RequestMapping("ops/backup")
@Tag(name = "备份回调")
public class BackupCallbackController {

    private final BackupCallbackService backupCallbackService;

    public BackupCallbackController(BackupCallbackService backupCallbackService) {
        this.backupCallbackService = backupCallbackService;
    }

    @PostMapping("callback")
    @Operation(summary = "备份回调")
    public Result<Object> callback(@RequestHeader(value = "agent-token", required = false) String token,
                                   @RequestBody List<BackupCallbackItem> items) {
        return backupCallbackService.callback(BackupCallbackRequest.of(token, items));
    }
}
