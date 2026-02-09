package net.leoch.modules.ops.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.utils.Result;
import net.leoch.modules.ops.vo.req.BackupCallbackItemReq;
import net.leoch.modules.ops.vo.req.BackupCallbackReq;
import net.leoch.modules.ops.service.IBackupCallbackService;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 备份回调接口
 */
@RestController
@RequestMapping("ops/backup")
@Tag(name = "备份回调")
@RequiredArgsConstructor
public class BackupCallbackController {

    private final IBackupCallbackService backupCallbackService;

    @PostMapping("callback")
    @Operation(summary = "备份回调")
    public Result<Object> callback(@RequestHeader(value = "agent-token", required = false) String token,
                                   @RequestBody List<BackupCallbackItemReq> items) {
        return backupCallbackService.callback(BackupCallbackReq.of(token, items));
    }
}
