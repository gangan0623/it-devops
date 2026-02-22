package net.leoch.modules.alert.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.data.result.Result;
import net.leoch.modules.alert.service.AlertWebhookService;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 告警Webhook
 */
@RestController
@RequestMapping("/webhook")
@Tag(name = "告警Webhook")
@RequiredArgsConstructor
public class AlertWebhookController {

    private final AlertWebhookService alertWebhookService;

    @PostMapping("/auto")
    @Operation(summary = "Webhook接收")
    public Result<Object> webhookAuto(@RequestBody String payload) {
        alertWebhookService.handle(null, payload);
        return new Result<>();
    }
}
