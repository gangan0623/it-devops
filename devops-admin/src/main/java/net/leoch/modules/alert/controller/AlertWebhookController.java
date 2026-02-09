package net.leoch.modules.alert.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import net.leoch.common.utils.Result;
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
public class AlertWebhookController {

    private final AlertWebhookService alertWebhookService;

    public AlertWebhookController(AlertWebhookService alertWebhookService) {
        this.alertWebhookService = alertWebhookService;
    }

    @PostMapping("/auto")
    @Operation(summary = "Webhook接收")
    public Result<Object> webhookAuto(@RequestBody String payload) {
        alertWebhookService.handle(null, payload);
        return new Result<>();
    }
}
