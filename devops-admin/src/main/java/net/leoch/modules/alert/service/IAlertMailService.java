package net.leoch.modules.alert.service;

import cn.hutool.core.util.StrUtil;
import jakarta.mail.internet.MimeMessage;
import net.leoch.common.exception.ServiceException;
import net.leoch.modules.alert.entity.AlertMediaEntity;
import org.springframework.mail.javamail.JavaMailSenderImpl;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Properties;

/**
 * 邮件发送
 */
@Service
public class IAlertMailService {

    private static final org.slf4j.Logger LOGGER = org.slf4j.LoggerFactory.getLogger(IAlertMailService.class);

    public void send(AlertMediaEntity media, List<String> receivers, String subject, String content, String html) {
        if (media == null || receivers == null || receivers.isEmpty()) {
            return;
        }
        JavaMailSenderImpl sender = new JavaMailSenderImpl();
        sender.setHost(media.getHost());
        if (media.getPort() != null) {
            sender.setPort(media.getPort());
        }
        sender.setUsername(media.getUsername());
        sender.setPassword(media.getPassword());
        if (StrUtil.isNotBlank(media.getProtocol())) {
            sender.setProtocol(media.getProtocol());
        }

        Properties properties = new Properties();
        applySwitch(properties, "mail.smtp.auth", media.getSmtpAuth());
        applySwitch(properties, "mail.smtp.starttls.enable", media.getStarttlsEnable());
        applySwitch(properties, "mail.smtp.ssl.enable", media.getTlsEnable());
        applySwitch(properties, "mail.smtp.tls.enable", media.getTlsEnable());
        sender.setJavaMailProperties(properties);

        try {
            MimeMessage message = sender.createMimeMessage();
            MimeMessageHelper helper = new MimeMessageHelper(message, true, "UTF-8");
            String from = StrUtil.isNotBlank(media.getFromAddr()) ? media.getFromAddr() : media.getUsername();
            if (StrUtil.isNotBlank(from)) {
                helper.setFrom(from);
            }
            helper.setTo(receivers.toArray(new String[0]));
            helper.setSubject(StrUtil.nullToEmpty(subject));
            String body = StrUtil.isNotBlank(html) ? html : content;
            helper.setText(body, StrUtil.isNotBlank(html));
            sender.send(message);
            LOGGER.info("Alert mail sent. host={}, to={}, subject={}", media.getHost(), receivers, subject);
        } catch (Exception e) {
            LOGGER.error("Alert mail send failed. host={}, to={}, subject={}, error={}", media.getHost(), receivers, subject, e.getMessage(), e);
            throw new ServiceException("发送邮件失败: " + e.getMessage());
        }
    }

    private void applySwitch(Properties properties, String key, Integer value) {
        if (value == null) {
            return;
        }
        properties.put(key, value == 1 ? "true" : "false");
    }
}
