

package net.leoch.framework.config.doc;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.ExternalDocumentation;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.servers.Server;
import net.leoch.common.base.Constant;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * OpenAPI 3.0 配置 (Apifox 标准规范)
 *
 * @author Taohongqiang
 */
@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI createRestApi() {
        return new OpenAPI()
                .info(apiInfo())
                .servers(servers())
                .externalDocs(externalDocs())
                .components(securitySchemes())
                .addSecurityItem(securityRequirement());
    }

    /**
     * API 基本信息
     */
    private Info apiInfo() {
        return new Info()
                .title("IT DevOps 管理平台 API")
                .description("""
                        IT 基础建设管理平台 RESTful API 文档

                        ## 功能模块
                        - 系统管理：用户/角色/菜单/部门/字典/参数
                        - 安全认证：登录/登出/Token 管理
                        - 日志审计：登录日志/操作日志/异常日志
                        - 对象存储：文件上传/下载/管理（MinIO/OSS/COS/七牛）
                        - 运维管理：主机管理/设备备份/监控组件/看板
                        - 告警管理：媒介/模板/触发规则/告警记录/SSE推送
                        - 定时任务：任务调度/执行日志

                        ## 认证方式
                        使用 SA-Token 进行身份认证，请在请求头中携带 `token` 字段。

                        ## 导入到 Apifox
                        1. 复制 OpenAPI JSON URL: `/api/v3/api-docs`
                        2. 在 Apifox 中选择 "导入" -> "URL导入"
                        3. 粘贴 URL 并导入
                        """)
                .version("5.5.0")
                .contact(contact())
                .license(license());
    }

    /**
     * 联系人信息
     */
    private Contact contact() {
        return new Contact()
                .name("IT DevOps Team")
                .email("devops@leoch.com")
                .url("https://github.com/your-org/it-devops");
    }

    /**
     * 许可证信息
     */
    private License license() {
        return new License()
                .name("Apache 2.0")
                .url("https://www.apache.org/licenses/LICENSE-2.0.html");
    }

    /**
     * 服务器配置（多环境）
     */
    private List<Server> servers() {
        return List.of(
                new Server()
                        .url("http://localhost:10001/api")
                        .description("本地开发环境"),
                new Server()
                        .url("http://192.168.17.98:10001/api")
                        .description("测试环境"),
                new Server()
                        .url("https://devops.leoch.com/api")
                        .description("生产环境")
        );
    }

    /**
     * 外部文档
     */
    private ExternalDocumentation externalDocs() {
        return new ExternalDocumentation()
                .description("项目 GitHub 仓库")
                .url("https://github.com/your-org/it-devops");
    }

    /**
     * 安全方案配置（Token 认证）
     */
    private Components securitySchemes() {
        return new Components()
                .addSecuritySchemes(Constant.TOKEN_HEADER,
                        new SecurityScheme()
                                .type(SecurityScheme.Type.APIKEY)
                                .in(SecurityScheme.In.HEADER)
                                .name(Constant.TOKEN_HEADER)
                                .description("SA-Token 认证令牌，登录后从响应头或响应体中获取")
                );
    }

    /**
     * 全局安全要求
     */
    private SecurityRequirement securityRequirement() {
        return new SecurityRequirement()
                .addList(Constant.TOKEN_HEADER);
    }
}
