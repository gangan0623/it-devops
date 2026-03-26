package net.leoch.modules.ops.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.enums.ParameterIn;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.base.Constant;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.result.Result;
import net.leoch.modules.ops.service.IDomainRecordHistoryService;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordHistoryPageReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordHistoryRsp;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("ops/domain-record/history")
@Tag(name = "域名记录操作历史")
@RequiredArgsConstructor
public class DomainRecordHistoryController {

    private final IDomainRecordHistoryService domainRecordHistoryService;

    @GetMapping("page")
    @Operation(summary = "分页")
    @Parameters({
            @Parameter(name = Constant.PAGE, description = "当前页码，从1开始", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.LIMIT, description = "每页显示记录数", in = ParameterIn.QUERY, required = true, ref = "int"),
            @Parameter(name = Constant.ORDER_FIELD, description = "排序字段", in = ParameterIn.QUERY, ref = "String"),
            @Parameter(name = Constant.ORDER, description = "排序方式，可选值(asc、desc)", in = ParameterIn.QUERY, ref = "String")
    })
    @SaCheckPermission("ops:domain-record:history")
    public Result<PageData<DomainRecordHistoryRsp>> page(@Parameter(hidden = true) DomainRecordHistoryPageReq request) {
        return new Result<PageData<DomainRecordHistoryRsp>>().ok(domainRecordHistoryService.page(request));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("ops:domain-record:history")
    public Result<DomainRecordHistoryDetailRsp> get(@PathVariable Long id) {
        return new Result<DomainRecordHistoryDetailRsp>().ok(domainRecordHistoryService.get(DomainRecordHistoryIdReq.of(id)));
    }
}
