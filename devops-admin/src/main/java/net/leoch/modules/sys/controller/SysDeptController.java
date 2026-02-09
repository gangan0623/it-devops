

package net.leoch.modules.sys.controller;

import cn.dev33.satoken.annotation.SaCheckPermission;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import net.leoch.common.core.annotation.LogOperation;
import net.leoch.common.support.utils.Result;
import net.leoch.modules.sys.vo.rsp.SysDeptRsp;
import net.leoch.modules.sys.vo.req.SysDeptReq;
import net.leoch.modules.sys.service.ISysDeptService;
import org.springframework.web.bind.annotation.*;

import java.util.HashMap;
import java.util.List;

/**
 * 部门管理
 *
 * @author Taohongqiang
 */
@RestController
@RequestMapping("/sys/dept")
@Tag(name = "部门管理")
@RequiredArgsConstructor
public class SysDeptController {
    private final ISysDeptService sysDeptService;

    @GetMapping("list")
    @Operation(summary = "列表")
    @SaCheckPermission("sys:dept:list")
    public Result<List<SysDeptRsp>> list() {
        return new Result<List<SysDeptRsp>>().ok(sysDeptService.list(new HashMap<>(1)));
    }

    @GetMapping("{id}")
    @Operation(summary = "信息")
    @SaCheckPermission("sys:dept:info")
    public Result<SysDeptRsp> get(@PathVariable("id") Long id) {
        return new Result<SysDeptRsp>().ok(sysDeptService.get(id));
    }

    @PostMapping
    @Operation(summary = "保存")
    @LogOperation("保存")
    @SaCheckPermission("sys:dept:save")
    public Result<Object> save(@RequestBody SysDeptReq dto) {
        sysDeptService.save(dto);
        return new Result<>();
    }

    @PutMapping
    @Operation(summary = "修改")
    @LogOperation("修改")
    @SaCheckPermission("sys:dept:update")
    public Result<Object> update(@RequestBody SysDeptReq dto) {
        sysDeptService.update(dto);
        return new Result<>();
    }

    @DeleteMapping("{id}")
    @Operation(summary = "删除")
    @LogOperation("删除")
    @SaCheckPermission("sys:dept:delete")
    public Result<Object> delete(@PathVariable("id") Long id) {
        sysDeptService.delete(id);
        return new Result<>();
    }

}
