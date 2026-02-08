package net.leoch.modules.ops.service;

import net.leoch.modules.ops.vo.rsp.DashboardSummaryRsp;

/**
 * 工作台统计
 */
public interface IDashboardService {
    DashboardSummaryRsp summary();
}
