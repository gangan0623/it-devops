package net.leoch.modules.ops.service;

import com.baomidou.mybatisplus.extension.service.IService;
import net.leoch.common.data.page.PageData;
import net.leoch.modules.ops.entity.NetworkHostEntity;
import net.leoch.modules.ops.vo.req.NetworkHostPageReq;
import net.leoch.modules.ops.vo.req.NetworkHostBackupSaveReq;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceDetailRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostRsp;
import net.leoch.modules.ops.vo.rsp.NetworkInterfaceTrendRsp;
import net.leoch.modules.ops.vo.rsp.NetworkHostBackupDetailRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;

import java.util.List;

public interface INetworkHostService extends IService<NetworkHostEntity> {
    PageData<NetworkHostRsp> page(NetworkHostPageReq request);

    OpsHostStatusSummaryRsp summary(NetworkHostPageReq request);

    NetworkHostBackupDetailRsp backupDetail(Long networkHostId);

    void saveBackupDetail(NetworkHostBackupSaveReq request);

    List<NetworkInterfaceDetailRsp> interfaceDetails(String instance, boolean includeZeroTraffic);

    NetworkInterfaceTrendRsp interfaceTrend(String instance, String interfaceIndex, Long timeFrom, Long timeTill);
}
