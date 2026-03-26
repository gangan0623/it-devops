package net.leoch.modules.ops.service.impl;

import cn.hutool.core.util.ObjectUtil;
import cn.hutool.json.JSONUtil;
import net.leoch.modules.ops.entity.DomainRecordHistoryDetailEntity;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@Component
public class DomainRecordDiffBuilder {

    public String toSnapshotJson(DomainRecordDetailRsp detail) {
        if (detail == null) {
            return null;
        }
        return JSONUtil.toJsonStr(buildSnapshot(detail));
    }

    public List<DomainRecordHistoryDetailEntity> buildUpdateDiffs(Long historyId,
                                                                  DomainRecordDetailRsp before,
                                                                  DomainRecordDetailRsp after) {
        List<DomainRecordHistoryDetailEntity> diffs = new ArrayList<>();
        if (before == null || after == null) {
            return diffs;
        }
        addDiff(diffs, historyId, "projectName", "项目名称", before.getProjectName(), after.getProjectName());
        addDiff(diffs, historyId, "domainName", "域名", before.getDomainName(), after.getDomainName());
        addDiff(diffs, historyId, "adEnabled", "是否走应用交付", before.getAdEnabled(), after.getAdEnabled());
        addDiff(diffs, historyId, "internalEnabled", "是否启用内网解析", before.getInternalEnabled(), after.getInternalEnabled());
        addDiff(diffs, historyId, "externalEnabled", "是否启用外网解析", before.getExternalEnabled(), after.getExternalEnabled());
        addDiff(diffs, historyId, "externalAddress", "外网访问地址", before.getExternalAddress(), after.getExternalAddress());
        addDiff(diffs, historyId, "description", "描述", before.getDescription(), after.getDescription());
        addDiff(diffs, historyId, "projectOwner", "项目负责人", before.getProjectOwner(), after.getProjectOwner());
        addDiff(diffs, historyId, "applyTime", "申请时间", before.getApplyTime(), after.getApplyTime());
        addDiff(diffs, historyId, "remark", "备注", before.getRemark(), after.getRemark());
        addDiff(diffs, historyId, "delivery", "应用交付", before.getDelivery(), after.getDelivery());
        addDiff(diffs, historyId, "delivery.nodes", "节点池明细", before.getDelivery() == null ? null : before.getDelivery().getNodes(),
                after.getDelivery() == null ? null : after.getDelivery().getNodes());
        addDiff(diffs, historyId, "dns.internal", "内网解析", before.getDnsInternal(), after.getDnsInternal());
        addDiff(diffs, historyId, "dns.external", "外网解析", before.getDnsExternal(), after.getDnsExternal());
        addDiff(diffs, historyId, "firewall.mapping", "防火墙映射", before.getFirewallMapping(), after.getFirewallMapping());
        return diffs;
    }

    private Map<String, Object> buildSnapshot(DomainRecordDetailRsp detail) {
        Map<String, Object> snapshot = new LinkedHashMap<>();
        Map<String, Object> base = new LinkedHashMap<>();
        base.put("id", detail.getId());
        base.put("projectName", detail.getProjectName());
        base.put("domainName", detail.getDomainName());
        base.put("adEnabled", detail.getAdEnabled());
        base.put("internalEnabled", detail.getInternalEnabled());
        base.put("externalEnabled", detail.getExternalEnabled());
        base.put("externalAddress", detail.getExternalAddress());
        base.put("description", detail.getDescription());
        base.put("projectOwner", detail.getProjectOwner());
        base.put("applyTime", detail.getApplyTime());
        base.put("remark", detail.getRemark());
        snapshot.put("base", base);
        snapshot.put("delivery", detail.getDelivery());
        snapshot.put("dnsInternal", detail.getDnsInternal());
        snapshot.put("dnsExternal", detail.getDnsExternal());
        snapshot.put("firewallMapping", detail.getFirewallMapping());
        return snapshot;
    }

    private void addDiff(List<DomainRecordHistoryDetailEntity> diffs,
                         Long historyId,
                         String fieldCode,
                         String fieldName,
                         Object before,
                         Object after) {
        String beforeValue = stringify(before);
        String afterValue = stringify(after);
        if (ObjectUtil.equal(beforeValue, afterValue)) {
            return;
        }
        DomainRecordHistoryDetailEntity entity = new DomainRecordHistoryDetailEntity();
        entity.setHistoryId(historyId);
        entity.setFieldCode(fieldCode);
        entity.setFieldName(fieldName);
        entity.setBeforeValue(beforeValue);
        entity.setAfterValue(afterValue);
        diffs.add(entity);
    }

    private String stringify(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof CharSequence || value instanceof Number || value instanceof Boolean) {
            return String.valueOf(value);
        }
        return JSONUtil.toJsonStr(value);
    }
}
