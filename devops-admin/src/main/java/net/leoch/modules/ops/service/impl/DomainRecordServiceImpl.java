package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.alibaba.excel.EasyExcel;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.excel.DomainRecordExcel;
import net.leoch.common.integration.excel.template.DomainRecordImportExcel;
import net.leoch.common.integration.security.SecurityUser;
import net.leoch.common.utils.excel.DomainRecordExcelUtils;
import net.leoch.modules.ops.entity.DomainDeliveryEntity;
import net.leoch.modules.ops.entity.DomainDeliveryNodeEntity;
import net.leoch.modules.ops.entity.DomainDnsExternalEntity;
import net.leoch.modules.ops.entity.DomainDnsInternalEntity;
import net.leoch.modules.ops.entity.DomainFirewallMappingEntity;
import net.leoch.modules.ops.entity.DomainRecordEntity;
import net.leoch.modules.ops.entity.DomainRecordHistoryDetailEntity;
import net.leoch.modules.ops.entity.DomainRecordHistoryEntity;
import net.leoch.modules.ops.mapper.DomainDeliveryMapper;
import net.leoch.modules.ops.mapper.DomainDeliveryNodeMapper;
import net.leoch.modules.ops.mapper.DomainDnsExternalMapper;
import net.leoch.modules.ops.mapper.DomainDnsInternalMapper;
import net.leoch.modules.ops.mapper.DomainFirewallMappingMapper;
import net.leoch.modules.ops.mapper.DomainRecordHistoryDetailMapper;
import net.leoch.modules.ops.mapper.DomainRecordHistoryMapper;
import net.leoch.modules.ops.mapper.DomainRecordMapper;
import net.leoch.modules.ops.service.IDomainRecordService;
import net.leoch.modules.ops.vo.req.*;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import net.leoch.modules.ops.vo.rsp.OpsHostStatusSummaryRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

@Slf4j
@Service
@RequiredArgsConstructor
public class DomainRecordServiceImpl extends ServiceImpl<DomainRecordMapper, DomainRecordEntity> implements IDomainRecordService {

    private final DomainDeliveryMapper domainDeliveryMapper;
    private final DomainDeliveryNodeMapper domainDeliveryNodeMapper;
    private final DomainDnsInternalMapper domainDnsInternalMapper;
    private final DomainDnsExternalMapper domainDnsExternalMapper;
    private final DomainFirewallMappingMapper domainFirewallMappingMapper;
    private final DomainRecordHistoryMapper domainRecordHistoryMapper;
    private final DomainRecordHistoryDetailMapper domainRecordHistoryDetailMapper;
    private final DomainRecordDiffBuilder diffBuilder;

    @Override
    public PageData<DomainRecordRsp> page(DomainRecordPageReq request) {
        Page<DomainRecordRsp> page = request.buildPage();
        IPage<DomainRecordRsp> result = this.baseMapper.selectPageSummary(page, request);
        return new PageData<>(result.getRecords(), result.getTotal());
    }

    @Override
    public OpsHostStatusSummaryRsp summary(DomainRecordPageReq request) {
        LambdaQueryWrapper<DomainRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.select(DomainRecordEntity::getStatus, DomainRecordEntity::getOnlineStatus);
        List<DomainRecordEntity> list = this.list(wrapper);
        OpsHostStatusSummaryRsp summary = new OpsHostStatusSummaryRsp();
        summary.setTotalCount((long) list.size());
        for (DomainRecordEntity item : list) {
            if (item == null) {
                continue;
            }
            Integer status = item.getStatus();
            if (Integer.valueOf(1).equals(status)) {
                summary.setEnabledCount(summary.getEnabledCount() + 1);
            } else if (Integer.valueOf(0).equals(status)) {
                summary.setDisabledCount(summary.getDisabledCount() + 1);
            }
            Boolean onlineStatus = item.getOnlineStatus();
            if (Boolean.TRUE.equals(onlineStatus)) {
                summary.setOnlineCount(summary.getOnlineCount() + 1);
            } else if (Boolean.FALSE.equals(onlineStatus)) {
                summary.setOfflineCount(summary.getOfflineCount() + 1);
            } else {
                summary.setUnknownCount(summary.getUnknownCount() + 1);
            }
        }
        return summary;
    }

    @Override
    public DomainRecordDetailRsp get(DomainRecordIdReq request) {
        if (request == null || request.getId() == null) {
            return null;
        }
        return this.baseMapper.selectDetailById(request.getId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void save(DomainRecordSaveReq request) {
        ValidatorUtils.validateEntity(request, AddGroup.class, DefaultGroup.class);
        validateBusinessRules(request.getAdEnabled(), request.getInternalEnabled(), request.getExternalEnabled(),
                request.getDelivery(), request.getDnsInternal(), request.getDnsExternal(), request.getFirewallMapping(),
                request.getDomainName(), null);

        DomainRecordEntity entity = new DomainRecordEntity();
        fillBaseEntity(entity, request.getProjectName(), request.getDomainName(), request.getAreaName(), request.getGroupName(),
                request.getSiteLocation(), request.getStatus(), request.getAdEnabled(), request.getInternalEnabled(),
                request.getExternalEnabled(), request.getDescription(), request.getProjectOwner(),
                request.getApplyTime(), request.getRemark(), request.getApiUrl());
        this.save(entity);
        saveOrUpdateChildren(entity.getId(), request.getAdEnabled(), request.getInternalEnabled(), request.getExternalEnabled(),
                request.getDelivery(), request.getDnsInternal(), request.getDnsExternal(), request.getFirewallMapping());

        DomainRecordDetailRsp after = loadDetail(entity.getId());
        saveHistory(entity.getId(), "CREATE", buildSummary("新增", request.getDomainName()), null, after, List.of());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(DomainRecordUpdateReq request) {
        ValidatorUtils.validateEntity(request, UpdateGroup.class, DefaultGroup.class);
        DomainRecordEntity existing = this.getById(request.getId());
        if (existing == null) {
            throw new ServiceException("域名记录不存在");
        }
        validateBusinessRules(request.getAdEnabled(), request.getInternalEnabled(), request.getExternalEnabled(),
                request.getDelivery(), request.getDnsInternal(), request.getDnsExternal(), request.getFirewallMapping(),
                request.getDomainName(), request.getId());

        DomainRecordDetailRsp before = loadDetail(request.getId());

        fillBaseEntity(existing, request.getProjectName(), request.getDomainName(), request.getAreaName(), request.getGroupName(),
                request.getSiteLocation(), request.getStatus(), request.getAdEnabled(), request.getInternalEnabled(),
                request.getExternalEnabled(), request.getDescription(), request.getProjectOwner(),
                request.getApplyTime(), request.getRemark(), request.getApiUrl());
        this.updateById(existing);
        saveOrUpdateChildren(existing.getId(), request.getAdEnabled(), request.getInternalEnabled(), request.getExternalEnabled(),
                request.getDelivery(), request.getDnsInternal(), request.getDnsExternal(), request.getFirewallMapping());

        DomainRecordDetailRsp after = loadDetail(existing.getId());
        DomainRecordHistoryEntity history = saveHistory(existing.getId(), "UPDATE", buildSummary("修改", request.getDomainName()),
                before, after, List.of());
        List<DomainRecordHistoryDetailEntity> diffs = diffBuilder.buildUpdateDiffs(history.getId(), before, after);
        if (CollUtil.isNotEmpty(diffs)) {
            diffs.forEach(domainRecordHistoryDetailMapper::insert);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void delete(DomainRecordDeleteReq request) {
        if (request == null || request.getIds() == null || request.getIds().length == 0) {
            throw new ServiceException("ID不能为空");
        }
        for (Long id : request.getIds()) {
            DomainRecordEntity existing = this.getById(id);
            if (existing == null) {
                continue;
            }
            DomainRecordDetailRsp before = loadDetail(id);
            deleteChildren(id);
            this.removeById(id);
            saveHistory(id, "DELETE", buildSummary("删除", existing.getDomainName()), before, null, List.of());
        }
    }

    private void validateBusinessRules(Integer adEnabled,
                                       Integer internalEnabled,
                                       Integer externalEnabled,
                                       Object delivery,
                                       Object dnsInternal,
                                       Object dnsExternal,
                                       Object firewallMapping,
                                       String domainName,
                                       Long excludeId) {
        if (!Integer.valueOf(1).equals(internalEnabled) && !Integer.valueOf(1).equals(externalEnabled)) {
            throw new ServiceException("内网解析和外网解析至少启用一个");
        }
        if (existsDomainName(domainName, excludeId)) {
            throw new ServiceException("域名已存在");
        }
        if (Integer.valueOf(1).equals(adEnabled) && delivery == null) {
            throw new ServiceException("启用应用交付时必须填写应用交付信息");
        }
        if (Integer.valueOf(1).equals(internalEnabled) && dnsInternal == null) {
            throw new ServiceException("启用内网解析时必须填写内网解析信息");
        }
        if (Integer.valueOf(1).equals(externalEnabled) && dnsExternal == null) {
            throw new ServiceException("启用外网解析时必须填写外网解析信息");
        }
        if (Integer.valueOf(1).equals(adEnabled) && Integer.valueOf(1).equals(externalEnabled) && firewallMapping == null) {
            throw new ServiceException("启用应用交付且启用外网解析时必须填写防火墙映射信息");
        }
    }

    private boolean existsDomainName(String domainName, Long excludeId) {
        LambdaQueryWrapper<DomainRecordEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainRecordEntity::getDomainName, domainName);
        wrapper.ne(excludeId != null, DomainRecordEntity::getId, excludeId);
        return this.count(wrapper) > 0;
    }

    private void fillBaseEntity(DomainRecordEntity entity,
                                String projectName,
                                String domainName,
                                String areaName,
                                String groupName,
                                String siteLocation,
                                Integer status,
                                Integer adEnabled,
                                Integer internalEnabled,
                                Integer externalEnabled,
                                String description,
                                String projectOwner,
                                Date applyTime,
                                String remark,
                                String apiUrl) {
        entity.setProjectName(projectName);
        entity.setDomainName(domainName);
        entity.setAreaName(areaName);
        entity.setGroupName(groupName);
        entity.setSiteLocation(siteLocation);
        entity.setStatus(status);
        entity.setAdEnabled(adEnabled);
        entity.setInternalEnabled(internalEnabled);
        entity.setExternalEnabled(externalEnabled);
        entity.setDescription(description);
        entity.setProjectOwner(projectOwner);
        entity.setApplyTime(applyTime);
        entity.setRemark(remark);
        String normalizedApiUrl = StrUtil.trim(apiUrl);
        entity.setApiUrl(StrUtil.isBlank(normalizedApiUrl) ? null : normalizedApiUrl);
    }

    private void saveOrUpdateChildren(Long domainRecordId,
                                      Integer adEnabled,
                                      Integer internalEnabled,
                                      Integer externalEnabled,
                                      Object deliveryObj,
                                      Object dnsInternalObj,
                                      Object dnsExternalObj,
                                      Object firewallMappingObj) {
        if (Integer.valueOf(1).equals(adEnabled) && deliveryObj != null) {
            saveOrUpdateDelivery(domainRecordId, deliveryObj);
        } else {
            deleteDelivery(domainRecordId);
        }

        if (Integer.valueOf(1).equals(internalEnabled) && dnsInternalObj != null) {
            saveOrUpdateInternalDns(domainRecordId, dnsInternalObj, Integer.valueOf(1).equals(adEnabled) ? "AD" : "DIRECT");
        } else {
            deleteInternalDns(domainRecordId);
        }

        if (Integer.valueOf(1).equals(externalEnabled) && dnsExternalObj != null) {
            saveOrUpdateExternalDns(domainRecordId, dnsExternalObj, Integer.valueOf(1).equals(adEnabled) ? "AD" : "DIRECT");
        } else {
            deleteExternalDns(domainRecordId);
        }

        if (Integer.valueOf(1).equals(externalEnabled) && Integer.valueOf(1).equals(adEnabled) && firewallMappingObj != null) {
            saveOrUpdateFirewall(domainRecordId, firewallMappingObj, deliveryObj);
        } else {
            deleteFirewall(domainRecordId);
        }
    }

    private void saveOrUpdateDelivery(Long domainRecordId, Object payload) {
        DomainDeliveryEntity entity = getDelivery(domainRecordId);
        if (entity == null) {
            entity = new DomainDeliveryEntity();
            entity.setDomainRecordId(domainRecordId);
        }
        String json = JSONUtil.toJsonStr(payload);
        DomainRecordSaveReq.DeliveryReq delivery = JSONUtil.toBean(json, DomainRecordSaveReq.DeliveryReq.class);
        entity.setVirtualServiceName(delivery.getVirtualServiceName());
        entity.setVirtualServiceIp(delivery.getVirtualServiceIp());
        entity.setVirtualServicePort(delivery.getVirtualServicePort());
        entity.setVirtualServiceProtocol(delivery.getVirtualServiceProtocol());
        entity.setPoolName(delivery.getPoolName());
        entity.setRemark(delivery.getRemark());
        entity.setExternalVirtualServiceName(delivery.getExternalVirtualServiceName());
        entity.setExternalVirtualServiceIp(delivery.getExternalVirtualServiceIp());
        entity.setExternalVirtualServicePort(delivery.getExternalVirtualServicePort());
        entity.setExternalVirtualServiceProtocol(delivery.getExternalVirtualServiceProtocol());
        if (entity.getId() == null) {
            domainDeliveryMapper.insert(entity);
        } else {
            domainDeliveryMapper.updateById(entity);
        }
        replaceDeliveryNodes(entity.getId(), delivery.getNodes());
    }

    private void replaceDeliveryNodes(Long deliveryId, List<DomainDeliveryNodeReq> nodes) {
        LambdaQueryWrapper<DomainDeliveryNodeEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainDeliveryNodeEntity::getDomainDeliveryId, deliveryId);
        domainDeliveryNodeMapper.delete(wrapper);
        if (CollUtil.isEmpty(nodes)) {
            throw new ServiceException("节点池明细不能为空");
        }
        for (DomainDeliveryNodeReq node : nodes) {
            DomainDeliveryNodeEntity entity = new DomainDeliveryNodeEntity();
            entity.setDomainDeliveryId(deliveryId);
            entity.setNodeIp(node.getNodeIp());
            entity.setNodePort(node.getNodePort());
            entity.setSort(node.getSort());
            entity.setRemark(node.getRemark());
            domainDeliveryNodeMapper.insert(entity);
        }
    }

    private void saveOrUpdateInternalDns(Long domainRecordId, Object payload, String resolveMode) {
        DomainDnsInternalEntity entity = getInternalDns(domainRecordId);
        if (entity == null) {
            entity = new DomainDnsInternalEntity();
            entity.setDomainRecordId(domainRecordId);
        }
        DomainRecordSaveReq.DnsInternalReq req = JSONUtil.toBean(JSONUtil.toJsonStr(payload), DomainRecordSaveReq.DnsInternalReq.class);
        validateResolveMode(resolveMode, req.getResolveMode(), "内网解析");
        entity.setResolveMode(req.getResolveMode());
        entity.setDnsTargetIp(req.getDnsTargetIp());
        entity.setRemark(req.getRemark());
        if (entity.getId() == null) {
            domainDnsInternalMapper.insert(entity);
        } else {
            domainDnsInternalMapper.updateById(entity);
        }
    }

    private void saveOrUpdateExternalDns(Long domainRecordId, Object payload, String resolveMode) {
        DomainDnsExternalEntity entity = getExternalDns(domainRecordId);
        if (entity == null) {
            entity = new DomainDnsExternalEntity();
            entity.setDomainRecordId(domainRecordId);
        }
        DomainRecordSaveReq.DnsExternalReq req = JSONUtil.toBean(JSONUtil.toJsonStr(payload), DomainRecordSaveReq.DnsExternalReq.class);
        validateResolveMode(resolveMode, req.getResolveMode(), "外网解析");
        entity.setResolveMode(req.getResolveMode());
        entity.setRecordValue(req.getRecordValue());
        entity.setRemark(req.getRemark());
        if (entity.getId() == null) {
            domainDnsExternalMapper.insert(entity);
        } else {
            domainDnsExternalMapper.updateById(entity);
        }
    }

    private void saveOrUpdateFirewall(Long domainRecordId, Object payload, Object deliveryPayload) {
        DomainFirewallMappingEntity entity = getFirewall(domainRecordId);
        if (entity == null) {
            entity = new DomainFirewallMappingEntity();
            entity.setDomainRecordId(domainRecordId);
        }
        DomainRecordSaveReq.FirewallMappingReq req = JSONUtil.toBean(JSONUtil.toJsonStr(payload), DomainRecordSaveReq.FirewallMappingReq.class);
        DomainRecordSaveReq.DeliveryReq delivery = deliveryPayload == null ? null : JSONUtil.toBean(JSONUtil.toJsonStr(deliveryPayload), DomainRecordSaveReq.DeliveryReq.class);
        if (delivery != null) {
            String expectedIp = StrUtil.isNotBlank(delivery.getExternalVirtualServiceIp()) ? delivery.getExternalVirtualServiceIp() : delivery.getVirtualServiceIp();
            Integer expectedPort = delivery.getExternalVirtualServicePort() != null ? delivery.getExternalVirtualServicePort() : delivery.getVirtualServicePort();
            if (!StrUtil.equals(req.getInternalIp(), expectedIp)
                    || !ObjectUtil.equal(req.getInternalPort(), expectedPort)) {
                throw new ServiceException("防火墙内部IP和端口必须与外网虚拟服务IP和端口一致");
            }
        }
        entity.setPublicIp(req.getPublicIp());
        entity.setExternalPort(req.getExternalPort());
        entity.setInternalIp(req.getInternalIp());
        entity.setInternalPort(req.getInternalPort());
        entity.setMappingDesc(req.getMappingDesc());
        if (entity.getId() == null) {
            domainFirewallMappingMapper.insert(entity);
        } else {
            domainFirewallMappingMapper.updateById(entity);
        }
    }

    private DomainRecordHistoryEntity saveHistory(Long domainRecordId,
                                                  String operationType,
                                                  String summary,
                                                  DomainRecordDetailRsp before,
                                                  DomainRecordDetailRsp after,
                                                  List<DomainRecordHistoryDetailEntity> details) {
        DomainRecordHistoryEntity history = new DomainRecordHistoryEntity();
        history.setDomainRecordId(domainRecordId);
        history.setOperationType(operationType);
        history.setOperatorId(SecurityUser.getUserId());
        history.setOperatorName(SecurityUser.getUser() == null ? null : SecurityUser.getUser().getRealName());
        history.setOperationTime(new Date());
        history.setOperationSummary(summary);
        history.setSnapshotBefore(diffBuilder.toSnapshotJson(before));
        history.setSnapshotAfter(diffBuilder.toSnapshotJson(after));
        domainRecordHistoryMapper.insert(history);
        if (CollUtil.isNotEmpty(details)) {
            details.forEach(item -> {
                item.setHistoryId(history.getId());
                domainRecordHistoryDetailMapper.insert(item);
            });
        }
        return history;
    }

    private String buildSummary(String action, String domainName) {
        return action + "域名记录: " + domainName;
    }

    private void deleteChildren(Long domainRecordId) {
        deleteFirewall(domainRecordId);
        deleteExternalDns(domainRecordId);
        deleteInternalDns(domainRecordId);
        deleteDelivery(domainRecordId);
    }

    private void deleteDelivery(Long domainRecordId) {
        DomainDeliveryEntity delivery = getDelivery(domainRecordId);
        if (delivery != null) {
            LambdaQueryWrapper<DomainDeliveryNodeEntity> nodeWrapper = new LambdaQueryWrapper<>();
            nodeWrapper.eq(DomainDeliveryNodeEntity::getDomainDeliveryId, delivery.getId());
            domainDeliveryNodeMapper.delete(nodeWrapper);
            domainDeliveryMapper.deleteById(delivery.getId());
        }
    }

    private void deleteInternalDns(Long domainRecordId) {
        DomainDnsInternalEntity entity = getInternalDns(domainRecordId);
        if (entity != null) {
            domainDnsInternalMapper.deleteById(entity.getId());
        }
    }

    private void deleteExternalDns(Long domainRecordId) {
        DomainDnsExternalEntity entity = getExternalDns(domainRecordId);
        if (entity != null) {
            domainDnsExternalMapper.deleteById(entity.getId());
        }
    }

    private void deleteFirewall(Long domainRecordId) {
        DomainFirewallMappingEntity entity = getFirewall(domainRecordId);
        if (entity != null) {
            domainFirewallMappingMapper.deleteById(entity.getId());
        }
    }

    private DomainDeliveryEntity getDelivery(Long domainRecordId) {
        LambdaQueryWrapper<DomainDeliveryEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainDeliveryEntity::getDomainRecordId, domainRecordId).last("limit 1");
        return domainDeliveryMapper.selectOne(wrapper);
    }

    private DomainDnsInternalEntity getInternalDns(Long domainRecordId) {
        LambdaQueryWrapper<DomainDnsInternalEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainDnsInternalEntity::getDomainRecordId, domainRecordId).last("limit 1");
        return domainDnsInternalMapper.selectOne(wrapper);
    }

    private DomainDnsExternalEntity getExternalDns(Long domainRecordId) {
        LambdaQueryWrapper<DomainDnsExternalEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainDnsExternalEntity::getDomainRecordId, domainRecordId).last("limit 1");
        return domainDnsExternalMapper.selectOne(wrapper);
    }

    private DomainFirewallMappingEntity getFirewall(Long domainRecordId) {
        LambdaQueryWrapper<DomainFirewallMappingEntity> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(DomainFirewallMappingEntity::getDomainRecordId, domainRecordId).last("limit 1");
        return domainFirewallMappingMapper.selectOne(wrapper);
    }

    private DomainRecordDetailRsp loadDetail(Long id) {
        return this.baseMapper.selectDetailById(id);
    }

    private void validateResolveMode(String expected, String actual, String fieldName) {
        if (!StrUtil.equals(expected, actual)) {
            throw new ServiceException(fieldName + "解析方式与当前场景不一致");
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void importExcel(
            DomainRecordImportReq request) throws Exception {
        List<DomainRecordImportExcel> dataList = EasyExcel.read(request.getFile().getInputStream())
            .head(DomainRecordImportExcel.class)
            .sheet()
            .doReadSync();
        if (CollUtil.isEmpty(dataList)) {
            throw new ServiceException("Excel 文件为空");
        }
        // 按域名分组，同一域名的多行合并为一条记录（节点池多行）
        // 跳过以"示例"开头的域名（模板示例数据）和空域名行（节点池多行）
        List<List<DomainRecordImportExcel>> grouped = groupImportRows(dataList);
        int successCount = 0;
        int failCount = 0;
        StringBuilder errorMsg = new StringBuilder();
        for (List<DomainRecordImportExcel> group : grouped) {
            String domainName = group.get(0).getDomainName();
            // 行号 = 在原始数据列表中的位置 + 表头行 + 分区标题行 + 1-based
            int startRow = dataList.indexOf(group.get(0)) + 3;
            try {
                DomainRecordSaveReq saveReq = buildSaveRequestFromGroup(group);
                ValidatorUtils.validateEntity(saveReq, AddGroup.class, DefaultGroup.class);
                save(saveReq);
                successCount++;
            } catch (ServiceException e) {
                failCount++;
                errorMsg.append("第").append(startRow).append("-").append(startRow + group.size() - 1)
                    .append("行[").append(domainName).append("]: ").append(e.getMessage()).append("; ");
            }
        }
        if (failCount > 0) {
            throw new ServiceException("导入完成: 成功 " + successCount + " 条, 失败 " + failCount + " 条。\n" + errorMsg);
        }
    }

    @Override
    public void template(HttpServletResponse response) throws Exception {
        List<DomainRecordImportExcel> examples = buildTemplateExamples();
        List<int[]> mergeRanges = buildTemplateMergeRanges();
        DomainRecordExcelUtils.exportTemplateWithStyle(response, "域名记录导入模板", "域名记录",
            examples, DomainRecordImportExcel.class, mergeRanges);
    }

    @Override
    public void export(DomainRecordPageReq request, HttpServletResponse response) throws Exception {
        Page<DomainRecordRsp> page = request.buildPage();
        IPage<DomainRecordRsp> result = this.baseMapper.selectPageSummary(page, request);
        List<DomainRecordRsp> rspList = result.getRecords();

        // 收集走AD的记录ID，批量查询节点
        List<Long> adIds = rspList.stream()
            .filter(r -> Integer.valueOf(1).equals(r.getAdEnabled()))
            .map(DomainRecordRsp::getId)
            .collect(java.util.stream.Collectors.toList());
        java.util.Map<Long, List<DomainRecordDetailRsp.DeliveryNodeRsp>> nodesMap = batchLoadNodes(adIds);

        // 展开为多行（每个节点一行）
        List<DomainRecordExcel> excelList = new java.util.ArrayList<>();
        List<int[]> mergeRanges = new java.util.ArrayList<>();
        int dataRowStart = 2; // 第0行=分区标题，第1行=表头
        int currentRow = dataRowStart;
        for (DomainRecordRsp rsp : rspList) {
            List<DomainRecordDetailRsp.DeliveryNodeRsp> nodes = nodesMap.getOrDefault(rsp.getId(), java.util.Collections.emptyList());
            int rowCount = Math.max(1, nodes.size());
            for (int i = 0; i < rowCount; i++) {
                DomainRecordExcel row = new DomainRecordExcel();
                row.setProjectName(rsp.getProjectName());
                row.setDomainName(rsp.getDomainName());
                row.setAreaName(rsp.getAreaName());
                row.setGroupName(rsp.getGroupName());
                row.setSiteLocation(rsp.getSiteLocation());
                row.setStatus(rsp.getStatus());
                row.setProjectOwner(rsp.getProjectOwner());
                row.setApplyTime(rsp.getApplyTime());
                row.setApiUrl(rsp.getApiUrl());
                row.setDescription(rsp.getDescription());
                row.setRemark(rsp.getRemark());
                row.setAdEnabled(rsp.getAdEnabled());
                row.setVirtualServiceName(rsp.getVirtualServiceName());
                row.setVirtualServiceIp(rsp.getVirtualServiceIp());
                row.setVirtualServicePort(rsp.getVirtualServicePort());
                row.setVirtualServiceProtocol(rsp.getVirtualServiceProtocol());
                row.setPoolName(rsp.getPoolName());
                row.setExternalVirtualServiceName(rsp.getExternalVirtualServiceName());
                row.setExternalVirtualServiceIp(rsp.getExternalVirtualServiceIp());
                row.setExternalVirtualServicePort(rsp.getExternalVirtualServicePort());
                row.setExternalVirtualServiceProtocol(rsp.getExternalVirtualServiceProtocol());
                row.setInternalEnabled(rsp.getInternalEnabled());
                row.setInternalTargetIp(rsp.getInternalTargetIp());
                row.setExternalEnabled(rsp.getExternalEnabled());
                row.setExternalRecordValue(rsp.getExternalRecordValue());
                row.setFmPublicIp(rsp.getFmPublicIp());
                row.setFmExternalPort(rsp.getFmExternalPort());
                row.setFmInternalIp(rsp.getFmInternalIp());
                row.setFmInternalPort(rsp.getFmInternalPort());
                // 节点明细（仅第一个节点行之后填写）
                if (i < nodes.size()) {
                    row.setNodeIp(nodes.get(i).getNodeIp());
                    row.setNodePort(nodes.get(i).getNodePort());
                    row.setNodeSort(nodes.get(i).getSort());
                }
                excelList.add(row);
            }
            // 如果有多个节点，需要合并公共列
            if (rowCount > 1) {
                int endRow = currentRow + rowCount - 1;
                // 合并公共列：0-22（基础信息 + 高级策略 + 应用交付）和 26-31（解析配置 + 防火墙）
                for (int col : new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 26, 27, 28, 29, 30, 31}) {
                    mergeRanges.add(new int[]{currentRow, endRow, col, col});
                }
            }
            currentRow += rowCount;
        }

        DomainRecordExcelUtils.exportWithStyle(response, "域名记录", "域名记录",
            excelList, DomainRecordExcel.class, mergeRanges);
    }

    /**
     * 批量加载节点池明细
     */
    private java.util.Map<Long, List<DomainRecordDetailRsp.DeliveryNodeRsp>> batchLoadNodes(List<Long> adRecordIds) {
        if (CollUtil.isEmpty(adRecordIds)) {
            return java.util.Collections.emptyMap();
        }
        // 查出 delivery id -> domain_record_id
        LambdaQueryWrapper<DomainDeliveryEntity> dWrapper = new LambdaQueryWrapper<>();
        dWrapper.in(DomainDeliveryEntity::getDomainRecordId, adRecordIds);
        List<DomainDeliveryEntity> deliveries = domainDeliveryMapper.selectList(dWrapper);

        java.util.Map<Long, List<DomainRecordDetailRsp.DeliveryNodeRsp>> result = new java.util.HashMap<>();
        for (DomainDeliveryEntity delivery : deliveries) {
            List<DomainRecordDetailRsp.DeliveryNodeRsp> nodes = domainDeliveryNodeMapper
                .selectByDomainDeliveryId(delivery.getId());
            result.put(delivery.getDomainRecordId(), nodes);
        }
        return result;
    }

    /**
     * 按域名分组导入行，跳过以"示例"开头的域名（模板示例数据）
     */
    private List<List<DomainRecordImportExcel>> groupImportRows(List<DomainRecordImportExcel> dataList) {
        java.util.LinkedHashMap<String, List<DomainRecordImportExcel>> map = new java.util.LinkedHashMap<>();
        for (DomainRecordImportExcel row : dataList) {
            String domainName = StrUtil.blankToDefault(row.getDomainName(), "");
            // 跳过模板示例数据
            if (domainName.startsWith("示例")) {
                continue;
            }
            // 跳过空域名行（节点池多行中，节点行的域名列为空）
            if (StrUtil.isBlank(domainName)) {
                continue;
            }
            map.computeIfAbsent(domainName, k -> new java.util.ArrayList<>()).add(row);
        }
        return new java.util.ArrayList<>(map.values());
    }

    /**
     * 从分组行构建保存请求
     */
    private DomainRecordSaveReq buildSaveRequestFromGroup(List<DomainRecordImportExcel> group) {
        DomainRecordImportExcel first = group.get(0);
        int adEnabled = parseSwitch(first.getAdEnabled());
        int internalEnabled = parseSwitch(first.getInternalEnabled());
        int externalEnabled = parseSwitch(first.getExternalEnabled());

        DomainRecordSaveReq req = new DomainRecordSaveReq();
        req.setProjectName(first.getProjectName());
        req.setDomainName(first.getDomainName());
        req.setAreaName(first.getAreaName());
        req.setGroupName(first.getGroupName());
        req.setSiteLocation(first.getSiteLocation());
        req.setStatus(parseStatus(first.getStatus()));
        req.setProjectOwner(first.getProjectOwner());
        req.setApplyTime(first.getApplyTime());
        req.setApiUrl(first.getApiUrl());
        req.setDescription(first.getDescription());
        req.setRemark(first.getRemark());
        req.setAdEnabled(adEnabled);
        req.setInternalEnabled(internalEnabled);
        req.setExternalEnabled(externalEnabled);

        if (adEnabled == 1) {
            DomainRecordSaveReq.DeliveryReq delivery = new DomainRecordSaveReq.DeliveryReq();
            delivery.setVirtualServiceName(first.getVirtualServiceName());
            delivery.setVirtualServiceIp(first.getVirtualServiceIp());
            delivery.setVirtualServicePort(first.getVirtualServicePort());
            delivery.setVirtualServiceProtocol(first.getVirtualServiceProtocol());
            delivery.setPoolName(first.getPoolName());
            delivery.setExternalVirtualServiceName(first.getExternalVirtualServiceName());
            delivery.setExternalVirtualServiceIp(first.getExternalVirtualServiceIp());
            delivery.setExternalVirtualServicePort(first.getExternalVirtualServicePort());
            delivery.setExternalVirtualServiceProtocol(first.getExternalVirtualServiceProtocol());
            // 从多行提取节点
            List<DomainDeliveryNodeReq> nodes = new java.util.ArrayList<>();
            for (DomainRecordImportExcel row : group) {
                if (StrUtil.isNotBlank(row.getNodeIp())) {
                    DomainDeliveryNodeReq node = new DomainDeliveryNodeReq();
                    node.setNodeIp(row.getNodeIp().trim());
                    node.setNodePort(row.getNodePort());
                    node.setSort(row.getNodeSort() != null ? row.getNodeSort() : nodes.size() + 1);
                    nodes.add(node);
                }
            }
            if (nodes.isEmpty()) {
                throw new ServiceException("启用应用交付时，至少需要一个节点（节点IP不能为空）");
            }
            delivery.setNodes(nodes);
            req.setDelivery(delivery);
        }

        if (internalEnabled == 1) {
            DomainRecordSaveReq.DnsInternalReq internal = new DomainRecordSaveReq.DnsInternalReq();
            internal.setResolveMode(adEnabled == 1 ? "AD" : "DIRECT");
            internal.setDnsTargetIp(first.getInternalTargetIp());
            req.setDnsInternal(internal);
        }

        if (externalEnabled == 1) {
            DomainRecordSaveReq.DnsExternalReq external = new DomainRecordSaveReq.DnsExternalReq();
            external.setResolveMode(adEnabled == 1 ? "AD" : "DIRECT");
            external.setRecordValue(first.getExternalRecordValue());
            req.setDnsExternal(external);
        }

        if (adEnabled == 1 && externalEnabled == 1) {
            DomainRecordSaveReq.FirewallMappingReq firewall = new DomainRecordSaveReq.FirewallMappingReq();
            firewall.setPublicIp(first.getFmPublicIp());
            firewall.setExternalPort(first.getFmExternalPort());
            firewall.setInternalIp(first.getFmInternalIp());
            firewall.setInternalPort(first.getFmInternalPort());
            req.setFirewallMapping(firewall);
        }
        return req;
    }

    /** 是/否 → 1/0，兼容数字和中文 */
    private int parseSwitch(Object value) {
        if (value instanceof Integer) return (Integer) value;
        String s = String.valueOf(value);
        if ("是".equals(s) || "1".equals(s)) return 1;
        return 0;
    }

    /** 启用/禁用 → 1/0，兼容数字和中文 */
    private int parseStatus(Object value) {
        if (value instanceof Integer) return (Integer) value;
        String s = String.valueOf(value);
        if ("启用".equals(s) || "1".equals(s)) return 1;
        return 0;
    }

    private List<DomainRecordImportExcel> buildTemplateExamples() {
        List<DomainRecordImportExcel> list = new java.util.ArrayList<>();

        // 场景1: 走AD + 内网解析（2个节点，展示合并单元格）
        list.add(buildExampleRow1a());
        list.add(buildExampleRow1b());

        // 场景2: 走AD + 外网解析（2个节点）
        list.add(buildExampleRow2a());
        list.add(buildExampleRow2b());

        // 场景3: 走AD + 内网+公网（双向AD，2个节点，展示外网虚拟服务）
        list.add(buildExampleRow3a());
        list.add(buildExampleRow3b());

        // 场景4: 不走AD + 内网解析
        list.add(buildExampleRow4());

        // 场景5: 不走AD + 外网解析
        list.add(buildExampleRow5());

        return list;
    }

    private DomainRecordImportExcel buildExampleRow1a() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setProjectName("示例项目A");
        r.setDomainName("示例-AD仅内网.example.com");
        r.setAreaName("华南");
        r.setGroupName("业务组A");
        r.setSiteLocation("机房A");
        r.setStatus("启用");
        r.setProjectOwner("张三");
        r.setApplyTime(new Date());
        r.setAdEnabled("是");
        r.setVirtualServiceName("vs-app-a");
        r.setVirtualServiceIp("10.0.1.100");
        r.setVirtualServicePort(443);
        r.setVirtualServiceProtocol("HTTPS");
        r.setPoolName("pool-app-a");
        r.setNodeIp("10.0.1.10");
        r.setNodePort(8080);
        r.setNodeSort(1);
        r.setInternalEnabled("是");
        r.setInternalTargetIp("10.0.1.100");
        r.setExternalEnabled("否");
        return r;
    }

    private DomainRecordImportExcel buildExampleRow1b() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setNodeIp("10.0.1.11");
        r.setNodePort(8080);
        r.setNodeSort(2);
        return r;
    }

    private DomainRecordImportExcel buildExampleRow2a() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setProjectName("示例项目B");
        r.setDomainName("示例-AD仅公网.example.com");
        r.setAreaName("华东");
        r.setGroupName("业务组B");
        r.setSiteLocation("机房B");
        r.setStatus("启用");
        r.setProjectOwner("李四");
        r.setApplyTime(new Date());
        r.setAdEnabled("是");
        r.setVirtualServiceName("vs-app-b");
        r.setVirtualServiceIp("10.0.2.100");
        r.setVirtualServicePort(443);
        r.setVirtualServiceProtocol("HTTPS");
        r.setPoolName("pool-app-b");
        r.setNodeIp("10.0.2.10");
        r.setNodePort(8080);
        r.setNodeSort(1);
        r.setInternalEnabled("否");
        r.setExternalEnabled("是");
        r.setExternalRecordValue("120.1.1.1");
        r.setFmPublicIp("120.1.1.1");
        r.setFmExternalPort(443);
        r.setFmInternalIp("10.0.2.100");
        r.setFmInternalPort(443);
        return r;
    }

    private DomainRecordImportExcel buildExampleRow2b() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setNodeIp("10.0.2.11");
        r.setNodePort(8080);
        r.setNodeSort(2);
        return r;
    }

    private DomainRecordImportExcel buildExampleRow3a() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setProjectName("示例项目C");
        r.setDomainName("示例-AD双向.example.com");
        r.setAreaName("华北");
        r.setGroupName("业务组C");
        r.setSiteLocation("机房C");
        r.setStatus("启用");
        r.setProjectOwner("王五");
        r.setApplyTime(new Date());
        r.setAdEnabled("是");
        r.setVirtualServiceName("vs-app-c-internal");
        r.setVirtualServiceIp("10.0.3.100");
        r.setVirtualServicePort(443);
        r.setVirtualServiceProtocol("HTTPS");
        r.setPoolName("pool-app-c");
        r.setExternalVirtualServiceName("vs-app-c-external");
        r.setExternalVirtualServiceIp("10.0.3.200");
        r.setExternalVirtualServicePort(443);
        r.setExternalVirtualServiceProtocol("HTTPS");
        r.setNodeIp("10.0.3.10");
        r.setNodePort(8080);
        r.setNodeSort(1);
        r.setInternalEnabled("是");
        r.setInternalTargetIp("10.0.3.100");
        r.setExternalEnabled("是");
        r.setExternalRecordValue("120.3.3.3");
        r.setFmPublicIp("120.3.3.3");
        r.setFmExternalPort(443);
        r.setFmInternalIp("10.0.3.200");
        r.setFmInternalPort(443);
        return r;
    }

    private DomainRecordImportExcel buildExampleRow3b() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setNodeIp("10.0.3.11");
        r.setNodePort(8080);
        r.setNodeSort(2);
        return r;
    }

    private DomainRecordImportExcel buildExampleRow4() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setProjectName("示例项目D");
        r.setDomainName("示例-直连内网.example.com");
        r.setAreaName("华北");
        r.setGroupName("业务组D");
        r.setSiteLocation("机房C");
        r.setStatus("启用");
        r.setProjectOwner("王五");
        r.setApplyTime(new Date());
        r.setAdEnabled("否");
        r.setInternalEnabled("是");
        r.setInternalTargetIp("10.0.3.10");
        r.setExternalEnabled("否");
        return r;
    }

    private DomainRecordImportExcel buildExampleRow5() {
        DomainRecordImportExcel r = new DomainRecordImportExcel();
        r.setProjectName("示例项目E");
        r.setDomainName("示例-直连公网.example.com");
        r.setAreaName("华南");
        r.setGroupName("业务组E");
        r.setSiteLocation("机房A");
        r.setStatus("启用");
        r.setProjectOwner("赵六");
        r.setApplyTime(new Date());
        r.setAdEnabled("否");
        r.setInternalEnabled("否");
        r.setExternalEnabled("是");
        r.setExternalRecordValue("120.2.2.2");
        return r;
    }

    /**
     * 计算模板示例的合并区域
     * 模板数据：场景1(2行) + 场景2(2行) + 场景3(2行) + 场景4(1行) + 场景5(1行) = 8行
     * 数据从第3行开始（第0行=分区标题，第1行=说明，第2行=表头）
     */
    private List<int[]> buildTemplateMergeRanges() {
        List<int[]> ranges = new java.util.ArrayList<>();
        // 场景1: 第3-4行合并（2个节点）
        addMergeForRows(ranges, 3, 4);
        // 场景2: 第5-6行合并（2个节点）
        addMergeForRows(ranges, 5, 6);
        // 场景3: 第7-8行合并（2个节点，双向AD）
        addMergeForRows(ranges, 7, 8);
        return ranges;
    }

    private void addMergeForRows(List<int[]> ranges, int startRow, int endRow) {
        if (startRow == endRow) return;
        // 合并非节点列：0-22（基础+高级策略+应用交付）和 26-31（解析配置+防火墙）
        for (int col : new int[]{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 26, 27, 28, 29, 30, 31}) {
            ranges.add(new int[]{startRow, endRow, col, col});
        }
    }
}
