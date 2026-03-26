package net.leoch.modules.ops.service.impl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import net.leoch.common.data.page.PageData;
import net.leoch.common.data.validator.ValidatorUtils;
import net.leoch.common.data.validator.group.AddGroup;
import net.leoch.common.data.validator.group.DefaultGroup;
import net.leoch.common.data.validator.group.UpdateGroup;
import net.leoch.common.exception.ServiceException;
import net.leoch.common.integration.security.SecurityUser;
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
import net.leoch.modules.ops.vo.req.DomainDeliveryNodeReq;
import net.leoch.modules.ops.vo.req.DomainRecordDeleteReq;
import net.leoch.modules.ops.vo.req.DomainRecordIdReq;
import net.leoch.modules.ops.vo.req.DomainRecordPageReq;
import net.leoch.modules.ops.vo.req.DomainRecordSaveReq;
import net.leoch.modules.ops.vo.req.DomainRecordUpdateReq;
import net.leoch.modules.ops.vo.rsp.DomainRecordDetailRsp;
import net.leoch.modules.ops.vo.rsp.DomainRecordRsp;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

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
        fillBaseEntity(entity, request.getProjectName(), request.getDomainName(), request.getAdEnabled(), request.getInternalEnabled(),
                request.getExternalEnabled(), request.getExternalAddress(), request.getDescription(), request.getProjectOwner(),
                request.getApplyTime(), request.getRemark());
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

        fillBaseEntity(existing, request.getProjectName(), request.getDomainName(), request.getAdEnabled(), request.getInternalEnabled(),
                request.getExternalEnabled(), request.getExternalAddress(), request.getDescription(), request.getProjectOwner(),
                request.getApplyTime(), request.getRemark());
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
                                Integer adEnabled,
                                Integer internalEnabled,
                                Integer externalEnabled,
                                String externalAddress,
                                String description,
                                String projectOwner,
                                Date applyTime,
                                String remark) {
        entity.setProjectName(projectName);
        entity.setDomainName(domainName);
        entity.setAdEnabled(adEnabled);
        entity.setInternalEnabled(internalEnabled);
        entity.setExternalEnabled(externalEnabled);
        entity.setExternalAddress(externalAddress);
        entity.setDescription(description);
        entity.setProjectOwner(projectOwner);
        entity.setApplyTime(applyTime);
        entity.setRemark(remark);
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
        entity.setLoadStrategy(delivery.getLoadStrategy());
        entity.setRemark(delivery.getRemark());
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
        if (delivery != null && (!StrUtil.equals(req.getInternalIp(), delivery.getVirtualServiceIp())
                || !ObjectUtil.equal(req.getInternalPort(), delivery.getVirtualServicePort()))) {
            throw new ServiceException("防火墙内部IP和端口必须与虚拟服务IP和端口一致");
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
}
