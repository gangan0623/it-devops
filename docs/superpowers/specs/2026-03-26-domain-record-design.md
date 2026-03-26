# 域名记录功能设计

## 1. 背景与目标

新增“域名记录”功能，用于统一管理项目域名与其对应的应用交付、内外网 DNS、阿里云外网解析、防火墙映射、节点池明细，并提供完整的增删改审计能力。

本次设计目标：

- 以“一个域名一条主记录”为核心对象进行管理
- 支持走 AD / 不走 AD 两类链路
- 支持内网 / 外网两类解析场景
- 支持对新增、修改、删除进行操作摘要审计
- 支持对修改行为进行字段级差异记录
- 在列表页直接展示业务方关心的摘要字段

## 2. 范围

本功能主要覆盖 `devops-admin` 和 `devops-ui`：

- 后端新增 `ops` 下的域名记录业务模块
- 前端新增域名记录列表页、编辑弹窗/页面、操作记录查看
- 数据库新增域名记录相关业务表与审计表

本期不包含：

- 自动调用阿里云 DNS API、AD API、防火墙 API、应用交付 API 做配置下发
- 自动探测 DNS 实际生效结果
- 节点池健康检查与负载状态采集

## 3. 核心业务模型

### 3.1 主记录粒度

- 一条域名主记录对应一个域名
- 一条域名主记录只对应一套配置
- 除节点池明细外，其余配置均为一对一关系

### 3.2 业务关系

一条域名记录包含以下信息块：

1. 基础信息
2. 应用交付
3. 内网解析
4. 外网解析
5. 防火墙映射
6. 操作历史

关系约束如下：

- 一个域名记录可配置一个虚拟服务
- 一个虚拟服务对应一个节点池
- 一个节点池可包含多个节点服务器 IP + 端口
- 一个外网 IP 对应一个防火墙策略
- 一个防火墙策略记录外部端口到内部 IP/端口的映射
- 该内部 IP/端口对应应用交付的虚拟服务 IP/端口
- 虚拟服务再关联节点池明细

## 4. 四种链路场景

### 4.1 走 AD + 内网

链路：

`DNS -> 应用交付虚拟服务 IP -> 节点池服务器 IP + 端口`

特点：

- 有应用交付配置
- 有节点池明细
- 内网 DNS 指向虚拟服务 IP
- 不涉及阿里云外网解析
- 不涉及防火墙映射

### 4.2 走 AD + 外网

链路：

`阿里云 DNS -> 公网 IP -> 防火墙外部端口映射 -> 虚拟服务 IP + 端口 -> 节点池服务器 IP + 端口`

特点：

- 有应用交付配置
- 有节点池明细
- 有阿里云外网解析
- 有防火墙映射

### 4.3 不走 AD + 内网

链路：

`DNS -> 目标 IP`

特点：

- 无应用交付
- 无节点池
- 内网 DNS 直接解析到目标 IP

### 4.4 不走 AD + 外网

链路：

`DNS -> 目标公网 IP`

特点：

- 无应用交付
- 无节点池
- 外网 DNS 直接解析到目标公网 IP

## 5. 数据模型设计

### 5.1 主表 `domain_record`

用途：记录域名基础信息与汇总字段。

建议字段：

- `id`
- `project_name` 项目名称
- `domain_name` 域名
- `ad_enabled` 是否走应用交付
- `internal_enabled` 是否启用内网解析
- `external_enabled` 是否启用外网解析
- `external_address` 外网访问地址，用于展示访问入口，可填写域名或 URL，不参与 DNS 链路计算
- `description` 描述
- `project_owner` 项目负责人
- `apply_time` 申请时间
- `remark` 备注
- `creator` / `create_date` / `updater` / `update_date`

约束建议：

- `domain_name` 唯一
- `external_enabled = 0` 时，`external_address` 可为空
- 列表中“外网状态/是否启用”直接展示 `external_enabled`
- `external_address` 为独立业务摘要字段，不与外网 DNS 记录值自动同步

### 5.2 应用交付表 `domain_delivery`

用途：记录虚拟服务与节点池摘要信息。

建议字段：

- `id`
- `domain_record_id`
- `virtual_service_name` 虚拟服务名称
- `virtual_service_ip` 虚拟服务 IP
- `virtual_service_port` 虚拟服务端口
- `virtual_service_protocol` 虚拟服务协议，枚举值建议：`HTTP` / `HTTPS` / `TCP`
- `pool_name` 节点池名称
- `load_strategy` 负载策略

约束建议：

- 一个 `domain_record_id` 只允许一条应用交付记录

### 5.3 节点池明细表 `domain_delivery_node`

用途：记录节点池中的后端服务器明细。

建议字段：

- `id`
- `domain_delivery_id`
- `node_ip`
- `node_port`
- `sort`
- `remark`

约束建议：

- 一个 `domain_delivery_id` 可对应多条节点

### 5.4 内网解析表 `domain_dns_internal`

用途：记录内网 DNS 解析方式。

建议字段：

- `id`
- `domain_record_id`
- `resolve_mode`，枚举值建议：`AD` / `DIRECT`
- `dns_target_ip`
- `remark`

规则：

- `resolve_mode = AD` 时，`dns_target_ip` 表示虚拟服务 IP
- `resolve_mode = DIRECT` 时，`dns_target_ip` 表示目标 IP

### 5.5 外网解析表 `domain_dns_external`

用途：记录外网 DNS 解析方式。

建议字段：

- `id`
- `domain_record_id`
- `resolve_mode`，枚举值建议：`AD` / `DIRECT`
- `record_value`
- `remark`

规则：

- `record_value` 表示 DNS 记录值，也是外网解析表的唯一事实来源字段
- `resolve_mode = AD` 时，`record_value` 表示外网 DNS 指向的公网 IP
- `resolve_mode = DIRECT` 时，`record_value` 表示直接解析的目标公网 IP
- 为保持内外网字段口径一致，内网使用 `dns_target_ip` 表示最终解析目标，外网使用 `record_value` 表示 DNS 记录值

### 5.6 防火墙映射表 `domain_firewall_mapping`

用途：记录外网且走 AD 时的防火墙端口映射关系。

建议字段：

- `id`
- `domain_record_id`
- `public_ip`
- `external_port`
- `internal_ip`
- `internal_port`
- `mapping_desc`

规则：

- 仅用于“外网 + 走 AD”场景
- `internal_ip + internal_port` 应与虚拟服务 `virtual_service_ip + virtual_service_port` 对齐

### 5.7 操作记录主表 `domain_record_history`

用途：记录增删改摘要，并保存快照。

建议字段：

- `id`
- `domain_record_id`
- `operation_type`，枚举值建议：`CREATE` / `UPDATE` / `DELETE`
- `operator_id`
- `operator_name`
- `operation_time`
- `operation_summary`
- `snapshot_before`
- `snapshot_after`

规则：

- `snapshot_before` / `snapshot_after` 统一采用 JSON 字符串存储完整聚合快照
- 聚合快照包含主记录、应用交付、节点池、内网解析、外网解析、防火墙映射
- `CREATE` 仅保留 `snapshot_after`
- `DELETE` 仅保留 `snapshot_before`
- `UPDATE` 同时保留前后快照

### 5.8 字段差异表 `domain_record_history_detail`

用途：记录字段级差异。

建议字段：

- `id`
- `history_id`
- `field_code`
- `field_name`
- `before_value`
- `after_value`

规则：

- `UPDATE` 场景必须写入字段差异明细
- `CREATE` / `DELETE` 不写入字段差异明细，统一通过摘要和完整快照展示
- 子表变更统一按“字段路径 + JSON 摘要”记录到差异表，不做逐列逐行拆分
- 字段路径约定示例：
  - `delivery.virtual_service_ip`
  - `delivery.nodes`
  - `dns.internal`
  - `dns.external`
  - `firewall.mapping`
- 对于集合型字段（如节点池明细），`before_value` / `after_value` 直接存对应子对象数组的 JSON 字符串

## 6. 页面设计

### 6.1 列表页

列表页展示字段：

- 项目名称
- 域名
- 虚拟服务名称
- 虚拟服务 IP
- 虚拟服务端口
- 虚拟服务协议
- 外网状态/是否启用
- 外网地址
- 描述
- 项目负责人
- 申请时间
- 备注

字段说明：

- `外网地址` 对应 `domain_record.external_address`，表示面向使用方的访问入口，例如外网域名或 URL
- 外网 DNS 实际解析值来自 `domain_dns_external.record_value`，通常为公网 IP

列表页操作：

- 新增
- 编辑
- 删除
- 查看详情
- 查看操作记录

查询条件建议：

- 项目名称
- 域名
- 是否走 AD
- 是否启用外网
- 项目负责人
- 申请时间范围

### 6.2 编辑页/弹窗

编辑界面分三块：

1. 域名基础信息
2. 应用交付
3. 解析配置

其中“解析配置”内再分：

- 内网解析
- 外网解析
- 外网且走 AD 时显示防火墙映射

### 6.3 操作记录抽屉/详情

展示：

- 操作类型
- 操作人
- 操作时间
- 操作摘要
- 字段变更明细或完整快照

字段变更明细示例：

- 外网地址：空 -> `https://example.com`
- 虚拟服务 IP：`10.1.1.1` -> `10.1.1.2`

展示规则：

- `UPDATE` 展示字段差异明细，并支持查看修改前后快照
- `CREATE` 展示新增后的完整快照
- `DELETE` 展示删除前的完整快照

## 7. 后端接口建议

建议提供以下接口：

- `GET /ops/domain-record/page` 分页查询
- `GET /ops/domain-record/{id}` 查询详情
- `POST /ops/domain-record` 新增
- `PUT /ops/domain-record` 修改
- `DELETE /ops/domain-record` 删除
- `GET /ops/domain-record/history/page` 操作记录分页
- `GET /ops/domain-record/history/{id}` 操作记录详情

后端返回统一使用 `Result` 包装。

## 8. 保存与审计规则

### 8.1 新增

- 保存主记录
- 根据场景保存应用交付、节点池、内外网解析、防火墙映射
- 写入一条 `CREATE` 操作摘要
- 保存新增后的完整快照

### 8.2 修改

- 读取修改前完整数据快照
- 更新主记录和关联配置
- `ad_enabled` 从 1 改为 0 时，物理删除 `domain_delivery`、`domain_delivery_node`，并同步删除仅依赖 AD 的 `domain_firewall_mapping`
- `internal_enabled` 从 1 改为 0 时，物理删除 `domain_dns_internal`
- `external_enabled` 从 1 改为 0 时，物理删除 `domain_dns_external` 和 `domain_firewall_mapping`
- 当 `external_enabled = 1` 且 `ad_enabled` 从 1 改为 0 时，物理删除 `domain_firewall_mapping`，外网解析改按 `DIRECT` 规则保存
- 节点池明细采用“整体替换”策略：编辑时先删除该虚拟服务下全部旧节点，再按提交内容重建节点列表
- 修改外网 DNS 配置时，仅更新 `domain_dns_external.record_value`
- `domain_record.external_address` 仅在用户显式修改时更新，不根据 DNS 记录自动生成或回填
- 对比前后字段
- 写入一条 `UPDATE` 操作摘要
- 写入字段级差异明细
- 保存修改前后快照

### 8.3 删除

- 读取删除前完整数据快照
- 删除主记录及关联子记录
- 写入一条 `DELETE` 操作摘要
- 保存删除前完整快照

## 9. 校验规则

- 域名必填，格式合法
- 项目名称必填
- 项目负责人必填
- 申请时间必填
- `ad_enabled = 1` 时必须填写应用交付信息
- `internal_enabled` 与 `external_enabled` 至少一个为 `1`
- 允许三种有效组合：仅内网、仅外网、内外网同时启用
- 不允许 `internal_enabled = 0` 且 `external_enabled = 0`
- `ad_enabled = 1` 的前提是至少存在一个启用中的解析场景
- 存在应用交付时必须至少有一个节点池节点
- `internal_enabled = 1` 时必须填写内网解析
- `external_enabled = 1` 时必须填写外网解析
- `external_enabled = 1` 且 `ad_enabled = 1` 时必须填写防火墙映射
- 防火墙映射的 `internal_ip + internal_port` 应与虚拟服务 IP + 端口一致

## 10. 实施建议

优先按以下顺序实施：

1. 数据库表结构与菜单 SQL
2. 后端实体、DTO、Mapper、Service、Controller
3. 历史记录与差异对比能力
4. 前端列表页与编辑页
5. 前端操作记录查看

## 11. 风险与边界

- 当前设计面向人工维护记录，不包含外部系统自动同步
- 删除方式明确采用物理删除，与当前项目现有删除风格保持一致；删除前必须完整写入历史快照与删除摘要
- 若后续出现“一个域名多套虚拟服务”需求，需要扩展主记录与应用交付之间的关系，但当前设计不为此额外引入复杂度
- 关闭某项能力时，其对应子表数据不保留，统一物理删除，避免残留脏数据影响后续编辑与展示

## 12. 权限与菜单编码建议

建议沿用 `ops` 模块现有命名规则，新增以下权限编码：

- `ops:domain-record:page`
- `ops:domain-record:info`
- `ops:domain-record:save`
- `ops:domain-record:update`
- `ops:domain-record:delete`
- `ops:domain-record:history`

菜单与前端视图建议：

- 路由路径：`/ops/domain-record`
- 列表页：`src/views/ops/domain-record.vue`
- 编辑页：`src/views/ops/domain-record-add-or-update.vue`
