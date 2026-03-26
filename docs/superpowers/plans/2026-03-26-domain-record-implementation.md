# Domain Record Implementation Plan

> **For agentic workers:** REQUIRED: Use superpowers:subagent-driven-development (if subagents available) or superpowers:executing-plans to implement this plan. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build the domain-record feature across database, backend, and frontend so operators can maintain one domain-centric record with delivery/DNS/firewall configuration and full change history.

**Architecture:** Add a new `ops/domain-record` module in `devops-admin` with one aggregate root plus child tables for delivery, node pool, internal DNS, external DNS, firewall mapping, and history. Mirror existing `ops` CRUD conventions in the API and `useView()`-based Vue pages, with save/update operations treating the record as a whole aggregate and writing audit snapshots plus field-level diffs.

**Tech Stack:** Spring Boot 3, Java 17, MyBatis-Plus, MySQL, Vue 3, TypeScript, Element Plus

---

### Task 1: Add database schema and menu seed data

**Files:**
- Add: `/mnt/c/Project/it-devops/devops-admin/db/20260326_add_domain_record_tables.sql`

- [ ] **Step 1: Create versioned migration SQL only**

Do not modify `devops_dev.sql`. Put all schema work in the versioned migration file so the change is safe for already-initialized environments.

- [ ] **Step 2: Add production migration SQL**

Create `/mnt/c/Project/it-devops/devops-admin/db/20260326_add_domain_record_tables.sql` with explicit `CREATE TABLE` statements and menu seed inserts for the new `ops:domain-record:*` permissions.
Include schema constraints from the spec:

- unique index on `tb_domain_record.domain_name`
- unique constraints on 1:1 child tables by `domain_record_id`
- normal indexes on child foreign keys used by joins/history queries

- [ ] **Step 3: Define seed menu entries**

Add a menu entry for `/ops/domain-record` and button permissions for:

- `ops:domain-record:page`
- `ops:domain-record:info`
- `ops:domain-record:save`
- `ops:domain-record:update`
- `ops:domain-record:delete`
- `ops:domain-record:history`

- [ ] **Step 4: Verify SQL consistency manually**

Check that table names, field names, and menu IDs line up with the spec and existing `ops` naming conventions.

- [ ] **Step 5: Apply migration against a running MySQL instance**

Run the new SQL against a local/dev MySQL instance and verify table creation plus menu insert statements succeed without syntax or constraint errors.

- [ ] **Step 6: Commit**

```bash
git add devops-admin/db/20260326_add_domain_record_tables.sql
git commit -m "feat: add domain record schema"
```

### Task 2: Add backend entities, request/response models, and mapper skeletons

**Files:**
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainRecordEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainDeliveryEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainDeliveryNodeEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainDnsInternalEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainDnsExternalEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainFirewallMappingEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainRecordHistoryEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/entity/DomainRecordHistoryDetailEntity.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainRecordMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainDeliveryMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainDeliveryNodeMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainDnsInternalMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainDnsExternalMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainFirewallMappingMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainRecordHistoryMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/mapper/DomainRecordHistoryDetailMapper.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/resources/mapper/ops/DomainRecordMapper.xml`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/resources/mapper/ops/DomainRecordHistoryMapper.xml`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/resources/mapper/ops/DomainRecordHistoryDetailMapper.xml`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordPageReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordSaveReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordUpdateReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordDeleteReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordIdReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordHistoryPageReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainRecordHistoryIdReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/req/DomainDeliveryNodeReq.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/rsp/DomainRecordRsp.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/rsp/DomainRecordDetailRsp.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/rsp/DomainRecordHistoryRsp.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/vo/rsp/DomainRecordHistoryDetailRsp.java`

- [ ] **Step 1: Create aggregate entity classes**

Follow existing `ops` entity patterns: extend `BaseEntity`, add updater/updateDate where the module already does so, and keep each entity focused on one table.

- [ ] **Step 2: Define nested request/response DTOs**

Model the save/update payload as one aggregate:

- base domain fields
- optional delivery block
- `nodes: DomainDeliveryNodeReq[]`
- optional internal DNS block
- optional external DNS block
- optional firewall block

- [ ] **Step 3: Add validation annotations**

Use `@NotBlank`, `@NotNull`, and custom service-side validation for cross-field rules:

- at least one of internal/external enabled
- AD requires delivery
- delivery requires at least one node
- external + AD requires firewall mapping

- [ ] **Step 4: Add mapper interfaces and XML joins**

Put list-page joins in `DomainRecordMapper.xml` and history queries in dedicated history XML files. Keep child-table CRUD on simple MyBatis-Plus mappers unless a custom join is needed.

- [ ] **Step 5: Commit**

```bash
git add devops-admin/src/main/java/net/leoch/modules/ops/entity devops-admin/src/main/java/net/leoch/modules/ops/mapper devops-admin/src/main/java/net/leoch/modules/ops/vo devops-admin/src/main/resources/mapper/ops
git commit -m "feat: scaffold domain record backend models"
```

### Task 3: Implement backend service logic for aggregate CRUD and history

**Files:**
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/service/IDomainRecordService.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/service/IDomainRecordHistoryService.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/service/impl/DomainRecordServiceImpl.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/service/impl/DomainRecordHistoryServiceImpl.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/service/impl/DomainRecordDiffBuilder.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/controller/DomainRecordController.java`
- Create: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/ops/controller/DomainRecordHistoryController.java`
- Modify: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/framework/config/security/SaTokenStpInterface.java`

- [ ] **Step 1: Implement page and detail queries**

`page()` should return the list columns from the spec with joined delivery fields. `get()` should return the full aggregate for editing.

- [ ] **Step 2: Implement aggregate save**

Save the main record first, then child records based on flags:

- save delivery + nodes only when `ad_enabled = 1`
- save internal DNS only when `internal_enabled = 1`
- save external DNS only when `external_enabled = 1`
- save firewall only when `external_enabled = 1 && ad_enabled = 1`
- keep the entire save flow, child writes, and history writes inside one `@Transactional(rollbackFor = Exception.class)` boundary

- [ ] **Step 3: Implement aggregate update**

Load the full before-snapshot, update the main record, and enforce lifecycle rules from the spec:

- disable AD => delete delivery, nodes, firewall
- disable internal => delete internal DNS
- disable external => delete external DNS and firewall
- external enabled + AD changes from `1 -> 0` => delete firewall, keep external DNS, and persist it using `DIRECT` semantics without deriving `external_address`
- update nodes via replace-all
- treat `domain_record.external_address` as an independently edited summary field and never overwrite it from DNS values unless the user submitted a new value
- keep the full update plus history write atomic with `@Transactional(rollbackFor = Exception.class)`

- [ ] **Step 4: Implement delete**

Load the full snapshot, delete child records first, then delete the main record, then write a `DELETE` history row with `snapshot_before`.
Keep the delete flow and history write inside the same `@Transactional(rollbackFor = Exception.class)` boundary.

- [ ] **Step 5: Implement history recording**

Use one helper to serialize full aggregate snapshots to JSON and one helper to build diff rows:

- persist `operation_summary` on every history row
- scalar fields as direct before/after values
- nested objects as field-path rows
- node collections as JSON arrays under `delivery.nodes`
- persist `field_name` alongside `field_code` for detail rows
- only `UPDATE` writes detail rows; `CREATE` and `DELETE` are snapshot-only history entries

- [ ] **Step 6: Implement history queries**

Return paged history rows and a detail view that includes summary, diff rows, and snapshots for create/delete/update rendering.

- [ ] **Step 7: Register permissions and operation logs**

Add controller permission annotations using `ops:domain-record:*` and service/controller `@LogOperation` annotations matching existing `ops` conventions.

- [ ] **Step 8: Compile backend**

Run:

```bash
cd /mnt/c/Project/it-devops/devops-admin && mvn -DskipTests compile
```

Expected: backend compilation succeeds because this repository does not have usable automated test coverage.

- [ ] **Step 9: Commit**

```bash
git add devops-admin/src/main/java/net/leoch/modules/ops/service devops-admin/src/main/java/net/leoch/modules/ops/controller devops-admin/src/main/java/net/leoch/framework/config/security/SaTokenStpInterface.java
git commit -m "feat: implement domain record backend"
```

### Task 4: Build frontend list page and edit dialog

**Files:**
- Create: `/mnt/c/Project/it-devops/devops-ui/src/views/ops/domain-record.vue`
- Create: `/mnt/c/Project/it-devops/devops-ui/src/views/ops/domain-record-add-or-update.vue`

- [ ] **Step 1: Build the list page with `useView()`**

Follow existing `ops` list-page conventions:

- query form for project name, domain, AD flag, external flag, owner, date range
- table columns from the spec
- toolbar actions for add/edit/delete/history

- [ ] **Step 2: Build the aggregate edit dialog**

Split the form into three sections:

- 基础信息
- 应用交付
- 解析配置

Use conditional sections so delivery and firewall inputs only show when relevant flags are enabled.

- [ ] **Step 3: Add node-pool editing UI**

Use an inline editable table or repeated row layout for `node_ip`, `node_port`, and optional remark. Keep the UX simple and consistent with Element Plus patterns already used in the project.

- [ ] **Step 4: Add frontend validation and payload shaping**

Enforce required fields before submit and send one aggregate payload to:

- `POST /ops/domain-record`
- `PUT /ops/domain-record`

- [ ] **Step 5: Wire detail loading**

On edit, call `GET /ops/domain-record/{id}` and hydrate the nested form sections, including empty defaults when optional child blocks are absent.

- [ ] **Step 6: Run frontend build**

Run:

```bash
cd /mnt/c/Project/it-devops/devops-ui && npm run build
```

Expected: frontend build succeeds with the new views and no type/build errors.

- [ ] **Step 7: Commit**

```bash
git add devops-ui/src/views/ops/domain-record.vue devops-ui/src/views/ops/domain-record-add-or-update.vue
git commit -m "feat: add domain record ui"
```

### Task 5: Add frontend history drawer and polish integration

**Files:**
- Modify: `/mnt/c/Project/it-devops/devops-ui/src/views/ops/domain-record.vue`
- Optionally create: `/mnt/c/Project/it-devops/devops-ui/src/views/ops/domain-record-history.vue`
- Modify: `/mnt/c/Project/it-devops/devops-admin/src/main/java/net/leoch/modules/sys/vo/rsp/SysMenuRsp.java` only if route metadata requires parity with recent menu changes

- [ ] **Step 1: Add history entrypoint in the list page**

Add a “操作记录” action that opens a drawer or dialog.

- [ ] **Step 2: Render history list and details**

Use:

- `GET /ops/domain-record/history/page`
- `GET /ops/domain-record/history/{id}`

Show summary rows first, then diff details for updates, and snapshots for create/delete.

- [ ] **Step 3: Test end-to-end manually**

Exercise these flows against a running app:

- create AD + internal record
- create AD + external record
- create direct internal record
- create direct external record
- update flags to remove child data
- delete a record
- inspect history after each action

- [ ] **Step 4: Re-run backend and frontend verification**

Run:

```bash
cd /mnt/c/Project/it-devops/devops-admin && mvn -DskipTests compile
cd /mnt/c/Project/it-devops/devops-ui && npm run build
```

Expected: both commands succeed after the final integration changes.

- [ ] **Step 5: Commit**

```bash
git add devops-ui/src/views/ops/domain-record.vue devops-ui/src/views/ops/domain-record-history.vue devops-admin/src/main/java/net/leoch/modules/sys/vo/rsp/SysMenuRsp.java
git commit -m "feat: add domain record history view"
```
