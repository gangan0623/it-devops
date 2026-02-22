# devops-ui

前端界面，基于 Vue 3 + Vite + TypeScript。

## 系统架构（前后端协作）
- 入口：浏览器 -> `devops-ui`
- 交互：通过 `src/service` 调用 `devops-admin` REST API（默认 `/api` 前缀）
- 权限：登录获取 Token，按菜单/角色渲染路由与按钮权限

页面模块与后端 `net.leoch.modules` 对应关系（核心）：
- `sys`：用户/角色/菜单/部门/字典/参数等基础管理
- `ops`：业务系统、主机资产、监控组件、备份代理/设备备份、看板
- `alert`：告警媒介、模板、触发规则、告警记录与测试
- `job`：定时任务与执行日志
- `log`：登录/操作/异常日志
- `oss`：对象存储配置与文件管理
- `monitor`：与 `ops` 的监控组件/看板相关页面

## 开发与构建
- 安装依赖：`npm install`
- 本地开发：`npm run dev`
- 构建生产：`npm run build`
- 本地预览：`npm run serve`

## 配置
- 开发环境：`.env.development`
- 生产环境：`.env.production`

## 目录说明
- `src/`：业务代码
- `public/`：静态资源
- `dist/`：构建产物（构建后生成）
