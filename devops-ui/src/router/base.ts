import Layout from "@/layout/layout.vue";
import Error from "@/views/error.vue";
import {RouteRecordRaw} from "vue-router";
import Login from "@/views/login.vue";
import Iframe from "@/views/iframe.vue";

/**
 * 框架基础路由
 */
const routes: Array<RouteRecordRaw> = [
  {
    path: "/",
    component: Layout,
    redirect: "/home",
    meta: { title: "工作台", icon: "icon-desktop" },
    children: [
      {
        path: "/home",
        component: () => import("@/views/home.vue"),
        meta: { title: "主页", icon: "icon-home" }
      },
      {
        path: "/problem",
        component: () => import("@/views/problem.vue"),
        meta: { title: "问题", icon: "icon-notification" }
      }
    ]
  },
  {
    path: "/login",
    component: Login,
    meta: { title: "登录", isNavigationMenu: false }
  },
  {
    path: "/user/profile",
    component: () => import("@/views/sys/user-update-password.vue"),
    meta: { title: "更改个人信息", requiresAuth: true, isNavigationMenu: false }
  },
  {
    path: "/user/password",
    redirect: "/user/profile",
    meta: { isNavigationMenu: false }
  },
  {
    path: "/alert/problem",
    redirect: "/problem",
    meta: { isNavigationMenu: false }
  },
  {
    path: "/iframe/:id?",
    component: Iframe,
    meta: { title: "iframe", isNavigationMenu: false }
  },
  {
    path: "/error",
    name: "error",
    component: Error,
    meta: { title: "错误页面", isNavigationMenu: false }
  },
  {
    path: "/:path(.*)*",
    redirect: { path: "/error", query: { to: 404 }, replace: true },
    meta: { isNavigationMenu: false }
  }
];

export default routes;
