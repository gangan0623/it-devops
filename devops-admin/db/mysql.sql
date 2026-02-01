-- MySQL dump 10.13  Distrib 8.0.44, for Linux (x86_64)
--
-- Host: 192.168.159.121    Database: devops
-- ------------------------------------------------------
-- Server version	8.0.40

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!50503 SET NAMES utf8mb4 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Current Database: `devops`
--

CREATE DATABASE /*!32312 IF NOT EXISTS*/ `devops` /*!40100 DEFAULT CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci */ /*!80016 DEFAULT ENCRYPTION='N' */;

USE `devops`;

--
-- Table structure for table `qrtz_blob_triggers`
--

DROP TABLE IF EXISTS `qrtz_blob_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_blob_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `BLOB_DATA` blob,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  KEY `SCHED_NAME` (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  CONSTRAINT `qrtz_blob_triggers_ibfk_1` FOREIGN KEY (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) REFERENCES `qrtz_triggers` (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_blob_triggers`
--

LOCK TABLES `qrtz_blob_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_blob_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_blob_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_calendars`
--

DROP TABLE IF EXISTS `qrtz_calendars`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_calendars` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `CALENDAR_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `CALENDAR` blob NOT NULL,
  PRIMARY KEY (`SCHED_NAME`,`CALENDAR_NAME`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_calendars`
--

LOCK TABLES `qrtz_calendars` WRITE;
/*!40000 ALTER TABLE `qrtz_calendars` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_calendars` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_cron_triggers`
--

DROP TABLE IF EXISTS `qrtz_cron_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_cron_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `CRON_EXPRESSION` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TIME_ZONE_ID` varchar(80) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  CONSTRAINT `qrtz_cron_triggers_ibfk_1` FOREIGN KEY (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) REFERENCES `qrtz_triggers` (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_cron_triggers`
--

LOCK TABLES `qrtz_cron_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_cron_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_cron_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_fired_triggers`
--

DROP TABLE IF EXISTS `qrtz_fired_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_fired_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `ENTRY_ID` varchar(95) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `INSTANCE_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `FIRED_TIME` bigint NOT NULL,
  `SCHED_TIME` bigint NOT NULL,
  `PRIORITY` int NOT NULL,
  `STATE` varchar(16) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `JOB_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `IS_NONCONCURRENT` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `REQUESTS_RECOVERY` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  PRIMARY KEY (`SCHED_NAME`,`ENTRY_ID`) USING BTREE,
  KEY `IDX_QRTZ_FT_TRIG_INST_NAME` (`SCHED_NAME`,`INSTANCE_NAME`) USING BTREE,
  KEY `IDX_QRTZ_FT_INST_JOB_REQ_RCVRY` (`SCHED_NAME`,`INSTANCE_NAME`,`REQUESTS_RECOVERY`) USING BTREE,
  KEY `IDX_QRTZ_FT_J_G` (`SCHED_NAME`,`JOB_NAME`,`JOB_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_FT_JG` (`SCHED_NAME`,`JOB_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_FT_T_G` (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_FT_TG` (`SCHED_NAME`,`TRIGGER_GROUP`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_fired_triggers`
--

LOCK TABLES `qrtz_fired_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_fired_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_fired_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_job_details`
--

DROP TABLE IF EXISTS `qrtz_job_details`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_job_details` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `DESCRIPTION` varchar(250) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `JOB_CLASS_NAME` varchar(250) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `IS_DURABLE` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `IS_NONCONCURRENT` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `IS_UPDATE_DATA` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `REQUESTS_RECOVERY` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_DATA` blob,
  PRIMARY KEY (`SCHED_NAME`,`JOB_NAME`,`JOB_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_J_REQ_RECOVERY` (`SCHED_NAME`,`REQUESTS_RECOVERY`) USING BTREE,
  KEY `IDX_QRTZ_J_GRP` (`SCHED_NAME`,`JOB_GROUP`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_job_details`
--

LOCK TABLES `qrtz_job_details` WRITE;
/*!40000 ALTER TABLE `qrtz_job_details` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_job_details` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_locks`
--

DROP TABLE IF EXISTS `qrtz_locks`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_locks` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `LOCK_NAME` varchar(40) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  PRIMARY KEY (`SCHED_NAME`,`LOCK_NAME`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_locks`
--

LOCK TABLES `qrtz_locks` WRITE;
/*!40000 ALTER TABLE `qrtz_locks` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_locks` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_paused_trigger_grps`
--

DROP TABLE IF EXISTS `qrtz_paused_trigger_grps`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_paused_trigger_grps` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_GROUP`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_paused_trigger_grps`
--

LOCK TABLES `qrtz_paused_trigger_grps` WRITE;
/*!40000 ALTER TABLE `qrtz_paused_trigger_grps` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_paused_trigger_grps` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_scheduler_state`
--

DROP TABLE IF EXISTS `qrtz_scheduler_state`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_scheduler_state` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `INSTANCE_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `LAST_CHECKIN_TIME` bigint NOT NULL,
  `CHECKIN_INTERVAL` bigint NOT NULL,
  PRIMARY KEY (`SCHED_NAME`,`INSTANCE_NAME`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_scheduler_state`
--

LOCK TABLES `qrtz_scheduler_state` WRITE;
/*!40000 ALTER TABLE `qrtz_scheduler_state` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_scheduler_state` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_simple_triggers`
--

DROP TABLE IF EXISTS `qrtz_simple_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_simple_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `REPEAT_COUNT` bigint NOT NULL,
  `REPEAT_INTERVAL` bigint NOT NULL,
  `TIMES_TRIGGERED` bigint NOT NULL,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  CONSTRAINT `qrtz_simple_triggers_ibfk_1` FOREIGN KEY (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) REFERENCES `qrtz_triggers` (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_simple_triggers`
--

LOCK TABLES `qrtz_simple_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_simple_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_simple_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_simprop_triggers`
--

DROP TABLE IF EXISTS `qrtz_simprop_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_simprop_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `STR_PROP_1` varchar(512) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `STR_PROP_2` varchar(512) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `STR_PROP_3` varchar(512) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `INT_PROP_1` int DEFAULT NULL,
  `INT_PROP_2` int DEFAULT NULL,
  `LONG_PROP_1` bigint DEFAULT NULL,
  `LONG_PROP_2` bigint DEFAULT NULL,
  `DEC_PROP_1` decimal(13,4) DEFAULT NULL,
  `DEC_PROP_2` decimal(13,4) DEFAULT NULL,
  `BOOL_PROP_1` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `BOOL_PROP_2` varchar(1) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  CONSTRAINT `qrtz_simprop_triggers_ibfk_1` FOREIGN KEY (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) REFERENCES `qrtz_triggers` (`SCHED_NAME`, `TRIGGER_NAME`, `TRIGGER_GROUP`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_simprop_triggers`
--

LOCK TABLES `qrtz_simprop_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_simprop_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_simprop_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `qrtz_triggers`
--

DROP TABLE IF EXISTS `qrtz_triggers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `qrtz_triggers` (
  `SCHED_NAME` varchar(120) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `JOB_GROUP` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `DESCRIPTION` varchar(250) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `NEXT_FIRE_TIME` bigint DEFAULT NULL,
  `PREV_FIRE_TIME` bigint DEFAULT NULL,
  `PRIORITY` int DEFAULT NULL,
  `TRIGGER_STATE` varchar(16) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `TRIGGER_TYPE` varchar(8) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci NOT NULL,
  `START_TIME` bigint NOT NULL,
  `END_TIME` bigint DEFAULT NULL,
  `CALENDAR_NAME` varchar(200) CHARACTER SET utf8mb3 COLLATE utf8mb3_general_ci DEFAULT NULL,
  `MISFIRE_INSTR` smallint DEFAULT NULL,
  `JOB_DATA` blob,
  PRIMARY KEY (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_T_J` (`SCHED_NAME`,`JOB_NAME`,`JOB_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_T_JG` (`SCHED_NAME`,`JOB_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_T_C` (`SCHED_NAME`,`CALENDAR_NAME`) USING BTREE,
  KEY `IDX_QRTZ_T_G` (`SCHED_NAME`,`TRIGGER_GROUP`) USING BTREE,
  KEY `IDX_QRTZ_T_STATE` (`SCHED_NAME`,`TRIGGER_STATE`) USING BTREE,
  KEY `IDX_QRTZ_T_N_STATE` (`SCHED_NAME`,`TRIGGER_NAME`,`TRIGGER_GROUP`,`TRIGGER_STATE`) USING BTREE,
  KEY `IDX_QRTZ_T_N_G_STATE` (`SCHED_NAME`,`TRIGGER_GROUP`,`TRIGGER_STATE`) USING BTREE,
  KEY `IDX_QRTZ_T_NEXT_FIRE_TIME` (`SCHED_NAME`,`NEXT_FIRE_TIME`) USING BTREE,
  KEY `IDX_QRTZ_T_NFT_ST` (`SCHED_NAME`,`TRIGGER_STATE`,`NEXT_FIRE_TIME`) USING BTREE,
  KEY `IDX_QRTZ_T_NFT_MISFIRE` (`SCHED_NAME`,`MISFIRE_INSTR`,`NEXT_FIRE_TIME`) USING BTREE,
  KEY `IDX_QRTZ_T_NFT_ST_MISFIRE` (`SCHED_NAME`,`MISFIRE_INSTR`,`NEXT_FIRE_TIME`,`TRIGGER_STATE`) USING BTREE,
  KEY `IDX_QRTZ_T_NFT_ST_MISFIRE_GRP` (`SCHED_NAME`,`MISFIRE_INSTR`,`NEXT_FIRE_TIME`,`TRIGGER_GROUP`,`TRIGGER_STATE`) USING BTREE,
  CONSTRAINT `qrtz_triggers_ibfk_1` FOREIGN KEY (`SCHED_NAME`, `JOB_NAME`, `JOB_GROUP`) REFERENCES `qrtz_job_details` (`SCHED_NAME`, `JOB_NAME`, `JOB_GROUP`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb3;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `qrtz_triggers`
--

LOCK TABLES `qrtz_triggers` WRITE;
/*!40000 ALTER TABLE `qrtz_triggers` DISABLE KEYS */;
/*!40000 ALTER TABLE `qrtz_triggers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schedule_job`
--

DROP TABLE IF EXISTS `schedule_job`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `schedule_job` (
  `id` bigint NOT NULL COMMENT 'id',
  `bean_name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'spring bean名称',
  `params` varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '参数',
  `cron_expression` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'cron表达式',
  `status` tinyint unsigned DEFAULT NULL COMMENT '任务状态  0：暂停  1：正常',
  `remark` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='定时任务';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schedule_job`
--

LOCK TABLES `schedule_job` WRITE;
/*!40000 ALTER TABLE `schedule_job` DISABLE KEYS */;
INSERT INTO `schedule_job` VALUES (2016429000000000201,'deviceBackupTask','','0 0 10 * * ?',1,'设备备份每天10点',1067246875800000001,'2026-01-29 15:36:19',1067246875800000001,'2026-01-29 15:36:19');
/*!40000 ALTER TABLE `schedule_job` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schedule_job_log`
--

DROP TABLE IF EXISTS `schedule_job_log`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `schedule_job_log` (
  `id` bigint NOT NULL COMMENT 'id',
  `job_id` bigint NOT NULL COMMENT '任务id',
  `bean_name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'spring bean名称',
  `params` varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '参数',
  `status` tinyint unsigned NOT NULL COMMENT '任务状态    0：失败    1：成功',
  `error` varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '失败信息',
  `times` int NOT NULL COMMENT '耗时(单位：毫秒)',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_job_id` (`job_id`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='定时任务日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schedule_job_log`
--

LOCK TABLES `schedule_job_log` WRITE;
/*!40000 ALTER TABLE `schedule_job_log` DISABLE KEYS */;
INSERT INTO `schedule_job_log` VALUES (2016787905796337666,2016429000000000201,'deviceBackupTask','',1,NULL,445,'2026-01-29 16:18:01'),(2016788372731400193,2016429000000000201,'deviceBackupTask','',1,NULL,777,'2026-01-29 16:19:52'),(2016788679876026369,2016429000000000201,'deviceBackupTask','',1,NULL,15652,'2026-01-29 16:20:50'),(2016794941217034242,2016429000000000201,'deviceBackupTask','',1,NULL,42292,'2026-01-29 16:45:17'),(2016803220970262530,2016429000000000201,'deviceBackupTask','',1,NULL,314195,'2026-01-29 17:13:39'),(2016807290053324801,2016429000000000201,'deviceBackupTask','',1,NULL,51,'2026-01-29 17:35:03'),(2016807461508083714,2016429000000000201,'deviceBackupTask','',1,NULL,39,'2026-01-29 17:35:44'),(2016808611871453185,2016429000000000201,'deviceBackupTask','',1,NULL,45,'2026-01-29 17:40:18'),(2016810189193998337,2016429000000000201,'deviceBackupTask','',1,NULL,177,'2026-01-29 17:46:34'),(2016811581442879489,2016429000000000201,'deviceBackupTask','',1,NULL,1097,'2026-01-29 17:52:05'),(2016812205513371650,2016429000000000201,'deviceBackupTask','',1,NULL,86,'2026-01-29 17:54:35'),(2016813948049551361,2016429000000000201,'deviceBackupTask','',1,NULL,389,'2026-01-29 18:01:30'),(2016818183055011842,2016429000000000201,'deviceBackupTask','',1,NULL,454,'2026-01-29 18:18:20'),(2017055162569043969,2016429000000000201,'deviceBackupTask','',1,NULL,432,'2026-01-30 10:00:00'),(2017152473282355202,2016429000000000201,'deviceBackupTask','',1,NULL,401,'2026-01-30 16:26:41'),(2017417550673666050,2016429000000000201,'deviceBackupTask','',1,NULL,476,'2026-01-31 10:00:00');
/*!40000 ALTER TABLE `schedule_job_log` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_dept`
--

DROP TABLE IF EXISTS `sys_dept`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_dept` (
  `id` bigint NOT NULL COMMENT 'id',
  `pid` bigint DEFAULT NULL COMMENT '上级ID',
  `pids` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '所有上级ID，用逗号分开',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '部门名称',
  `sort` int unsigned DEFAULT NULL COMMENT '排序',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_pid` (`pid`) USING BTREE,
  KEY `idx_sort` (`sort`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='部门管理';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_dept`
--

LOCK TABLES `sys_dept` WRITE;
/*!40000 ALTER TABLE `sys_dept` DISABLE KEYS */;
INSERT INTO `sys_dept` VALUES (1067246875800000066,0,'0','IT基础建设部',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 11:34:04');
/*!40000 ALTER TABLE `sys_dept` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_dict_data`
--

DROP TABLE IF EXISTS `sys_dict_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_dict_data` (
  `id` bigint NOT NULL COMMENT 'id',
  `dict_type_id` bigint NOT NULL COMMENT '字典类型ID',
  `dict_label` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '字典标签',
  `dict_value` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '字典值',
  `remark` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `sort` int unsigned DEFAULT NULL COMMENT '排序',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_dict_type_value` (`dict_type_id`,`dict_value`) USING BTREE,
  KEY `idx_sort` (`sort`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='字典数据';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_dict_data`
--

LOCK TABLES `sys_dict_data` WRITE;
/*!40000 ALTER TABLE `sys_dict_data` DISABLE KEYS */;
INSERT INTO `sys_dict_data` VALUES (1160061112075464705,1160061077912858625,'男','0','',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1160061146967879681,1160061077912858625,'女','1','',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1160061190127267841,1160061077912858625,'保密','2','',2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1225814069634195457,1225813644059140097,'公告','0','',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1225814107559092225,1225813644059140097,'会议','1','',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1225814271879340034,1225813644059140097,'其他','2','',2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1260000000000000101,1260000000000000100,'安徽','安徽','',0,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-29 09:39:43'),(1260000000000000102,1260000000000000100,'江苏','江苏','',1,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-29 09:39:46'),(1260000000000000103,1260000000000000100,'深圳','深圳','',2,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-29 09:39:53'),(1260000000000000104,1260000000000000100,'肇庆','肇庆','',3,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-29 09:40:02'),(1260000000000000105,1260000000000000100,'海外','海外','',4,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-29 09:40:06'),(1260000000000000201,1260000000000000200,'核心网络设备','核心网络设备','',0,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-29 09:39:34'),(1260000000000000202,1260000000000000200,'普通网络设备','普通网络设备','',1,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-29 09:39:37'),(1260000000000000301,1260000000000000300,'00.VMware','00.VMware','',0,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-29 09:39:28'),(1260000000000000302,1260000000000000300,'01.基础服务','01.基础服务','',1,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000303,1260000000000000300,'02.安全运维','02.安全运维','',2,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000304,1260000000000000300,'03.基础业务','03.基础业务','',3,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000305,1260000000000000300,'04.供应链','04.供应链','',4,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000306,1260000000000000300,'05.销售','05.销售','',5,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000307,1260000000000000300,'06.财务','06.财务','',6,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000308,1260000000000000300,'07.研发','07.研发','',7,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000309,1260000000000000300,'10.云电脑','10.云电脑','',8,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000310,1260000000000000300,'11.桌面管理','11.桌面管理','',9,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000311,1260000000000000300,'12.测试系统','12.测试系统','',10,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000312,1260000000000000300,'99.待处理','99.待处理','',11,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000313,1260000000000000300,'99.模板','99.模板','',12,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000401,1260000000000000400,'华三','华三','',0,1067246875800000001,'2026-01-29 09:49:04',1067246875800000001,'2026-01-29 09:49:04'),(1260000000000000402,1260000000000000400,'华为','华为','',1,1067246875800000001,'2026-01-29 09:49:04',1067246875800000001,'2026-01-29 09:49:04'),(1260000000000000403,1260000000000000400,'飞塔','飞塔','',2,1067246875800000001,'2026-01-29 09:49:04',1067246875800000001,'2026-01-29 09:49:04'),(1260000000000000501,1260000000000000500,'安徽理士','安徽理士','',0,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000502,1260000000000000500,'安慧锂电','安慧锂电','',1,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000503,1260000000000000500,'江苏理士','江苏理士','',2,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000504,1260000000000000500,'深圳理士','深圳理士','',3,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000505,1260000000000000500,'肇庆理士','肇庆理士','',4,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000506,1260000000000000500,'东莞锂电','东莞锂电','',5,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000507,1260000000000000500,'新加坡','新加坡','',6,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000508,1260000000000000500,'马来西亚','马来西亚','',7,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000509,1260000000000000500,'越南理士','越南理士','',8,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000510,1260000000000000500,'天津杰士','天津杰士','',9,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000511,1260000000000000500,'顺德汤浅','顺德汤浅','',10,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000601,1260000000000000600,'Prometheus','Prometheus','',0,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 21:42:00'),(1260000000000000602,1260000000000000600,'VMAlert','VMAlert','',1,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 21:42:09'),(1260000000000000603,1260000000000000600,'Alertmanager','Alertmanager','',2,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 21:42:13'),(1260000000000000604,1260000000000000600,'VictoriaMetrics','VictoriaMetrics','',3,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 21:42:18'),(1260000000000000605,1260000000000000600,'BlackBox','BlackBox','',4,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 21:42:28');
/*!40000 ALTER TABLE `sys_dict_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_dict_type`
--

DROP TABLE IF EXISTS `sys_dict_type`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_dict_type` (
  `id` bigint NOT NULL COMMENT 'id',
  `dict_type` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '字典类型',
  `dict_name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '字典名称',
  `remark` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `sort` int unsigned DEFAULT NULL COMMENT '排序',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `dict_type` (`dict_type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='字典类型';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_dict_type`
--

LOCK TABLES `sys_dict_type` WRITE;
/*!40000 ALTER TABLE `sys_dict_type` DISABLE KEYS */;
INSERT INTO `sys_dict_type` VALUES (1160061077912858625,'gender','性别','',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1225813644059140097,'notice_type','站内通知-类型','',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1260000000000000100,'area_name_type','区域名称','',2,1067246875800000001,'2026-01-28 17:20:17',1067246875800000001,'2026-01-28 17:20:17'),(1260000000000000200,'network_device_group','网络设备分组','',3,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000300,'virtual_host_group','服务器分组','',4,1067246875800000001,'2026-01-28 18:00:19',1067246875800000001,'2026-01-28 18:00:19'),(1260000000000000400,'network_device_model','设备型号','',5,1067246875800000001,'2026-01-29 09:48:54',1067246875800000001,'2026-01-29 09:48:54'),(1260000000000000500,'base_site_location','站点位置','',6,1067246875800000001,'2026-01-30 10:43:13',1067246875800000001,'2026-01-30 10:43:13'),(1260000000000000600,'monitor_component_type','监控组件类型','',7,1067246875800000001,'2026-01-30 17:33:12',1067246875800000001,'2026-01-30 17:33:12');
/*!40000 ALTER TABLE `sys_dict_type` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_log_error`
--

DROP TABLE IF EXISTS `sys_log_error`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_log_error` (
  `id` bigint NOT NULL COMMENT 'id',
  `request_uri` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '请求URI',
  `request_method` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '请求方式',
  `request_params` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '请求参数',
  `user_agent` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户代理',
  `ip` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作IP',
  `error_info` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '异常信息',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='异常日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_log_error`
--

LOCK TABLES `sys_log_error` WRITE;
/*!40000 ALTER TABLE `sys_log_error` DISABLE KEYS */;
/*!40000 ALTER TABLE `sys_log_error` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_log_login`
--

DROP TABLE IF EXISTS `sys_log_login`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_log_login` (
  `id` bigint NOT NULL COMMENT 'id',
  `operation` tinyint unsigned DEFAULT NULL COMMENT '用户操作   0：用户登录   1：用户退出',
  `status` tinyint unsigned NOT NULL COMMENT '状态  0：失败    1：成功    2：账号已锁定',
  `user_agent` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户代理',
  `ip` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作IP',
  `creator_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_status` (`status`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='登录日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_log_login`
--

LOCK TABLES `sys_log_login` WRITE;
/*!40000 ALTER TABLE `sys_log_login` DISABLE KEYS */;
INSERT INTO `sys_log_login` VALUES (2016345628036816898,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-28 11:00:34'),(2016391467283185666,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-28 14:02:43'),(2016391693813350402,1,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-28 14:03:37'),(2016391734779117570,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-28 14:03:47'),(2016426117992955906,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-28 16:20:24'),(2016439377542754305,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','159535',2016354214011985921,'2026-01-28 17:13:06'),(2016440793233600513,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','159535',2016354214011985921,'2026-01-28 17:18:43'),(2016660807198904321,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-29 07:52:59'),(2016842281885761537,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','159535',2016354214011985921,'2026-01-29 19:54:06'),(2016842394276331522,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-29 19:54:32'),(2017024402294288386,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','159535',2016354214011985921,'2026-01-30 07:57:47'),(2017061325373087746,1,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','159535',2016354214011985921,'2026-01-30 10:24:30'),(2017061350106898434,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-30 10:24:36'),(2017115662992498690,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-30 14:00:25'),(2017392283292983297,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-31 08:19:36'),(2017442448209043457,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','0:0:0:0:0:0:0:1','admin',1067246875800000001,'2026-01-31 11:38:56'),(2017497581950648321,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','10.2.53.104','admin',1067246875800000001,'2026-01-31 15:18:01'),(2017497716394868737,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','10.2.53.104','admin',1067246875800000001,'2026-01-31 15:18:33'),(2017504564367900673,0,1,'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/144.0.0.0 Safari/537.36','10.2.53.104','admin',1067246875800000001,'2026-01-31 15:45:46');
/*!40000 ALTER TABLE `sys_log_login` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_log_operation`
--

DROP TABLE IF EXISTS `sys_log_operation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_log_operation` (
  `id` bigint NOT NULL COMMENT 'id',
  `operation` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户操作',
  `request_uri` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '请求URI',
  `request_method` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '请求方式',
  `request_params` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '请求参数',
  `request_time` int unsigned NOT NULL COMMENT '请求时长(毫秒)',
  `user_agent` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户代理',
  `ip` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '操作IP',
  `status` tinyint unsigned NOT NULL COMMENT '状态  0：失败   1：成功',
  `creator_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='操作日志';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_log_operation`
--

LOCK TABLES `sys_log_operation` WRITE;
/*!40000 ALTER TABLE `sys_log_operation` DISABLE KEYS */;
/*!40000 ALTER TABLE `sys_log_operation` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_menu`
--

DROP TABLE IF EXISTS `sys_menu`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_menu` (
  `id` bigint NOT NULL COMMENT 'id',
  `pid` bigint DEFAULT NULL COMMENT '上级ID，一级菜单为0',
  `name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '名称',
  `url` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单URL',
  `permissions` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '授权(多个用逗号分隔，如：sys:user:list,sys:user:save)',
  `menu_type` tinyint unsigned DEFAULT NULL COMMENT '类型   0：菜单   1：按钮',
  `icon` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '菜单图标',
  `sort` int DEFAULT NULL COMMENT '排序',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_pid` (`pid`) USING BTREE,
  KEY `idx_sort` (`sort`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='菜单管理';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_menu`
--

LOCK TABLES `sys_menu` WRITE;
/*!40000 ALTER TABLE `sys_menu` DISABLE KEYS */;
INSERT INTO `sys_menu` VALUES (1067246875800000002,0,'权限管理',NULL,NULL,0,'icon-safetycertificate',4,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-29 15:56:25'),(1067246875800000003,1067246875800000055,'新增',NULL,'sys:user:save,sys:dept:list,sys:role:list',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000004,1067246875800000055,'修改',NULL,'sys:user:update,sys:dept:list,sys:role:list',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000005,1067246875800000055,'删除',NULL,'sys:user:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000006,1067246875800000055,'导出',NULL,'sys:user:export',1,NULL,4,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000007,1067246875800000002,'角色管理','sys/role',NULL,0,'icon-team',2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000008,1067246875800000007,'查看',NULL,'sys:role:page,sys:role:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000009,1067246875800000007,'新增',NULL,'sys:role:save,sys:menu:select,sys:dept:list',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000010,1067246875800000007,'修改',NULL,'sys:role:update,sys:menu:select,sys:dept:list',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000011,1067246875800000007,'删除',NULL,'sys:role:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000012,1067246875800000002,'部门管理','sys/dept',NULL,0,'icon-apartment',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000014,1067246875800000012,'查看',NULL,'sys:dept:list,sys:dept:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000015,1067246875800000012,'新增',NULL,'sys:dept:save',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000016,1067246875800000012,'修改',NULL,'sys:dept:update',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000017,1067246875800000012,'删除',NULL,'sys:dept:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000025,1067246875800000035,'菜单管理','sys/menu',NULL,0,'icon-unorderedlist',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000026,1067246875800000025,'查看',NULL,'sys:menu:list,sys:menu:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000027,1067246875800000025,'新增',NULL,'sys:menu:save',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000028,1067246875800000025,'修改',NULL,'sys:menu:update',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000029,1067246875800000025,'删除',NULL,'sys:menu:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000030,1067246875800000035,'定时任务','job/schedule',NULL,0,'icon-dashboard',3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000031,1067246875800000030,'查看',NULL,'sys:schedule:page,sys:schedule:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000032,1067246875800000030,'新增',NULL,'sys:schedule:save',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000033,1067246875800000030,'修改',NULL,'sys:schedule:update',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000034,1067246875800000030,'删除',NULL,'sys:schedule:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000035,0,'系统设置',NULL,NULL,0,'icon-setting',3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-29 15:56:12'),(1067246875800000036,1067246875800000030,'暂停',NULL,'sys:schedule:pause',1,NULL,4,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000037,1067246875800000030,'恢复',NULL,'sys:schedule:resume',1,NULL,5,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000038,1067246875800000030,'立即执行',NULL,'sys:schedule:run',1,NULL,6,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000039,1067246875800000030,'日志列表',NULL,'sys:schedule:log',1,NULL,7,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000040,1067246875800000035,'参数管理','sys/params','',0,'icon-fileprotect',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000041,1067246875800000035,'字典管理','sys/dict-type',NULL,0,'icon-golden-fill',2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000042,1067246875800000041,'查看',NULL,'sys:dict:page,sys:dict:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000043,1067246875800000041,'新增',NULL,'sys:dict:save',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000044,1067246875800000041,'修改',NULL,'sys:dict:update',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000045,1067246875800000041,'删除',NULL,'sys:dict:delete',1,NULL,3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000046,0,'日志管理',NULL,NULL,0,'icon-container',5,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-29 15:57:06'),(1067246875800000047,1067246875800000035,'文件上传','oss/oss','sys:oss:all',0,'icon-upload',4,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000048,1067246875800000046,'登录日志','sys/log-login','sys:log:login',0,'icon-filedone',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000049,1067246875800000046,'操作日志','sys/log-operation','sys:log:operation',0,'icon-solution',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000050,1067246875800000046,'异常日志','sys/log-error','sys:log:error',0,'icon-file-exception',2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000051,1067246875800000053,'SQL监控','{{ApiUrl}}/druid/sql.html',NULL,0,'icon-database',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000053,0,'系统监控',NULL,NULL,0,'icon-desktop',6,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-29 15:57:13'),(1067246875800000055,1067246875800000002,'用户管理','sys/user',NULL,0,'icon-user',0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000056,1067246875800000055,'查看',NULL,'sys:user:page,sys:user:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000057,1067246875800000040,'新增',NULL,'sys:params:save',1,NULL,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000058,1067246875800000040,'导出',NULL,'sys:params:export',1,NULL,4,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000059,1067246875800000040,'查看','','sys:params:page,sys:params:info',1,NULL,0,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000060,1067246875800000040,'修改',NULL,'sys:params:update',1,NULL,2,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1067246875800000061,1067246875800000040,'删除','','sys:params:delete',1,'',3,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1156748733921165314,1067246875800000053,'接口文档','{{ApiUrl}}/doc.html','',0,'icon-file-word',1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(1900000000000000001,0,'告警管理',NULL,NULL,0,'icon-bell',1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 15:56:01'),(1900000000000000101,1900000000000000001,'告警媒介','alert/media',NULL,0,'icon-mail',1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000102,1900000000000000001,'告警模板','alert/template',NULL,0,'icon-file-text',2,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000103,1900000000000000001,'告警触发器','alert/trigger',NULL,0,'icon-Partition',3,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 08:43:46'),(1900000000000000104,1900000000000000001,'告警记录','alert/record',NULL,0,'icon-notification',4,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000201,1900000000000000101,'查看',NULL,'alert:media:page,alert:media:info',1,NULL,0,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000202,1900000000000000101,'新增',NULL,'alert:media:save',1,NULL,1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000203,1900000000000000101,'修改',NULL,'alert:media:update',1,NULL,2,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000204,1900000000000000101,'删除',NULL,'alert:media:delete',1,NULL,3,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000205,1900000000000000101,'测试',NULL,'alert:media:test',1,NULL,4,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000301,1900000000000000102,'查看',NULL,'alert:template:page,alert:template:info',1,NULL,0,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000302,1900000000000000102,'新增',NULL,'alert:template:save',1,NULL,1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000303,1900000000000000102,'修改',NULL,'alert:template:update',1,NULL,2,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000304,1900000000000000102,'删除',NULL,'alert:template:delete',1,NULL,3,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000305,1900000000000000102,'测试',NULL,'alert:template:test',1,NULL,4,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000401,1900000000000000103,'查看',NULL,'alert:trigger:page,alert:trigger:info',1,NULL,0,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000402,1900000000000000103,'新增',NULL,'alert:trigger:save',1,NULL,1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000403,1900000000000000103,'修改',NULL,'alert:trigger:update',1,NULL,2,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000404,1900000000000000103,'删除',NULL,'alert:trigger:delete',1,NULL,3,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000501,1900000000000000104,'查看',NULL,'alert:record:page,alert:record:info',1,NULL,0,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(1900000000000000502,1900000000000000104,'删除',NULL,'alert:record:delete',1,NULL,1,1067246875800000001,'2026-01-29 00:55:05',1067246875800000001,'2026-01-29 00:55:05'),(2016424054264758274,2016429000000000301,'备份设备管理','ops/devicebackup',NULL,0,'icon-filesync',1,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:53:26'),(2016424054264758275,2016424054264758274,'查看',NULL,'ops:devicebackup:page,ops:devicebackup:info',1,NULL,0,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:19:02'),(2016424054264758276,2016424054264758274,'新增',NULL,'ops:devicebackup:save',1,NULL,1,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:19:02'),(2016424054264758277,2016424054264758274,'修改',NULL,'ops:devicebackup:update',1,NULL,2,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:19:02'),(2016424054264758278,2016424054264758274,'删除',NULL,'ops:devicebackup:delete',1,NULL,3,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:19:02'),(2016424054264758279,2016424054264758274,'导出',NULL,'ops:devicebackup:export',1,NULL,4,1067246875800000001,'2026-01-28 16:19:02',1067246875800000001,'2026-01-28 16:19:02'),(2016424054654828545,2016426590938480641,'WIN设备管理','ops/windowhost',NULL,0,'icon-windows',2,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:26:32'),(2016424054654828546,2016424054654828545,'查看',NULL,'ops:windowhost:page,ops:windowhost:info',1,NULL,0,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054654828547,2016424054654828545,'新增',NULL,'ops:windowhost:save',1,NULL,1,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054654828548,2016424054654828545,'修改',NULL,'ops:windowhost:update',1,NULL,2,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054654828549,2016424054654828545,'删除',NULL,'ops:windowhost:delete',1,NULL,3,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054654828550,2016424054654828545,'导出',NULL,'ops:windowhost:export',1,NULL,4,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054856155138,2016429000000000301,'备份节点管理','ops/backupagent',NULL,0,'icon-sisternode',0,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:53:21'),(2016424054856155139,2016424054856155138,'查看',NULL,'ops:backupagent:page,ops:backupagent:info',1,NULL,0,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054856155140,2016424054856155138,'新增',NULL,'ops:backupagent:save',1,NULL,1,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054856155141,2016424054856155138,'修改',NULL,'ops:backupagent:update',1,NULL,2,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054856155142,2016424054856155138,'删除',NULL,'ops:backupagent:delete',1,NULL,3,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054856155143,2016424054856155138,'导出',NULL,'ops:backupagent:export',1,NULL,4,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054986178562,2016426590938480641,'业务系统管理','ops/businesssystem',NULL,0,'icon-file-text',3,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:53:15'),(2016424054986178563,2016424054986178562,'查看',NULL,'ops:businesssystem:page,ops:businesssystem:info',1,NULL,0,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054986178564,2016424054986178562,'新增',NULL,'ops:businesssystem:save',1,NULL,1,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054986178565,2016424054986178562,'修改',NULL,'ops:businesssystem:update',1,NULL,2,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054986178566,2016424054986178562,'删除',NULL,'ops:businesssystem:delete',1,NULL,3,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016424054986178567,2016424054986178562,'导出',NULL,'ops:businesssystem:export',1,NULL,4,1067246875800000001,'2026-01-28 16:19:01',1067246875800000001,'2026-01-28 16:19:01'),(2016426590938480641,0,'设备管理','','',0,'icon-wrench-fill',0,1067246875800000001,'2026-01-28 16:22:17',1067246875800000001,'2026-01-29 15:55:52'),(2016427449415434242,2016426590938480641,'Linux设备管理','ops/linuxhost',NULL,0,'icon-cloud-server',1,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:52:47'),(2016427449415434243,2016427449415434242,'查看',NULL,'ops:linuxhost:page,ops:linuxhost:info',1,NULL,0,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:51:24'),(2016427449415434244,2016427449415434242,'新增',NULL,'ops:linuxhost:save',1,NULL,1,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:51:24'),(2016427449415434245,2016427449415434242,'修改',NULL,'ops:linuxhost:update',1,NULL,2,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:51:24'),(2016427449415434246,2016427449415434242,'删除',NULL,'ops:linuxhost:delete',1,NULL,3,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:51:24'),(2016427449415434247,2016427449415434242,'导出',NULL,'ops:linuxhost:export',1,NULL,4,1067246875800000001,'2026-01-28 16:51:24',1067246875800000001,'2026-01-28 16:51:24'),(2016427449415434248,2016424054654828545,'导入',NULL,'ops:windowhost:import',1,NULL,5,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434249,2016424054654828545,'下载模板',NULL,'ops:windowhost:template',1,NULL,6,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434250,2016427449415434242,'导入',NULL,'ops:linuxhost:import',1,NULL,5,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434251,2016427449415434242,'下载模板',NULL,'ops:linuxhost:template',1,NULL,6,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434252,2016424054986178562,'导入',NULL,'ops:businesssystem:import',1,NULL,5,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434253,2016424054986178562,'下载模板',NULL,'ops:businesssystem:template',1,NULL,6,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434254,2016424054264758274,'导入',NULL,'ops:devicebackup:import',1,NULL,5,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434255,2016424054264758274,'下载模板',NULL,'ops:devicebackup:template',1,NULL,6,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434256,2016424054856155138,'导入',NULL,'ops:backupagent:import',1,NULL,5,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016427449415434257,2016424054856155138,'下载模板',NULL,'ops:backupagent:template',1,NULL,6,1067246875800000001,'2026-01-28 17:37:22',1067246875800000001,'2026-01-28 17:37:22'),(2016429000000000001,2016429000000000301,'备份记录','ops/devicebackup-record',NULL,0,'icon-filedone',2,1067246875800000001,'2026-01-29 15:31:45',1067246875800000001,'2026-01-29 15:31:45'),(2016429000000000101,2016429000000000001,'查看',NULL,'ops:devicebackuprecord:page,ops:devicebackuprecord:info',1,NULL,0,1067246875800000001,'2026-01-29 15:31:45',1067246875800000001,'2026-01-29 15:31:45'),(2016429000000000102,2016429000000000001,'删除',NULL,'ops:devicebackuprecord:delete',1,NULL,1,1067246875800000001,'2026-01-29 15:31:45',1067246875800000001,'2026-01-29 15:31:45'),(2016429000000000103,2016429000000000001,'历史',NULL,'ops:devicebackuprecord:history',1,NULL,2,1067246875800000001,'2026-01-29 18:16:58',1067246875800000001,'2026-01-29 18:16:58'),(2016429000000000104,2016429000000000001,'对比',NULL,'ops:devicebackuprecord:diff',1,NULL,3,1067246875800000001,'2026-01-29 18:16:58',1067246875800000001,'2026-01-29 18:16:58'),(2016429000000000105,2016429000000000001,'预览',NULL,'ops:devicebackuprecord:preview',1,NULL,4,1067246875800000001,'2026-01-29 18:24:15',1067246875800000001,'2026-01-29 18:24:15'),(2016429000000000106,2016429000000000001,'下载',NULL,'ops:devicebackuprecord:download',1,NULL,5,1067246875800000001,'2026-01-29 19:32:40',1067246875800000001,'2026-01-29 19:32:40'),(2016429000000000301,0,'备份管理',NULL,NULL,0,'icon-filesync',2,1067246875800000001,'2026-01-29 15:52:39',1067246875800000001,'2026-01-29 15:56:48'),(2100000000000000101,2016426590938480641,'监控组件管理','monitor/component',NULL,0,'icon-project',0,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000102,2100000000000000101,'查看',NULL,'ops:monitorcomponent:page,ops:monitorcomponent:info',1,NULL,0,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000103,2100000000000000101,'新增',NULL,'ops:monitorcomponent:save',1,NULL,1,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000104,2100000000000000101,'修改',NULL,'ops:monitorcomponent:update',1,NULL,2,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000105,2100000000000000101,'删除',NULL,'ops:monitorcomponent:delete',1,NULL,3,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000106,2100000000000000101,'探测',NULL,'ops:monitorcomponent:probe',1,NULL,4,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44'),(2100000000000000107,2100000000000000101,'版本检测',NULL,'ops:monitorcomponent:version',1,NULL,5,1067246875800000001,'2026-01-30 17:33:44',1067246875800000001,'2026-01-30 17:33:44');
/*!40000 ALTER TABLE `sys_menu` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_oss`
--

DROP TABLE IF EXISTS `sys_oss`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_oss` (
  `id` bigint NOT NULL COMMENT 'id',
  `url` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'URL地址',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='文件上传';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_oss`
--

LOCK TABLES `sys_oss` WRITE;
/*!40000 ALTER TABLE `sys_oss` DISABLE KEYS */;
INSERT INTO `sys_oss` VALUES (2016391129385861122,'https://minioback.leoch.net/devops/basic/20260128/043e8f63465d49ebbfd3fb2f74b830c0.png',1067246875800000001,'2026-01-28 14:01:23');
/*!40000 ALTER TABLE `sys_oss` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_oss_config`
--

DROP TABLE IF EXISTS `sys_oss_config`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_oss_config` (
  `id` bigint NOT NULL COMMENT 'id',
  `config_json` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '配置JSON',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='云存储配置';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_oss_config`
--

LOCK TABLES `sys_oss_config` WRITE;
/*!40000 ALTER TABLE `sys_oss_config` DISABLE KEYS */;
INSERT INTO `sys_oss_config` VALUES (1,'{\"type\":4,\"minioDomain\":\"https://minioback.leoch.net\",\"minioPath\":\"basic\",\"minioEndPoint\":\"https://minioback.leoch.net\",\"minioAccessKey\":\"admin\",\"minioSecretKey\":\"Leoch.sz.17100\",\"minioBucketName\":\"devops\",\"qiniuDomain\":\"http://test.oss.renren.io\",\"qiniuPrefix\":\"upload\",\"qiniuAccessKey\":\"NrgMfABZxWLo5B-YYSjoE8-AZ1EISdi1Z3ubLOeZ\",\"qiniuSecretKey\":\"uIwJHevMRWU0VLxFvgy0tAcOdGqasdtVlJkdy6vV\",\"qiniuBucketName\":\"renren-oss\",\"aliyunDomain\":\"\",\"aliyunPrefix\":\"\",\"aliyunEndPoint\":\"\",\"aliyunAccessKeyId\":\"\",\"aliyunAccessKeySecret\":\"\",\"aliyunBucketName\":\"\",\"qcloudDomain\":\"\",\"qcloudPrefix\":\"\",\"qcloudAppId\":null,\"qcloudSecretId\":\"\",\"qcloudSecretKey\":\"\",\"qcloudBucketName\":\"\",\"qcloudRegion\":null}',1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16');
/*!40000 ALTER TABLE `sys_oss_config` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_params`
--

DROP TABLE IF EXISTS `sys_params`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_params` (
  `id` bigint NOT NULL COMMENT 'id',
  `param_code` varchar(32) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '参数编码',
  `param_value` varchar(2000) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '参数值',
  `param_type` tinyint unsigned DEFAULT '1' COMMENT '类型   0：系统参数   1：非系统参数',
  `remark` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_param_code` (`param_code`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='参数管理';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_params`
--

LOCK TABLES `sys_params` WRITE;
/*!40000 ALTER TABLE `sys_params` DISABLE KEYS */;
INSERT INTO `sys_params` VALUES (1067246875800000074,'ZABBIX_CONFIG','{\"url\":\"http://192.168.17.123:8080/api_jsonrpc.php\",\"username\":\"Admin\",\"password\":\"Leoch.ah.2025zb\",\"templates\":[\"001.飞塔监控模板\",\"002.华为监控模板\",\"003.华三监控模板\"]}',0,'Zabbix配置',1067246875800000001,'2026-02-01 15:00:00',1067246875800000001,'2026-02-01 15:00:00');
/*!40000 ALTER TABLE `sys_params` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_role`
--

DROP TABLE IF EXISTS `sys_role`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_role` (
  `id` bigint NOT NULL COMMENT 'id',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '角色名称',
  `remark` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '备注',
  `dept_id` bigint DEFAULT NULL COMMENT '部门ID',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_dept_id` (`dept_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色管理';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_role`
--

LOCK TABLES `sys_role` WRITE;
/*!40000 ALTER TABLE `sys_role` DISABLE KEYS */;
INSERT INTO `sys_role` VALUES (2016354427531419649,'普通用户','',NULL,1067246875800000001,'2026-01-28 11:35:32',1067246875800000001,'2026-01-29 19:55:17');
/*!40000 ALTER TABLE `sys_role` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_role_data_scope`
--

DROP TABLE IF EXISTS `sys_role_data_scope`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_role_data_scope` (
  `id` bigint NOT NULL COMMENT 'id',
  `role_id` bigint DEFAULT NULL COMMENT '角色ID',
  `dept_id` bigint DEFAULT NULL COMMENT '部门ID',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_role_id` (`role_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色数据权限';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_role_data_scope`
--

LOCK TABLES `sys_role_data_scope` WRITE;
/*!40000 ALTER TABLE `sys_role_data_scope` DISABLE KEYS */;
INSERT INTO `sys_role_data_scope` VALUES (2016842579484213255,2016354427531419649,1067246875800000066,1067246875800000001,'2026-01-29 19:55:17');
/*!40000 ALTER TABLE `sys_role_data_scope` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_role_menu`
--

DROP TABLE IF EXISTS `sys_role_menu`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_role_menu` (
  `id` bigint NOT NULL COMMENT 'id',
  `role_id` bigint DEFAULT NULL COMMENT '角色ID',
  `menu_id` bigint DEFAULT NULL COMMENT '菜单ID',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_role_id` (`role_id`) USING BTREE,
  KEY `idx_menu_id` (`menu_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色菜单关系';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_role_menu`
--

LOCK TABLES `sys_role_menu` WRITE;
/*!40000 ALTER TABLE `sys_role_menu` DISABLE KEYS */;
INSERT INTO `sys_role_menu` VALUES (2016842578976702466,2016354427531419649,2016426590938480641,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702467,2016354427531419649,2016427449415434242,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702468,2016354427531419649,2016427449415434243,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702469,2016354427531419649,2016427449415434244,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702470,2016354427531419649,2016427449415434245,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702471,2016354427531419649,2016427449415434246,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702472,2016354427531419649,2016427449415434247,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702473,2016354427531419649,2016427449415434250,1067246875800000001,'2026-01-29 19:55:17'),(2016842578976702474,2016354427531419649,2016427449415434251,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811330,2016354427531419649,2016424054654828545,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811331,2016354427531419649,2016424054654828546,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811332,2016354427531419649,2016424054654828547,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811333,2016354427531419649,2016424054654828548,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811334,2016354427531419649,2016424054654828549,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811335,2016354427531419649,2016424054654828550,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811336,2016354427531419649,2016427449415434248,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811337,2016354427531419649,2016427449415434249,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811338,2016354427531419649,2016424054986178562,1067246875800000001,'2026-01-29 19:55:17'),(2016842579043811339,2016354427531419649,2016424054986178563,1067246875800000001,'2026-01-29 19:55:17'),(2016842579106725890,2016354427531419649,2016424054986178564,1067246875800000001,'2026-01-29 19:55:17'),(2016842579106725891,2016354427531419649,2016424054986178565,1067246875800000001,'2026-01-29 19:55:17'),(2016842579106725892,2016354427531419649,2016424054986178566,1067246875800000001,'2026-01-29 19:55:17'),(2016842579106725893,2016354427531419649,2016424054986178567,1067246875800000001,'2026-01-29 19:55:17'),(2016842579106725894,2016354427531419649,2016427449415434252,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640449,2016354427531419649,2016427449415434253,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640450,2016354427531419649,1900000000000000001,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640451,2016354427531419649,1900000000000000101,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640452,2016354427531419649,1900000000000000201,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640453,2016354427531419649,1900000000000000202,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640454,2016354427531419649,1900000000000000203,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640455,2016354427531419649,1900000000000000204,1067246875800000001,'2026-01-29 19:55:17'),(2016842579169640456,2016354427531419649,1900000000000000205,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555010,2016354427531419649,1900000000000000102,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555011,2016354427531419649,1900000000000000301,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555012,2016354427531419649,1900000000000000302,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555013,2016354427531419649,1900000000000000303,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555014,2016354427531419649,1900000000000000304,1067246875800000001,'2026-01-29 19:55:17'),(2016842579232555015,2016354427531419649,1900000000000000305,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469569,2016354427531419649,1900000000000000103,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469570,2016354427531419649,1900000000000000401,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469571,2016354427531419649,1900000000000000402,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469572,2016354427531419649,1900000000000000403,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469573,2016354427531419649,1900000000000000404,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469574,2016354427531419649,1900000000000000104,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469575,2016354427531419649,1900000000000000501,1067246875800000001,'2026-01-29 19:55:17'),(2016842579295469576,2016354427531419649,1900000000000000502,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384129,2016354427531419649,2016429000000000301,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384130,2016354427531419649,2016424054856155138,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384131,2016354427531419649,2016424054856155139,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384132,2016354427531419649,2016424054856155140,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384133,2016354427531419649,2016424054856155141,1067246875800000001,'2026-01-29 19:55:17'),(2016842579358384134,2016354427531419649,2016424054856155142,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298689,2016354427531419649,2016424054856155143,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298690,2016354427531419649,2016427449415434256,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298691,2016354427531419649,2016427449415434257,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298692,2016354427531419649,2016424054264758274,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298693,2016354427531419649,2016424054264758275,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298694,2016354427531419649,2016424054264758276,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298695,2016354427531419649,2016424054264758277,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298696,2016354427531419649,2016424054264758278,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298697,2016354427531419649,2016424054264758279,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298698,2016354427531419649,2016427449415434254,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298699,2016354427531419649,2016427449415434255,1067246875800000001,'2026-01-29 19:55:17'),(2016842579421298700,2016354427531419649,2016429000000000001,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213249,2016354427531419649,2016429000000000101,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213250,2016354427531419649,2016429000000000102,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213251,2016354427531419649,2016429000000000103,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213252,2016354427531419649,2016429000000000104,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213253,2016354427531419649,2016429000000000105,1067246875800000001,'2026-01-29 19:55:17'),(2016842579484213254,2016354427531419649,2016429000000000106,1067246875800000001,'2026-01-29 19:55:17');
/*!40000 ALTER TABLE `sys_role_menu` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_role_user`
--

DROP TABLE IF EXISTS `sys_role_user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_role_user` (
  `id` bigint NOT NULL COMMENT 'id',
  `role_id` bigint DEFAULT NULL COMMENT '角色ID',
  `user_id` bigint DEFAULT NULL COMMENT '用户ID',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `idx_role_id` (`role_id`) USING BTREE,
  KEY `idx_user_id` (`user_id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='角色用户关系';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_role_user`
--

LOCK TABLES `sys_role_user` WRITE;
/*!40000 ALTER TABLE `sys_role_user` DISABLE KEYS */;
INSERT INTO `sys_role_user` VALUES (2016547703152644098,2016354427531419649,2016354214011985921,1067246875800000001,'2026-01-29 00:23:33');
/*!40000 ALTER TABLE `sys_role_user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_user`
--

DROP TABLE IF EXISTS `sys_user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_user` (
  `id` bigint NOT NULL COMMENT 'id',
  `username` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '用户名',
  `password` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '密码',
  `real_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '姓名',
  `head_url` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '头像',
  `gender` tinyint unsigned DEFAULT NULL COMMENT '性别   0：男   1：女    2：保密',
  `email` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '邮箱',
  `mobile` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '手机号',
  `dept_id` bigint DEFAULT NULL COMMENT '部门ID',
  `super_admin` tinyint unsigned DEFAULT NULL COMMENT '超级管理员   0：否   1：是',
  `status` tinyint DEFAULT NULL COMMENT '状态  0：停用   1：正常',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `uk_username` (`username`) USING BTREE,
  KEY `idx_create_date` (`create_date`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='系统用户';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_user`
--

LOCK TABLES `sys_user` WRITE;
/*!40000 ALTER TABLE `sys_user` DISABLE KEYS */;
INSERT INTO `sys_user` VALUES (1067246875800000001,'admin','$2a$10$012Kx2ba5jzqr9gLlG4MX.bnQJTD9UWqF57XDo2N3.fPtLne02u/m','管理员',NULL,0,'root@renren.io','13612345678',NULL,1,1,1067246875800000001,'2026-01-28 10:56:16',1067246875800000001,'2026-01-28 10:56:16'),(2016354214011985921,'159535','$2a$10$Q3G9vOygM7Nt5DyG5288VOW6E4Ka4yR9BUfWAV0FtvsO8H54U9bPq','陶洪强',NULL,0,'tao.hongqiang@leoch.com','19955791704',1067246875800000066,0,1,1067246875800000001,'2026-01-28 11:34:41',1067246875800000001,'2026-01-29 00:23:33');
/*!40000 ALTER TABLE `sys_user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sys_user_token`
--

DROP TABLE IF EXISTS `sys_user_token`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `sys_user_token` (
  `id` bigint NOT NULL COMMENT 'id',
  `user_id` bigint NOT NULL COMMENT '用户id',
  `token` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '用户token',
  `expire_date` datetime DEFAULT NULL COMMENT '过期时间',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE,
  UNIQUE KEY `user_id` (`user_id`) USING BTREE,
  UNIQUE KEY `token` (`token`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='系统用户Token';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sys_user_token`
--

LOCK TABLES `sys_user_token` WRITE;
/*!40000 ALTER TABLE `sys_user_token` DISABLE KEYS */;
INSERT INTO `sys_user_token` VALUES (2016345628166840321,1067246875800000001,'33ee086794db0c2393d607962c71148f','2026-02-01 03:45:46','2026-01-31 15:45:46','2026-01-28 11:00:34'),(2016439377614057473,2016354214011985921,'70b639abd31096e125366aa4e0e451c6','2026-01-30 19:57:47','2026-01-30 07:57:47','2026-01-28 17:13:06');
/*!40000 ALTER TABLE `sys_user_token` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_alert_media`
--

DROP TABLE IF EXISTS `tb_alert_media`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_alert_media` (
  `id` bigint NOT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '媒介名称',
  `host` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'SMTP Host',
  `port` int DEFAULT NULL COMMENT '端口',
  `username` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '用户名',
  `password` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '密码',
  `protocol` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '协议',
  `smtp_auth` tinyint DEFAULT NULL COMMENT 'SMTP认证',
  `starttls_enable` tinyint DEFAULT NULL COMMENT 'STARTTLS',
  `tls_enable` tinyint DEFAULT NULL COMMENT 'TLS/SSL',
  `from_addr` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '发件人',
  `status` tinyint DEFAULT NULL COMMENT '状态 0禁用 1启用',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警媒介';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_alert_media`
--

LOCK TABLES `tb_alert_media` WRITE;
/*!40000 ALTER TABLE `tb_alert_media` DISABLE KEYS */;
INSERT INTO `tb_alert_media` VALUES (2016663504140627970,'email-html','smtp.exmail.qq.com',25,'alert@leoch.com','Ls66877268','smtp',1,NULL,NULL,'alert@leoch.com',1,1067246875800000001,'2026-01-29 08:03:42',1067246875800000001,'2026-01-29 08:03:42');
/*!40000 ALTER TABLE `tb_alert_media` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_alert_record`
--

DROP TABLE IF EXISTS `tb_alert_record`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_alert_record` (
  `id` bigint NOT NULL,
  `alert_name` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '告警名称',
  `status` varchar(20) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '告警状态',
  `severity` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '级别',
  `instance` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '实例',
  `summary` varchar(500) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '摘要',
  `description` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '描述',
  `starts_at` datetime DEFAULT NULL COMMENT '开始时间',
  `ends_at` datetime DEFAULT NULL COMMENT '结束时间',
  `receiver` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '接收器',
  `raw_json` longtext CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '原始JSON',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警记录';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_alert_record`
--

LOCK TABLES `tb_alert_record` WRITE;
/*!40000 ALTER TABLE `tb_alert_record` DISABLE KEYS */;
INSERT INTO `tb_alert_record` VALUES (2017143966562754561,'BlackboxProbeFailed','firing','critical','https://file.leoch.net','HTTP probe failed','HTTP probe failed for https://file.leoch.net','2026-01-30 15:52:00','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP probe failed for https://file.leoch.net\",\"summary\":\"HTTP probe failed\"},\"startsAt\":\"2026-01-30T15:52:00+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=14452964390831500238\\u0026alert_id=10687398888944759773\",\"fingerprint\":\"a25fba21e8ca6b57\"}],\"groupLabels\":{\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP probe failed for https://file.leoch.net\",\"summary\":\"HTTP probe failed\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"BlackboxProbeFailed\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 15:52:53',NULL,'2026-01-30 15:52:53'),(2017144748024504322,'HttpProbeFailed','firing','critical','https://file.leoch.net','HTTP 探测失败','HTTP 探测失败 https://file.leoch.net','2026-01-30 15:55:00','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HttpProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"startsAt\":\"2026-01-30T15:55:00+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=18369491523091459250\\u0026alert_id=8066329893628891245\",\"fingerprint\":\"2dc12ab89ace0677\"}],\"groupLabels\":{\"alertname\":\"HttpProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HttpProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HttpProbeFailed\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 15:55:59',NULL,'2026-01-30 15:55:59'),(2017145224673599490,'BlackboxProbeFailed','resolved','recover','https://file.leoch.net','HTTP probe failed','HTTP probe failed for https://file.leoch.net','2026-01-30 15:52:00','2026-01-30 15:54:00','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP probe failed for https://file.leoch.net\",\"summary\":\"HTTP probe failed\"},\"startsAt\":\"2026-01-30T15:52:00+08:00\",\"endsAt\":\"2026-01-30T15:54:00+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=14452964390831500238\\u0026alert_id=10687398888944759773\",\"fingerprint\":\"a25fba21e8ca6b57\"}],\"groupLabels\":{\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"BlackboxProbeFailed\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP probe failed for https://file.leoch.net\",\"summary\":\"HTTP probe failed\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"BlackboxProbeFailed\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 15:57:53',NULL,'2026-01-30 15:57:53'),(2017147893706366978,'HTTP探测失败','firing','critical','https://file.leoch.net','HTTP 探测失败','HTTP 探测失败 https://file.leoch.net','2026-01-30 16:07:30','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"startsAt\":\"2026-01-30T16:07:30+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=18369491523091459250\\u0026alert_id=18090065997569712957\",\"fingerprint\":\"1cc19139f696182f\"}],\"groupLabels\":{\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP探测失败\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:08:29',NULL,'2026-01-30 16:08:29'),(2017148648672698369,'HTTP响应慢告警','firing','warning','https://file.leoch.net','HTTP 响应慢(告警)','HTTP https://file.leoch.net 响应时间 > 3s','2026-01-30 16:10:30','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"startsAt\":\"2026-01-30T16:10:30+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=18369491523091459250\\u0026alert_id=11775317750156918891\",\"fingerprint\":\"f065d75c9c17696d\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢告警\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:11:29',NULL,'2026-01-30 16:11:29'),(2017148648739807234,'HTTP响应慢信息','firing','info','https://file.leoch.net','HTTP 响应慢(信息)','HTTP https://file.leoch.net 响应时间 > 1s','2026-01-30 16:10:30','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 1s\",\"summary\":\"HTTP 响应慢(信息)\"},\"startsAt\":\"2026-01-30T16:10:30+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=18369491523091459250\\u0026alert_id=6502880346948663456\",\"fingerprint\":\"fd43feec191b99c4\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 1s\",\"summary\":\"HTTP 响应慢(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢信息\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:11:29',NULL,'2026-01-30 16:11:29'),(2017148648739807235,'HTTP响应慢严重','firing','critical','https://file.leoch.net','HTTP 响应慢(严重)','HTTP https://file.leoch.net 响应时间 > 8s','2026-01-30 16:10:30','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"startsAt\":\"2026-01-30T16:10:30+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=18369491523091459250\\u0026alert_id=3578907524623262313\",\"fingerprint\":\"15ba4d60d8d5e713\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢严重\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:11:29',NULL,'2026-01-30 16:11:29'),(2017151281340850177,'Linux内存过高告警','firing','warning','192.168.145.114','Linux 内存使用率高(告警)','Linux 192.168.145.114 内存使用率 > 80%','2026-01-30 16:21:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 80%\",\"summary\":\"Linux 内存使用率高(告警)\"},\"startsAt\":\"2026-01-30T16:21:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=4646072665381991833\",\"fingerprint\":\"d227cab642f76e31\"}],\"groupLabels\":{\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 80%\",\"summary\":\"Linux 内存使用率高(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存过高告警\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:21:57',NULL,'2026-01-30 16:21:57'),(2017151281340850178,'Linux内存偏高信息','firing','info','192.168.145.114','Linux 内存使用率偏高(信息)','Linux 192.168.145.114 内存使用率 > 70%','2026-01-30 16:21:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 70%\",\"summary\":\"Linux 内存使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:21:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10771219820745473367\",\"fingerprint\":\"d58e260eef0b4155\"}],\"groupLabels\":{\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 70%\",\"summary\":\"Linux 内存使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存偏高信息\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:21:57',NULL,'2026-01-30 16:21:57'),(2017151281340850179,'Linux内存严重','firing','critical','192.168.145.114','Linux 内存使用率严重(严重)','Linux 192.168.145.114 内存使用率 > 90%','2026-01-30 16:21:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 90%\",\"summary\":\"Linux 内存使用率严重(严重)\"},\"startsAt\":\"2026-01-30T16:21:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=9342005148731894959\",\"fingerprint\":\"66e0842ed776be93\"}],\"groupLabels\":{\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 90%\",\"summary\":\"Linux 内存使用率严重(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存严重\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:21:57',NULL,'2026-01-30 16:21:57'),(2017151335942299649,'HTTP响应慢告警','resolved','recover','https://file.leoch.net','HTTP 响应慢(告警)','HTTP https://file.leoch.net 响应时间 > 3s','2026-01-30 16:11:20','2026-01-30 16:22:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"2026-01-30T16:22:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=11775317750156918891\",\"fingerprint\":\"f065d75c9c17696d\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢告警\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:22:10',NULL,'2026-01-30 16:22:10'),(2017151335942299650,'HTTP响应慢严重','resolved','recover','https://file.leoch.net','HTTP 响应慢(严重)','HTTP https://file.leoch.net 响应时间 > 8s','2026-01-30 16:11:20','2026-01-30 16:22:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"2026-01-30T16:22:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=3578907524623262313\",\"fingerprint\":\"15ba4d60d8d5e713\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢严重\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:22:10',NULL,'2026-01-30 16:22:10'),(2017152539640438785,'Linux内存偏高信息','resolved','recover','192.168.145.114','Linux 内存使用率偏高(信息)','Linux 192.168.145.114 内存使用率 > 70%','2026-01-30 16:25:20','2026-01-30 16:26:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 70%\",\"summary\":\"Linux 内存使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:25:20+08:00\",\"endsAt\":\"2026-01-30T16:26:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10771219820745473367\",\"fingerprint\":\"d58e260eef0b4155\"}],\"groupLabels\":{\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存偏高信息\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 70%\",\"summary\":\"Linux 内存使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存偏高信息\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:26:57',NULL,'2026-01-30 16:26:57'),(2017152539640438786,'Linux内存过高告警','resolved','recover','192.168.145.114','Linux 内存使用率高(告警)','Linux 192.168.145.114 内存使用率 > 80%','2026-01-30 16:21:20','2026-01-30 16:22:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 80%\",\"summary\":\"Linux 内存使用率高(告警)\"},\"startsAt\":\"2026-01-30T16:21:20+08:00\",\"endsAt\":\"2026-01-30T16:22:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=4646072665381991833\",\"fingerprint\":\"d227cab642f76e31\"}],\"groupLabels\":{\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存过高告警\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 80%\",\"summary\":\"Linux 内存使用率高(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存过高告警\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:26:57',NULL,'2026-01-30 16:26:57'),(2017152539661410306,'Linux内存严重','resolved','recover','192.168.145.114','Linux 内存使用率严重(严重)','Linux 192.168.145.114 内存使用率 > 90%','2026-01-30 16:21:20','2026-01-30 16:22:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 90%\",\"summary\":\"Linux 内存使用率严重(严重)\"},\"startsAt\":\"2026-01-30T16:21:20+08:00\",\"endsAt\":\"2026-01-30T16:22:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=9342005148731894959\",\"fingerprint\":\"66e0842ed776be93\"}],\"groupLabels\":{\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"Linux内存严重\",\"instance\":\"192.168.145.114\",\"job\":\"linux\",\"menu_name\":\"10.云电脑\",\"name\":\"陶洪强个人\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"监控\",\"type\":\"linux\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 内存使用率 \\u003e 90%\",\"summary\":\"Linux 内存使用率严重(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"Linux内存严重\\\", instance=\\\"192.168.145.114\\\", job=\\\"linux\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:26:57',NULL,'2026-01-30 16:26:57'),(2017152594208333826,'HTTP响应慢告警','firing','warning','https://file.leoch.net','HTTP 响应慢(告警)','HTTP https://file.leoch.net 响应时间 > 3s','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=11775317750156918891\",\"fingerprint\":\"f065d75c9c17696d\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢告警\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:27:10',NULL,'2026-01-30 16:27:10'),(2017152594208333827,'HTTP响应慢严重','firing','critical','https://file.leoch.net','HTTP 响应慢(严重)','HTTP https://file.leoch.net 响应时间 > 8s','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=3578907524623262313\",\"fingerprint\":\"15ba4d60d8d5e713\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢严重\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:27:10',NULL,'2026-01-30 16:27:10'),(2017154427240710146,'LinuxCPU偏高信息','firing','info','192.168.145.114','Linux CPU 使用率偏高(信息)','Linux 192.168.145.114 CPU 使用率 > 70%','2026-01-30 16:33:50','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:33:50+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10569566760568189187\",\"fingerprint\":\"3368f44bbcd73499\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU偏高信息\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:34:27',NULL,'2026-01-30 16:34:27'),(2017154678575988737,'LinuxCPU过高告警','firing','warning','192.168.145.114','Linux CPU 使用率高(告警)','Linux 192.168.145.114 CPU 使用率 > 80%','2026-01-30 16:34:50','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\",\"severity\":\"warning\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 80%\",\"summary\":\"Linux CPU 使用率高(告警)\"},\"startsAt\":\"2026-01-30T16:34:50+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=15757280218657844109\",\"fingerprint\":\"3fec8ec5e51991cf\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\",\"severity\":\"warning\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 80%\",\"summary\":\"Linux CPU 使用率高(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU过高告警\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:35:27',NULL,'2026-01-30 16:35:27'),(2017154804367360001,'LinuxCPU严重','firing','critical','192.168.145.114','Linux CPU 使用率严重(严重)','Linux 192.168.145.114 CPU 使用率 > 90%','2026-01-30 16:35:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\",\"severity\":\"critical\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 90%\",\"summary\":\"Linux CPU 使用率严重(严重)\"},\"startsAt\":\"2026-01-30T16:35:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=6475864017248772989\",\"fingerprint\":\"21e76a7f43a59c23\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\",\"severity\":\"critical\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 90%\",\"summary\":\"Linux CPU 使用率严重(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU严重\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:35:57',NULL,'2026-01-30 16:35:57'),(2017156062780194817,'LinuxCPU严重','resolved','recover','192.168.145.114','Linux CPU 使用率严重(严重)','Linux 192.168.145.114 CPU 使用率 > 90%','2026-01-30 16:35:20','2026-01-30 16:40:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\",\"severity\":\"critical\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 90%\",\"summary\":\"Linux CPU 使用率严重(严重)\"},\"startsAt\":\"2026-01-30T16:35:20+08:00\",\"endsAt\":\"2026-01-30T16:40:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=6475864017248772989\",\"fingerprint\":\"21e76a7f43a59c23\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU严重\",\"instance\":\"192.168.145.114\",\"severity\":\"critical\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 90%\",\"summary\":\"Linux CPU 使用率严重(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU严重\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:40:57',NULL,'2026-01-30 16:40:57'),(2017156943584034818,'LinuxCPU偏高信息','resolved','recover','192.168.145.114','Linux CPU 使用率偏高(信息)','Linux 192.168.145.114 CPU 使用率 > 70%','2026-01-30 16:33:50','2026-01-30 16:41:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:33:50+08:00\",\"endsAt\":\"2026-01-30T16:41:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10569566760568189187\",\"fingerprint\":\"3368f44bbcd73499\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU偏高信息\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:44:27',NULL,'2026-01-30 16:44:27'),(2017157195162583042,'LinuxCPU过高告警','resolved','recover','192.168.145.114','Linux CPU 使用率高(告警)','Linux 192.168.145.114 CPU 使用率 > 80%','2026-01-30 16:34:50','2026-01-30 16:40:40','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\",\"severity\":\"warning\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 80%\",\"summary\":\"Linux CPU 使用率高(告警)\"},\"startsAt\":\"2026-01-30T16:34:50+08:00\",\"endsAt\":\"2026-01-30T16:40:40+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=15757280218657844109\",\"fingerprint\":\"3fec8ec5e51991cf\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU过高告警\",\"instance\":\"192.168.145.114\",\"severity\":\"warning\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 80%\",\"summary\":\"Linux CPU 使用率高(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU过高告警\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:45:27',NULL,'2026-01-30 16:45:27'),(2017159502277328898,'LinuxCPU偏高信息','firing','info','192.168.145.114','Linux CPU 使用率偏高(信息)','Linux 192.168.145.114 CPU 使用率 > 70%','2026-01-30 16:52:50','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:52:50+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10569566760568189187\",\"fingerprint\":\"3368f44bbcd73499\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU偏高信息\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:54:37',NULL,'2026-01-30 16:54:37'),(2017160760371396610,'LinuxCPU偏高信息','resolved','recover','192.168.145.114','Linux CPU 使用率偏高(信息)','Linux 192.168.145.114 CPU 使用率 > 70%','2026-01-30 16:52:50','2026-01-30 16:56:10','webhook','{\"receiver\":\"webhook\",\"status\":\"resolved\",\"alerts\":[{\"status\":\"resolved\",\"labels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"annotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"startsAt\":\"2026-01-30T16:52:50+08:00\",\"endsAt\":\"2026-01-30T16:56:10+08:00\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=13929140221811352434\\u0026alert_id=10569566760568189187\",\"fingerprint\":\"3368f44bbcd73499\"}],\"groupLabels\":{\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\"},\"commonLabels\":{\"alertgroup\":\"linux_exporter\",\"alertname\":\"LinuxCPU偏高信息\",\"instance\":\"192.168.145.114\",\"severity\":\"info\"},\"commonAnnotations\":{\"description\":\"Linux 192.168.145.114 CPU 使用率 \\u003e 70%\",\"summary\":\"Linux CPU 使用率偏高(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"LinuxCPU偏高信息\\\", instance=\\\"192.168.145.114\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 16:59:37',NULL,'2026-01-30 16:59:37'),(2017209216897114113,'HTTP探测失败','firing','critical','https://file.leoch.net','HTTP 探测失败','HTTP 探测失败 https://file.leoch.net','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=18090065997569712957\",\"fingerprint\":\"1cc19139f696182f\"}],\"groupLabels\":{\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP探测失败\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-30 20:12:10',NULL,'2026-01-30 20:12:10'),(2017330011883388930,'HTTP响应慢信息','firing','info','https://file.leoch.net','HTTP 响应慢(信息)','HTTP https://file.leoch.net 响应时间 > 1s','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 1s\",\"summary\":\"HTTP 响应慢(信息)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=6502880346948663456\",\"fingerprint\":\"fd43feec191b99c4\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢信息\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"info\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 1s\",\"summary\":\"HTTP 响应慢(信息)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢信息\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-31 04:12:10',NULL,'2026-01-31 04:12:10'),(2017335044800724993,'HTTP响应慢严重','firing','critical','https://file.leoch.net','HTTP 响应慢(严重)','HTTP https://file.leoch.net 响应时间 > 8s','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=3578907524623262313\",\"fingerprint\":\"15ba4d60d8d5e713\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢严重\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 8s\",\"summary\":\"HTTP 响应慢(严重)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢严重\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-31 04:32:10',NULL,'2026-01-31 04:32:10'),(2017335044800724994,'HTTP响应慢告警','firing','warning','https://file.leoch.net','HTTP 响应慢(告警)','HTTP https://file.leoch.net 响应时间 > 3s','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=11775317750156918891\",\"fingerprint\":\"f065d75c9c17696d\"}],\"groupLabels\":{\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP响应慢告警\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"warning\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP https://file.leoch.net 响应时间 \\u003e 3s\",\"summary\":\"HTTP 响应慢(告警)\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP响应慢告警\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-31 04:32:10',NULL,'2026-01-31 04:32:10'),(2017391667405602817,'HTTP探测失败','firing','critical','https://file.leoch.net','HTTP 探测失败','HTTP 探测失败 https://file.leoch.net','2026-01-30 16:11:20','0001-01-03 08:00:00','webhook','{\"receiver\":\"webhook\",\"status\":\"firing\",\"alerts\":[{\"status\":\"firing\",\"labels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"annotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"startsAt\":\"2026-01-30T16:11:20+08:00\",\"endsAt\":\"0001-01-01T00:00:00Z\",\"generatorURL\":\"http://192.168.17.121:8880/vmalert/alert?group_id=1110099061253543324\\u0026alert_id=18090065997569712957\",\"fingerprint\":\"1cc19139f696182f\"}],\"groupLabels\":{\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\"},\"commonLabels\":{\"alertgroup\":\"blackbox_exporter\",\"alertname\":\"HTTP探测失败\",\"instance\":\"https://file.leoch.net\",\"job\":\"http_probe\",\"menu_name\":\"03.基础业务\",\"name\":\"文档管理\",\"severity\":\"critical\",\"base_site_location\":\"安徽理士\",\"sub_menu_name\":\"文档管理\",\"type\":\"http_probe\"},\"commonAnnotations\":{\"description\":\"HTTP 探测失败 https://file.leoch.net\",\"summary\":\"HTTP 探测失败\"},\"externalURL\":\"http://ubuntu-server:9093\",\"version\":\"4\",\"groupKey\":\"{}:{alertname=\\\"HTTP探测失败\\\", instance=\\\"https://file.leoch.net\\\", job=\\\"http_probe\\\"}\",\"truncatedAlerts\":0}',NULL,'2026-01-31 08:17:09',NULL,'2026-01-31 08:17:09');
/*!40000 ALTER TABLE `tb_alert_record` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_alert_template`
--

DROP TABLE IF EXISTS `tb_alert_template`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_alert_template` (
  `id` bigint NOT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '模板名称',
  `email_subject` varchar(200) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '主题',
  `email_html` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT 'HTML内容',
  `status` tinyint DEFAULT NULL COMMENT '状态 0禁用 1启用',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警模板';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_alert_template`
--

LOCK TABLES `tb_alert_template` WRITE;
/*!40000 ALTER TABLE `tb_alert_template` DISABLE KEYS */;
INSERT INTO `tb_alert_template` VALUES (1900000000000001001,'灾难告警模板','【灾难告警】${alertname}','<div style=\"border:1px solid #e4e7ed;font-family:\'Microsoft YaHei\',Arial;\">\n <div style=\"background:#E45959;color:#ffffff;padding:12px 14px;\">\n  <span style=\"font-size:16px;font-weight:bold;\">灾难告警</span>\n </div>\n <div style=\"padding:14px;\">\n  <div style=\"background:#fff3f3;border:1px solid #f4c7c7;border-left:6px solid #E45959;padding:12px;margin-bottom:12px;\">\n   <div style=\"font-size:14px;font-weight:bold;color:#b54242;margin-bottom:6px;\">\n    问题/原因\n   </div>\n   <div style=\"font-size:14px;font-weight:bold;\">\n    ${alertname}\n   </div>\n   <div style=\"margin-top:6px;\">\n    ${description}\n   </div>\n  </div>\n  <table style=\"width:100%;font-size:13px;\">\n   <tbody>\n    <tr>\n     <td style=\"width:110px;color:#666;\">设备名称</td>\n     <td>${instance}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">发生时间</td>\n     <td>${startsAt}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">告警级别</td>\n     <td>${severity}</td>\n    </tr>\n   </tbody>\n  </table>\n  <div style=\"margin-top:10px;font-size:12px;color:#999;\">\n   此邮件由 Prometheus 监控中心发送，请勿回复\n  </div>\n </div>\n</div>',1,1067246875800000001,'2026-01-29 09:10:01',1067246875800000001,'2026-01-29 09:10:01'),(1900000000000001002,'重要告警模板','【重要告警】${alertname}','<div style=\"border:1px solid #e4e7ed;font-family:\'Microsoft YaHei\',Arial;\">\n <div style=\"background:#FFA059;color:#ffffff;padding:12px 14px;\"><span style=\"font-size:16px;font-weight:bold;\">重要告警</span>\n </div>\n <div style=\"padding:14px;\">\n  <div style=\"background:#fff6f0;border:1px solid #ffd6b3;border-left:6px solid #FFA059;padding:12px;margin-bottom:12px;\">\n   <div style=\"font-size:14px;font-weight:bold;color:#d97a1f;margin-bottom:6px;\">\n    问题/原因\n   </div>\n   <div style=\"font-size:14px;font-weight:bold;\">\n    ${alertname}\n   </div>\n   <div style=\"margin-top:6px;\">\n    ${description}\n   </div>\n  </div>\n  <table style=\"width:100%;font-size:13px;\">\n   <tbody>\n    <tr>\n     <td style=\"width:110px;color:#666;\">设备名称</td>\n     <td>${instance}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">发生时间</td>\n     <td>${startsAt}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">告警级别</td>\n     <td>${severity}</td>\n    </tr>\n   </tbody>\n  </table>\n  <div style=\"margin-top:10px;font-size:12px;color:#999;\">\n   此邮件由 Prometheus 监控中心发送，请勿回复\n  </div>\n </div>\n</div>',1,1067246875800000001,'2026-01-29 09:10:01',1067246875800000001,'2026-01-29 09:10:01'),(1900000000000001003,'信息提示模板','【信息提示】${alertname}','<div style=\"border:1px solid #e4e7ed;font-family:\'Microsoft YaHei\',Arial;\">\n <div style=\"background:#7499FF;color:#ffffff;padding:12px 14px;\"><span style=\"font-size:16px;font-weight:bold;\">信息提示</span>\n </div>\n <div style=\"padding:14px;\">\n  <div style=\"background:#f3f6ff;border:1px solid #cfdcff;border-left:6px solid #7499FF;padding:12px;margin-bottom:12px;\">\n   <div style=\"font-size:14px;font-weight:bold;color:#3e64c4;margin-bottom:6px;\">\n    信息\n   </div>\n   <div style=\"font-size:14px;font-weight:bold;\">\n    ${alertname}\n   </div>\n   <div style=\"margin-top:6px;\">\n    ${description}\n   </div>\n  </div>\n  <table style=\"width:100%;font-size:13px;\">\n   <tbody>\n    <tr>\n     <td style=\"width:110px;color:#666;\">设备名称</td>\n     <td>${instance}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">发生时间</td>\n     <td>${startsAt}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">告警级别</td>\n     <td>${severity}</td>\n    </tr>\n   </tbody>\n  </table>\n  <div style=\"margin-top:10px;font-size:12px;color:#999;\">\n   此邮件由 Prometheus 监控中心发送，请勿回复\n  </div>\n </div>\n</div>',1,1067246875800000001,'2026-01-29 09:10:01',1067246875800000001,'2026-01-29 09:10:01'),(1900000000000001004,'告警恢复模板','【告警恢复】${alertname}','<div style=\"border:1px solid #e4e7ed;font-family:\'Microsoft YaHei\',Arial;\">\n <div style=\"background:#4CAF50;color:#ffffff;padding:12px 14px;\">\n  <span style=\"font-size:16px;font-weight:bold;\">告警恢复</span>\n </div>\n <div style=\"padding:14px;\">\n  <div style=\"background:#f3fff3;border:1px solid #cde9cd;border-left:6px solid #4CAF50;padding:12px;margin-bottom:12px;\">\n   <div style=\"font-size:14px;font-weight:bold;color:#2f7a35;margin-bottom:6px;\">\n    恢复信息\n   </div>\n   <div style=\"font-size:14px;font-weight:bold;\">\n    ${alertname}\n   </div>\n   <div style=\"margin-top:6px;\">\n    ${description}\n   </div>\n  </div>\n  <table style=\"width:100%;font-size:13px;\">\n   <tbody>\n    <tr>\n     <td style=\"width:110px;color:#666;\">设备名称</td>\n     <td>${instance}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">恢复时间</td>\n     <td>${endsAt}</td>\n    </tr>\n    <tr>\n     <td style=\"color:#666;\">告警级别</td>\n     <td>${severity}</td>\n    </tr>\n   </tbody>\n  </table>\n  <div style=\"margin-top:10px;font-size:12px;color:#999;\">\n   此邮件由 Prometheus 监控中心发送，请勿回复\n  </div>\n </div>\n</div>',1,1067246875800000001,'2026-01-29 09:10:01',1067246875800000001,'2026-01-29 09:10:01');
/*!40000 ALTER TABLE `tb_alert_template` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_alert_trigger`
--

DROP TABLE IF EXISTS `tb_alert_trigger`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_alert_trigger` (
  `id` bigint NOT NULL,
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '触发器名称',
  `template_id` bigint DEFAULT NULL COMMENT '模板ID',
  `media_id` bigint DEFAULT NULL COMMENT '媒介ID',
  `receiver_user_ids` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '接收用户ID列表',
  `severity` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '告警级别(逗号分隔)',
  `match_labels` text CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci COMMENT '匹配标签(JSON)',
  `status` tinyint DEFAULT NULL COMMENT '状态 0禁用 1启用',
  `creator` bigint DEFAULT NULL COMMENT '创建者',
  `create_date` datetime DEFAULT NULL COMMENT '创建时间',
  `updater` bigint DEFAULT NULL COMMENT '更新者',
  `update_date` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='告警触发器';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_alert_trigger`
--

LOCK TABLES `tb_alert_trigger` WRITE;
/*!40000 ALTER TABLE `tb_alert_trigger` DISABLE KEYS */;
INSERT INTO `tb_alert_trigger` VALUES (2016695677321187330,'灾难告警',1900000000000001001,2016663504140627970,'2016354214011985921','critical','',1,1067246875800000001,'2026-01-29 10:11:33',1067246875800000001,'2026-01-29 10:11:33'),(2016696655718092801,'重要告警',1900000000000001002,2016663504140627970,'2016354214011985921','warning','',1,1067246875800000001,'2026-01-29 10:11:33',1067246875800000001,'2026-01-29 10:11:33'),(2016696860442071041,'信息提示',1900000000000001003,2016663504140627970,'2016354214011985921','info','',1,1067246875800000001,'2026-01-29 10:16:15',1067246875800000001,'2026-01-29 10:16:15'),(2016697002700279810,'告警恢复',1900000000000001004,2016663504140627970,'2016354214011985921','recover','',1,1067246875800000001,'2026-01-29 10:16:49',1067246875800000001,'2026-01-29 10:16:49');
/*!40000 ALTER TABLE `tb_alert_trigger` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_backup_agent`
--

DROP TABLE IF EXISTS `tb_backup_agent`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_backup_agent` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `instance` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '名称',
  `area_name_type` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '区域名称',
  `token` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT 'Token',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 0禁用 1启用',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='备份节点表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_backup_agent`
--

LOCK TABLES `tb_backup_agent` WRITE;
/*!40000 ALTER TABLE `tb_backup_agent` DISABLE KEYS */;
INSERT INTO `tb_backup_agent` VALUES (2016771152441659393,'192.168.159.122:8120','安徽备份节点','安徽理士','VQ2ThFmYSu6FeHFc',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659394,'192.168.138.122:8120','江苏备份节点','江苏理士','k4JRYzzSVju981yx',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659395,'192.168.9.122:8120','马来备份节点','海外理士','vE5wEgyw6Fl3QhFR',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659396,'192.168.166.122:8120','安徽锂电备份节点','安徽理士','NtNzjtCWPP3K86qy',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659397,'192.168.7.122:8120','新加坡备份节点','海外理士','uDDndwGo444t1OVk',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659398,'192.168.17.122:8120','深圳备份节点','深圳理士','91shsL8ELYnWCleI',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659399,'192.168.15.122:8120','越南备份节点','海外理士','m53RZe3Y6rTVV8N2',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27'),(2016771152441659400,'192.168.79.122:8120','肇庆备份节点','肇庆理士','kLJqDGiHz7KZwIgs',1,1067246875800000001,'2026-01-29 15:11:27',1067246875800000001,'2026-01-29 15:11:27');
/*!40000 ALTER TABLE `tb_backup_agent` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_business_system`
--

DROP TABLE IF EXISTS `tb_business_system`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_business_system` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `instance` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '名称',
  `area_name_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '区域名称',
  `base_site_location` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '站点位置',
  `menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '分组名称',
  `sub_menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '子组名称',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 0禁用 1启用',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='业务系统表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_business_system`
--

LOCK TABLES `tb_business_system` WRITE;
/*!40000 ALTER TABLE `tb_business_system` DISABLE KEYS */;
INSERT INTO `tb_business_system` VALUES (2017069465481240577,'https://file.leoch.net','文档管理','安徽','安徽理士','03.基础业务','文档管理',1,1067246875800000001,'2026-01-30 10:56:51',1067246875800000001,'2026-01-30 14:41:05');
/*!40000 ALTER TABLE `tb_business_system` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_device_backup`
--

DROP TABLE IF EXISTS `tb_device_backup`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_device_backup` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `instance` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '名称',
  `username` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '用户名',
  `password` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '密码',
  `area_name_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '区域名称',
  `group_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '分组名称',
  `network_device_model` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '设备型号',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 0禁用 1启用',
  `agent_id` bigint NOT NULL COMMENT '节点ID',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE,
  KEY `fk_device_backup_agent` (`agent_id`) USING BTREE,
  CONSTRAINT `fk_device_backup_agent` FOREIGN KEY (`agent_id`) REFERENCES `tb_backup_agent` (`id`) ON DELETE RESTRICT ON UPDATE RESTRICT
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='设备备份表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_device_backup`
--

LOCK TABLES `tb_device_backup` WRITE;
/*!40000 ALTER TABLE `tb_device_backup` DISABLE KEYS */;
/*!40000 ALTER TABLE `tb_device_backup` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_device_backup_history`
--

DROP TABLE IF EXISTS `tb_device_backup_history`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_device_backup_history` (
  `id` bigint NOT NULL COMMENT '主键',
  `name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '主机名',
  `ip` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT 'IP',
  `url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '备份url',
  `backup_time` datetime NOT NULL COMMENT '备份时间',
  `backup_status` int NOT NULL COMMENT '备份状态（1已完成/0异常）',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='设备备份历史表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_device_backup_history`
--

LOCK TABLES `tb_device_backup_history` WRITE;
/*!40000 ALTER TABLE `tb_device_backup_history` DISABLE KEYS */;
/*!40000 ALTER TABLE `tb_device_backup_history` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_device_backup_record`
--

DROP TABLE IF EXISTS `tb_device_backup_record`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_device_backup_record` (
  `id` bigint NOT NULL COMMENT '主键',
  `name` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '主机名',
  `ip` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT 'IP',
  `url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '备份url',
  `last_backup_time` datetime NOT NULL COMMENT '最后备份时间',
  `last_backup_status` int NOT NULL COMMENT '最后备份状态（1已完成/0异常）',
  `backup_num` int DEFAULT '0' COMMENT '备份次数',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='设备备份信息表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_device_backup_record`
--

LOCK TABLES `tb_device_backup_record` WRITE;
/*!40000 ALTER TABLE `tb_device_backup_record` DISABLE KEYS */;
/*!40000 ALTER TABLE `tb_device_backup_record` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_linux_host`
--

DROP TABLE IF EXISTS `tb_linux_host`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_linux_host` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `instance` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '名称',
  `area_name_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '区域名称',
  `base_site_location` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '站点位置',
  `menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '分组名称',
  `sub_menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '子组名称',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 0禁用 1启用',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='Linux主机表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_linux_host`
--

LOCK TABLES `tb_linux_host` WRITE;
/*!40000 ALTER TABLE `tb_linux_host` DISABLE KEYS */;
INSERT INTO `tb_linux_host` VALUES (2016442258870185986,'192.168.152.38:9100','mongoDB服务器','安徽','安徽理士','00.VMware','1',1,1067246875800000001,'2026-01-28 17:24:33',1067246875800000001,'2026-01-30 14:40:51'),(2017140747602227201,'192.168.145.114:9100','陶洪强个人','安徽','安徽理士','10.云电脑','监控',1,1067246875800000001,'2026-01-30 15:40:06',1067246875800000001,'2026-01-30 15:40:06');
/*!40000 ALTER TABLE `tb_linux_host` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_monitor_component`
--

DROP TABLE IF EXISTS `tb_monitor_component`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_monitor_component` (
  `id` bigint NOT NULL COMMENT '主键',
  `name` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '名称',
  `type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT '类型',
  `ip` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci NOT NULL COMMENT 'IP',
  `port` int DEFAULT NULL COMMENT '端口',
  `web_url` varchar(255) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT 'Web地址',
  `online_status` int DEFAULT NULL COMMENT '在线状态 0离线 1在线',
  `version` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '当前版本',
  `latest_version` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci DEFAULT NULL COMMENT '最新版本',
  `update_available` int DEFAULT NULL COMMENT '是否可更新 0否 1是',
  `last_check_time` datetime DEFAULT NULL COMMENT '最后检测时间',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci COMMENT='监控组件';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_monitor_component`
--

LOCK TABLES `tb_monitor_component` WRITE;
/*!40000 ALTER TABLE `tb_monitor_component` DISABLE KEYS */;
INSERT INTO `tb_monitor_component` VALUES (2017171444173778946,'Alertmanager','alertmanager','192.168.17.121',9093,'http://192.168.17.121:9093/#/alerts',1,'0.28.0','0.30.1',1,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:42:04',1067246875800000001,'2026-01-31 15:45:56'),(2017171648344109057,'VMAlert','vmalert','192.168.17.121',8880,'http://192.168.17.121:8880/',1,'v1.96.0','1.134.0',1,'2026-01-31 15:46:00',1067246875800000001,'2026-01-30 17:42:53',1067246875800000001,'2026-01-31 15:46:00'),(2017171795484487681,'Blackbox','blackbox','192.168.159.125',9115,'http://192.168.159.125:9115/',1,'0.28.0','0.28.0',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:43:28',1067246875800000001,'2026-01-31 15:45:56'),(2017171921347162113,'VictoriaMetrics','victoriametrics','192.168.17.121',8428,'http://192.168.17.121:8428/',1,'2.24.0','1.134.0',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:43:58',1067246875800000001,'2026-01-31 15:45:56'),(2017172025529479169,'Prometheus-ah','prometheus','192.168.159.125',9090,'http://192.168.159.125:9090/',1,'3.9.1','3.9.1',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:44:23',1067246875800000001,'2026-01-31 15:45:56'),(2017172278827692034,'Prometheus-sz','prometheus','192.168.17.125',9090,'http://192.168.17.125:9090/ ',1,'3.9.1','3.9.1',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:45:23',1067246875800000001,'2026-01-31 15:45:56'),(2017172393302831105,'Prometheus-js','prometheus','192.168.138.125',9090,'http://192.168.138.125:9090/',1,'3.9.1','3.9.1',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:45:50',1067246875800000001,'2026-01-31 15:45:56'),(2017172504380583937,'Prometheus-zq','prometheus','192.168.79.125',9090,'http://192.168.79.125:9090/',1,'3.9.1','3.9.1',0,'2026-01-31 15:45:56',1067246875800000001,'2026-01-30 17:46:17',1067246875800000001,'2026-01-31 15:45:56');
/*!40000 ALTER TABLE `tb_monitor_component` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tb_window_host`
--

DROP TABLE IF EXISTS `tb_window_host`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!50503 SET character_set_client = utf8mb4 */;
CREATE TABLE `tb_window_host` (
  `id` bigint NOT NULL COMMENT '主键ID',
  `instance` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '地址',
  `name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '名称',
  `area_name_type` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '区域名称',
  `base_site_location` varchar(100) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci DEFAULT NULL COMMENT '站点位置',
  `menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '分组名称',
  `sub_menu_name` varchar(50) CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci NOT NULL COMMENT '子组名称',
  `status` tinyint NOT NULL DEFAULT '1' COMMENT '状态 0禁用 1启用',
  `creator` bigint NOT NULL COMMENT '创建者',
  `create_date` datetime NOT NULL COMMENT '创建时间',
  `updater` bigint NOT NULL COMMENT '更新者',
  `update_date` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_general_ci COMMENT='Windows主机表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tb_window_host`
--

LOCK TABLES `tb_window_host` WRITE;
/*!40000 ALTER TABLE `tb_window_host` DISABLE KEYS */;
INSERT INTO `tb_window_host` VALUES (2017120599168225281,'192.168.155.10:9182','磁盘柜管理','安徽','安徽理士','01.基础服务','1',1,1067246875800000001,'2026-01-30 14:20:02',1067246875800000001,'2026-01-30 14:20:02');
/*!40000 ALTER TABLE `tb_window_host` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2026-01-31 15:55:51
