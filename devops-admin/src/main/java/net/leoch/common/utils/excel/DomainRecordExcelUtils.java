package net.leoch.common.utils.excel;

import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.core.util.URLUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.converters.longconverter.LongStringConverter;
import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.SheetWriteHandler;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import com.alibaba.excel.write.handler.context.SheetWriteHandlerContext;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import com.alibaba.excel.write.style.HorizontalCellStyleStrategy;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.List;

/**
 * 域名记录 Excel 工具类 — 支持分区标题行、节点池合并单元格、隔行变色
 *
 * 列布局（共32列，0-based）：
 *   0-10  基础信息
 *   11-13 高级策略（走应用交付、开启内网配置、开启公网配置）
 *   14-25 应用交付（内网虚拟服务14-17 + 外网虚拟服务18-21 + 节点池22 + 节点明细23-25）
 *   26-27 解析配置
 *   28-31 防火墙映射
 */
public class DomainRecordExcelUtils {

    // 分区定义: [标题, 起始列索引(0-based), 结束列索引]
    private static final String[][] PARTITIONS = {
        {"基础信息", "0", "10"},
        {"高级策略", "11", "13"},
        {"应用交付", "14", "25"},
        {"解析配置", "26", "27"},
        {"防火墙映射", "28", "31"}
    };

    // 节点明细列
    private static final int NODE_IP_COL = 23;
    private static final int NODE_PORT_COL = 24;
    private static final int NODE_SORT_COL = 25;

    private static final short TITLE_BG = IndexedColors.GREY_25_PERCENT.getIndex();
    private static final short HEADER_BG = IndexedColors.PALE_BLUE.getIndex();
    private static final short SECTION_FONT_COLOR = IndexedColors.WHITE.getIndex();
    private static final short SECTION_BG = IndexedColors.TEAL.getIndex();

    /**
     * 带分区样式 + 节点合并单元格的导出（无说明行）
     *
     * @param mergeRanges 需要合并的区域列表，每项 [firstRow, lastRow, firstCol, lastCol]
     */
    public static <T> void exportWithStyle(HttpServletResponse response, String fileName, String sheetName,
                                           List<T> list, Class<T> pojoClass,
                                           List<int[]> mergeRanges) throws IOException {
        writeExcel(response, fileName, sheetName, list, pojoClass, mergeRanges, false);
    }

    /**
     * 带分区样式 + 说明行 + 节点合并单元格的导出（用于模板下载）
     *
     * @param mergeRanges 需要合并的区域列表，每项 [firstRow, lastRow, firstCol, lastCol]
     */
    public static <T> void exportTemplateWithStyle(HttpServletResponse response, String fileName, String sheetName,
                                                    List<T> list, Class<T> pojoClass,
                                                    List<int[]> mergeRanges) throws IOException {
        writeExcel(response, fileName, sheetName, list, pojoClass, mergeRanges, true);
    }

    private static <T> void writeExcel(HttpServletResponse response, String fileName, String sheetName,
                                       List<T> list, Class<T> pojoClass,
                                       List<int[]> mergeRanges, boolean withDescription) throws IOException {
        if (StrUtil.isBlank(fileName)) {
            fileName = DateUtil.format(new Date(), "yyyyMMddHHmmss");
        }

        response.setContentType("application/vnd.ms-excel");
        response.setCharacterEncoding("UTF-8");
        fileName = URLUtil.encode(fileName, StandardCharsets.UTF_8);
        response.setHeader("Content-disposition", "attachment;filename=" + fileName + ".xlsx");

        WriteCellStyle headerStyle = buildHeaderStyle();
        WriteCellStyle dataStyle = buildDataStyle(IndexedColors.WHITE.getIndex());
        WriteCellStyle evenStyle = buildDataStyle(TITLE_BG);
        HorizontalCellStyleStrategy styleStrategy = new HorizontalCellStyleStrategy(headerStyle, dataStyle);

        int extraRows = withDescription ? 1 : 0;

        EasyExcel.write(response.getOutputStream(), pojoClass)
            .registerConverter(new LongStringConverter())
            .registerWriteHandler(styleStrategy)
            .registerWriteHandler(withDescription ? new SectionHeaderWithDescWriteHandler() : new SectionHeaderWriteHandler())
            .registerWriteHandler(new AlternatingRowStyleHandler(dataStyle, evenStyle, 1 + extraRows))
            .registerWriteHandler(new NodeMergeWriteHandler(mergeRanges))
            .sheet(sheetName)
            .relativeHeadRowIndex(1 + extraRows)
            .doWrite(list);
    }

    private static WriteCellStyle buildHeaderStyle() {
        WriteCellStyle style = new WriteCellStyle();
        style.setFillForegroundColor(HEADER_BG);
        style.setFillPatternType(FillPatternType.SOLID_FOREGROUND);
        style.setHorizontalAlignment(HorizontalAlignment.CENTER);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        style.setWrapped(true);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        WriteFont font = new WriteFont();
        font.setFontName("微软雅黑");
        font.setFontHeightInPoints((short) 10);
        font.setBold(true);
        font.setColor(IndexedColors.ROYAL_BLUE.getIndex());
        style.setWriteFont(font);
        return style;
    }

    private static WriteCellStyle buildDataStyle(short bg) {
        WriteCellStyle style = new WriteCellStyle();
        style.setFillForegroundColor(bg);
        style.setFillPatternType(FillPatternType.SOLID_FOREGROUND);
        style.setVerticalAlignment(VerticalAlignment.CENTER);
        style.setBorderTop(BorderStyle.THIN);
        style.setBorderBottom(BorderStyle.THIN);
        style.setBorderLeft(BorderStyle.THIN);
        style.setBorderRight(BorderStyle.THIN);
        WriteFont font = new WriteFont();
        font.setFontName("微软雅黑");
        font.setFontHeightInPoints((short) 10);
        style.setWriteFont(font);
        return style;
    }

    /**
     * 分区标题行写入器（仅标题行，无说明行）— 用于普通导出
     */
    private static class SectionHeaderWriteHandler implements SheetWriteHandler {
        @Override
        public void afterSheetCreate(SheetWriteHandlerContext context) {
            Sheet sheet = context.getWriteSheetHolder().getSheet();
            Workbook workbook = sheet.getWorkbook();

            CellStyle sectionStyle = workbook.createCellStyle();
            sectionStyle.setFillForegroundColor(SECTION_BG);
            sectionStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            sectionStyle.setAlignment(HorizontalAlignment.CENTER);
            sectionStyle.setVerticalAlignment(VerticalAlignment.CENTER);
            sectionStyle.setBorderTop(BorderStyle.MEDIUM);
            sectionStyle.setBorderBottom(BorderStyle.MEDIUM);
            sectionStyle.setBorderLeft(BorderStyle.MEDIUM);
            sectionStyle.setBorderRight(BorderStyle.MEDIUM);

            Font sectionFont = workbook.createFont();
            sectionFont.setFontName("微软雅黑");
            sectionFont.setFontHeightInPoints((short) 12);
            sectionFont.setBold(true);
            sectionFont.setColor(SECTION_FONT_COLOR);
            sectionStyle.setFont(sectionFont);

            Row sectionRow = sheet.createRow(0);
            sectionRow.setHeightInPoints(28);

            for (String[] partition : PARTITIONS) {
                String title = partition[0];
                int firstCol = Integer.parseInt(partition[1]);
                int lastCol = Integer.parseInt(partition[2]);
                Cell cell = sectionRow.createCell(firstCol);
                cell.setCellValue(title);
                cell.setCellStyle(sectionStyle);
                if (firstCol != lastCol) {
                    sheet.addMergedRegionUnsafe(new CellRangeAddress(0, 0, firstCol, lastCol));
                }
            }

            // 冻结窗格：分区标题 + 表头
            sheet.createFreezePane(0, 2);
        }
    }

    /**
     * 分区标题 + 说明行写入器 — 用于模板下载
     * 在分区标题和表头之间插入说明行
     */
    private static class SectionHeaderWithDescWriteHandler implements SheetWriteHandler {
        private static final String DESCRIPTION =
            "说明：【高级策略】3个开关控制后续列是否需要填写——" +
            "关闭「走应用交付」→ 应用交付整区不填；" +
            "关闭「开启内网配置」→ 内网虚拟服务、内网目标IP不填；" +
            "关闭「开启公网配置」→ 外网虚拟服务、外网记录值、防火墙映射不填。" +
            "本模板含5种场景示例（以\"示例\"开头的域名导入时自动跳过），可直接修改后导入。";

        @Override
        public void afterSheetCreate(SheetWriteHandlerContext context) {
            Sheet sheet = context.getWriteSheetHolder().getSheet();
            Workbook workbook = sheet.getWorkbook();

            // ── 分区标题行（第0行）──
            CellStyle sectionStyle = workbook.createCellStyle();
            sectionStyle.setFillForegroundColor(SECTION_BG);
            sectionStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            sectionStyle.setAlignment(HorizontalAlignment.CENTER);
            sectionStyle.setVerticalAlignment(VerticalAlignment.CENTER);
            sectionStyle.setBorderTop(BorderStyle.MEDIUM);
            sectionStyle.setBorderBottom(BorderStyle.MEDIUM);
            sectionStyle.setBorderLeft(BorderStyle.MEDIUM);
            sectionStyle.setBorderRight(BorderStyle.MEDIUM);
            Font sectionFont = workbook.createFont();
            sectionFont.setFontName("微软雅黑");
            sectionFont.setFontHeightInPoints((short) 12);
            sectionFont.setBold(true);
            sectionFont.setColor(SECTION_FONT_COLOR);
            sectionStyle.setFont(sectionFont);

            Row sectionRow = sheet.createRow(0);
            sectionRow.setHeightInPoints(28);
            for (String[] partition : PARTITIONS) {
                String title = partition[0];
                int firstCol = Integer.parseInt(partition[1]);
                int lastCol = Integer.parseInt(partition[2]);
                Cell cell = sectionRow.createCell(firstCol);
                cell.setCellValue(title);
                cell.setCellStyle(sectionStyle);
                if (firstCol != lastCol) {
                    sheet.addMergedRegionUnsafe(new CellRangeAddress(0, 0, firstCol, lastCol));
                }
            }

            // ── 说明行（第1行，黄底）──
            CellStyle descStyle = workbook.createCellStyle();
            descStyle.setFillForegroundColor(IndexedColors.LEMON_CHIFFON.getIndex());
            descStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
            descStyle.setAlignment(HorizontalAlignment.LEFT);
            descStyle.setVerticalAlignment(VerticalAlignment.CENTER);
            descStyle.setWrapText(true);
            descStyle.setBorderTop(BorderStyle.THIN);
            descStyle.setBorderBottom(BorderStyle.THIN);
            descStyle.setBorderLeft(BorderStyle.THIN);
            descStyle.setBorderRight(BorderStyle.THIN);
            Font descFont = workbook.createFont();
            descFont.setFontName("微软雅黑");
            descFont.setFontHeightInPoints((short) 10);
            descFont.setColor(IndexedColors.ROSE.getIndex());
            descStyle.setFont(descFont);

            Row descRow = sheet.createRow(1);
            descRow.setHeightInPoints(50);
            Cell descCell = descRow.createCell(0);
            descCell.setCellValue(DESCRIPTION);
            descCell.setCellStyle(descStyle);
            sheet.addMergedRegionUnsafe(new CellRangeAddress(1, 1, 0, 31));

            // ── 冻结窗格：分区标题 + 说明行 + 表头 ──
            sheet.createFreezePane(0, 3);
        }
    }

    /**
     * 隔行变色处理器
     */
    private static class AlternatingRowStyleHandler implements CellWriteHandler {
        private final WriteCellStyle oddStyle;
        private final WriteCellStyle evenStyle;
        private final int headerOffset;

        AlternatingRowStyleHandler(WriteCellStyle oddStyle, WriteCellStyle evenStyle, int headerOffset) {
            this.oddStyle = oddStyle;
            this.evenStyle = evenStyle;
            this.headerOffset = headerOffset;
        }

        @Override
        public void afterCellDispose(CellWriteHandlerContext context) {
            Cell cell = context.getCell();
            int rowIndex = cell.getRowIndex();
            if (rowIndex >= headerOffset) {
                int dataRowIndex = rowIndex - headerOffset;
                WriteCellStyle style = (dataRowIndex % 2 == 0) ? evenStyle : oddStyle;
                cell.setCellStyle(cell.getSheet().getWorkbook().createCellStyle());
                CellStyle cellStyle = cell.getCellStyle();
                cellStyle.setFillForegroundColor(style.getFillForegroundColor());
                cellStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
                cellStyle.setVerticalAlignment(style.getVerticalAlignment());
                cellStyle.setBorderTop(style.getBorderTop());
                cellStyle.setBorderBottom(style.getBorderBottom());
                cellStyle.setBorderLeft(style.getBorderLeft());
                cellStyle.setBorderRight(style.getBorderRight());
                WriteFont wFont = style.getWriteFont();
                Font poiFont = cell.getSheet().getWorkbook().createFont();
                poiFont.setFontName(wFont.getFontName());
                poiFont.setFontHeightInPoints(wFont.getFontHeightInPoints());
                poiFont.setBold(wFont.getBold() != null && wFont.getBold());
                if (wFont.getColor() != null) {
                    poiFont.setColor(wFont.getColor());
                }
                cellStyle.setFont(poiFont);
            }
        }
    }

    /**
     * 节点池合并单元格处理器 — 在写入完成后合并指定区域
     */
    private static class NodeMergeWriteHandler implements SheetWriteHandler {
        private final List<int[]> mergeRanges;

        NodeMergeWriteHandler(List<int[]> mergeRanges) {
            this.mergeRanges = mergeRanges;
        }

        @Override
        public void afterSheetCreate(SheetWriteHandlerContext context) {
            // 合并操作在 afterSheetCreate 中注册，实际在写入完成后生效
            if (mergeRanges != null) {
                Sheet sheet = context.getWriteSheetHolder().getSheet();
                for (int[] range : mergeRanges) {
                    sheet.addMergedRegionUnsafe(new CellRangeAddress(range[0], range[1], range[2], range[3]));
                }
            }
        }
    }
}
