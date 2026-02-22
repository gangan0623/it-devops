

package net.leoch.common.utils.excel;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.core.util.URLUtil;
import com.alibaba.excel.EasyExcel;
import com.alibaba.excel.converters.longconverter.LongStringConverter;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.List;

/**
 * excel工具类
 *
 * @author Taohongqiang
 */
public class ExcelUtils {

    /**
     * Excel导出
     *
     * @param response  response
     * @param fileName  文件名
     * @param sheetName sheetName
     * @param list      数据List
     * @param pojoClass 对象Class
     */
    public static void exportExcel(HttpServletResponse response, String fileName, String sheetName, List<?> list,
                                   Class<?> pojoClass) throws IOException {
        if (StrUtil.isBlank(fileName)) {
            //当前日期
            fileName = DateUtil.format(new Date(), "yyyyMMddHHmmss");
        }

        response.setContentType("application/vnd.ms-excel");
        response.setCharacterEncoding("UTF-8");
        fileName = URLUtil.encode(fileName, StandardCharsets.UTF_8);
        response.setHeader("Content-disposition", "attachment;filename=" + fileName + ".xlsx");
        EasyExcel.write(response.getOutputStream(), pojoClass).registerConverter(new LongStringConverter()).sheet(sheetName).doWrite(list);
    }

    /**
     * Excel导出，先sourceList转换成List<targetClass>，再导出
     *
     * @param response    response
     * @param fileName    文件名
     * @param sheetName   sheetName
     * @param sourceList  原数据List
     * @param targetClass 目标对象Class
     */
    public static void exportExcelToTarget(HttpServletResponse response, String fileName, String sheetName, List<?> sourceList,
                                           Class<?> targetClass) throws Exception {
        List<?> targetList = BeanUtil.copyToList(sourceList, targetClass);
        exportExcel(response, fileName, sheetName, targetList, targetClass);
    }

}
