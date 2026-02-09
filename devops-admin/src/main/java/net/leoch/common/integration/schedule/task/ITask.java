

package net.leoch.common.integration.schedule.task;

/**
 * 定时任务接口，所有定时任务都要实现该接口
 *
 * @author Taohongqiang
 * @since 1.0.0
 */
public interface ITask {

    /**
     * 执行定时任务接口
     *
     * @param params   参数，多参数使用JSON数据
     */
    void run(String params);
}