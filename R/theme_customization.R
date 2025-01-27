#' 自定义 ggplot2 主题生成脚本
#'
#' 此脚本为多个 ggplot2 主题提供了自定义的修改版本。修改内容包括调整轴文本、图例位置和背景样式等。
#' 生成的主题以 `theme_<名称>_modified` 的形式命名，可以直接调用。

# 加载必要的库
library(tidyverse)

# 定义需要修改的主题名称列表
theme_names <- c("gray","grey","bw","linedraw","dark","light","minimal","classic","void","test")

# 定义一个函数，用于创建修改后的主题
#' @param base_theme 基础 ggplot2 主题函数 (例如：theme_bw)。
#' @param base_size 主题的基本字号，默认值为 8。
#' @param base_line_size 基本线条粗细，默认为 0.5 / 2.126。
#' @param base_rect_size 矩形边框粗细，默认为 0.5 / 2.126。
#' @return 返回一个修改后的 ggplot2 主题对象。
#' @examples
#' theme_bw_modified <- generate_modified_theme(theme_bw)
#' ggplot(mpg, aes(displ, hwy)) + geom_point() + theme_bw_modified
#' @export
generate_modified_theme <- function(base_theme, base_size = 8, base_line_size = 0.5 / 2.126, base_rect_size = 0.5 / 2.126) {
  base_theme(base_size = base_size,
             base_line_size = base_line_size,
             base_rect_size = base_rect_size) +
    theme(
      aspect.ratio = NULL,                             # 不固定图表长宽比
      axis.text = element_text(color = "black"),       # 坐标轴文本颜色改为黑色
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), # x轴文本倾斜 45 度
      axis.ticks = element_line(color = "black"),      # 坐标轴刻度颜色改为黑色
      legend.background = element_blank(),             # 图例背景透明
      legend.position = "top",                        # 图例位置调整到顶部
      panel.background = element_blank(),              # 面板背景透明
      panel.grid.minor = element_blank(),              # 移除次网格线
      plot.background = element_blank(),               # 图表背景透明
      plot.title = element_text(hjust = 0.5),          # 图表标题居中
      plot.subtitle = element_text(hjust = 0.5),       # 副标题居中
      plot.caption = element_text(hjust = 0.5)         # 图表说明居中
    )
}

# 使用 purrr::map 生成修改后的主题
# 将 ggplot2 自带的主题传递给 `generate_modified_theme`，批量生成修改后的主题
# 名称以 "theme_<主题名称>_modified" 的形式保存
theme_modified_list <- map(theme_names, ~ {
  theme_func <- get(paste0("theme_", .x), envir = asNamespace("ggplot2"), inherits = TRUE)
  generate_modified_theme(theme_func)
})

# 为生成的主题添加名称
theme_modified_list <- set_names(theme_modified_list, paste0("theme_", theme_names, "_modified"))

# 将生成的主题逐一导出为对象，方便直接调用
list2env(theme_modified_list, envir = .GlobalEnv)

# 备注:
# - 生成的主题可以通过 "theme_<名称>_modified" 调用，例如 theme_bw_modified。
# - 修改内容包括调整坐标轴文本颜色、图例位置、网格线和背景等。
# - 此脚本使用 purrr 包中的 map 进行函数式编程，以批量生成主题。
