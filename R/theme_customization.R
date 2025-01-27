#' 自定义 ggplot2 主题生成脚本
#'
#' 此脚本为多个 ggplot2 主题提供了自定义的修改版本。修改内容包括调整轴文本、图例位置和背景样式等。
#' 每个生成的主题以 `theme_<名称>_modified` 的形式命名，可以直接调用。
#'
#' @import ggplot2
#' @importFrom purrr map set_names walk2
#' @name custom_ggplot2_themes
NULL

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
      legend.position = "top",                         # 图例位置调整到顶部
      panel.background = element_blank(),              # 面板背景透明
      panel.grid.minor = element_blank(),              # 移除次网格线
      plot.background = element_blank(),               # 图表背景透明
      plot.title = element_text(hjust = 0.5),          # 图表标题居中
      plot.subtitle = element_text(hjust = 0.5),       # 副标题居中
      plot.caption = element_text(hjust = 0.5)         # 图表说明居中
    )
}

# 定义主题名称列表
theme_names <- c("gray", "grey", "bw", "linedraw", "dark", "light", "minimal", "classic", "void", "test")

# 逐一定义并导出主题
purrr::walk(theme_names, function(theme_name) {
  theme_func <- get(paste0("theme_", theme_name), envir = asNamespace("ggplot2"), inherits = TRUE)
  theme_modified <- generate_modified_theme(theme_func)
  assign(paste0("theme_", theme_name, "_modified"), theme_modified, envir = .GlobalEnv) # 写入全局环境
  eval(parse(text = sprintf(
    "#' @export\ntheme_%s_modified <- theme_modified", theme_name
  )))
})
