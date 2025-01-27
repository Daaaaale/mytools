#' 保存图形为多种格式
#'
#' @description
#' 该函数用于将图形保存为 PDF（使用 `cairo_pdf` 设备）、PNG 和 JPEG 格式。
#' 默认情况下，函数会同时保存为 PDF、PNG 和 JPEG 格式。
#' 用户可以通过 `formats` 参数指定需要保存的格式。
#'
#' @param plot 要保存的图形对象（如 `ggplot2` 对象）。默认值为 `ggplot2::last_plot()`，即最近创建的 `ggplot2` 图形。
#' @param filename 文件名（不带扩展名），默认为lastplot。文件将保存到指定路径，路径中的文件夹如果不存在会自动创建。
#' @param width 图形的宽度，默认值为 7。
#' @param height 图形的高度，默认值为 7。
#' @param units 宽度和高度的单位，支持 `"cm"`（厘米，默认）、`"in"`（英寸）和 `"px"`（像素）。
#' @param dpi 图像的分辨率（每英寸点数），默认值为 600。
#' @param path 保存文件的路径（可选）。如果未指定，文件将保存到当前工作目录。
#' @param formats 要保存的文件格式，支持 `"pdf"`、`"png"` 和 `"jpeg"`。默认同时保存为三种格式。
#'
#' @return 无返回值。函数会将图形保存到指定路径，并在控制台显示成功消息。
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' # 创建一个简单的 ggplot 对象
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#' # 默认保存为 PDF、PNG 和 JPEG，尺寸为 7 cm x 7 cm，分辨率为 600 DPI
#' ddsave(filename = "output/my_plot", width = 7, height = 7, units = "cm", dpi = 600)
#'
#' # 仅保存为 PDF 和 PNG
#' ddsave(filename = "output/my_plot", formats = c("pdf", "png"))
#' }
#'
#' @export
ddsave <- function(plot = ggplot2::last_plot(), filename = "lastplot", width = 7, height = 7, units = "cm", dpi = 600, path = NULL, formats = c("pdf", "png", "jpeg")) {
  # 检查 ggplot2 是否已加载
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请安装并加载 ggplot2 包：install.packages('ggplot2')")
  }

  # 检查 ggpubr 是否已加载，如果未加载则尝试加载
  if (!requireNamespace("ggpubr", quietly = TRUE)) {
    stop("请安装 ggpubr 包：install.packages('ggpubr')")
  }

  # 检查 filename 是否包含扩展名
  if (grepl("\\.", filename)) {
    stop("文件名不应包含扩展名，只需提供基本名称。")
  }

  # 检查 units 是否有效
  if (!units %in% c("cm", "in", "px")) {
    stop("units 必须是 'cm'、'in' 或 'px'。")
  }

  # 检查 formats 是否有效
  valid_formats <- c("pdf", "png", "jpeg")
  if (!all(formats %in% valid_formats)) {
    stop("formats 参数只能包含 'pdf'、'png' 或 'jpeg'。")
  }

  # 根据单位计算宽度和高度（转换为英寸）
  if (units == "cm") {
    width_in <- width / 2.54
    height_in <- height / 2.54
    width_px <- width * (dpi / 2.54)
    height_px <- height * (dpi / 2.54)
  } else if (units == "in") {
    width_in <- width
    height_in <- height
    width_px <- width * dpi
    height_px <- height * dpi
  } else if (units == "px") {
    width_in <- width / dpi
    height_in <- height / dpi
    width_px <- width
    height_px <- height
  }

  # 如果指定了路径，拼接路径和文件名
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }

  # 提取文件夹路径
  folder <- dirname(filename)

  # 如果文件夹不存在，则创建
  if (!dir.exists(folder) && folder != ".") {
    dir.create(folder, recursive = TRUE)
    message("文件夹已创建：", folder)
  }

  # 保存为 PDF（使用 ggsave 和 cairo_pdf 设备）
  if ("pdf" %in% formats) {
    ggplot2::ggsave(
      paste0(filename, ".pdf"),
      plot = plot,
      width = width_in,
      height = height_in,
      units = "in",
      dpi = dpi,
      device = cairo_pdf,
      create.dir = TRUE
    )
    message("PDF 文件已保存：", paste0(filename, ".pdf"))
  }

  # 保存为 PNG 和 JPEG（使用 ggpubr::ggexport）
  if ("png" %in% formats) {
    ggpubr::ggexport(
      plot = plot,
      filename = paste0(filename, ".png"),
      width = width_px,
      height = height_px,
      res = dpi,
      verbose = FALSE  # 关闭 ggexport 的提示信息
    )
    message("PNG 文件已保存：", paste0(filename, ".png"))
  }

  if ("jpeg" %in% formats) {
    ggpubr::ggexport(
      plot = plot,
      filename = paste0(filename, ".jpeg"),
      width = width_px,
      height = height_px,
      res = dpi,
      verbose = FALSE  # 关闭 ggexport 的提示信息
    )
    message("JPEG 文件已保存：", paste0(filename, ".jpeg"))
  }
}
