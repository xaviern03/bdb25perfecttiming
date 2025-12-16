library(tidyverse)
library(gt)
library(scales)

top5 <- read_csv("ptg_route_cov_top5.csv", show_col_types = FALSE)
bottom5 <- read_csv("ptg_route_cov_bottom.csv", show_col_types = FALSE)

make_table <- function(df, title_txt){
  df %>%
    mutate(Rank = row_number()) %>%
    select(
      Rank,
      `Route x Coverage Interaction` = route_coverage,
      `Opportunities` = n_opps,
      `Average Perfect Timing Grade` = avg_ptg
    ) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_txt, "**"))
    ) %>%
    fmt_number(columns = c(`Average Perfect Timing Grade`), decimals = 3) %>%
    fmt_number(columns = c(Opportunities), decimals = 0) %>%
    data_color(
      columns = `Average Perfect Timing Grade`,
      colors = col_numeric(palette = c("#a30808ff", "#fde0dd"), domain = NULL)
    ) %>%
    cols_align(align = "center", columns = c(Rank, Opportunities, `Average Perfect Timing Grade`)) %>%
    cols_align(align = "left", columns = c(`Route x Coverage Interaction`)) %>%
    tab_options(
      table.font.size = px(14),
      heading.title.font.size = px(22)
    )
}

gt_top <- make_table(top5, "Top 5 Route x Coverage Interactions by Average PTG")
gt_bot <- make_table(bottom5, "Bottom 5 Route x Coverage Interactions by Average PTG")

gtsave(gt_top, "ptg_top5_route_cov.png")
gtsave(gt_bot, "ptg_bottom5_route_cov.png")
