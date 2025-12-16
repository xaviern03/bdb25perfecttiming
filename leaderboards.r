library (tidyverse)
library (gt)
library (scales)

cb <- read_csv("ptg_cb_leaderboard.csv")
s  <- read_csv("ptg_s_leaderboard.csv")
lb <- read_csv("ptg_lb_leaderboard.csv")

make_leaderboard <- function(df, title_txt, palette){
  
  df %>%
    mutate(Rank = row_number()) %>%
    select(
      Rank,
      Player = player_name,
      `Average Perfect Timing Grade` = ptg_mean
    ) %>%
    gt() %>%
    tab_header(
      title = md(paste0("**", title_txt, "**"))
    ) %>%
    fmt_number(
      columns = `Average Perfect Timing Grade`,
      decimals = 3
    ) %>%
    data_color(
      columns = `Average Perfect Timing Grade`,
      colors = col_numeric(
        palette = palette,
        domain = NULL
      )
    ) %>%
    cols_align(
      align = "center",
      columns = c(Rank, `Average Perfect Timing Grade`)
    ) %>%
    cols_align(
      align = "left",
      columns = Player
    ) %>%
    tab_options(
      table.font.size = px(14),
      heading.title.font.size = px(22),
      data_row.padding = px(6)
    )
}

gt_cb <- make_leaderboard(
  cb %>% slice_head(n=12),
  "Top Cornerbacks by Perfect Timing Grade (PTG)",
  c("#FEE0D2", "#b802a8ff")
)

gt_s <- make_leaderboard(
  s %>% slice_head(n=12),
  "Top Safeties by Perfect Timing Grade (PTG)",
  c("#DEEBF7", "#08306B")
)

gt_lb <- make_leaderboard(
  lb %>% slice_head(n=12),
  "Top Linebackers by Perfect Timing Grade (PTG)",
  c("#E5F5E0", "#006D2C")
)


gtsave(gt_cb, "ptg_cb_leaderboard.png")
gtsave(gt_s,  "ptg_s_leaderboard.png")
gtsave(gt_lb, "ptg_lb_leaderboard.png")

