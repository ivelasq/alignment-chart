library(magrittr)
library(gt)

# install homebrew: https://brew.sh/
# brew tap homebrew/cask-fonts
# brew install font-inconsolata
# brew install font-anton

dat <-
  tibble::tibble(
    col_1 = c(
      "LAWFUL TIDY",
      "mtcars %>%<br />select(contains(\"p\")) %>% names()",
      "LAWFUL NEUTRAL",
      "grep(\"p\", names(mtcars),<br>value = TRUE)",
      "LAWFUL BASE",
      "names(mtcars) |><br />(\\\\(.) grep(\"p\", ., value = TRUE))()"
    ),
    col_2 = c(
      "NEUTRAL TIDY",
      "mtcars %>%<br />select(contains(\"p\")) %>% names",
      "TRUE NEUTRAL",
      "names(mtcars)[grep(\"p\",<br>names(mtcars))]",
      "NEUTRAL BASE",
      "names(mtcars) |><br>(\\\\(.) .[grep(\"p\", .)])()"
    ),
    col_3 = c(
      "CHAOTIC TIDY",
      "mtcars %>%<br />select(grep(\"p\", names(.))) %>% names",
      "CHAOTIC NEUTRAL",
      "mtcars %>%<br>names %>% grep(\"p\", ., value = TRUE)",
      "CHAOTIC BASE",
      "names(mtcars) |><br />(\\\\(.) \\`[\\`(., grep(\"p\", .)))()"
    )
  )

align_gt <-
  dat %>%
  gt() %>%
  fmt_markdown(columns = everything()) %>%
  cols_align(align = "center") %>%
  tab_options(
    column_labels.hidden = TRUE,
    data_row.padding = px(10),
    table_body.border.bottom.style = "dashed",
    table_body.border.bottom.width = px(3),
    table_body.border.bottom.color = "#000000"
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "lightgrey"),
      cell_text(
        font = "Inconsolata",
        size = 8
      ),
      cell_borders(
        sides = "all",
        color = "#000000",
        style = "dashed",
        weight = px(3)
      )
    ),
    locations = cells_body(
      columns = c(1:3),
      rows = c(2, 4, 6)
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(
        font = "Anton",
        size = 26,
        weight = "bold"
      ),
      cell_borders(
        sides = "top",
        color = "#000000",
        style = "dashed",
        weight = px(3)
      ),
      cell_borders(
        sides = "bottom",
        color = "#000000",
        style = "dashed",
        weight = px(3)
      ),
      cell_borders(
        sides = "right",
        color = "#FFFFFF",
        style = "solid",
        weight = px(3)
      ),
      cell_borders(
        sides = "left",
        color = "#FFFFFF",
        style = "solid",
        weight = px(3)
      )
    ),
    locations = cells_body(
      columns = c(1:3),
      rows = c(1, 3, 5)
    )
  ) %>%
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        color = "#FFFFFF",
        style = "solid",
        weight = px(3)
      )
    ),
    locations = cells_body(
      columns = c(1:3),
      rows = 1
    )
  ) %>%
  cols_width(everything() ~ px(310))

align_gt

gtsave(align_gt, here::here("figs", "alignment_chart.html"))
gtsave(align_gt, here::here("figs", "alignment_chart.png"))
