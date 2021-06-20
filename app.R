library(shiny)
library(gt)
library(tidyverse)

gtDat <-
    tibble::tibble(
        col_1 = c(
            "LAWFUL GOOD",
            "",
            "LAWFUL NEUTRAL",
            "",
            "LAWFUL EVIL",
            ""
        ),
        col_2 = c(
            "NEUTRAL GOOD",
            "",
            "TRUE NEUTRAL",
            "",
            "NEUTRAL EVIL",
            ""
        ),
        col_3 = c(
            "CHAOTIC GOOD",
            "",
            "CHAOTIC NEUTRAL",
            "",
            "CHAOTIC EVIL",
            ""
        )
    )

# Create UI
ui <- fluidPage(

    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")), 
    
    # Application title
    titlePanel(windowTitle = "{gt} Alignment Chart Creator",
               h1("{gt} Alignment Chart Creator")),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            width = 3,
            textInput(
                inputId = "lawfulGood",
                label = "Lawful Good"
            ),
            textInput(
                inputId = "Neutral Good",
                label = "Neutral Good"
            ),
            textInput(
                inputId = "chaoticGood",
                label = "Chaotic Good"
            ),
            textInput(
                inputId = "lawfulNeutral",
                label = "Lawful Neutral"
            ),
            textInput(
                inputId = "trueNeutral",
                label = "True Neutral"
            ),
            textInput(
                inputId = "chaoticNeutral",
                label = "Chaotic Neutral"
            ),
            textInput(
                inputId = "lawfulEvil",
                label = "Lawful Evil"
            ),
            textInput(
                inputId = "neutralEvi",
                label = "Neutral Evil"
            ),
            textInput(
                inputId = "chaoticEvil",
                label = "Chaotic Evil"
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            width = 9,
            gt_output(outputId = "alignmentChart")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gtTabReact <- reactiveVal(NULL)
    gtTabReact(gtDat)

    gtTabUpdate <- reactive({
        if (!is.null(input$lawfulGood)) {
            gtTabReact() %>%
                mutate_at(vars(col_1),
                          function(x) replace(x,
                                              row_number(x) == 14,
                                              input$lawfulGood))
            
        }
        if (!is.null(input$lawfulNeutral)) {
            gtTabReact() %>%
                mutate_at(vars(col_1),
                          function(x) replace(x,
                                              row_number(x) == 2,
                                              input$lawfulNeutral))
            
        }
        if (!is.null(input$lawfulNeutral)) {
            gtTabReact() %>%
                mutate_at(vars(col_1),
                          function(x) replace(x,
                                              row_number(x) == 2,
                                              input$lawfulNeutral))
            
        }
    })

    output$alignmentChart <- ({
        
        render_gt(gtTabUpdate() %>% 
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
                      cols_width(everything() ~ px(290)))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
