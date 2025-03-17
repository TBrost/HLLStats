#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(bslib)
library(gt)
source('HLL_Lists.R')

# Define UI for application that draws a histogram
navbarPage( # UI ####
  title = "HLL Stats",
  selected = "Line Plots",
  collapsible = TRUE,
  tabPanel(
    title = "Primary Stats",
    page_sidebar(
      theme = bs_theme(version = 5, bootswatch = "darkly"),
      tags$style(
        HTML("
          #a1 .card-header {
             background-color: #228b22 !important;
             color: white;
          }
          #a2 .card-header {
             background-color: #daa520 !important;
             color: white;
          }
          #flex .card-header {
             background-color: #9932cc !important;
             color: white;
          }
          #def .card-header {
             background-color: #009acd !important;
             color: white;
          }
          #sup .card-header {
             background-color: #ee3b3b !important;
             color: white;
          }
          #armor .card-header {
             background-color: #b3b3b3 !important;
             color: white;
          }
          #mg .card-header {
             background-color: #c95bff !important;
             color: white;
          }
          .card-body {
          padding-top: 0;
          padding-bottom: 0;
          }
          .card-body .gt_table {
            width: 100%;
            margin-left: auto; /* Optional: Center the table if needed */
            margin-right: auto; /* Optional: Center the table if needed */
          }
        "
        )
      ),
      sidebar = sidebar(
        title = "Filters",
        # Add any filters here if needed, e.g., date range, player selection
        selectInput(
            "game_outcome",
            "Game Outcome:",
            choices = c(
                "WIN - 5/0", "WIN - 4/1", "WIN - 3/2",
                "LOSS - 2/3", "LOSS - 1/4", "LOSS - 0/5"
            ),
            selected = "WIN - 3/2" # Default selection
        ),
      ),
      layout_column_wrap(
        width = 1/2,
        gap = "2px",
        layout_column_wrap( ### Left side ####
          width = 1,
          heights_equal = "row",
          gap = "2px",
          layout_column_wrap( ### top ####
            width = 1/2,
            heights_equal = "row",
            gap = "2px",
            layout_column_wrap(
              width = 1,
              heights_equal = "row",
              fill = F,
              gap = "2px",
              card(
                  height = 100,
                  card_body(
                      uiOutput("game_info_card"),
                      style = "display: flex; justify-content: center; align-items: center; flex-direction: column; padding: 10px 0px; overflow: hidden; box-sizing: border-box;" # Adjusted padding and min-height
                  )
              ),
              layout_column_wrap( # KDs
                width = 1/2,
                heights_equal = "row",
                gap = "2px",
                card(
                    height = 88,
                    card_body(uiOutput("BOTNKD"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                    )
                ),
                card(
                    height = 88,
                    card_body(uiOutput("opponentKD"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                    )
                  )
              ),
              card(
                  height = 118,
                  full_screen = TRUE,
                  card_body(
                      plotOutput(outputId = "TotalKillsByTeam", width = "100%"),
                      style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                  ) 
              ),
              card(
                  height = 100,
                  full_screen = TRUE,
                  card_body(
                      plotOutput(outputId = "KillsPerMemberByTeam", width = "100%"),
                      style = "display: flex; justify-content: center; align-items: stretch; padding: 0px; overflow: hidden; box-sizing: border-box;"
                  ) 
              ),
                
            ),
            layout_column_wrap(
              width = 1,
              fill = F,
              gap = "2px",
              heights_equal = "row",
              card( # some plot
                  height = 300,
                  full_screen = TRUE,
                  card_body(
                      plotOutput(outputId = "InfRegular", width = "100%"),
                      style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                  ) 
              ),
              card(
                  height = 110,
                  full_screen = TRUE,
                  card_body(
                      plotOutput(outputId = "TotalInfantryRegularKills", width = "100%"),
                      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                  )
              )
            )
          ),
          layout_column_wrap( ### Bottom ####
            width = 1/2,
            heights_equal = "row",
            gap = "2px",
            card(
                height = 350,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "CEByPlayer", width = "100%"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            ),
            card(
                height = 350,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "SEByPlayer", width = "100%"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            )
          )
        ),
        layout_column_wrap( ### Right side ####
          width = 1,
          heights_equal = "row",
          gap = "2px",
          fill = F,
          layout_column_wrap( ### top
            width = 1/2,
            heights_equal = "row",
            gap = "2px",
            card(
                height = 300,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "InfUtility"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            ),
            card(
                height = 300,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "ArmorKills"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            ),
            card(
                height = 110,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "TotalInfantryUtilityKills"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            ),
            card(
                height = 110,
                full_screen = TRUE,
                card_body(
                    plotOutput(outputId = "TotalArmorKills"),
                    style = "display: flex; justify-content: center; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
                )
            )
          ),
          card( ### Bottom ####
            height = 350,
            full_screen = TRUE,
            card_body(
                gt_output(outputId = "divisionPerformance"),
                style = "display: flex; justify-content: center; align-items: center; padding: 2px; overflow: hidden; box-sizing: border-box;"
            )
          )
        )
      )
    )
  ),
  tabPanel( ### Divs ####
    title = "Division Performance",
    layout_column_wrap(
      width = 1/2, # Two columns
      heights_equal = "row",
      gap = "2px",
      layout_column_wrap( ### Left
        width = 1,
        fill = F,
        gap = "2px",
        heights_equal = "row",
        card(
          height = 345,
          full_screen = TRUE,
          id = "a1",
          card_header("Assault 1"),
          card_body(
            gt_output(outputId = "A1Performance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        ),
        card(
          height = 380,
          full_screen = TRUE,
          id = "def",
          card_header("Defense"),
          card_body(
            gt_output(outputId = "DefPerformance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        ),
        card(
          height = 315,
          full_screen = TRUE,
          id = "sup",
          card_header("Support"),
          card_body(
            gt_output(outputId = "SupportPerformance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        ),
        card(
          height = 175,
          full_screen = TRUE,
          id = "mg",
          card_header("MG's"),
          card_body(
            gt_output(outputId = "MGPerformance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        )
      ),
      layout_column_wrap( ### Right
        width = 1,
        fill = F,
        gap = "2px",
        heights_equal = "row",
        card(
          height = 345,
          full_screen = TRUE,
          id = "a2",
          card_header("Assault 2"),
          card_body(
            gt_output(outputId = "A2Performance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        ),
        card(
          height = 380,
          full_screen = TRUE,
          id = "armor",
          card_header("Armor"),
          card_body(
            gt_output(outputId = "ArmorPerformance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        ),
        card(
          height = 315,
          full_screen = TRUE,
          id = "flex",
          card_header("Flex"),
          card_body(
            gt_output(outputId = "FlexPerformance"),
            style = "display: flex; justify-content: flex-start; align-items: center; padding: 0px; overflow: hidden; box-sizing: border-box;"
          )
        )
      )
    )
  )
)
