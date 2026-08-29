#Economic Forecast  - National, Federal Reserve Bank (FRED)

library(fredr)
# Pull the FRED key from the GitHub Action environment
fred_key <- Sys.getenv("FRED_API_KEY")
fredr_set_key(fred_key)

library(fredr)
library(tidyverse)
library(ggplot2)
library(scales)
library(plotly)
library(htmlwidgets)
#######################################################################################################
#Get national data for market yields on treasuries

#Desired series ids
nvars <- c("CPIAUCSL",       # CPI all cities all goods
           "DGS1MO",         # Market Yield on U.S. Treasury Securities at 1-Month Constant Maturity, Quoted on an Investment Basis
           "DGS3MO",         # Market Yield on U.S. Treasury Securities at 3-Month Constant Maturity, Quoted on an Investment Basis
           "DGS2",           # Market Yield on U.S. Treasury Securities at 2-Year Constant Maturity, Quoted on an Investment Basis
           "DGS5",           # Market Yield on U.S. Treasury Securities at 5-Year Constant Maturity, Quoted on an Investment Basis
           "DGS10",          # Market Yield on U.S. Treasury Securities at 10-Year Constant Maturity, Quoted on an Investment Basis
           "DGS20",          # Market Yield on U.S. Treasury Securities at 20-Year Constant Maturity, Quoted on an Investment Basis
           "DGS30",          # Market Yield on U.S. Treasury Securities at 30-Year Constant Maturity, Quoted on an Investment Basis
           "DPCREDIT"        # Discount Window Primary Credit Rate
           #          "PRS85006092",         # labor productivity
)

countr <- 1
for (svarname in nvars) {                                                       #For Loop: Retrieve data from FRED for specified series ids
  if (countr == 1) {                                                            #IF STATEMENT: if first pull, create data frame, else bind to existing data frame
    tbl_fred <- fredr(
      series_id = svarname,
      frequency = "m",
      aggregation_method = "avg",
      observation_start = as.Date("1990-01-01")
    )
  } else {
    tbl_tmp <- fredr(
      series_id = svarname,
      frequency = "m",
      aggregation_method = "avg",
      observation_start = as.Date("1990-01-01")
    )
    tbl_fred <- bind_rows(tbl_fred,tbl_tmp)
  }                                                                             #END if else statement
  countr <- countr + 1
}                                                                               #END for loop

rm(countr, nvars, svarname, tbl_tmp)                                            #remove superfluous variables

tbl_fred <- tbl_fred |> 
  select(-c(realtime_start, realtime_end))                                      #clean data
tdy_fred <- tbl_fred |> 
  pivot_wider(names_from = series_id, values_from = value)                      #create pivot table

#######################################################################################################

#Graphing monthly yields on treasuries

g2_tbl_fred <- tbl_fred |> 
  filter(series_id != "CPIAUCSL") |>
  filter(date >= "2020-01-01") |>
  filter(series_id %in% c("DGS2", "DGS5", "DGS10", "DGS30")) |>
  mutate(
    Duration = factor(series_id, 
                      levels = c("DGS2", "DGS5", "DGS10", "DGS30"),
                      labels = c("2-Year", "5-Year", "10-Year", "30-Year")),
    hover_text = paste("Date:", format(date, "%b %Y"), 
                       "<br>Yield:", round(value, 2), "%",
                       "<br>Duration:", Duration)
  ) |>
  # ADDED: group = Duration
  ggplot(aes(x = date,  y = value, colour = Duration, group = Duration, text = hover_text)) + 
  scale_x_date(date_labels = "%b-%Y", date_breaks = "6 month") +
  scale_color_manual(values=c("red", "green", "blue", "black")) +
  labs(title = "Monthly Yields on Treasuries", caption ="Prepared by Steven Miller") +
  xlab("Month") + ylab("Percent") +
  geom_line()

# Tell ggplotly to ONLY use your custom 'text' aesthetic for the tooltip
interactive_g2 <- ggplotly(g2_tbl_fred, tooltip = "text")

# Save as a standalone, postable HTML file
#saveWidget(interactive_g2, "C:/Users/mill1707/OneDrive - Michigan State University/TMP/treasury_yields_interactive.html", selfcontained = TRUE)
saveWidget(interactive_g2, "treasury_yields_interactive.html", selfcontained = TRUE)
