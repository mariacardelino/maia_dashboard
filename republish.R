#automation file 

# Load necessary libraries

library(rsconnect)

#authenticate
rsconnect::setAccountInfo(name='mcardelino',
                          token='F75BCF2625B3A8CEDEAFCC02845C92D0',
                          secret='MU9by+GxBSuZdeJCXDxZbUtRNnifV++BuYiYRC7c')

# Publish the Shiny app
rsconnect::deployApp(appDir = ".", account = "mcardelino", forceUpdate = TRUE, lint = FALSE)