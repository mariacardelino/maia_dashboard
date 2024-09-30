# Authenticate
setAccountInfo(name = Sys.getenv("mariacardelino"),
               token = Sys.getenv("F75BCF2625B3A8CEDEAFCC02845C92D0"),
               secret = Sys.getenv("MU9by+GxBSuZdeJCXDxZbUtRNnifV++BuYiYRC7c"))
# Deploy
deployApp(appFiles = c("app.R"))