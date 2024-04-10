
#devtools::install_github('mingjerli/IMFData')
library(IMFData)
# https://github.com/mingjerli/IMFData
#find out available dataset in IMF data
availableDB <- DataflowMethod()
availableDB$DatabaseID

#Findout how many dimensions are available in a given dataset. 
# Available dimension code
IFS.available.codes <- DataStructureMethod("IFS")
names(IFS.available.codes)
# Possible code in the first dimension
IFS.available.codes[[1]]
IFS.data.category <-  IFS.available.codes[[3]]
#Search possible code to use in each dimension. 
#Here, we want to search code related to GDP in CL_INDICATOR_IFS dimension
CodeSearch(IFS.available.codes, "CL_INDICATOR_IFS", "CPI")

#Make API call to get data
databaseID <- "IFS"
startdate = "2011-01-01"
enddate = "2023-09-01"
checkquery = FALSE

#International Investment Positions Data
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'IAD_BP6_USD')
CN.FDI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.FDI.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'IAD_BP6_USD')
JP.FDI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.FDI.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'IAD_BP6_USD')
US.FDI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.FDI.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'IAD_BP6_USD')
AU.FDI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.FDI.query$Obs[[1]]



#Exchange Rate
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'ENDA_XDC_USD_RATE')
CN.XCH.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.XCH.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'ENDA_XDC_USD_RATE')
JP.XCH.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.XCH.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'ENDA_XDC_USD_RATE')
US.XCH.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.XCH.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'ENDA_XDC_USD_RATE')
AU.XCH.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.XCH.query$Obs[[1]]


#Balance of Payments 
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'BGS_BP6_USD')
CN.BOP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.BOP.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'BGS_BP6_USD')
JP.BOP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.BOP.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'BGS_BP6_USD')
US.BOP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.BOP.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'BGS_BP6_USD')
AU.BOP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.BOP.query$Obs[[1]]

#Bond Yield
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'FITBBE_PA')
CN.IR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                 checkquery)
CN.IR.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'FIGB_PA')
JP.IR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                 checkquery)
JP.IR.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'FIGB_PA')
US.IR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                 checkquery)
US.IR.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'FIGB_PA')
AU.IR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                 checkquery)
AU.IR.query$Obs[[1]]

#Government Expenditure
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'NGDP_NSA_XDC')
CN.GDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.GDP.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'NGDP_NSA_XDC')
JP.GDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.GDP.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'NGDP_NSA_XDC')
US.GDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.GDP.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'NGDP_NSA_XDC')
AU.GDP.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.GDP.query$Obs[[1]]


#CPI
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'PCPI_PC_PP_PT')
CN.CPI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.CPI.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'PCPI_PC_PP_PT')
JP.CPI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.CPI.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'PCPI_PC_PP_PT')
US.CPI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.CPI.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'PCPI_PC_PP_PT')
AU.CPI.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.CPI.query$Obs[[1]]


#Unemployment Rate
#China
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "CN", CL_INDICATOR_IFS = 'LUR_PT')
CN.UMR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
CN.UMR.query$Obs[[1]]
#Japan
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "JP", CL_INDICATOR_IFS = 'LUR_PT')
JP.UMR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
JP.UMR.query$Obs[[1]]
#United States
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "US", CL_INDICATOR_IFS = 'LUR_PT')
US.UMR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
US.UMR.query$Obs[[1]]

#Australia
queryfilter <- list(CL_FREA = "Q", CL_AREA_IFS = "AU", CL_INDICATOR_IFS = 'LUR_PT')
AU.UMR.query <- CompactDataMethod(databaseID, queryfilter, startdate, enddate, 
                                  checkquery)
AU.UMR.query$Obs[[1]]




library(zoo)
library(ggplot2)
library(tidyr)
library(dplyr)

CPI_data <- data.frame(CN.CPI.query$Obs[[1]]$`@TIME_PERIOD`, CN.CPI.query$Obs[[1]]$`@OBS_VALUE`, JP.CPI.query$Obs[[1]]$`@OBS_VALUE`, 
                       US.CPI.query$Obs[[1]]$`@OBS_VALUE`, AU.CPI.query$Obs[[1]]$`@OBS_VALUE`)
names(CPI_data) = c('Time', 'China', 'Japan', 'United_States', 'Australia')
CPI_data$Time = as.yearqtr(CPI_data$Time, format = "%Y-Q%q")

CPI_data$Time = as.Date(CPI_data$Time)
CPI_data$China = as.numeric(CPI_data$China)
CPI_data$United_States = as.numeric(CPI_data$United_States)
CPI_data$Japan = as.numeric(CPI_data$Japan)
CPI_data$Australia = as.numeric(CPI_data$Australia)
CPI_data_long <- pivot_longer(CPI_data, 
                              cols = c(China, Japan, United_States, Australia), 
                              names_to = "Country", 
                              values_to = "CPI")

CPI_plot = ggplot(data = CPI_data_long, aes(x = Time, y = CPI, color = Country)) +
  geom_line() +
  scale_color_manual(values = c("China" = "blue", "Japan" = "red", "United_States" = "green", "Australia" = "orange")) +
  theme_minimal() +
  labs(x = "Time", y = "CPI", title = "CPI Trends for China, Japan, the United States, and Australia", color = "Country")

ggsave("CPI.png", plot = CPI_plot, path = paste0(getwd(), "/plots", sep = ""), width = 10, height = 7)

FDI_data <- data.frame(CN.FDI.query$Obs[[1]]$`@TIME_PERIOD`, CN.FDI.query$Obs[[1]]$`@OBS_VALUE`, JP.FDI.query$Obs[[1]]$`@OBS_VALUE`, 
                       US.FDI.query$Obs[[1]]$`@OBS_VALUE`, AU.FDI.query$Obs[[1]]$`@OBS_VALUE`)
names(FDI_data) = c('Time', 'China', 'Japan', 'United_States', 'Australia')
FDI_data$Time = as.yearqtr(FDI_data$Time, format = "%Y-Q%q")

FDI_data$Time = as.Date(FDI_data$Time)
FDI_data$China = as.numeric(FDI_data$China)
FDI_data$United_States = as.numeric(FDI_data$United_States)
FDI_data$Japan = as.numeric(FDI_data$Japan)
FDI_data$Australia = as.numeric(FDI_data$Australia)
FDI_data <- FDI_data %>%
  mutate(across(c(China, Japan, United_States, Australia), 
                ~ (./lag(.) - 1) * 100, 
                .names = "{.col}"))
FDI_data_long <- pivot_longer(FDI_data, 
                              cols = c(China, Japan, United_States, Australia), 
                              names_to = "Country", 
                              values_to = "FDI")

FDI_plot = ggplot(data = FDI_data_long, aes(x = Time, y = FDI, color = Country)) +
  geom_line() +
  scale_color_manual(values = c("China" = "blue", "Japan" = "red", "United_States" = "green", "Australia" = "orange")) +
  theme_minimal() +
  labs(x = "Time", y = "FDI", title = "Percentage change in Foreign Direct Investments for China, Japan, the United States, and Australia", color = "Country")

ggsave("FDI.png", plot = FDI_plot, path = paste0(getwd(), "/plots", sep = ""), width = 10, height = 7)

XCH_data <- data.frame(CN.XCH.query$Obs[[1]]$`@TIME_PERIOD`, CN.XCH.query$Obs[[1]]$`@OBS_VALUE`, JP.XCH.query$Obs[[1]]$`@OBS_VALUE`, 
                       US.XCH.query$Obs[[1]]$`@OBS_VALUE`, AU.XCH.query$Obs[[1]]$`@OBS_VALUE`)
names(XCH_data) = c('Time', 'China', 'Japan', 'United_States', 'Australia')
XCH_data$Time = as.yearqtr(XCH_data$Time, format = "%Y-Q%q")

XCH_data$Time = as.Date(XCH_data$Time)
XCH_data$China = as.numeric(XCH_data$China)
XCH_data$United_States = as.numeric(XCH_data$United_States)
XCH_data$Japan = as.numeric(XCH_data$Japan)
XCH_data$Australia = as.numeric(XCH_data$Australia)
XCH_data <- XCH_data %>%
  mutate(across(c(China, Japan, United_States, Australia), 
                ~ (./lag(.) - 1) * 100, 
                .names = "{.col}"))
XCH_data_long <- pivot_longer(XCH_data, 
                              cols = c(China, Japan, United_States, Australia), 
                              names_to = "Country", 
                              values_to = "XCH")

XCH_plot = ggplot(data = XCH_data_long, aes(x = Time, y = XCH, color = Country)) +
  geom_line() +
  scale_color_manual(values = c("China" = "blue", "Japan" = "red", "United_States" = "green", "Australia" = "orange")) +
  theme_minimal() +
  labs(x = "Time", y = "XCH", title = "Percentage change in Exchange Rates Against the Dollar for China, Japan, the United States, and Australia", color = "Country")

ggsave("XCH.png", plot = XCH_plot, path = paste0(getwd(), "/plots", sep = ""), width = 10, height = 7)

BOP_data <- data.frame(CN.BOP.query$Obs[[1]]$`@TIME_PERIOD`, CN.BOP.query$Obs[[1]]$`@OBS_VALUE`, JP.BOP.query$Obs[[1]]$`@OBS_VALUE`, 
                       US.BOP.query$Obs[[1]]$`@OBS_VALUE`, AU.BOP.query$Obs[[1]]$`@OBS_VALUE`)
names(BOP_data) = c('Time', 'China', 'Japan', 'United_States', 'Australia')
BOP_data$Time = as.yearqtr(BOP_data$Time, format = "%Y-Q%q")

BOP_data$Time = as.Date(BOP_data$Time)
BOP_data$China = as.numeric(BOP_data$China)
BOP_data$United_States = as.numeric(BOP_data$United_States)
BOP_data$Japan = as.numeric(BOP_data$Japan)
BOP_data$Australia = as.numeric(BOP_data$Australia)
BOP_data <- BOP_data %>%
  mutate(across(c(China, Japan, United_States, Australia), 
                ~ (./lag(.) - 1) * 100, 
                .names = "{.col}"))
BOP_data_long <- pivot_longer(BOP_data, 
                              cols = c(China, Japan, United_States, Australia), 
                              names_to = "Country", 
                              values_to = "BOP")

BOP_plot = ggplot(data = BOP_data_long, aes(x = Time, y = BOP, color = Country)) +
  geom_line() +
  scale_color_manual(values = c("China" = "blue", "Japan" = "red", "United_States" = "green", "Australia" = "orange")) +
  theme_minimal() +
  labs(x = "Time", y = "BOP", title = "Percentage change in Balance of Payments for China, Japan, the United States, and Australia", color = "Country")
ggsave("BOP.png", plot = BOP_plot, path = paste0(getwd(), "/plots", sep = ""), width = 10, height = 7)

GDP_data <- data.frame(CN.GDP.query$Obs[[1]]$`@TIME_PERIOD`, CN.GDP.query$Obs[[1]]$`@OBS_VALUE`, JP.GDP.query$Obs[[1]]$`@OBS_VALUE`, 
                       US.GDP.query$Obs[[1]]$`@OBS_VALUE`, AU.GDP.query$Obs[[1]]$`@OBS_VALUE`)
names(GDP_data) = c('Time', 'China', 'Japan', 'United_States', 'Australia')
GDP_data$Time = as.yearqtr(GDP_data$Time, format = "%Y-Q%q")

GDP_data$Time = as.Date(GDP_data$Time)
GDP_data$China = as.numeric(GDP_data$China)
GDP_data$United_States = as.numeric(GDP_data$United_States)
GDP_data$Japan = as.numeric(GDP_data$Japan)
GDP_data$Australia = as.numeric(GDP_data$Australia)
GDP_data <- GDP_data %>%
  mutate(across(c(China, Japan, United_States, Australia), 
                ~ (./lag(.) - 1) * 100, 
                .names = "{.col}"))
GDP_data_long <- pivot_longer(GDP_data, 
                              cols = c(China, Japan, United_States, Australia), 
                              names_to = "Country", 
                              values_to = "GDP")

GDP_plot = ggplot(data = GDP_data_long, aes(x = Time, y = GDP, color = Country)) +
  geom_line() +
  scale_color_manual(values = c("China" = "blue", "Japan" = "red", "United_States" = "green", "Australia" = "orange")) +
  theme_minimal() +
  labs(x = "Time", y = "GDP", title = "Percentage change in GDP for China, Japan, the United States, and Australia", color = "Country")

ggsave("GDP.png", plot = GDP_plot, path = paste0(getwd(), "/plots", sep = ""), width = 10, height = 7)

