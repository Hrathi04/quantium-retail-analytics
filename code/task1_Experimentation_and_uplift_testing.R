library(data.table)
library(ggplot2)
library(tidyr)

data <- read_csv("C:/Users/hites/OneDrive/Desktop/Hitesh/Portfolio/Additional Skills/Data Analytics Job Simulation - Quantium/data/QVI_data.csv")
View(data)
setDT(data)
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Create YEARMONTH
data[, DATE := as.Date(DATE)]
data[, YEARMONTH := year(DATE) * 100 + month(DATE)]

class(data)
names(data)
head(data)

#Create monthly store metrics

measureOverTime <- data[, .(
  totSales = sum(TOT_SALES),
  nCustomers = uniqueN(LYLTY_CARD_NBR),
  nTxnPerCust = uniqueN(TXN_ID) / uniqueN(LYLTY_CARD_NBR),
  nChipsPerTxn = sum(PROD_QTY) / uniqueN(TXN_ID),
  avgPricePerUnit = sum(TOT_SALES) / sum(PROD_QTY)
), by = .(STORE_NBR, YEARMONTH)][order(STORE_NBR, YEARMONTH)]

head(measureOverTime)
summary(measureOverTime)

# Filter stores with full 12 months
storesWithFullObs <- unique(
  measureOverTime[, .N, STORE_NBR][N == 12, STORE_NBR]
)

preTrialMeasures <- measureOverTime[
  YEARMONTH < 201902 & STORE_NBR %in% storesWithFullObs,
]
length(storesWithFullObs)
head(preTrialMeasures)

# Create the correlation function

calculateCorrelation <- function(inputTable, metricCol, storeComparison) {
  
  calcCorrTable <- data.table(
    Store1 = numeric(),
    Store2 = numeric(),
    corr_measure = numeric()
  )
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    
    calculatedMeasure <- data.table(
      Store1 = storeComparison,
      Store2 = i,
      corr_measure = cor(
        inputTable[STORE_NBR == storeComparison, eval(metricCol)],
        inputTable[STORE_NBR == i, eval(metricCol)]
      )
    )
    
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  
  return(calcCorrTable)
}

calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison) {
  
  calcDistTable <- data.table(
    Store1 = numeric(),
    Store2 = numeric(),
    YEARMONTH = numeric(),
    measure = numeric()
  )
  
  storeNumbers <- unique(inputTable[, STORE_NBR])
  
  for (i in storeNumbers) {
    
    calculatedMeasure <- data.table(
      Store1 = storeComparison,
      Store2 = i,
      YEARMONTH = inputTable[STORE_NBR == storeComparison, YEARMONTH],
      measure = abs(
        inputTable[STORE_NBR == storeComparison, eval(metricCol)] -
          inputTable[STORE_NBR == i, eval(metricCol)]
      )
    )
    
    calcDistTable <- rbind(calcDistTable, calculatedMeasure)
  }
  
  minMaxDist <- calcDistTable[, .(
    minDist = min(measure),
    maxDist = max(measure)
  ), by = .(Store1, YEARMONTH)]
  
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  
  distTable[, magnitudeMeasure := 1 - (measure - minDist) / (maxDist - minDist)]
  
  finalDistTable <- distTable[, .(
    mag_measure = mean(magnitudeMeasure)
  ), by = .(Store1, Store2)]
  
  return(finalDistTable)
}

# Find Control Store for Trial Store 77

trial_store <- 77

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

corr_weight <- 0.5

score_nSales <- merge(
  corr_nSales,
  magnitude_nSales,
  by = c("Store1", "Store2")
)[, scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

score_nCustomers <- merge(
  corr_nCustomers,
  magnitude_nCustomers,
  by = c("Store1", "Store2")
)[, scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

score_Control <- merge(
  score_nSales[, .(Store1, Store2, scoreNSales)],
  score_nCustomers[, .(Store1, Store2, scoreNCust)],
  by = c("Store1", "Store2")
)

score_Control[, finalControlScore := 0.5 * scoreNSales + 0.5 * scoreNCust]

score_Control[order(-finalControlScore)][1:10]
# Control Store Selection - Trial Store 77
# Store 233 achieved the highest combined score based on
# sales and customer metrics during the pre-trial period.
# Therefore Store 233 was selected as the control store
# for Trial Store 77.

# Visual Validation
pastSales <- measureOverTime[
  STORE_NBR %in% c(77,233)
]

ggplot(
  pastSales,
  aes(
    x = YEARMONTH,
    y = totSales,
    color = factor(STORE_NBR)
  )
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Total Sales: Trial Store 77 vs Control Store 233",
    x = "Year Month",
    y = "Total Sales",
    color = "Store"
  )

# Customer Chart Comparison

ggplot(
  pastSales,
  aes(
    x = YEARMONTH,
    y = nCustomers,
    color = factor(STORE_NBR)
  )
) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(
    title = "Monthly Customers: Trial Store 77 vs Control Store 233",
    x = "Year Month",
    y = "Customers",
    color = "Store"
  )

# Find Control Store for Trial Store 86

trial_store <- 86

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

corr_weight <- 0.5

score_nSales <- merge(
  corr_nSales,
  magnitude_nSales,
  by = c("Store1", "Store2")
)[, scoreNSales := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

score_nCustomers <- merge(
  corr_nCustomers,
  magnitude_nCustomers,
  by = c("Store1", "Store2")
)[, scoreNCust := corr_weight * corr_measure + (1 - corr_weight) * mag_measure]

score_Control <- merge(
  score_nSales[, .(Store1, Store2, scoreNSales)],
  score_nCustomers[, .(Store1, Store2, scoreNCust)],
  by = c("Store1", "Store2")
)

score_Control[, finalControlScore := 0.5 * scoreNSales + 0.5 * scoreNCust]

score_Control[order(-finalControlScore)][1:10]

# Control Store Selection - Trial Store 86
# Store 155 achieved the highest combined score based on
# sales and customer metrics during the pre-trial period.
# Therefore Store 155 was selected as the control store
# for Trial Store 86.

# Trial Store 88

trial_store <- 88

corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales), trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)

score_nSales <- merge(
  corr_nSales,
  magnitude_nSales,
  by = c("Store1", "Store2")
)[, scoreNSales := 0.5 * corr_measure + 0.5 * mag_measure]

score_nCustomers <- merge(
  corr_nCustomers,
  magnitude_nCustomers,
  by = c("Store1", "Store2")
)[, scoreNCust := 0.5 * corr_measure + 0.5 * mag_measure]

score_Control <- merge(
  score_nSales[, .(Store1, Store2, scoreNSales)],
  score_nCustomers[, .(Store1, Store2, scoreNCust)],
  by = c("Store1", "Store2")
)

score_Control[, finalControlScore := 0.5 * scoreNSales + 0.5 * scoreNCust]

score_Control[order(-finalControlScore)][1:10]


# Control Store Selection Results
#
# Trial Store 77 → Control Store 233
# Trial Store 86 → Control Store 155
# Trial Store 88 → Control Store 237
#
# Control stores were selected based on a combined score
# incorporating both Pearson correlation and magnitude distance
# for total sales and number of customers during the pre-trial period.
#
# These stores demonstrated the strongest similarity to the
# respective trial stores before the trial commenced.


# Create a Reusable Plot Function

plotTrialControl <- function(store1, store2, metric) {
  
  plotData <- measureOverTime[
    STORE_NBR %in% c(store1, store2)
  ]
  
  ggplot(
    plotData,
    aes(
      x = YEARMONTH,
      y = get(metric),
      colour = factor(STORE_NBR),
      group = STORE_NBR
    )
  ) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = paste(metric, "-", store1, "vs", store2),
      colour = "Store"
    )
}

# Set Trial and Control Store
trial_store <- 77
control_store <- 233


# Visual Check - Total Sales
measureOverTimeSales <- copy(measureOverTime)

pastSales <- measureOverTimeSales[
  , Store_type := ifelse(
    STORE_NBR == trial_store,
    "Trial",
    ifelse(STORE_NBR == control_store, "Control", "Other stores")
  )
][
  , totSales := mean(totSales),
  by = c("YEARMONTH", "Store_type")
][
  , TransactionMonth := as.Date(
    paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
    "%Y-%m-%d"
  )
][
  YEARMONTH < 201903,
]

ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() +
  labs(
    x = "Month of operation",
    y = "Total sales",
    title = "Total sales by month"
  )

# Visual check — number of customers

measureOverTimeCusts <- copy(measureOverTime)

pastCustomers <- measureOverTimeCusts[
  , Store_type := ifelse(
    STORE_NBR == trial_store,
    "Trial",
    ifelse(STORE_NBR == control_store, "Control", "Other stores")
  )
][
  , numberCustomers := mean(nCustomers),
  by = c("YEARMONTH", "Store_type")
][
  , TransactionMonth := as.Date(
    paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
    "%Y-%m-%d"
  )
][
  YEARMONTH < 201903,
]

ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(
    x = "Month of operation",
    y = "Number of customers",
    title = "Number of customers by month"
  )

# Scale control store sale

scalingFactorForControlSales <- preTrialMeasures[
  STORE_NBR == trial_store & YEARMONTH < 201902,
  sum(totSales)
] / preTrialMeasures[
  STORE_NBR == control_store & YEARMONTH < 201902,
  sum(totSales)
]

scalingFactorForControlSales

# Apply scaling factor

measureOverTimeSales <- copy(measureOverTime)

scaledControlSales <- measureOverTimeSales[
  STORE_NBR == control_store,
][
  , controlSales := totSales * scalingFactorForControlSales
]

# Calculate percentage difference
percentageDiff <- merge(
  scaledControlSales[, .(YEARMONTH, controlSales)],
  measureOverTime[
    STORE_NBR == trial_store,
    .(YEARMONTH, trialSales = totSales)
  ],
  by = "YEARMONTH"
)[
  , percentageDiff := abs(controlSales - trialSales) / controlSales
]

percentageDiff

# Calculate Standard Deviation

stdDev <- sd(percentageDiff[YEARMONTH < 201902, percentageDiff])

degreesOfFreedom <- 7

stdDev

# Calculate t values

percentageDiff[
  , tValue := percentageDiff / stdDev
][
  , TransactionMonth := as.Date(
    paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
    "%Y-%m-%d"
  )
]

percentageDiff[
  YEARMONTH >= 201902 & YEARMONTH <= 201904,
  .(YEARMONTH, trialSales, controlSales, percentageDiff, tValue)
]

# get Critical t value
qt(0.95, df = degreesOfFreedom)

# Insights:
# Trial Store 77 recorded significantly higher sales during
# March and April 2019 compared to the scaled control store.
#
# The t-values for March (7.34) and April (12.48) exceeded
# the critical t-value of 1.89, indicating a statistically
# significant increase in sales during the trial period.
#
# February 2019 did not show a statistically significant uplift.
#
# Overall, the trial appears to have had a positive impact on sales
# performance in Store 77.

# Create Trial Assessment charts

measureOverTimeSales <- copy(measureOverTime)

pastSales <- measureOverTimeSales[
  , Store_type := ifelse(
    STORE_NBR == trial_store,
    "Trial",
    ifelse(STORE_NBR == control_store, "Control", "Other stores")
  )
][
  , totSales := mean(totSales),
  by = c("YEARMONTH", "Store_type")
][
  , TransactionMonth := as.Date(
    paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"),
    "%Y-%m-%d"
  )
][
  Store_type %in% c("Trial", "Control"),
]

pastSales_Controls95 <- pastSales[
  Store_type == "Control",
][
  , totSales := totSales * (1 + stdDev * 2)
][
  , Store_type := "Control 95th % confidence interval"
]

pastSales_Controls5 <- pastSales[
  Store_type == "Control",
][
  , totSales := totSales * (1 - stdDev * 2)
][
  , Store_type := "Control 5th % confidence interval"
]

trialAssessment <- rbind(
  pastSales,
  pastSales_Controls95,
  pastSales_Controls5
)

ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(
    data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901],
    aes(
      xmin = min(TransactionMonth),
      xmax = max(TransactionMonth),
      ymin = 0,
      ymax = Inf,
      color = NULL
    ),
    show.legend = FALSE
  ) +
  geom_line() +
  labs(
    x = "Month of operation",
    y = "Total sales",
    title = "Total sales by month"
  )


#Customer Analysis
scalingFactorForControlCustomers <- preTrialMeasures[
  STORE_NBR == trial_store & YEARMONTH < 201902,
  sum(nCustomers)
] / preTrialMeasures[
  STORE_NBR == control_store & YEARMONTH < 201902,
  sum(nCustomers)
]

scalingFactorForControlCustomers

# Scale Customer Counts
scaledControlCustomers <- measureOverTime[
  STORE_NBR == control_store,
][
  , controlCustomers := nCustomers * scalingFactorForControlCustomers
]

# Calculate percentage difference
customerDiff <- merge(
  scaledControlCustomers[, .(YEARMONTH, controlCustomers)],
  measureOverTime[
    STORE_NBR == trial_store,
    .(YEARMONTH, trialCustomers = nCustomers)
  ],
  by = "YEARMONTH"
)[
  , percentageDiff := abs(controlCustomers - trialCustomers) /
    controlCustomers
]

# Standard Deviation
stdDevCust <- sd(
  customerDiff[
    YEARMONTH < 201902,
    percentageDiff
  ]
)

# Customer t value
customerDiff[
  , tValue := percentageDiff / stdDevCust
]

customerDiff[
  YEARMONTH >= 201902 & YEARMONTH <= 201904,
  .(
    YEARMONTH,
    trialCustomers,
    controlCustomers,
    percentageDiff,
    tValue
  )
]

# Insights:
# Customer numbers increased significantly during March and April 2019.
#
# The customer uplift aligns with the observed increase in sales,
# suggesting that the trial layout attracted additional customers
# into the store rather than simply increasing spend from existing
# customers.
#
# Store 77 demonstrates strong evidence that the trial layout
# positively impacted performance.


# Craeting Function
assessTrialStore <- function(trial_store, control_store, metric_name) {
  
  # Step 1: Scale pre-trial control store to match trial store
  scalingFactor <- preTrialMeasures[
    STORE_NBR == trial_store & YEARMONTH < 201902,
    sum(get(metric_name))
  ] / preTrialMeasures[
    STORE_NBR == control_store & YEARMONTH < 201902,
    sum(get(metric_name))
  ]
  
  # Step 2: Apply scaling factor to control store
  scaledControl <- measureOverTime[
    STORE_NBR == control_store,
  ][
    , scaledControlMetric := get(metric_name) * scalingFactor
  ]
  
  # Step 3: Compare scaled control against trial store
  percentageDiff <- merge(
    scaledControl[, .(YEARMONTH, scaledControlMetric)],
    measureOverTime[
      STORE_NBR == trial_store,
      .(YEARMONTH, trialMetric = get(metric_name))
    ],
    by = "YEARMONTH"
  )[
    , percentageDiff := abs(scaledControlMetric - trialMetric) /
      scaledControlMetric
  ]
  
  # Step 4: Calculate standard deviation from pre-trial period
  stdDev <- sd(
    percentageDiff[
      YEARMONTH < 201902,
      percentageDiff
    ]
  )
  
  degreesOfFreedom <- 7
  criticalValue <- qt(0.95, df = degreesOfFreedom)
  
  # Step 5: Calculate t-values
  percentageDiff[
    , tValue := percentageDiff / stdDev
  ][
    , significant := tValue > criticalValue
  ]
  
  # Step 6: Return trial period only
  result <- percentageDiff[
    YEARMONTH >= 201902 & YEARMONTH <= 201904,
    .(
      trial_store = trial_store,
      control_store = control_store,
      metric = metric_name,
      YEARMONTH,
      trialMetric,
      scaledControlMetric,
      percentageDiff,
      tValue,
      criticalValue,
      significant
    )
  ]
  
  return(result)
}

# Store 86 and 155
assessTrialStore(86, 155, "totSales")
assessTrialStore(86, 155, "nCustomers")

# Store 88 and 237
assessTrialStore(88, 237, "totSales")
assessTrialStore(88, 237, "nCustomers")

# Combine
trial_results <- rbind(
  assessTrialStore(77, 233, "totSales"),
  assessTrialStore(77, 233, "nCustomers"),
  assessTrialStore(86, 155, "totSales"),
  assessTrialStore(86, 155, "nCustomers"),
  assessTrialStore(88, 237, "totSales"),
  assessTrialStore(88, 237, "nCustomers")
)

trial_results

# Transactions Per Customer Analysis

assessTrialStore(77, 233, "nTxnPerCust")
assessTrialStore(86, 155, "nTxnPerCust")
assessTrialStore(88, 237, "nTxnPerCust")

txn_results <- rbind(
  assessTrialStore(77, 233, "nTxnPerCust"),
  assessTrialStore(86, 155, "nTxnPerCust"),
  assessTrialStore(88, 237, "nTxnPerCust")
)

txn_results


# Confidence Interval Charts

createTrialChart <- function(
    trial_store,
    control_store,
    metric_name
) {
  
  scalingFactor <- preTrialMeasures[
    STORE_NBR == trial_store,
    sum(get(metric_name))
  ] /
    preTrialMeasures[
      STORE_NBR == control_store,
      sum(get(metric_name))
    ]
  
  controlData <- measureOverTime[
    STORE_NBR == control_store
  ][
    , scaledMetric := get(metric_name) * scalingFactor
  ]
  
  trialData <- measureOverTime[
    STORE_NBR == trial_store
  ]
  
  comparison <- merge(
    trialData[, .(YEARMONTH,
                  trialMetric = get(metric_name))],
    controlData[, .(YEARMONTH,
                    scaledMetric)],
    by = "YEARMONTH"
  )
  
  preTrialDiff <- comparison[
    YEARMONTH < 201902,
    abs(trialMetric - scaledMetric) /
      scaledMetric
  ]
  
  stdDev <- sd(preTrialDiff)
  
  comparison[
    ,
    upperCI := scaledMetric * (1 + 2 * stdDev)
  ]
  
  comparison[
    ,
    lowerCI := scaledMetric * (1 - 2 * stdDev)
  ]
  
  comparison[
    ,
    Month := as.Date(
      paste(
        YEARMONTH %/% 100,
        YEARMONTH %% 100,
        "01",
        sep = "-"
      )
    )
  ]
  
  return(comparison)
}

chart77 <- createTrialChart(
  77,
  233,
  "totSales"
)

head(chart77)


# Conclusion
# Store 77 Insights
# Store 77 recorded significant sales growth during March and April 2019.
# Customer numbers increased significantly during the trial period.
# Transactions per customer also increased significantly.
#
# The uplift appears to be driven by both increased customer traffic
# and increased purchasing behaviour.
#
# The trial layout was successful and should be considered for rollout.




## Store 86 Insights
# Store 86 experienced significant sales uplift during February
# and March 2019.
#
# Customer numbers increased significantly throughout the trial period,
# while transactions per customer remained unchanged.
#
# This suggests that the trial layout attracted additional customers
# rather than increasing purchase frequency.
#
# The trial layout appears successful and should be considered
# for rollout.


# Store 88 Insights
# Store 88 recorded significant sales growth during March and April 2019.
#
# Customer numbers increased significantly during the same period.
# Transactions per customer also increased significantly.
#
# The trial layout appears to have positively influenced both
# customer traffic and purchasing behaviour.
#
# The trial layout was successful and should be considered
# for rollout.




# Recommendations

# Final Recommendation
#
# The trial layouts implemented in Stores 77, 86 and 88
# delivered positive results during the trial period.
#
# Store 77 demonstrated significant increases in sales,
# customer numbers and transactions per customer.
#
# Store 86 showed strong customer growth and sales uplift,
# suggesting the trial successfully attracted additional shoppers.
#
# Store 88 recorded significant increases in sales,
# customer numbers and purchasing frequency.
#
# Based on the statistical analysis, all three trial stores
# outperformed their respective control stores during the trial period.
#
# It is recommended that the new layout be considered
# for broader implementation across suitable stores.



# Github Charts

dir.create("charts/task2", recursive = TRUE, showWarnings = FALSE)
# Create reusable GitHub chart function

createGithubTrialChart <- function(trial_store, control_store, metric_name, chart_title, y_label, file_name) {
  
  chartData <- createTrialChart(trial_store, control_store, metric_name)
  
  chartLong <- melt(
    chartData,
    id.vars = c("YEARMONTH", "Month"),
    measure.vars = c("trialMetric", "scaledMetric", "upperCI", "lowerCI"),
    variable.name = "MetricType",
    value.name = "Value"
  )
  
  chartLong[, MetricType := fifelse(
    MetricType == "trialMetric", "Trial Store",
    fifelse(
      MetricType == "scaledMetric", "Control Store (Scaled)",
      fifelse(
        MetricType == "upperCI", "Upper Confidence Interval",
        "Lower Confidence Interval"
      )
    )
  )]
  
  p <- ggplot(chartLong, aes(x = Month, y = Value, color = MetricType)) +
    geom_rect(
      data = chartData[YEARMONTH >= 201902 & YEARMONTH <= 201904],
      aes(xmin = min(Month), xmax = max(Month), ymin = -Inf, ymax = Inf),
      inherit.aes = FALSE,
      alpha = 0.15
    ) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    labs(
      title = chart_title,
      subtitle = paste("Trial Store", trial_store, "vs Control Store", control_store),
      x = "Month",
      y = y_label,
      color = "Legend",
      caption = "Shaded area represents trial period: Feb 2019 to Apr 2019"
    ) +
    theme_bw() +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "bottom"
    )
  
  ggsave(
    filename = paste0("charts/task2/", file_name),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
  
  return(p)
}
# Sales Chart
createGithubTrialChart(
  77, 233, "totSales",
  "Sales Uplift Assessment - Store 77",
  "Total Sales",
  "store_77_sales_uplift.png"
)

createGithubTrialChart(
  86, 155, "totSales",
  "Sales Uplift Assessment - Store 86",
  "Total Sales",
  "store_86_sales_uplift.png"
)

createGithubTrialChart(
  88, 237, "totSales",
  "Sales Uplift Assessment - Store 88",
  "Total Sales",
  "store_88_sales_uplift.png"
)

# Customer Charts

createGithubTrialChart(
  77, 233, "nCustomers",
  "Customer Uplift Assessment - Store 77",
  "Number of Customers",
  "store_77_customer_uplift.png"
)

createGithubTrialChart(
  86, 155, "nCustomers",
  "Customer Uplift Assessment - Store 86",
  "Number of Customers",
  "store_86_customer_uplift.png"
)

createGithubTrialChart(
  88, 237, "nCustomers",
  "Customer Uplift Assessment - Store 88",
  "Number of Customers",
  "store_88_customer_uplift.png"
)

# Transactions per customer charts
createGithubTrialChart(
  77, 233, "nTxnPerCust",
  "Transaction Frequency Assessment - Store 77",
  "Transactions per Customer",
  "store_77_transactions_per_customer.png"
)

createGithubTrialChart(
  86, 155, "nTxnPerCust",
  "Transaction Frequency Assessment - Store 86",
  "Transactions per Customer",
  "store_86_transactions_per_customer.png"
)

createGithubTrialChart(
  88, 237, "nTxnPerCust",
  "Transaction Frequency Assessment - Store 88",
  "Transactions per Customer",
  "store_88_transactions_per_customer.png"
)