
mydata$gender <- df$gender
mydata$where <- df$where
mydata$is_rescue_worker <- df$is_rescue_worker

mydata2 <- mydata[complete.cases(mydata), ]
nrow(mydata2)

numvar <- mydata2 %>% 
  select_if(is.numeric)

md <- mahalanobis(
  numvar,
  center = colMeans(numvar),
  cov = cov(numvar)
)

alpha <- .001
cutoff <- (qchisq(p = 1 - alpha, df = ncol(numvar)))
names_outliers_MH <- which(md > cutoff)
excluded_mh <- names_outliers_MH

data_clean_mh <- mydata2[-excluded_mh, ]

data_clean <- data_clean_mh %>% 
  dplyr::filter(is_rescue_worker != "No")

nrow(data_clean)

data_clean$is_rescue_worker <- NULL

only_items <- data_clean
only_items$gender <- NULL
only_items$where <- NULL


new_names <- c(
  "n1", "n2", "p3", "n4", "p5", "n6", "p7", "n8", "p9", "p10", 
  "n11", "p12", "n13", "p14", "p15", "n16", "p17", "n18", "p19", 
  "n20", "n21", "p22", "p23", "n24", "n25", "p26"
)

rio::export(data_clean, here("data", "processed", "SCS_20210224.csv"))
rio::export(only_items, here("data", "processed", "SCS_items.csv"))


