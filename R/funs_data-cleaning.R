# Get not-rescue-workers data
get_data_scs_not_rescue_workers <- function() {

  temp <- read_excel(here("data", "raw", "non_soccorritori_20210404.xlsx"))
  temp2 <- temp[, 176:201]
  scs <- temp2
  
  new_names <- c(
    "u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
    "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
    "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
    "c22", "c23", "u24", "u25", "c26"
  )
  colnames(scs) <- new_names
  
  my_recode <- function(x) {
    xr <- car::recode(x, '1=5; 2=4; 3=3; 4=2; 5=1')
    xr
  }
  
  scs$u1  = my_recode(scs$u1)
  scs$u2  = my_recode(scs$u2)
  scs$u4  = my_recode(scs$u4)
  scs$u6  = my_recode(scs$u6)
  scs$u8  = my_recode(scs$u8)
  scs$u11 = my_recode(scs$u11)
  scs$u13 = my_recode(scs$u13)
  scs$u16 = my_recode(scs$u16)
  scs$u18 = my_recode(scs$u18)
  scs$u20 = my_recode(scs$u20)
  scs$u21 = my_recode(scs$u21)
  scs$u24 = my_recode(scs$u24)
  scs$u25 = my_recode(scs$u25)
  
  neg_self_compassion <- scs$u1 + scs$u2 + scs$u4 + scs$u6 +
    scs$u8 + scs$u11 + scs$u13 + scs$u16 + scs$u18 + scs$u20 +
    scs$u21 + scs$u24 + scs$u25 
  
  pos_self_compassion <- scs$c3 + scs$c5 + scs$c7 + scs$c9 +
    scs$c10 + scs$c12 + scs$c14 + scs$c15 + scs$c17 + scs$c19 +
    scs$c22 + scs$c23 + scs$c26
  
  non_rescue_workers <- data.frame(
    u1 = scs$u1,
    u2 = scs$u2, 
    c3 = scs$c3, 
    u4 = scs$u4, 
    c5 = scs$c5, 
    u6 = scs$u6, 
    c7 = scs$c7,  
    u8 = scs$u8, 
    c9 = scs$c9, 
    c10 = scs$c10, 
    u11 = scs$u11, 
    c12 = scs$c12, 
    u13 = scs$u13, 
    c14 = scs$c14, 
    c15 = scs$c15, 
    u16 = scs$u16, 
    c17 = scs$c17, 
    u18 = scs$u18, 
    c19 = scs$c19, 
    u20 = scs$u20, 
    u21 = scs$u21, 
    c22 = scs$c22, 
    c23 = scs$c23,  
    u24 = scs$u24, 
    u25 = scs$u25, 
    c26 = scs$c26,
    is_rescue_worker = 0,
    neg_self_compassion,
    pos_self_compassion, 
    where = "toscana",
    sex = temp$Sesso,
    age = temp$`Quanti anni hai?`
  )
  
  # remove cases in which the SCS was not completed
  non_rescue_workers <- non_rescue_workers[!is.na(non_rescue_workers$u1), ]
  
  # age as a numeric variable
  non_rescue_workers$age <- as.numeric(as.character(non_rescue_workers$age))
  
  # multiple imputation for age
  data_imp <- mice::complete(mice::mice(non_rescue_workers, method = "mean", 1, print = FALSE))
  non_rescue_workers$age <- round(data_imp$age)
  # non_rescue_workers$age
  non_rescue_workers
}


get_data_scs_rescue_workers <- function() {
  
  df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
  df_toscana$where <- "toscana"
  df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
  df_lombardia$where <- "lombardia"
  # Merge the two data.frames 
  temp <- rbind(df_toscana, df_lombardia)
  # remove NAs
  df <- temp[!is.na(temp$X17), ]
  
  scs <- df %>% 
    dplyr::select(X194:X219)
  
  new_names <- c(
    "u1",  "u2",  "c3",  "u4",  "c5",  "u6",  "c7",  
    "u8",  "c9", "c10", "u11", "c12", "u13", "c14", 
    "c15", "u16", "c17", "u18", "c19", "u20", "u21", 
    "c22", "c23", "u24", "u25", "c26"
  )
  colnames(scs) <- new_names
  
  my_recode <- function(x) {
    xr <- car::recode(x, '1=5; 2=4; 3=3; 4=2; 5=1')
    xr
  }
  
  scs$u1  = my_recode(scs$u1)
  scs$u2  = my_recode(scs$u2)
  scs$u4  = my_recode(scs$u4)
  scs$u6  = my_recode(scs$u6)
  scs$u8  = my_recode(scs$u8)
  scs$u11 = my_recode(scs$u11)
  scs$u13 = my_recode(scs$u13)
  scs$u16 = my_recode(scs$u16)
  scs$u18 = my_recode(scs$u18)
  scs$u20 = my_recode(scs$u20)
  scs$u21 = my_recode(scs$u21)
  scs$u24 = my_recode(scs$u24)
  scs$u25 = my_recode(scs$u25)
  
  neg_self_compassion <- scs$u1 + scs$u2 + scs$u4 + scs$u6 +
    scs$u8 + scs$u11 + scs$u13 + scs$u16 + scs$u18 + scs$u20 +
    scs$u21 + scs$u24 + scs$u25 
  
  pos_self_compassion <- scs$c3 + scs$c5 + scs$c7 + scs$c9 +
    scs$c10 + scs$c12 + scs$c14 + scs$c15 + scs$c17 + scs$c19 +
    scs$c22 + scs$c23 + scs$c26
  
  rescue_workers <- data.frame(
    u1 = scs$u1,
    u2 = scs$u2, 
    c3 = scs$c3, 
    u4 = scs$u4, 
    c5 = scs$c5, 
    u6 = scs$u6, 
    c7 = scs$c7,  
    u8 = scs$u8, 
    c9 = scs$c9, 
    c10 = scs$c10, 
    u11 = scs$u11, 
    c12 = scs$c12, 
    u13 = scs$u13, 
    c14 = scs$c14, 
    c15 = scs$c15, 
    u16 = scs$u16, 
    c17 = scs$c17, 
    u18 = scs$u18, 
    c19 = scs$c19, 
    u20 = scs$u20, 
    u21 = scs$u21, 
    c22 = scs$c22, 
    c23 = scs$c23,  
    u24 = scs$u24, 
    u25 = scs$u25, 
    c26 = scs$c26,
    is_rescue_worker = 1,
    neg_self_compassion,
    pos_self_compassion, 
    where = df$where,
    sex = df$Sesso,
    age = df$`Quanti anni hai?`
  )
  
  # age as a numeric variable
  rescue_workers$age <- as.numeric(as.character(rescue_workers$age))
  
  # multiple imputation for age
  data_imp <- mice::complete(mice::mice(rescue_workers, method = "mean", 1, print = FALSE))
  rescue_workers$age <- round(data_imp$age)

  rescue_workers
}




# SCS items not-rescue-workers 
get_scs_items_not_rescue_workers <- function() {
  
  temp <- read_excel(here("data", "raw", "non_soccorritori_20210404.xlsx"))
  temp1 <- temp[, 9:213]
  temp2 <- temp1[, 176:201]
  scs <- temp2 # temp2[complete.cases(temp2), ]
  # Change variable names to facilitate coding
  item_names <- paste0("scs_", 1:26)
  names(scs) <- item_names
  
  not_rescue_workers <- data.frame(
    is_rescue_worker = 0,
    scs, 
    where = "toscana"
  )
  
  not_rescue_workers$sex <- temp$Sesso
  not_rescue_workers$age <- as.numeric(
    as.character(temp$`Quanti anni hai?`)
  )
  # some variables are incomplete
  nrw <- not_rescue_workers[complete.cases(not_rescue_workers), ]
  
  nrw
}


# SCS items rescue-workers 
get_scs_items_rescue_workers <- function() {
  
  df_toscana <- read_excel(here("data", "raw", "toscana_1.xlsx"))
  df_toscana$where <- "toscana"
  df_lombardia <- read_excel(here("data", "raw", "lombardia_1.xlsx"))
  df_lombardia$where <- "lombardia"
  # Merge the two data.frames 
  temp <- rbind(df_toscana, df_lombardia)
  # remove NAs
  df <- temp[!is.na(temp$X17), ]
  
  scs <- df %>% 
    dplyr::select(X194:X219)
  
  # Change variable names to facilitate coding
  item_names <- paste0("scs_", 1:26)
  names(scs) <- item_names
  
  rescue_workers <- data.frame(
    is_rescue_worker = 1,
    scs, 
    where = df$where,
    sex = df$Sesso,
    age = df$`Quanti anni hai?`
  )
  
  imp <- mice::mice(
    rescue_workers, 
    method = "norm.predict", 
    m = 1, 
    printFlag=FALSE
  ) 
  rw <- complete(imp) 
  rw$age <- round(rw$age)
  
  rw
}


