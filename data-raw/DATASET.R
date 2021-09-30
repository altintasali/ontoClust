## code to prepare `DATASET` dataset goes here
sample_files <- list.files("./data-raw/sample_ora/", full.names = TRUE)
dataset_names <- list.files("./data-raw/sample_ora/", full.names = FALSE)
dataset_names <- stringr::str_split_fixed(dataset_names, "_|[.]", Inf)[,3] %>% toupper()

dat <- list()
dat <- lapply(sample_files, function(x){
  dt <- data.table::fread(x)
  dt[, c("Cluster", "geneID") := NULL]
  dt <- dt[!is.na(pvalue),]
  dt$Condition <- factor(dt$Condition, levels = c("CTRL_CvsM", "PCOS_CvsM"), labels = c("AvsB", "CvsD"))
  dt <- dt[!is.na(Condition), ]
  return(dt)
  }
)
names(dat) <- dataset_names
sample_data <- dat

usethis::use_data(sample_data, overwrite = TRUE, compress = "xz")
