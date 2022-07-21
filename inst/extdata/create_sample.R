library(dplyr)

CDNOW_sample <- readr::read_table2("inst/extdata/CDNOW_sample.txt", col_names = FALSE)
colnames(CDNOW_sample) <- c('cust_id', 'sample_cust_id', 'trans_date', 'num_cds', 'trans_amount')

CDNOW_sample$trans_date <- strptime(CDNOW_sample$trans_date, format = '%Y%m%d')
max_date <- min(CDNOW_sample$trans_date)
max_date$mday <- max_date$mday + 273

CDNOW_sample <- CDNOW_sample %>%
  group_by(cust_id, sample_cust_id, trans_date) %>%
  summarise(num_cds = sum(num_cds), trans_amount = sum(trans_amount), .groups = "drop") %>%
  arrange(sample_cust_id, trans_date)

CDNOW_sample$trans_date_1 <- CDNOW_sample$trans_date
CDNOW_sample$trans_date_1$mday <- CDNOW_sample$trans_date_1$mday + 1

CDNOW_sample_summary <- CDNOW_sample %>%
  group_by(sample_cust_id) %>%
  mutate(trans_days = as.numeric(difftime(cur_data()$trans_date, strptime('19970101', format = '%Y%m%d'), units = 'days')),
         q1 = ifelse(trans_days / 7 < 39, 1, 0),
         q2 = ifelse(trans_days / 7 >= 39, 1, 0)) %>%
  group_by(sample_cust_id, q1, q2) %>%
  mutate(repeat_purchases = sum(n()) - 1,
         t_last_repeat = if_else(repeat_purchases > 0, as.numeric(difftime(max(cur_data()$trans_date_1), min(cur_data()$trans_date_1), units = 'days')) / 7, 0),
         effective_cal_period = ifelse(t_last_repeat > 39,
                                       39 - (t_last_repeat - 39),
                                       as.numeric(difftime(max_date, min(cur_data()$trans_date_1), units = 'days')) / 7)
  ) %>%
  ungroup() %>%
  filter(q1 == 1) %>%
  distinct(sample_cust_id, repeat_purchases, t_last_repeat, effective_cal_period)

colnames(CDNOW_sample_summary) <- c('sample_id', 'x', 't_x', 'T_')

usethis::use_data(CDNOW_sample_summary)
