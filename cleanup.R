username_id_match <- users %>% select(username) %>% mutate(user_id = row_number())


tmp <- readRDS("data/questionnaires/data_postq.rds") %>% 
  mutate(username=as.character(username))%>% 
  left_join(username_id_match) %>% 
  mutate(username=user_id) %>% 
  select(-user_id)


saveRDS(tmp, "data/questionnaires/data_postq.rds")




tmp2 <- readRDS("data/app/users.rds") %>% 
  rename(user_id_2 = user_id) %>%
  mutate(username=as.character(username))%>% 
  left_join(username_id_match) %>% 
  mutate(username=user_id) %>% 
  select(-user_id) %>%
  rename(user_id = user_id_2)
saveRDS(tmp2,"data/app/users.rds")



saveRDS(username_id_match,"username_id_match.rds")


######

ps_dashboard_usage <- readRDS("data/app/ps_dashboard_usage.rds")
tmp2 <- ps_dashboard_usage %>% 
  rename(user_id_2 = user_id) %>%
  mutate(username=as.character(username))%>% 
  left_join(username_id_match) %>% 
  mutate(username=user_id) %>% 
  select(-user_id) %>%
  rename(user_id = user_id_2)
saveRDS(tmp2,"data/app/ps_dashboard_usage.rds")


####
ps_usage <- readRDS("data/app/ps_usage.rds")
tmp_u <- ps_usage %>% 
  rename(user_id_2 = user_id) %>%
  mutate(username=as.character(username))%>% 
  left_join(username_id_match) %>% 
  mutate(username=user_id) %>% 
  select(-user_id) %>%
  rename(user_id = user_id_2)
tmp_u <- tmp_u %>% 
  select(-activity_hash, -app_store_id,-calendar_id,-communication_id,-deviceinfo_id,-music_id,-contact_id,-snapshot_id,-location_id,-notification_id,-sleepsegmentdata_id,-sleepclassifydata_id,-created_at,-updated_at)
tmp_u %>% saveRDS("data/app/ps_usage.rds")
