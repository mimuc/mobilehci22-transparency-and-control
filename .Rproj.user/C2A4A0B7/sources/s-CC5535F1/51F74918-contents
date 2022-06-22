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
