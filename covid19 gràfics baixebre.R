
Sys.setlocale(category = "LC_ALL", locale = "Catalan")
library(tidyverse) ; library(jsonlite) ; library(httr)
setwd("D:/Stats and programming links/R scripts la Cala")

codis_ebre = c("43013","43104","43906","43903","43904","43901","43155","43149","43025","43133","43006","43052","43008","43102") %>% 
  as.data.frame() %>% rename("codes" = ".")

codis_ebre = 
  bind_rows(
  str_c("https://analisi.transparenciacatalunya.cat/resource/b4rr-d25b.json") %>% 
    httr::GET() %>% 
    httr::content(as="text") %>% 
    gsub("^angular.callbacks._2\\(", "", .) %>% 
    gsub("\\);$", "", .) %>%
    jsonlite::fromJSON(simplifyDataFrame = T)
  ) %>% ### Clean
  filter(any == "2019") %>%
  select(codi, literal, contains("total")) %>%
  mutate(total_de_0_a_14_anys = total_de_0_a_14_anys %>% as.numeric()) %>%
  mutate(total_de_15_a_64_anys = total_de_15_a_64_anys %>% as.numeric()) %>%
  mutate(total_de_65_anys_i_m_s = total_de_65_anys_i_m_s %>% as.numeric()) %>%
  mutate(poblacio = total_de_0_a_14_anys + total_de_15_a_64_anys + total_de_65_anys_i_m_s) %>%
  select(!contains("total")) %>%
  mutate(codes = codi %>% str_sub(1, 5)) %>% 
  select(-codi) %>% 
  right_join(codis_ebre, by = "codes")

df = str_c("https://analisi.transparenciacatalunya.cat/resource/jj6z-iyrp.json?sexecodi=0&municipicodi=43133") %>% 
    httr::GET() %>% 
    httr::content(as="text") %>% 
    gsub("^angular.callbacks._2\\(", "", .) %>% 
    gsub("\\);$", "", .) %>%
    jsonlite::fromJSON(simplifyDataFrame = T) %>% filter(is.na(municipicodi))

for (code in codis_ebre$codes) {
  df = rbind(df, bind_rows(
    str_c("https://analisi.transparenciacatalunya.cat/resource/jj6z-iyrp.json?sexecodi=0&municipicodi=", code) %>% 
      httr::GET() %>% 
      httr::content(as="text") %>% 
      gsub("^angular.callbacks._2\\(", "", .) %>% 
      gsub("\\);$", "", .) %>%
      jsonlite::fromJSON(simplifyDataFrame = T) ,
    str_c("https://analisi.transparenciacatalunya.cat/resource/jj6z-iyrp.json?sexecodi=1&municipicodi=", code) %>% 
      httr::GET() %>% 
      httr::content(as="text") %>% 
      gsub("^angular.callbacks._2\\(", "", .) %>% 
      gsub("\\);$", "", .) %>%
      jsonlite::fromJSON(simplifyDataFrame = T)
  ))
}

df = df %>% ### Clean
  # select_if(function(.) !n_distinct(.) == 1) %>% 
  mutate(data = gsub("T00:00:00.000", "", data)) %>% 
  mutate(data = as.POSIXct(data, format = "%Y-%m-%d")) %>% 
  mutate(numcasos = as.numeric(numcasos)) %>% 
  select(-sexecodi, -comarcacodi, -comarcadescripcio) %>% ### Rename and get genders together
  rename(date = data) %>% 
  pivot_wider(names_from = sexedescripcio, values_from = numcasos) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  rowwise() %>% mutate(numcasos = sum(Home, Dona, na.rm = T)) %>% 
  select(-c(Home, Dona)) %>% ### Select positives and suspicious
  pivot_wider(names_from = resultatcoviddescripcio, values_from = numcasos, values_fill = 0) %>% 
  rename(codes = municipicodi) %>%
  group_by(date, codes, municipidescripcio) %>% nest() %>% 
  mutate(n = map(data, function(df) sum(select(df, contains("Positiu"))))) %>% 
  unnest(cols = c(n, data)) %>% 
  select(., date, codes, municipidescripcio, n)
date_min = min(df$date)
date_max = max(df$date)
poblacio = sum(codis_ebre$poblacio)

df = df %>% 
  select(-c(codes, municipidescripcio)) %>%
  group_by(date) %>% summarise(n = sum(n)) %>% ungroup()

df = df %>% ### Fill not found dates with zeros
  left_join(as.Date(seq(date_min, (date_max + 60*60*24*1), by = "days")) %>% as.data.frame() %>% rename("date" = "."), ., by = "date") %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(trend = timsac::decomp(n, plot = F) %>% pluck("trend")) %>% 
  mutate(seasonal = timsac::decomp(n, plot = F) %>% pluck("seasonal")) %>% 
  mutate(ar = timsac::decomp(n, plot = F) %>% pluck("ar")) %>% 
  mutate(trad = timsac::decomp(n, plot = F) %>% pluck("trad")) %>% 
  mutate(noise = timsac::decomp(n, plot = F) %>% pluck("noise")) %>% 
  left_join(as.Date(seq(date_min, (date_max + 60*60*24*50), by = "days")) %>% as.data.frame() %>% rename("date" = "."), ., by = "date") %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  mutate(trend_lastyear = lag(trend, n=365, default=0)) %>%
  mutate(seasonal_lastyear = lag(seasonal, n=365, default=0)) %>%
  mutate(ar_lastyear = lag(ar, n=365, default=0)) %>%
  mutate(trad_lastyear = lag(trad, n=365, default=0)) %>%
  mutate(noise_lastyear = lag(noise, n=365, default=0)) %>%
  mutate(ia14 = cumsum(n) - lag(cumsum(n), n=14, default=0)) %>% 
  mutate(ia14_lastyear = lag(ia14, n=365, default=0)) %>%
  mutate(n7 = (n + lag(n,1) + lag(n,2) + lag(n,3) + lag(n,4) + lag(n,5) + lag(n,6))/7) %>%
  mutate(rho7 = (lag(n7,1) + n7 + lead(n7,1))/(lag(n7,6) + lag(n7,5) + lag(n7,4))) %>%
  mutate(rho7 = case_when(rho7 > 4 ~ 4, TRUE ~ rho7)) %>%
  mutate(rho7_lastyear = lag(rho7, n=365, default=0)) %>%
  mutate(epg = (ia14/(poblacio/100000))*rho7) %>% 
  mutate(epg_lastyear = lag(epg, n=365, default=0)) %>%
  filter(date >= Sys.Date()-240) %>%
  mutate(trend = case_when((date == date_max & n != 0) | date < date_max ~ trend), 
         seasonal = case_when((date == date_max & n != 0) | date < date_max ~ seasonal), 
         ar = case_when((date == date_max & n != 0) | date < date_max ~ ar), 
         trad = case_when((date == date_max & n != 0) | date < date_max ~ trad), 
         noise = case_when((date == date_max & n != 0) | date < date_max ~ noise), 
         ia14 = case_when((date == date_max & n != 0) | date < date_max ~ ia14), 
         n7 = case_when((date == date_max & n != 0) | date < date_max ~ n7), 
         rho7 = case_when((date == date_max & n != 0) | date < date_max ~ rho7), 
         epg = case_when((date == date_max & n != 0) | date < date_max ~ epg), 
         n = case_when((date == date_max & n != 0) | date < date_max ~ n))

datelength = df %>% pluck("date") %>% length()
maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
endvalue = df %>% filter(date == maxdate) %>% pluck("trend") %>% round(2)
maxvalue = df %>% filter(!is.na(n)) %>% pluck("trend") %>% max()
print(
  (ggplot(data = df, aes(x=as.Date(date)+1)) +
     geom_point(aes(y=n), size=1/4, color="#111111") +
     geom_line(aes(y=trend), size=1/4, color="#000000", linetype = "solid") +
     geom_line(aes(y=trend_lastyear), size=1/4, color="#222222", linetype = "dotted") +
     ggtitle(label = "Baix Ebre", subtitle = "Tendència desestacionalitzada de casos nous diaris") + 
     scale_y_continuous(name = "trend") + expand_limits(y = 0) +
     theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
     geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
     scale_x_date(date_breaks = 'month', date_labels = '%b', name = element_blank())) %>% 
    ggsave(filename = str_c("baixebre/trend ext.png"), device = "png", dpi = 200, width = 7, height = 4)
)

df = df %>% filter(date >= Sys.Date()-90)

datelength = df %>% pluck("date") %>% length()
maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
endvalue = df %>% filter(date == maxdate) %>% pluck("ia14") %>% round(2)
maxvalue = df %>% filter(!is.na(n)) %>% pluck("ia14") %>% max()
print(
  (ggplot(data = df, aes(x=as.Date(date)+1)) +
     # geom_point(aes(y=ia14), size=1/2, color="#000000") +
     geom_line(aes(y=ia14), size=1/4, color="#000000", linetype = "solid") +
     geom_line(aes(y=ia14_lastyear), size=1/4, color="#222222", linetype = "dotted") +
     ggtitle(label = "Baix Ebre", subtitle = "Incidència acumulada darrers 14 dies") + 
     scale_y_continuous(name = "IA14") + expand_limits(y = 0) +
     theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
     geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
     scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
    ggsave(filename = str_c("baixebre/ia14.png"), device = "png", dpi = 200, width = 7, height = 4)
)

datelength = df %>% pluck("date") %>% length()
maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
endvalue = df %>% filter(date == maxdate) %>% pluck("epg") %>% round(2)
maxvalue = df %>% filter(!is.na(n)) %>% pluck("epg") %>% max()
print(
  (ggplot(data = df, aes(x=as.Date(date)+1)) +
     # geom_point(aes(y=epg), size=1/2, color="#000000") +
     geom_line(aes(y=epg), size=1/4, color="#000000", linetype = "solid") +
     geom_line(aes(y=epg_lastyear), size=1/4, color="#222222", linetype = "dotted") +
     ggtitle(label = "Baix Ebre", subtitle = "Índex de risc de rebrot") + 
     scale_y_continuous(name = "EPG") + expand_limits(y = 0) +
     theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
     geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
     scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
    ggsave(filename = str_c("baixebre/epg.png"), device = "png", dpi = 200, width = 7, height = 4)
)

datelength = df %>% pluck("date") %>% length()
maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
endvalue = df %>% filter(date == maxdate) %>% pluck("trend") %>% round(2)
maxvalue = df %>% filter(!is.na(n)) %>% pluck("trend") %>% max()
print(
  (ggplot(data = df, aes(x=as.Date(date)+1)) +
     geom_point(aes(y=n), size=1/4, color="#111111") +
     geom_line(aes(y=trend), size=1/4, color="#000000", linetype = "solid") +
     geom_line(aes(y=trend_lastyear), size=1/4, color="#222222", linetype = "dotted") +
     ggtitle(label = "Baix Ebre", subtitle = "Tendència desestacionalitzada de casos nous diaris") + 
     scale_y_continuous(name = "trend") + expand_limits(y = 0) +
     theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
     geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
     scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
    ggsave(filename = str_c("baixebre/trend.png"), device = "png", dpi = 200, width = 7, height = 4)
)


