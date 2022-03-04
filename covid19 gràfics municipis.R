
Sys.setlocale(category = "LC_ALL", locale = "Catalan")
library(tidyverse) ; library(jsonlite) ; library(httr)
setwd("D:/Stats and programming links/R scripts la Cala")

# Aldover, Pobla de Massaluca, Reus, Xerta

codes_25 = c("43013", "43162", "43052", "43133", "43903", "43904", "43901", "43102", "43149", "43008", "43006", "43025", "43104", "43906", "43155", "43136", "43014", "43004", "43044", "43902", "43156", "43138", "43068", "43078", "43063", "43062", "43077", "43094", "43093", "43019", "43060", "43152", "43065", "43150", "43026", "43067", "43084", "43125", "43099", "43121", "43177", "43064", "43032", "43106", "43022", "43071", "43048", "43018", "43056", "43110", "43175", "43041")
codes_50 = c("43092", "43123", "43038", "43129", "43011", "43009", "43178", "43031", "43088", "43128", "43145", "43127", "43081", "43003", "43007", "43042", "43033", "43169", "43118", "43116", "43167", "431613")

codes = bind_rows(codes_25 %>% as.data.frame() %>% rename("codes" = ".") %>% mutate(minimum = 25),
                  codes_50 %>% as.data.frame() %>% rename("codes" = ".") %>% mutate(minimum = 50))

poblacio = bind_rows(
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
  select(-codi)

codes = codes %>% left_join(poblacio, by = "codes"); rm(poblacio, codes_25, codes_50)
codes$literal[codes$literal == "Sant Carles de la Ràpita"] = "la Ràpita"

for (code in codes$codes) {
  df = bind_rows(
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
  ) %>% ### Clean
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
  municipidescripcio = unique(df$municipidescripcio)
  df = df %>% ### Fill not found dates with zeros
    left_join(as.Date(seq(date_min, (date_max + 60*60*24*1), by = "days")) %>% as.data.frame() %>% rename("date" = "."), ., by = "date") %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    mutate(trend = timsac::decomp(n, plot = F, trad = 7) %>% pluck("trend")) %>% 
    # mutate(seasonal = timsac::decomp(n, plot = F, trad = 7) %>% pluck("seasonal")) %>%
    # mutate(noise = timsac::decomp(n, plot = F, trad = 7) %>% pluck("noise")) %>%
    # mutate(deseasoned = trend + noise) %>%
    left_join(as.Date(seq(date_min, (date_max + 60*60*24*50), by = "days")) %>% as.data.frame() %>% rename("date" = "."), ., by = "date") %>% 
    mutate_if(is.numeric, replace_na, replace = 0) %>% 
    select(-c(codes, municipidescripcio)) %>%
    mutate(codes = code, municipidescripcio = municipidescripcio) %>%
    left_join(codes, by = "codes") %>%
    mutate(trend_lastyear = lag(trend, n=365, default=0)) %>%
    # mutate(deseasoned_lastyear = lag(deseasoned, n=365, default=0)) %>%
    mutate(ia14 = cumsum(n) - lag(cumsum(n), n=14, default=0)) %>% 
    mutate(ia14_lastyear = lag(ia14, n=365, default=0)) %>%
    mutate(n7 = (n + lag(n,1) + lag(n,2) + lag(n,3) + lag(n,4) + lag(n,5) + lag(n,6))/7) %>%
    mutate(rho7 = (lag(n7,1) + n7 + lead(n7,1))/(lag(n7,6) + lag(n7,5) + lag(n7,4))) %>%
    mutate(rho7 = case_when(rho7 > 4 ~ 4, TRUE ~ rho7)) %>%
    mutate(epg = (ia14/(poblacio/100000))*rho7) %>% 
    mutate(epg_lastyear = lag(epg, n=365, default=0)) %>%
    # mutate(municipidescripcio = unique(municipidescripcio[!is.na(municipidescripcio)])) %>% ### Line graphs (cumulative lags)
    filter(date >= Sys.Date()-240) %>%
    mutate(trend = case_when((date <= date_max) ~ trend), 
           # deseasoned = case_when((date <= date_max) ~ deseasoned), 
           ia14 = case_when((date <= date_max) ~ ia14), 
           n7 = case_when((date <= date_max) ~ n7), 
           rho7 = case_when((date <= date_max) ~ rho7), 
           epg = case_when((date <= date_max) ~ epg), 
           n = case_when((date <= date_max) ~ n))
  if (length(df[,1]) > 0) {
    literal = df$literal[1]
    textpla = literal %>% str_replace_all("ò", "o") %>% str_replace_all("ó", "o") %>% str_replace_all("é", "e") %>% str_replace_all("à", "a") %>% str_remove_all(", l\'") %>% str_remove_all(", el") %>% str_remove_all(", la")
    if (max(df$ia14, na.rm = T) >= codes$minimum[codes$codes == code]) {
      datelength = df %>% pluck("date") %>% length()
      maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
      endvalue = df %>% filter(date == maxdate) %>% pluck("trend") %>% round(2)
      maxvalue = df %>% filter(!is.na(n)) %>% pluck("trend") %>% max()
      print(
        (ggplot(data = df, aes(x=as.Date(date)+1)) +
           geom_point(aes(y=n), size=1/4, color="#111111") +
           geom_line(aes(y=trend), size=1/4, color="#000000", linetype = "solid") +
           geom_line(aes(y=trend_lastyear), size=1/4, color="#222222", linetype = "dotted") +
           ggtitle(label = literal, subtitle = "Tendència desestacionalitzada de casos nous diaris") + 
           scale_y_continuous(name = "trend") + expand_limits(y = 0) +
           theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
           geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
           scale_x_date(date_breaks = 'month', date_labels = '%b', name = element_blank())) %>% 
          ggsave(filename = str_c("graphs trend ext/",textpla,".png"), device = "png", dpi = 200, width = 7, height = 4)
      )
      df = df %>% filter(date >= Sys.Date()-90)
      datelength = df %>% pluck("date") %>% length()
      maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
      endvalue = df %>% filter(date == maxdate) %>% pluck("trend") %>% round(2)
      maxvalue = df %>% filter(!is.na(n)) %>% pluck("trend") %>% max()
      print(
        (ggplot(data = df, aes(x=as.Date(date)+1)) +
           geom_point(aes(y=n), size=1/4, color="#111111") +
           geom_line(aes(y=trend), size=1/4, color="#000000", linetype = "solid") +
           geom_line(aes(y=trend_lastyear), size=1/4, color="#222222", linetype = "dotted") +
           ggtitle(label = literal, subtitle = "Tendència desestacionalitzada de casos nous diaris") + 
           scale_y_continuous(name = "trend") + expand_limits(y = 0) +
           theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
           geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
           scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
          ggsave(filename = str_c("graphs trend/",textpla,".png"), device = "png", dpi = 200, width = 7, height = 4)
      )
      datelength = df %>% pluck("date") %>% length()
      maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
      endvalue = df %>% filter(date == maxdate) %>% pluck("ia14") %>% round(2)
      maxvalue = df %>% filter(!is.na(n)) %>% pluck("ia14") %>% max()
      print(
        (ggplot(data = df, aes(x=as.Date(date)+1)) +
           geom_line(aes(y=ia14), size=1/4, color="#000000", linetype = "solid") +
           geom_line(aes(y=ia14_lastyear), size=1/4, color="#222222", linetype = "dotted") +
           ggtitle(label = literal, subtitle = "Incidència acumulada darrers 14 dies") + 
           scale_y_continuous(name = "IA14") + expand_limits(y = 0) +
           theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
           geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
           scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
          ggsave(filename = str_c("graphs ia14/",textpla,".png"), device = "png", dpi = 200, width = 7, height = 4)
      )
      datelength = df %>% pluck("date") %>% length()
      maxdate = df %>% filter(!is.na(n)) %>% filter(date == max(date)) %>% select(date) %>% unlist() %>% as.Date.POSIXct() + 1
      endvalue = df %>% filter(date == maxdate) %>% pluck("epg") %>% round(2)
      maxvalue = df %>% filter(!is.na(n)) %>% pluck("epg") %>% max()
      print(
        (ggplot(data = df, aes(x=as.Date(date)+1)) +
           geom_line(aes(y=epg), size=1/4, color="#000000", linetype = "solid") +
           geom_line(aes(y=epg_lastyear), size=1/4, color="#222222", linetype = "dotted") +
           ggtitle(label = literal, subtitle = "Índex de risc de rebrot") + 
           scale_y_continuous(name = "EPG") + expand_limits(y = 0) +
           theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
           geom_label(data=as.data.frame(endvalue),aes(x=maxdate+(datelength/50),y=endvalue), label = endvalue, label.size = NA, hjust = 0, fill=NA) + 
           scale_x_date(date_breaks = 'week', date_labels = '%d\n%b', name = element_blank())) %>% 
          ggsave(filename = str_c("graphs epg/",textpla,".png"), device = "png", dpi = 200, width = 7, height = 4)
      )
    }
  }
}

