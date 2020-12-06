
library(tidyverse) ; library(jsonlite) ; library(httr)

### Scrap

url = "https://analisi.transparenciacatalunya.cat/resource/jj6z-iyrp.json?municipicodi=43013" # l'Ametlla de Mar
a = httr::GET(url) %>% httr::content(as="text")
a = gsub("^angular.callbacks._2\\(", "", a)
a = gsub("\\);$", "", a)
df = jsonlite::fromJSON(a, simplifyDataFrame = T)
rm(a, url)

### Clean

df = df %>% 
  # select_if(function(.) !n_distinct(.) == 1) %>% 
  mutate(data = gsub("T00:00:00.000", "", data)) %>% 
  mutate(data = as.POSIXct(data, format = "%Y-%m-%d")) %>% 
  mutate(numcasos = as.numeric(numcasos)) %>% 
  select(-sexecodi, -comarcacodi, -comarcadescripcio)

### Rename and get genders together

df = df %>% 
  rename(date = data) %>% 
  pivot_wider(names_from = sexedescripcio, values_from = numcasos) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  rowwise() %>% mutate(numcasos = sum(Home, Dona, na.rm = T)) %>% 
  select(-c(Home, Dona))

### Select positives and suspicious

df = df %>% 
  pivot_wider(names_from = resultatcoviddescripcio, values_from = numcasos) %>% 
  mutate_if(is.numeric, replace_na, replace = 0) %>% 
  group_by(date, municipicodi, municipidescripcio) %>% nest() %>% 
  mutate(Positiu = map(data, function(df) sum(select(df, contains("Positiu"))))) %>% 
  unnest(cols = c(Positiu, data)) %>% 
  select(., date, municipicodi, municipidescripcio, Positiu, Sospitós)

### Fill not found dates with zeros

date = as.Date(seq(min(df$date), (max(df$date) + 86400), by = "days")) %>% as.data.frame() %>% rename("date" = ".")
df = left_join(date, df, by = "date")
rm(date)
df = df %>% mutate_if(is.numeric, replace_na, replace = 0)

### Put weekdays

df = df %>% 
  mutate(weekday = lubridate::wday(date, label = T, abbr = F)) %>% 
  select(date, weekday, Sospitós, Positiu)

df$weekday = as.character(df$weekday)

### Graphs for last 60, 30 and 15 days

df %>% filter(date >= Sys.Date()-60) %>%
  ggplot(aes(x=as.Date(date)+1)) +
  geom_point(aes(y=Sospitós/5), size=1, color="#69b3a2") +
  geom_smooth(aes(y=Sospitós/5), size=1/2, method=loess, formula = 'y ~ x', span = .05, color="#69b3a2", se=F) +
  geom_point(aes(y=Positiu), size=2, color="#000000") +
  geom_line(aes(y=Positiu), size=1/4, color="#000000", linetype = "dotted") +
  geom_smooth(aes(y=Positiu), method=loess, formula = 'y ~ x', span = .05, color="#000000", se=F) +
  ggtitle("Nombre de positius i sospitosos a l'Ametlla de Mar (darrers 60 dies)") + 
  scale_y_continuous(name = "Casos positius", sec.axis = sec_axis( trans=~.*5, name="Casos sospitosos")) +
  theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank()) + 
  scale_x_date(date_breaks = 'week', date_labels = '%a %d\n%b')

df %>% filter(date >= Sys.Date()-30) %>%
  ggplot(aes(x=as.Date(date)+1)) +
  geom_point(aes(y=Sospitós/5), size=1, color="#69b3a2") +
  geom_smooth(aes(y=Sospitós/5), size=1/2, method=loess, formula = 'y ~ x', span = .05, color="#69b3a2", se=F) +
  geom_point(aes(y=Positiu), size=2, color="#000000") +
  geom_line(aes(y=Positiu), size=1/4, color="#000000", linetype = "dotted") +
  geom_smooth(aes(y=Positiu), method=loess, formula = 'y ~ x', span = .05, color="#000000", se=F) +
  ggtitle("Nombre de positius i sospitosos a l'Ametlla de Mar (darrers 30 dies)") + 
  scale_y_continuous(name = "Casos positius", sec.axis = sec_axis( trans=~.*5, name="Casos sospitosos")) +
  theme(axis.title.y.right = element_text(color = "#69b3a2"), axis.title.x=element_blank(), title = element_text()) + 
  scale_x_date(date_breaks = 'week', date_labels = '%a %d\n%b')

df %>% filter(date >= Sys.Date()-15) %>%
  ggplot(aes(x=as.Date(date)+1)) +
  geom_point(aes(y=Sospitós/5), size=1, color="#69b3a2") +
  geom_smooth(aes(y=Sospitós/5), size=1/2, method=loess, formula = 'y ~ x', span = .05, color="#69b3a2", se=F) +
  geom_point(aes(y=Positiu), size=2, color="#000000") +
  geom_line(aes(y=Positiu), size=1/4, color="#000000", linetype = "dotted") +
  geom_smooth(aes(y=Positiu), method=loess, formula = 'y ~ x', span = .05, color="#000000", se=F) + 
  ggtitle("Nombre de positius i sospitosos a l'Ametlla de Mar (darrers 15 dies)") + 
  scale_y_continuous(name = "Casos positius", sec.axis = sec_axis( trans=~.*5, name="Casos sospitosos")) +
  theme(axis.title.y.right = element_text(color = "#69b3a2"), 
        axis.ticks.y.right = element_line(color = "#69b3a2"), 
        axis.line.y.right = element_line(color = "#69b3a2"), 
        axis.title.x=element_blank()) + 
  scale_x_date(date_breaks = 'day', date_labels = '%a\n%d\n%b')

