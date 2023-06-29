library(tidyverse)
library(rvest)
library(janitor)
library(sf)
library(ggtext)

rm(list=ls())

nyc.eds <- read_sf('https://data.cityofnewyork.us/api/geospatial/h2n3-98hq?method=export&format=GeoJSON')
nyc.eds <- nyc.eds %>%
  mutate(AD = substr(elect_dist, 1,2) %>% as.numeric,
         ED = substr(elect_dist, 3,6) %>% as.numeric) %>%
  st_transform(3857)

read_html('https://web.enrboenyc.us/OF18AD0PY1.html') %>% 
  html_elements('table table td a') %>% 
  html_attr('href') %>% str_c('https://web.enrboenyc.us/', .) ->
  nyccd.urls

for (nyccd.url in nyccd.urls) {
  nyccd <- read_html(nyccd.url) 
  
  race_name <- nyccd %>% html_element('table table th') %>%
    html_text()
  
  nyccd %>% html_elements('table table td a') %>% 
    html_attr('href')  %>% str_c('https://web.enrboenyc.us/', .) %>%
    tail(n=1) %>% 
    read_html() %>%
    html_elements('table table td a') %>% 
    html_attr('href') %>% str_c('https://web.enrboenyc.us/', .) ->
    ads
  
  rm(race)
  
  for (ad in ads) {
    adpg <- read_html(ad) 
    
    adpg %>% html_table() %>%
      .[[3]] %>%
      row_to_names(1) %>%
      remove_empty(which='cols') -> race.ad
    
    colnames(race.ad)[c(1,2)] <- c('ED', 'Reported')
    race.ad <- race.ad[c(-1,-nrow(race.ad)),-2]
    
    adnum <- adpg %>% html_element('table table th') %>% html_text() %>%
      str_match('AD (\\d+)') %>% .[,2] %>% parse_number()
    
    if(!exists('race'))
      race <- race.ad %>%
      mutate(AD = adnum, 
             ED = parse_number(ED),
             across(2:ncol(.), parse_integer),
             .before = 1) 
    else
      race <- rbind(race, 
                    race.ad %>%
                      mutate(AD = adnum, 
                             ED = parse_number(ED),
                             across(2:ncol(.), parse_integer),
                             .before = 1) 
      )
    
    Sys.sleep(2)
  }
  
  rm(list=c('race.ad','adnum','ad'))
  
  cdbound <- nyc.eds %>%
    right_join(race) %>%
    rowwise() %>% 
    filter(sum(across(7:ncol(.))) > 0)
  
  bbox <- nyc.eds %>%
    right_join(race) %>%
    rowwise() %>% 
    filter(sum(across(7:ncol(.))) > 0) %>%
    st_bbox() 
  
  
    con <- DBI::dbConnect(RPostgres::Postgres(), dbname='gis', host='localhost', 
                          port=5432, user='postgres', password='xxxxx')
    
    qry <- paste("SELECT way FROM new_york_osm_line WHERE way && ST_MakeEnvelope(",
                 bbox[1],',',
                 bbox[2],',',
                 bbox[3],',',
                 bbox[4],
                 ", 3857) AND highway IS NOT NULL AND highway IS NOT NULL",sep="")
    
    roads <- st_read(con, query=qry, geom='way') %>% st_intersection(cdbound)
    
    qry <- paste("SELECT way FROM new_york_osm_polygon WHERE way && ST_MakeEnvelope(",
                 bbox[1],',',
                 bbox[2],',',
                 bbox[3],',',
                 bbox[4],
                 ", 3857) AND building='yes'",sep="")
    
    blding <- st_read(con, query=qry, geom='way')%>% st_intersection(cdbound)
    
    qry <- paste("SELECT name, ST_Simplify(way,500) as way FROM new_york_osm_point WHERE way && ST_MakeEnvelope(",
                 bbox[1],',',
                 bbox[2],',',
                 bbox[3],',',
                 bbox[4],
                 ", 3857) AND place IS NOT NULL",sep="")
    
    neig <- st_read(con, query=qry, geom='way') %>% st_intersection(cdbound)
    
    
    
  
  subtitle.str <- ''
    
  for (coln in colnames(race[,3:ncol(race)])) {
     subtitle.str <- str_c(
       subtitle.str,
       coln, ' ', round(sum(race[,coln])/sum(race[,3:ncol(race)])*100,1), '% (', format(sum(race[,coln]), big.mark = ','), ' votes) | '
       )  
  }
  
  subtitle.str <- substr(subtitle.str, 1, str_length(subtitle.str)-3)
  
  nyc.eds %>%
    right_join(race) %>%
    rowwise() %>%
    mutate(
      total = sum(across(7:ncol(.))), 
      across(7:ncol(.), ~(./total)*100)
    ) %>%
    select(AD:ncol(.), -total, geometry) %>%
    mutate(
      across(4:ncol(.)-1, ~replace_na(.x, 0)),
           maxval = max(across(3:ncol(.)-1)),
           across(4:ncol(.)-1, ~ifelse(maxval == ., ., NA))
           ) %>%
    pivot_longer(5:ncol(.)-2) %>%
    select(-maxval) %>%
    drop_na() %>%
    ggplot() +
    geom_sf(aes(fill=name, alpha=value)) +
    geom_sf(data=roads, linewidth=0.05) +
    geom_sf(data=blding, linewidth=0, alpha=0.4, fill='black') +
    ggsflabel::geom_sf_text_repel(data=neig, aes(label=name), size=3, family='Chivo', bg.r = 0.2, bg.color='white') +
    scale_alpha_binned(n.breaks=10, name='Winner %') +
    scale_fill_viridis_d(name='Won ED') +
    theme_void() +
    coord_sf(expand=F, xlim=c(bbox$xmin, bbox$xmax), ylim=c(bbox$ymin, bbox$ymax)) +
    guides(alpha = guide_legend(override.aes = list(fill = 'black')),
           fill = guide_legend(override.aes = list(size = 0)),
    ) +
    labs(title = str_c('<b style="font-size: 20pt">',race_name,' Primary 2023</b><br /><span style="font-size: 12pt">',
                       subtitle.str),
         tag=paste('Data from NYC Board of Elections, Election Night Results 2023.',
                   '<br /><em>',str_replace(nyccd.url,'https://',''),'</em><br /><br />',
                   '<br />Map by Andy Arthur,', format(Sys.Date(), format="%-m/%-d/%y."),
                   '. Buildings and streets &copy; Openstreetmap Contributors',
                   '<br /><em>Created and processed using R Statistical Language, see github.com/andyarthur'),
         fill = "") +
    theme(
      text= element_text(family='Chivo',size=14),
      plot.title=element_textbox(halign = 0.5, hjust=0.5, margin=margin(b=20)),
      plot.background = element_rect(fill = "white", color="white"),
      plot.tag=element_textbox(size=10,hjust=0, color='#555555'),
      plot.tag.position = 'bottom',
      plot.margin = unit(c(1,1,1,1), 'lines'),
      legend.direction = 'vertical',
      legend.position = 'left',
      legend.spacing.x = unit(0.5,'cm'),
      legend.spacing.y = unit(0.45,'cm'),
    ) 
  
  
  fn <- fs::path_sanitize(race_name) %>% str_replace_all('\\W', '_')
  ggsave(paste('/tmp/nycc/',fn,'.jpg',sep=''), width=1920, height=1300, units='px', dpi=120)
  ggsave(paste('/tmp/nycc/',fn,'.svg',sep=''), width=1920, height=1300, units='px', dpi=120, device = grDevices::svg)
  system(paste('scour /tmp/nycc/',fn,'.svg /tmp/nycc/',fn,'.svgz',sep=''))
  
  Sys.sleep(5)
}

