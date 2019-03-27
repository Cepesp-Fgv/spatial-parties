#rm(list=ls())
library(shiny)
library(cepespR)
library(tidyverse)
library(sp)
library(spdep)
library(maptools)
library(stargazer)
library(sf)
library(ggplot2)
library(seg)
library(leaflet)
library(mapview)
library(shinythemes)
#setwd("C:\\Users\\Jonny\\Google Drive\\Academic\\FGV-SP\\Concentration Paper\\Parties_Spatial_App\\Spatial_Parties")

mun <- readRDS("mun2.rds")
mun_sf <- st_as_sf(mun,crs = 4326)

set.seed(05410)

ui <- navbarPage("Parties Spatial Coordination",id="nav",theme=shinytheme("flatly"),
                 tabPanel("Clusters (Top 5)",
                          column(width=3,""),
                          column(width=4,h2("Real Data (Top 5 Candidates from Selected Party)"),leafletOutput("cluster_map"), htmlOutput("real_party_seg_measures_top_5"),htmlOutput("real_party_seg_measures"),htmlOutput("d_entropy_overall")),
                          column(width=4,h2("Randomized Candidates among Parties"),leafletOutput("cluster_map_RND"), htmlOutput("RND_party_seg_measures"))
                          ),
                 tabPanel("Randomized",
                          column(width=3,""),
                          column(width=4,h2("Dissimilarity of Top 5 candidates Randomized to Parties (Real Dissimilarity in Red)"),plotOutput("dist_RND"))
                          ),
                 tabPanel("Entropy",
                          column(width=3,""),
                          column(width=4,h2("Real Data (Top 5 Candidates from Seleted Party)"),leafletOutput("cluster_map2")),
                          column(width=4,h2("Entropy Measures: Yellow is LESS Overlap between Candidates of same Party"),leafletOutput("entropy_map_real"))
                          ),
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = FALSE, top = 120, left = 10, right = "auto", bottom = "auto",
                               width = 330, height = "auto",
                               selectInput("Year",
                                           label="Year:",
                                           choices=c(1998,2002,2006,2010,2014),
                                           selected=2014),
                               selectInput("State",
                                           label="State:",
                                           choices=c("AC","AM","AL","AP","BA","CE","DF","ES","GO","MA","MS","MG","MT","PA","PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO"),
                                           selected="CE"),
                               selectInput("Party",
                                           label="Party:",
                                           choices=c(10,11,12,13,14,15,16,17,18,19,20,21,22,23,25,27,28,29,30,31,33,35,36,40,43,44,45,50,51,54,55,65,70,77,90),
                                           selected=15),
                               sliderInput("Sims",
                                            label="Number of Simulations",
                                            value=10,
                                            min=10,
                                            max=100,
                                            step=10),
                               actionButton("RND_button","Randomize Candidates to Parties")
                               )
                  )
  

server <- function(input, output) {
 
#input <- State <- Party <- c() 
#input$Year <- 2014
#input$State <- "CE"
#input$Party <- 11
  
  d <- reactive({
    d <- get_elections(year=input$Year,
                       position=6,
                       regional_aggregation="Municipality",
                       political_aggregation="Candidate",
                       state=input$State)
    d <- d %>% dplyr::select(NUMERO_PARTIDO, NUMERO_CANDIDATO, COD_MUN_IBGE, QTDE_VOTOS)    
    d <- d %>% complete(NUMERO_CANDIDATO,COD_MUN_IBGE,fill=list(QTDE_VOTOS=0))
    d <- d %>% mutate(NUMERO_PARTIDO=ifelse(is.na(NUMERO_PARTIDO),substr(as.character(NUMERO_CANDIDATO),1,2),NUMERO_PARTIDO))
    
    d <- d %>% dplyr::mutate(State_QTDE_VOTOS=sum(QTDE_VOTOS)) 
    d <- d %>% dplyr::group_by(COD_MUN_IBGE) %>% dplyr::mutate(Mun_QTDE_VOTOS=sum(QTDE_VOTOS))
    d <- d %>% ungroup() %>% dplyr::group_by(NUMERO_CANDIDATO) %>% dplyr::mutate(Dep_QTDE_VOTOS=sum(QTDE_VOTOS))
    d <- d %>% ungroup() %>% dplyr::mutate(Cand_Vote_Share=Dep_QTDE_VOTOS/sum(QTDE_VOTOS))
    d <- d %>% ungroup() %>% dplyr::mutate(QL=(QTDE_VOTOS/Dep_QTDE_VOTOS) / (Mun_QTDE_VOTOS/State_QTDE_VOTOS))
  })
  
  d_party <- reactive({
    d_party <- d() %>% filter(NUMERO_PARTIDO==input$Party) %>%
      filter(nchar(NUMERO_CANDIDATO)!=2) #And remove vota legenda
    #d_party <- d %>% filter(NUMERO_PARTIDO==input$Party) %>%
    #filter(nchar(NUMERO_CANDIDATO)!=2)
  })
  
  d_party_top_5_cands <- reactive({
      top_5_cands <- d_party() %>% 
        dplyr::group_by(NUMERO_CANDIDATO) %>% 
        dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS)) %>% 
        arrange(-QTDE_VOTOS) %>%
        top_n(5) %>% 
        pull(NUMERO_CANDIDATO)
    
  d_party_top_5_cands <- d_party() %>% 
        filter(NUMERO_CANDIDATO %in% top_5_cands) %>% 
        select(NUMERO_CANDIDATO,COD_MUN_IBGE,QTDE_VOTOS,QL)
  })
  
  d_party_all_cands <- reactive({
    cands <- d_party() %>% 
      dplyr::group_by(NUMERO_CANDIDATO) %>% 
      dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS)) %>% 
      arrange(-QTDE_VOTOS) %>%
      pull(NUMERO_CANDIDATO)
    
    d_party_all_cands <- d_party() %>% 
      filter(NUMERO_CANDIDATO %in% cands) %>% 
      select(NUMERO_CANDIDATO,COD_MUN_IBGE,QTDE_VOTOS,QL)
  })
  
QL_wide_func <- function(d_party_cands){
    QL_wide <- d_party_cands %>% dplyr::mutate(NUMERO_CANDIDATO_tag=paste0("x",NUMERO_CANDIDATO)) %>%
      dplyr::select(-NUMERO_CANDIDATO) %>%
      dplyr::select(-QTDE_VOTOS) %>%
      spread(key=NUMERO_CANDIDATO_tag, value=QL,fill=0)
    return(QL_wide)
  }

d_QL_wide <- reactive({
    d_QL_wide <- QL_wide_func(d_party_top_5_cands())
    #d_QL_wide <- QL_wide_func(d_party_top_5_cands)
    })
  
  party_cands <- reactive({
    party_cands <- names(d_QL_wide()[which(grepl("^x",names(d_QL_wide())))])
    #party_cands <- names(d_QL_wide[which(grepl("^x",names(d_QL_wide)))])
  })

d_QL_wide_all <- reactive({
    d_QL_wide_all <- QL_wide_func(d_party_all_cands())
    #d_QL_wide_all <- QL_wide_func(d_party_all_cands)
  })
  
party_cands_all <- reactive({
    party_cands_all <- names(d_QL_wide_all()[which(grepl("^x",names(d_QL_wide_all())))])
    #party_cands_all <- names(d_QL_wide_all[which(grepl("^x",names(d_QL_wide_all)))])
  })

mun_state <- reactive({
    mun_state <- mun_sf %>% filter(UF==input$State) %>%
      dplyr::rename(COD_MUN_IBGE=GEOCOD) %>% st_as_sf(crs = 4326)
  })

create_links <- function(mun,a,b){
  mun_nb <- poly2nb(mun, row.names=mun$ID) 
  mun_nb[[which(mun@data[,"ID"]==a)]] <- as.integer(b)
  mun_nb[[which(mun@data[,"ID"]==b)]] <- sort(append(mun_nb[[which(mun@data[,"ID"]==b)]],as.integer(a)))
  return(mun_nb)
}

mun_state_d_top_5 <- reactive({
  mun_state_d <- mun_state() %>% left_join(d_QL_wide(), by="COD_MUN_IBGE") %>%
    mutate_at(party_cands(),funs(ifelse(is.na(.),0,.))) %>%
    st_as_sf(crs = 4326)
  #mun_state_d <- mun_state %>% left_join(d_QL_wide, by="COD_MUN_IBGE") %>%
  #mutate_at(party_cands,funs(ifelse(is.na(.),0,.))) %>%
  #  st_as_sf(crs = 4326)
  
  mun_state_d_top_5 <- as(mun_state_d,"Spatial")
})

mun_state_d_all <- reactive({
  mun_state_d <- mun_state() %>% left_join(d_QL_wide_all(), by="COD_MUN_IBGE") %>%
    mutate_at(party_cands_all(),funs(ifelse(is.na(.),0,.))) %>%
    st_as_sf(crs = 4326)
  #mun_state_d <- mun_state %>% left_join(d_QL_wide_all, by="COD_MUN_IBGE") %>%
  #  mutate_at(party_cands_all,funs(ifelse(is.na(.),0,.))) %>%
  #  st_as_sf(crs = 4326)
  
    mun_state_d_all <- as(mun_state_d,"Spatial")
})

nb_func <- function(mun,state){
if (state=="SP"){
  mun_nb <- create_links(mun,2919,5961)
  } else if (state=="RN"){
    mun_nb <- create_links(mun,5452,5276)
  } else if (state=="BA"){
    mun_nb <- create_links(mun,5464,4498)
    mun_nb <- create_links(mun,5464,4518)
    mun_nb <- create_links(mun,5464,5672)
  } else {
    mun_nb <- poly2nb(mun, row.names=mun$ID)
  }
nblist <- nb2listw(mun_nb,zero.policy=TRUE)
return(nblist)
}

mun_state_nb <- reactive({
  mun_state_nb <- nb_func(mun_state_d_all(),input$State)
  #mun_state_nb <- nb_func(mun_state_d_all,input$State)
})

#mun_state_party_d <- mun_state_d_top_5
#mun_state_d_nb <- mun_state_nb
#cands <- party_cands

LISA_func <- function(mun_state_party_d,mun_state_d_nb,cands){
  cand_clusters <- vector("list",length=length(cands))
  
  names(cand_clusters) <- cands
  
  for (c in cands){
    mun_state_contig <- mun_state_party_d
    lisa <- as.data.frame(localmoran(mun_state_contig@data[,c],mun_state_d_nb))
    
    mun_state_contig$LISA_I <- lisa[,"Ii"]
    mun_state_contig$LISA_p <- lisa[,"Pr(z > 0)"]
    mun_state_contig$LQ_stdzd <- as.vector(scale(mun_state_contig@data[,c]))
    mun_state_contig$LQ_stdzd_lag <- lag.listw(mun_state_d_nb,mun_state_contig$LQ_stdzd, NAOK=TRUE) #NAOK here helps or hinders?
    #mun_state_contig$LQ_stdzd_lag <- lag.listw(state_nb2listw,mun_state_contig$LQ_stdzd, NAOK=TRUE)
    
    mun_state_contig$category <- "Insignificant"
    mun_state_contig$category[mun_state_contig$LISA_p<0.05 & mun_state_contig$LQ_stdzd>=0 & mun_state_contig$LQ_stdzd_lag>=0] <- "High-High"
    mun_state_contig$category[mun_state_contig$LISA_p<0.05 & mun_state_contig$LQ_stdzd>=0 & mun_state_contig$LQ_stdzd_lag<=0] <- "High-Low"
    mun_state_contig$category[mun_state_contig$LISA_p<0.05 & mun_state_contig$LQ_stdzd<=0 & mun_state_contig$LQ_stdzd_lag>=0] <- "Low-High"
    mun_state_contig$category[mun_state_contig$LISA_p<0.05 & mun_state_contig$LQ_stdzd<=0 & mun_state_contig$LQ_stdzd_lag<=0] <- "Low-Low"
    mun_state_contig$category <- as.factor(mun_state_contig$category)
    if (dim(mun_state_contig[mun_state_contig$category=="High-High",])[1]==0){
      cand_clusters[[c]] <- NULL  
    } else {
    cand_clusters[[c]] <- mun_state_contig[mun_state_contig$category=="High-High",]
    cand_clusters[[c]] <- unionSpatialPolygons(cand_clusters[[c]],IDs=cand_clusters[[c]]@data[,"UF"]) #Need to do by cluster...but by UF seems to work, at least for presentational purposes!
    }
    
  }
  return(cand_clusters)
}

cand_clusters <- reactive({
  cand_clusters <- LISA_func(mun_state_d_top_5(),mun_state_nb(),party_cands())
  #cand_clusters <- LISA_func(mun_state_d_top_5,mun_state_nb,party_cands)
  })
  
output$cluster_map <- renderLeaflet({
    map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds()  
    colours <- c("red","blue","green","yellow","grey")
    
    party_cands_with_clusters <- party_cands()[party_cands() %in% names(cand_clusters())]
    #party_cands_with_clusters <- party_cands[party_cands %in% names(cand_clusters)]
    
    for (c in party_cands_with_clusters){
      map <- map %>% addPolygons(data=cand_clusters()[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters)])
      #map <- map %>% addPolygons(data=cand_clusters[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters)])
    }
    
    map
    
  })

output$cluster_map2 <- renderLeaflet({
  map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds()  
  colours <- c("red","blue","green","yellow","grey")
  
  party_cands_with_clusters <- party_cands()[party_cands() %in% names(cand_clusters())]
  #party_cands_with_clusters <- party_cands[party_cands %in% names(cand_clusters)]
  
  for (c in party_cands()){
    map <- map %>% addPolygons(data=cand_clusters()[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters)])
    #map <- map %>% addPolygons(data=cand_clusters[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters)])
  }
  
  map
  
})

spseg_func <- function(mun_state_party_d,cands){
  if (length(cands>1)){
    cands_QLs <- mun_state_party_d@data %>% dplyr::select(cands)
    spseg_out <- as.list(spseg(mun_state_party_d,data=cands_QLs))
  } else {
    spseg_out <- NULL
  }
  return(spseg_out)
}

output$real_party_seg_measures_top_5 <- renderUI({
  spseg_out <- spseg_func(mun_state_d_top_5(),party_cands())
  #spseg_out <- spseg_func(mun_state_d_top_5,party_cands)
  str_seg_measures <- paste0("For Top 5 candidates:","<br> Spatial Dissimilarity: ",round(spseg_out$d,3),"<br> Spatial Diversity: ",round(spseg_out$r,3), "<br> Spatial Information: ",round(spseg_out$h,3))
  HTML(str_seg_measures)
  })
  
output$real_party_seg_measures <- renderUI({
  spseg_out <- spseg_func(mun_state_d_all(),party_cands_all())
  #spseg_out <- spseg_func(mun_state_d_all,party_cands_all)
  str_seg_measures <- paste0("<br> For all candidates from party:","<br> Spatial Dissimilarity: ",round(spseg_out$d,3),"<br> Spatial Diversity: ",round(spseg_out$r,3), "<br> Spatial Information: ",round(spseg_out$h,3))
  HTML(str_seg_measures)
})

d_top10_top5 <- reactive({
  
  top_10_parties <- d() %>% group_by(NUMERO_PARTIDO,NUMERO_CANDIDATO) %>% dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=TRUE)) %>%
    group_by(NUMERO_PARTIDO) %>%
    dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=TRUE),num_cands=n()) %>%
    dplyr::filter(num_cands>=5) %>%
    arrange(-QTDE_VOTOS) %>%
    top_n(10) %>%
    pull(NUMERO_PARTIDO)

  d_top10 <- d() %>%
    filter(NUMERO_PARTIDO %in% top_10_parties) %>%
    select(NUMERO_PARTIDO,NUMERO_CANDIDATO,COD_MUN_IBGE,QTDE_VOTOS,QL)
  
  cands_top_5 <- d_top10 %>% dplyr::group_by(NUMERO_PARTIDO,NUMERO_CANDIDATO) %>% 
    dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS)) %>%
    group_by(NUMERO_PARTIDO) %>%
    arrange(-QTDE_VOTOS) %>%
    top_n(5) %>% 
    pull(NUMERO_CANDIDATO)
  
  d_top10_top5 <- d_top10 %>% filter(NUMERO_CANDIDATO %in% cands_top_5) %>% mutate(NUMERO_PARTIDO=as.integer(NUMERO_PARTIDO))
})

randomize_candidates <- function(d_top10_top5){
  top10_top5_cands <- d_top10_top5 %>% distinct(NUMERO_CANDIDATO)
  top10_parties <- d_top10_top5 %>% distinct(NUMERO_PARTIDO)
  
  cands_rnd_party <- vector("list",length=dim(top10_parties)[1])
  
  for (i in 1:dim(top10_parties)[1]){
    current_party <- top10_parties %>% slice(i) %>% as.numeric()
    cands_rnd_party[[i]] <- top10_top5_cands %>% sample_n(5) %>% mutate(RND_party=current_party)
    selected_cands <- cands_rnd_party[[i]] %>% dplyr::pull(NUMERO_CANDIDATO)
    top10_top5_cands <- top10_top5_cands %>% filter(!(NUMERO_CANDIDATO %in% selected_cands))
  }
  
  cands_rnd_party2 <- do.call("rbind",cands_rnd_party)
  
  data_RND_cands2 <- d_top10_top5 %>% 
    select(-NUMERO_PARTIDO) %>%
    left_join(cands_rnd_party2,by=c("NUMERO_CANDIDATO")) %>%
    dplyr::rename(NUMERO_PARTIDO=RND_party)
  
  return(data_RND_cands2)
}

data_RND_cands <- eventReactive(input$RND_button,{
  nsims <- input$Sims #nsims <- 10
  data_RND_cands_100 <- vector("list",nsims)
  for (i in 1:nsims){
    data_RND_cands_100[[i]] <- randomize_candidates(d_top10_top5())
    #data_RND_cands_100[[i]] <- randomize_candidates(d_top10_top5)
  }
  data_RND_cands <- data_RND_cands_100
})

data_RND_cands_party_100 <- reactive({
  data_RND_cands_party_100 <- lapply(data_RND_cands(), function(x) dplyr::filter(x, NUMERO_PARTIDO==input$Party))
  #data_RND_cands_party_100 <- lapply(data_RND_cands, function(x) dplyr::filter(x, NUMERO_PARTIDO==input$Party))
})

d_QL_wide_RND_100 <- reactive({
  d_QL_wide_RND_100 <- lapply(data_RND_cands_party_100(), function(x) QL_wide_func(x))
  #d_QL_wide_RND_100 <- lapply(data_RND_cands_party_100, function(x) QL_wide_func(x))
})

d_QL_wide_RND <- reactive({
  d_QL_wide_RND <- d_QL_wide_RND_100()[[5]]
  #d_QL_wide_RND <- d_QL_wide_RND_100[[5]]
})

party_cands_RND <- reactive({
  party_cands_RND <- names(d_QL_wide_RND()[which(grepl("^x",names(d_QL_wide_RND())))])
  #party_cands_RND <- names(d_QL_wide_RND[which(grepl("^x",names(d_QL_wide_RND)))])
})

mun_state_d_RND <- reactive({
  
  mun_state_d <- mun_state() %>% left_join(d_QL_wide_RND(), by="COD_MUN_IBGE") %>%
    mutate_at(party_cands_RND(),funs(ifelse(is.na(.),0,.))) %>%
    st_as_sf(crs = 4326)
  #mun_state_d <- mun_state %>% left_join(d_QL_wide_RND, by="COD_MUN_IBGE") %>%
  #  mutate_at(party_cands_RND,funs(ifelse(is.na(.),0,.))) %>%
  #  st_as_sf(crs = 4326)
  
  mun_state_d_RND <- as(mun_state_d,"Spatial")
  
})

cand_clusters_RND <- reactive({
  cand_clusters_RND <- LISA_func(mun_state_d_RND(),mun_state_nb(),party_cands_RND())
  #cand_clusters_RND <- LISA_func(mun_state_d_RND,mun_state_nb,party_cands_RND)
})

output$cluster_map_RND <- renderLeaflet({
  map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds()  
  colours <- c("red","blue","green","yellow","grey")
  
  
  party_cands_with_clusters_RND <- party_cands_RND()[party_cands_RND() %in% names(cand_clusters_RND())]
  #party_cands_with_clusters_RND <- party_cands_RND[party_cands_RND %in% names(cand_clusters_RND)]
  
  for (c in party_cands_with_clusters_RND){
    map <- map %>% addPolygons(data=cand_clusters_RND()[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters_RND)])
    #map <- map %>% addPolygons(data=cand_clusters_RND[[c]],fillOpacity=0.2,weight=2,color=colours[which(c==party_cands_with_clusters_RND)])
  }
  map
})

output$RND_party_seg_measures <- renderUI({
  spseg_out <- spseg_func(mun_state_d_RND(),party_cands_RND())
  #spseg_out <- spseg_func(mun_state_d_RND,party_cands_RND)
  str_seg_measures <- paste0("<br> For randomly-assigned candidates:","<br> Spatial Dissimilarity: ",round(spseg_out$d,3),"<br> Spatial Diversity: ",round(spseg_out$r,3), "<br> Spatial Information: ",round(spseg_out$h,3))
  HTML(str_seg_measures)
})

spseg_func_RND <- function(shapefile,d_QL_wide){
  cands <- names(d_QL_wide[which(grepl("^x",names(d_QL_wide)))])
  if (length(cands>1)){
    cands_QLs <- d_QL_wide %>% dplyr::select(cands)
    spseg_out <- as.list(spseg(shapefile,data=cands_QLs))
  } else {
    spseg_out <- NULL
  }
  return(spseg_out)
}

dissim_RND <- reactive({
  dissim_RND <- lapply(d_QL_wide_RND_100(),function(x) spseg_func_RND(mun_state_d_top_5(),x))
  #dissim_RND <- lapply(d_QL_wide_RND_100,function(x) spseg_func_RND(mun_state_d_top_5,x))
})

output$RND_party_seg_measures <- renderUI({
  str_seg_measures <- paste0("<br> For randomly-assigned candidates:","<br> Spatial Dissimilarity: ",round(dissim_RND()[[5]]$d,3),"<br> Spatial Diversity: ",round(dissim_RND()[[5]]$r,3), "<br> Spatial Information: ",round(dissim_RND()[[5]]$h,3))
  HTML(str_seg_measures)
})

output$dist_RND <- renderPlot({
  measures <- rapply(dissim_RND(),function(x) x[1])
  measures_dissim <- tibble(d=measures[which(names(measures)=="d")])
  
  ggplot() + stat_density(data=measures_dissim,aes(x=d),geom="line") + 
    theme_classic() +
    geom_vline(aes(xintercept=spseg_func(mun_state_d_top_5(),party_cands())$d),col="red") +
    xlab("Dissimilarity") +
    ylab("Density")
})

entropy <- function(data){
  
  d_SP_13_5 <- data %>% dplyr::group_by(COD_MUN_IBGE) %>% dplyr::mutate(vote_share_in_mun=QTDE_VOTOS/sum(QTDE_VOTOS)) %>% ungroup()
  
  #d_SP_13_5 %>% filter(COD_MUN_IBGE==3503950)
  
  d_SP_13_5 <- d_SP_13_5 %>% dplyr::mutate(plnp=ifelse(vote_share_in_mun==0,NA,vote_share_in_mun*log(vote_share_in_mun))) #Careful with treatment of zero vote share here
  
  d_SP_13_5_mun <- d_SP_13_5 %>% group_by(COD_MUN_IBGE) %>% dplyr::summarise(sum_plnp=-sum(plnp,na.rm=TRUE)) #Is the entropy index for each census tract
  
  #Plot of entropy index for top 5 PT candidates in SP
  mun_sf_entropy <- mun_state() %>% left_join(d_SP_13_5_mun,by="COD_MUN_IBGE") %>% st_as_sf(crs=4326)
  #mun_sf_entropy <- mun_state %>% left_join(d_SP_13_5_mun,by="COD_MUN_IBGE") %>% st_as_sf(crs=4326)
  
  #Statewide entropy: (state_H - avg_mun_H)/state_H
  avg_mun_H <- mean(d_SP_13_5_mun$sum_plnp)
  
  state_H <- d_SP_13_5 %>% 
    dplyr::group_by(NUMERO_CANDIDATO) %>% 
    dplyr::summarise(QTDE_VOTOS=sum(QTDE_VOTOS,na.rm=TRUE)) %>% 
    dplyr::mutate(vote_share_in_five=QTDE_VOTOS/sum(QTDE_VOTOS)) %>% 
    dplyr::mutate(plnp=ifelse(vote_share_in_five==0,NA,vote_share_in_five*log(vote_share_in_five))) %>% 
    dplyr::summarise(sum_plnp=-sum(plnp,na.rm=TRUE))
  
  statewide_entropy <- (state_H-avg_mun_H)/state_H
  return(list(mun_sf_entropy,statewide_entropy))
}

d_party_entropy <- reactive({
  d_party_entropy <- entropy(d_party_top_5_cands())
  #d_party_entropy <- entropy(d_party_all_cands)
})

output$entropy_map_real <- renderLeaflet({
  mapview(d_party_entropy()[[1]],zcol="sum_plnp")@map
  
  #entropy_scores <- d_party_entropy[[1]]["sum_plnp"]
  #st_geometry(entropy_scores) <- NULL
  #pal <- colorNumeric(
  #  palette = "Blues",
  #  domain = entropy_scores)
  #map <- leaflet() %>% addProviderTiles("CartoDB.Positron") %>% clearBounds() %>% addPolygons(data=d_party_entropy[[1]],fillOpacity=0.2,weight=2,fill=~pal(sum_plnp))
  #map
})

output$d_entropy_overall <- renderUI({
  entropy_summary <- paste0("<br> Overall entropy measure for this party: ",round(d_party_entropy()[[2]],3))
  HTML(entropy_summary)
})

}

# Run the application 
shinyApp(ui = ui, server = server)

