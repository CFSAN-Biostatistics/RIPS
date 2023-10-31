suppressWarnings(library(rvest))
suppressWarnings(library(httr))
suppressWarnings(library(RCurl))
#suppressWarnings(library(tidyverse))
suppressWarnings(library(dplyr))
suppressWarnings(library(shiny))
suppressWarnings(library(data.table))
suppressWarnings(library(DT))
suppressWarnings(library(purrr))
suppressWarnings(library(xml2))
suppressWarnings(library(ggplot2))
suppressWarnings(library(lubridate))
suppressWarnings(library(plotly))
suppressWarnings(library(stringr))
suppressWarnings(library(tidyr))
suppressWarnings(library(readr))
suppressWarnings(library(shinydashboard))
suppressWarnings(library(rintrojs))


add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}


#Get biosample function
get_samn_info2<-function(x){
  # #docs<- entrez_fetch(db='biosample',id=x,rettype ='xml')
  temp<-GET(x)
  #print(temp$status_code)
  #If status code is not 200, try to get the xml again
  while (temp$status_code!=200) {
    print("Waiting 60s, retrying, current status code:")
    print(temp$status_code)
    Sys.sleep(1)
    temp<-GET(x)
  }
  
  docsxml<-rawToChar(temp$content)%>%read_xml
  biosamplenodes<- xml_find_all(docsxml, "//BioSample")
  #biosamplenodes<- read_xml(x)%>%xml_find_all( "//BioSample")
  
 
  df_attrs<- biosamplenodes %>% 
    map_df(~{
      set_names(
        xml_find_all(.x,".//Attribute[@attribute_name!='HHS_region']")%>%xml_text(),
        xml_find_all(.x,".//Attribute[@attribute_name!='HHS_region']")%>% xml_attr("attribute_name")
      ) %>% 
        as.list()})
  ## alternate but collisions as.list()%>% flatten_df() })
  
  #suppressMessages(  df_bioaccess<-biosamplenodes%>%map_dfr(~flatten(c(xml_attrs(.x),map(xml_children(.x), ~set_names(as.list(xml_text(.x)),xml_name(.x))))))%>%type_convert())
  
  suppressMessages(df4_Ids<- data.frame(SRA= xml_find_first(biosamplenodes, ".//Id[@db='SRA']")%>%xml_text(),
                                        BioSample= xml_find_first(biosamplenodes, ".//Id[@db='BioSample']")%>%xml_text(), 
                                        Sample_name = xml_find_first(biosamplenodes, ".//Id[@db_label='Sample name']")%>%xml_text(),
                                        Title= xml_find_first(biosamplenodes, ".//Title")%>%xml_text(),
                                        submission_date= as.character(xml_attr(biosamplenodes, "submission_date")),
                                        Package=xml_find_first(biosamplenodes,".//Package")%>%xml_text(),
                                        attrs=biosamplenodes%>%xml_child("Attributes")%>%xml_text(),
                                        stringsAsFactors = FALSE)%>%type_convert()
                  )
  # if(nrow(df_attrs)!=nrow(df_bioaccess))
  # {
  #   bad_bio<-df_bioaccess%>%filter(is.na(Attributes))%>%select(accession)
  #   df4_Ids<-df4_Ids%>%filter(!(BioSample %in% bad_bio$accession))
  #   df_bioaccess<-df_bioaccess%>%filter(!(accession %in% bad_bio$accession))
  # }
  #browser()
  
 
  df4_Ids<-df4_Ids%>%drop_na(attrs)
  
  df_all<-cbind(df4_Ids, df_attrs)
  #df_all<- subset(df_all, select = -c(Links, access, Ids, id, Attributes, Status, Models))
  return(df_all)
}
##create cluster link 
createPDSLink <- function(val,val2) {
    sprintf('<a href="https://www.ncbi.nlm.nih.gov/pathogens/isolates/#%s" target="_blank" class="btn btn-primary">%s</a>',val,val2)  
   }
createBiosampleLink <- function(val, val2) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/biosample/%s" target="_blank" class="btn btn-primary">%s</a>',val, val2)
}
today<- as.Date(Sys.Date())-1
#NCBI API KEY
api_key<- "YOUR_NCBI_API_KEY_HERE!!!"

# Define UI for application \
sidebar <- dashboardSidebar(
  collapsed = TRUE,
  sidebarMenu(
    menuItem("Main", tabName = "Main"),
    menuItem("Alternate", tabName = "nometa")
  )
)
body <- dashboardBody(
   introjsUI(),
  tags$head(tags$style(HTML(".shiny-output-error-validation{color: #ff0000;font-weight:bold;}"))),
  tags$head(tags$style(HTML('.content-wrapper {background-color: #fff;}'))),
  
  tabItems(
  # main
  tabItem("Main",
    fluidRow(
      column(9, 
             box(width = NULL,
               
                plotlyOutput("freqplot"),
                ),
             box(width = NULL,
                 
                 tabsetPanel( id= "tabset",selected = "Main View",
                             
                              tabPanel("Main View", 
                                       column(width=12, div(h3("Focals (Isolates of interest)"),#style = 'overflow-x: scroll',
                                                            DTOutput("split_focal")))%>%add_class("focal5"),
                                       column(width=12, div(h3("Neighbors (Isolates close to focals)"),#style = 'overflow-x: scroll',
                                                            DTOutput("split_neighbors")))%>%add_class("neighbor6")
                              ),
                              tabPanel("All data",           
                                       column(width=12, div(style = 'overflow-x: scroll',
                                                            DTOutput("table")))
                              )
                 )
               
             )
             
            ),#end plot
      column(3, 
             box(width = NULL, status = "primary",
                 selectInput(inputId = "taxon", label = "Choose a pathogen:",
                             choices = c("Listeria", "Salmonella", "Escherichia_coli_Shigella"))%>%add_class("taxon1"),
                 dateInput("date", label = "Date input:", 
                           value = today)%>%add_class("date2"),
                 actionButton(inputId = "start",
                              label = "Run/Get report for new date", width = '100%', icon = icon("repeat"),
                              style="color: #fff; background-color: #337ab7; border-color: #2e6da4; padding:4px; font-size:90%")
                )
             
              ),# end selection
      column(3,
             
             box(title="Tunable filter",width = NULL,status="danger", 
                 radioButtons(inputId= "filter", label = "Filter type",
                              choices = list("No Filter", "Preset Filters"),
                              selected = "No Filter"
                 ),
                 conditionalPanel(
                   condition = "input.filter=='Preset Filters'",
                   
                   numericInput("filt_cl_count", label = "Min Number of Clinicals:",
                                value = 3),
                   numericInput("filt_env_count", label = "Min Number of Env/other:",
                                value = 0)
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.filter=='Preset Filters'", 
                   numericInput("filt_days", label = "Max days from focal submission:", 
                                value = 150),
                   numericInput("filt_allele", label = "Max Allele Difference:",
                                value = 10),
                 )
                 
  
             )%>%add_class("filters7"), #end
             
             box(title=" Filter out:", width = NULL, status = "warning",
                 # radioButtons(inputId = "filt_cl", label = "Clinical only clusters:",
                 #              choiceNames = list("Yes","No"),
                 #              choiceValues = list("cl", "")),
  
                 radioButtons(inputId = "filt_FSIS", label = "Non-FDA clusters:",
                              choiceNames = list("Yes","No"),
                              choiceValues = list("FSIS", "")),
                 # radioButtons(inputId = "filt_UK", label = "UK human clusters:",
                 #              choiceNames = list("Yes","No"),
                 #              choiceValues = list("UK", ""))
  
             ), #end
             
       
  
            
       
              box(width = NULL,
                  textInput("click","PDS_cluster"),
              
              downloadButton("downloadReport", icon = icon("file-arrow-down"), label = "Download Report", style="color: #fff; background-color: #1a9641; border-color: #607D3B; padding:4px; font-size:90%;width:200px")
              ),
             actionButton("help", "Press for instructions"),
             actionButton("show", "show dialog")
         
       
      
      )
      
      
            )# end row
    ),
  tabItem("nometa",
          h2("no meta tab")
  )
  )

) # end dashboardbody
ui <- dashboardPage(title = "R.I.P.S",
  dashboardHeader(titleWidth=400, title= div(tags$img(src='cfsan_logo_mod.png',style="position:absolute; left:0px", height = '50', width ='200',
  ), "FDA----CFSAN    R.I.P.S. v 1.0.3")),
  #dashboardSidebar(disable= FALSE,tags$head(tags$style(HTML('.content-wrapper {background-color: #fff;}')))),
  sidebar,
  body,
  skin = "blue"
)

# Define server logic 
server <- function(input, output, session) {
  
  steps <- reactive(
    data.frame(
      element=c( "#help",".taxon1",".date2", "#start","#freqplot", ".focal5", ".neighbor6", ".filters7"),
      intro=c(
        
        "<h3>Instructions</h3>RIPS is a pathogen surveillance app<br> Click the Next and Previous buttons to navigate this tutorial.<br>Click to Skip button to escape this tutorial at any time.",
        "<h3>Step 1:</h3> Click the dropdown menu to select a pathogen of interest.<br> Listeria <br> Salmonella<br>E.coli.",
        "<h3>Step 2:</h3> Select a date.Information is usually a day old. There is usually no reports on Saturday/Sunday.<br>You will be notified if a report does not exist for the selected date. ",
        "<h3>Step 3:</h3> Click this button to load a new report for either a new date or new taxon.<br>Progress will be updated on the bottom left of the screen.",
        "<h3>Step 4:</h3> This is the list of clusters, select on of them.<p>Data within the cluster will load below</p>",
        "<h3>Step 5:</h3> These are the focal isolates, the new isolates that triggered the Rapid Report from NCBI",
        "<h3>Step 6:</h3> These are the neighbor isolates closely related to the focal isolates. These are ordered by the difference in age from the clinical.<p> These are also ordered by the most recent environmentmal signals followed by the clinicals.</p><br> Blue buttons are clickable",
        "<h3>Step 7:</h3> Select <b>Preset Filters</b> to view the filter options. <br><br>Preset filter value thresholds change based on taxon.<p> Change these values by either typing new values or using the arrows.</p><br>Default Values:<br><b>Listeria:</b> 3 Clinicals in 150 days, within 7 alleles.<br><b>Salmonella:</b> 7 Clinicals in 90 days, within 10 alleles.<br><b>E.coli:</b> 5 Clinicals in 90 days, within 10 alleles.   "
      ),
      position=c("bottom","bottom", "bottom", "right", "right", "top", "top", "left")
    )
  )
  observeEvent(input$help,
               introjs(session,
                       options = list(steps=steps(),
                                      "nextLabel"="Next",
                                      "prevLabel"="Previous",
                                      "skipLabel"="Skip"
                       ),
                       events = list("oncomplete"=I('alert("Done!")'))
               )
  )
   PDG_val<-reactiveVal()
   snp_clusters<-reactiveVal()
  #counts_df<-eventReactive(input$start, {
   observeEvent(input$show, {
     showModal(modalDialog(title = "test","Some msg",easyClose = TRUE, footer = NULL))
   })
   
   full_db<- eventReactive(input$start,{
    
    id <- showNotification("Running...", duration = NULL, closeButton = FALSE)
    on.exit(removeNotification(id), add = TRUE)
    
    taxon_url= paste0("https://ftp.ncbi.nlm.nih.gov//pathogen/Results/",input$taxon)
    # PDG_max<-read_html(taxon_url)%>%html_nodes("a")%>%html_text2()%>%str_subset("PDG")%>%as.data.frame()%>%separate(1,into = c("a","b"),sep="\\.")%>%mutate(b=parse_number(b))%>%summarise(max=paste0(max(a),".", max(b)))%>%as.character()
    # PDG_val(PDG_max)
    # snp_tree_url<-paste0(taxon_url,"/",PDG_max,"/SNP_trees/")
    # withProgress(message = 'Reading Latest PDS cluster values', value = NULL, {
    # 
    # treedf<-read_html(snp_tree_url)%>%html_nodes("a")%>%html_text2()%>%as.data.frame()%>%separate(1,into = c("clusters"),sep = "[.](?=[a-z])",fill= 'right')
    # snp_clusters(treedf)
    # })
    #mlst_url<- "https://ftp.ncbi.nlm.nih.gov//pathogen/Results/Salmonella/Rapid_reports/2022-09-20/"
    mlst_url = paste0(taxon_url,"/Rapid_reports/",input$date,"/" )
    #link for old reports
    #mlst_url = paste0("https://ftp.ncbi.nlm.nih.gov//pathogen/Results/", input$taxon,"/Rapid_reports.old/2022/",input$date,"/" )
    
    validate(need(try(!http_error(mlst_url)),"Report does not exist for specified date. Try another date.")) #http_status(GET(mlst_url))$message))
    
    ###PDT_max<-read_html(PDS)%>%html_nodes("a")%>%html_text2()%>%str_subset("PDG")%>%as.data.frame()%>%separate(1,into = c("a","b"),sep="\\.")%>%mutate(b=parse_number(b))%>%summarise(max=paste0(max(a),".", max(b)))
    ###read_html(tree_link)%>%html_nodes("a")%>%html_text2()%>%as.data.frame()%>%separate(1,into = c("clusters"),sep = "[.](?=[a-z])",fill= 'right')
    withProgress(message = 'Reading MLST data', value = NULL, {
      mlsts<-read_html(mlst_url)%>%html_nodes("a")%>%html_text()%>%str_subset("\\.tsv")
    })
    SRRdf<-data.frame(mlsts)
    #remove duplicates where neighbor_run_acc = null or neighbor_assembly_acc = null
    #SRRdf <- SRRdf%>%group_by(neighbor_biosample_acc, run_acc, X.biosample_acc, neighbor_cluster)%>% summarise(neighbor_run_acc = if(n()==1){neighbor_run_acc}else{first(na.omit(neighbor_run_acc))}, neighbor_assembly_acc = if(n()==1){neighbor_assembly_acc} else{first(na.omit(neighbor_assembly_acc))}, alleles_different= max(alleles_different), loci_in_common = max(loci_in_common))
    SRRdf$SRR_url=paste0(mlst_url,SRRdf$mlsts)
    ###Not working_All_SRRdf<-SRRdf$SRR_url%>%map_df(~read.csv(.,sep = "\t"))
    #get headers
    srrh<-colnames(read.csv(SRRdf$SRR_url[1], sep = "\t"))
    #getall SRR data, skip headers
    mlst_num<-length(SRRdf$mlsts)
    mlst_total<-paste(" Reading Rapid Reports, found",mlst_num,"isolates, loading data...")
    withProgress(message = mlst_total, value = NULL, {
    #All_SRRdf<-SRRdf$SRR_url%>%map_df(~read.csv(.,sep = "\t", header=FALSE))
    #fread option
    All_SRRdf<-SRRdf$SRR_url%>%map_df(~fread(.,sep = "\t", header=FALSE, verbose = FALSE))
    })
    #set the col names
    colnames(All_SRRdf)= srrh
    #rename first column biosample
    names(All_SRRdf)[1]<-"focal_biosample"
    names(All_SRRdf)[2]<-"focal_run_acc"
    #remove all old headers by removing all lines that begin with "#biosample_acc"
    All_SRRdf<- subset(All_SRRdf, All_SRRdf$focal_biosample!="#biosample_acc")
    # turn "NULL" into NA
    All_SRRdf[All_SRRdf =="NULL"] = NA
    #remove duplicates where neighbor_run_acc = null or neighbor_assembly_acc = null
    #All_SRRdf <- All_SRRdf%>%group_by(neighbor_biosample_acc, focal_run_acc, focal_biosample, neighbor_cluster)%>% summarise(neighbor_run_acc = if(n()==1){neighbor_run_acc}else{first(na.omit(neighbor_run_acc))}, neighbor_assembly_acc = if(n()==1){neighbor_assembly_acc} else{first(na.omit(neighbor_assembly_acc))}, alleles_different= max(alleles_different), loci_in_common = max(loci_in_common))
    All_SRRdf <- All_SRRdf%>%group_by(focal_run_acc, focal_biosample, neighbor_biosample_acc, neighbor_isolate, neighbor_cluster)%>% summarise(neighbor_run_acc = if(n()==1){neighbor_run_acc}else{first(na.omit(neighbor_run_acc))}, neighbor_assembly_acc = if(n()==1){neighbor_assembly_acc} else{first(na.omit(neighbor_assembly_acc))}, alleles_different= max(alleles_different), loci_in_common = max(loci_in_common))
    #Filter on alleles_different<=20
    All_SRRdf<-All_SRRdf%>%filter(as.numeric(alleles_different)<=25)
    if(nrow(All_SRRdf)==0){validate("Error: All focals have been filtered out, alleles_different >20")}
    
    #fill in empty neighbor_cluster values if it exists elsewhere 
    All_SRRdf<-All_SRRdf%>%group_by(focal_run_acc)%>%fill(neighbor_cluster, .direction = "downup")%>%ungroup()%>%group_by(neighbor_biosample_acc)%>%fill(neighbor_cluster, .direction = "downup")%>%ungroup()
    #return(All_SRRdf)
    #create flag for overlaps, in neighbor_biosample or focal in neighbor_biosample
    All_SRRdf<-All_SRRdf%>%group_by(neighbor_cluster)%>%mutate(flag= ifelse( focal_biosample %in% neighbor_biosample_acc | duplicated(neighbor_biosample_acc,fromLast = TRUE)|duplicated(neighbor_biosample_acc),1,0))
    All_SRRdf<-All_SRRdf%>%group_by(neighbor_cluster)%>%add_tally(n_distinct(focal_biosample), name= "focal_count")%>%mutate(sub_group=case_when(focal_count>1 & flag == 1 ~ "dup_flag", focal_count>1~ focal_biosample, focal_count==1 ~ neighbor_cluster))
    All_SRRdf<-All_SRRdf%>%group_by(neighbor_cluster, sub_group)%>% summarise()%>%ungroup()%>%group_by(neighbor_cluster)%>% mutate(clust_id=paste0(neighbor_cluster,"_", seq_len(n())))%>%inner_join(All_SRRdf, by=c ('neighbor_cluster','sub_group'))
  #}) ##merged counts_df and full_db, uncomment to expose All_SRRdf
    #validate(need((length(All_SRRdf$focal_biosample)>0)), "MLST data filtered out, greater than 20 alleles.")
    ##testing

  
   # All_SRRdf<-counts_df()
    if (length(All_SRRdf$focal_biosample)==0)
       {return(NULL)}
    print("In full_db")
    print(dim(All_SRRdf))
    #make biosample urls
    #All_SRRdf$neighbor_biosample_url<-paste0("https://www.ncbi.nlm.nih.gov/biosample/?term=",All_SRRdf$neighbor_biosample_acc)
    #get all biosample info
    list<- All_SRRdf$neighbor_biosample_acc%>%unique()
    
    focal_list<- All_SRRdf$focal_biosample%>%unique()
    list<- append(list, focal_list)
    print("List length and chunks")
    print(length(list))
    chunk_num<- round(length(list)/200) + 1
    print(chunk_num)
    if (length(list)>200){
      chunk_num<- round(length(list)/200) + 1
      
      split_list<-split(list, cut(seq_along(list),chunk_num,labels = FALSE))
      listdf<-data.frame("chunk"= sapply(split_list, paste0,collapse = ","))
    } else{
      split_list<-list
      listdf<-data.frame("chunk"= paste0(split_list, collapse = ","))
    }
    
    #listdf<-data.frame("chunk"= sapply(split_list, paste0,collapse = ","))
    listdf$url<-paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=biosample&id=",listdf$chunk, "&api_key=",api_key)
    #browser()
    print("Before pages")
    p_num<-length(list)
    p_msg<-paste("Reading Data for",p_num,"BioSamples")
     withProgress(message = p_msg, value = NULL, {
      pages<-listdf$url%>%map(get_samn_info2)
    })
    # pages<-All_SRRdf$neighbor_biosample_url%>%map(get_samn_info)
     
     print("after pages")
     pages<-bind_rows(pages)
    pages<-pages%>%select(BioSample,strain, geo_loc_name,isolation_source, collection_date, collected_by,  everything())
    #tidy date for lubridate into new column
    pages$collection_date2<-parse_date_time(pages$collection_date,c('ymd', 'ym' ))
    #if collection date not available use sumission date
    pages<-pages%>%mutate(collection_date2= coalesce(collection_date2, submission_date))
    #made a new focals df and remove it from pages
    focals_biosample<- pages[pages$BioSample %in% focal_list, ]%>%unique()
    #pages<-pages%>%filter(!(BioSample %in% focal_list))
    focals_biosample_msg<-paste("rows in focal biosample data", nrow(focals_biosample) )
    print(focals_biosample_msg)
    # if(nrow(focals_biosample)==0){validate("New Submisson biosample data not in NCBI, try again later")}
    #focals_biosample<-focals_biosample[0,]
    if(nrow(focals_biosample)==0){showModal(modalDialog(title = "Warning:","Focal biosample data not in NCBI, focal dates are estimated",easyClose = TRUE, footer = NULL))
      f_list<-as.data.frame(focal_list)%>%rename("BioSample"=focal_list)
      focals_biosample<-bind_rows(f_list, focals_biosample)%>%mutate(collection_date2=today, Package="UNK")
      focals_biosample$collection_date2<-parse_date_time(focals_biosample$collection_date2, c('ymd', 'ym' ))
      }
    
    #merge data from SRRdf and biosample information
    names(pages)[1]<-"neighbor_biosample_acc"
    fulldf<- left_join(All_SRRdf,pages, by="neighbor_biosample_acc")%>%unique()
    #Correct Package names to catch clinicals labeled as environmental
    fulldf<- fulldf%>%mutate(Package=case_when(grepl("human", isolation_source) & grepl("env", Package) ~ "Pathogen.cl.1.0", grepl("cl",Package) ~ "Pathogen.cl.1.0", TRUE ~ "Pathogen.env.1.0" ))
    #fulldf<-focals_biosample%>%select(BioSample, collection_date2, Package)%>%rename(focal_date=collection_date2, focal_biosample=BioSample, focal_type=Package)%>%left_join(fulldf, by = "focal_biosample")%>%relocate(focal_date, .after = last_col())
    
    if(nrow(focals_biosample)==0){
      
    }
    
    fulldf<-focals_biosample%>%select(BioSample, collection_date2, Package, geo_loc_name, isolation_source, strain)%>%rename(focal_date=collection_date2, focal_biosample=BioSample, focal_type=Package, focal_loc=geo_loc_name, focal_source=isolation_source, focal_strain=strain)%>%
      left_join(fulldf, by = "focal_biosample")%>%relocate(focal_date, .after = last_col())
    #validate focal biosamples data in NCBI
    
    fulldf$days<- time_length(fulldf$focal_date-fulldf$collection_date2, unit = "days")%>%round()
    
    fulldf$focal_age<- time_length(as.Date(input$date)-as.Date(fulldf$focal_date), unit = "days")%>%round()
    
    
    #create counts
    fulldf<-fulldf%>%group_by(clust_id )%>%unique()%>%add_count(neighbor_cluster, name = "c_count")%>%add_tally(n_distinct(neighbor_biosample_acc[grepl("cl", Package)]), name = "cl_count")%>%add_tally(n_distinct(neighbor_biosample_acc[grepl("env", Package)]), name = "env_count")%>%ungroup()%>%arrange(desc(c_count))%>%group_by(neighbor_cluster, sub_group)
    
    #add focals to counts to cl_count and env_count
    fulldf<-fulldf%>%mutate(cl_count=n_distinct(focal_biosample[grepl("cl", focal_type)])+cl_count, env_count=n_distinct(focal_biosample[grepl("env", focal_type)])+ env_count)
    fulldf<-fulldf%>%mutate(c_count=env_count+cl_count)
    
    fulldf<-fulldf%>%ungroup()%>%arrange(desc(c_count))%>%group_by(neighbor_cluster, sub_group)
    
    fulldf<-fulldf%>%select(neighbor_cluster,neighbor_isolate, focal_biosample,focal_run_acc,neighbor_biosample_acc,  alleles_different, days, focal_age,collection_date2,focal_date, collection_date, Package, cl_count, env_count, c_count, everything())
    
    
    #for testing
    #FSIS flag for filtering out FSIS only signals
    fulldf<-fulldf%>%mutate(fsisflag=ifelse( env_count >=1 & (n_distinct(strain[grepl("FSIS",strain)& grepl("env", Package)])+n_distinct(focal_strain[grepl("FSIS",focal_strain)& grepl("env", focal_type)]))== env_count,1,0))%>%
      mutate(ukflag=ifelse( env_count >=1 & (n_distinct(strain[grepl("human",isolation_source)& grepl("env", Package)])+n_distinct(focal_strain[grepl("human",focal_source)& grepl("env", focal_type)]))== env_count,1,0))

    
    return(fulldf)
    
  })
# update UI for defaults based on taxon
  observeEvent(input$start,{
    if(input$taxon=="Listeria"){
      updateNumericInput(inputId = "filt_allele", value = 7)
      updateNumericInput(inputId = "filt_days", value = 150)
      updateNumericInput(inputId ="filt_cl_count", value = 3)
      updateNumericInput(inputId ="filt_env_count", value = 1)
    }
  })
  observeEvent(input$start,{
    if(input$taxon=="Salmonella"){
      updateNumericInput(inputId = "filt_allele", value = 10)
      updateNumericInput(inputId = "filt_days", value = 90 )
      updateNumericInput(inputId ="filt_cl_count", value = 7)
      updateNumericInput(inputId ="filt_env_count", value = 1)
      
    }
  })
  observeEvent(input$start,{
    if(input$taxon=="Escherichia_coli_Shigella"){
      updateNumericInput(inputId = "filt_allele", value = 10)
      updateNumericInput(inputId = "filt_days", value = 90 )
      updateNumericInput(inputId ="filt_cl_count", value = 5)
      updateNumericInput(inputId ="filt_env_count", value = 1)
      
    }
  })
  ######################################
  filt_db<-reactive({
    fulldf<-full_db()
    if (length(fulldf)==0)
    {return(NULL)}
    
    fulldf<-fulldf%>%group_by(neighbor_cluster, sub_group)%>% 
      filter(any(!grepl("NA", clust_id)))%>%
      
      filter(if(input$filt_FSIS=="FSIS") {!any(fsisflag=="1")} else {any(grepl("", strain))})
      
    
    if (input$filter=="Preset Filters"){
      
     filt_db<-fulldf%>%group_by(neighbor_cluster, sub_group)%>%
       
       filter(any((abs(days)<input$filt_days), 
              (as.numeric(alleles_different)<=input$filt_allele)))
     ####---minimum clinicals filter---###

     #prestep for min value counts
     filt_db<-filt_db%>%add_tally(n_distinct(neighbor_biosample_acc[days<=input$filt_days & Package=='Pathogen.cl.1.0' ])+n_distinct(focal_biosample[focal_age<=input$filt_days & focal_type== 'Pathogen.cl.1.0']), name = "cl_min")%>%
                        add_tally(n_distinct(neighbor_biosample_acc[ Package=='Pathogen.env.1.0' ])+n_distinct(focal_biosample[ focal_type== 'Pathogen.env.1.0']), name = "env_min")
     filt_db<-filt_db%>%filter(any(cl_min >=input$filt_cl_count), any(env_min>=input$filt_env_count))
     
    #print (input$filt_cl)
     } else {
      filt_db<-fulldf
    }
    return(filt_db)
  })
  #######################
  

  
 # pivot data longer for ploting
  df_plot<-reactive({
    df <- filt_db()
    if (length(df)==0)
    {return(NULL)}
    #pivot longer
    df<-df%>%group_by(clust_id)%>%
      summarise(env_count, cl_count, c_count)%>%unique()%>%ungroup()%>%
      
      pivot_longer(cols=c('env_count', 'cl_count'), names_to = 'Package', values_to = "Count")%>%
      mutate(Package=recode(Package,cl_count="Clinical", env_count="Environmental/<br>food/other"))%>%
      mutate(mytext = paste0("<b>Cluster:</b>", clust_id,"<br>", "<b>Isolate type:</b>", Package,"<br>" ,"<b>Count:</b> ", Count))%>%
    
    return(df)
  })
  
  #var for plotly click data
  click_val<-reactiveVal()
  
  focal_report<-reactiveVal()
  neighbors_report<-reactiveVal()
  
  output$freqplot<- renderPlotly({
    #print("rendering counts")
    print(dim(df))
    if(is_null(df_plot)){return(NULL)}else{
      print("rendering counts")
      
      cols<-c("Clinical"="#ef8a62", "Environmental/<br>food/other"="#67a9cf")
      
      col_names<-(c("cl_count"="Clinical", "env_count"="Env/other"))
      d<-highlight_key(df_plot(), ~clust_id)
      p<- ggplot(d, aes(x=reorder(clust_id, c_count), y=Count, fill= factor(Package), customdata=clust_id, text= mytext ))+ geom_bar(stat = 'identity')+theme_classic(base_size = 15)+coord_flip()+labs(y= "Isolate Count", x ="")+scale_fill_manual(name="", values = cols,  labels= col_names)
      ggplotly(p, tooltip = 'text' , dynamicTicks = TRUE)%>%highlight(on = 'plotly_click', off = 'plotly_doubleclick', opacityDim = .6)%>%layout(barmode="stack")
      
    }    
  })
  #clicked PDS
  observeEvent(event_data("plotly_click"), {
    d<-event_data("plotly_click")$customdata
    pds_d<- unlist(strsplit(d,"_"))[1]
    updateTextInput(session, "click", value = pds_d )
    #updateTextInput(session, "click", value = event_data("plotly_click")$customdata)
    click_val(d)
  })
  
  #output table in "All Data Tab"
  output$table <- renderDataTable({
    print("in renderDT")
    fulldf<-filt_db()
    d<-event_data("plotly_click")
    if(is_null(fulldf)){return(NULL)}
    #return(fulldf)
    #fulldf<-fulldf%>%filter(clust_id %in% d$customdata)
    
    fulldf$neighbor_cluster<-createPDSLink(fulldf$neighbor_cluster,fulldf$neighbor_cluster)
    datatable(fulldf, options = list( pageLength= 50, scrollX = TRUE),escape = FALSE,rownames = FALSE)%>%formatStyle(columns = "neighbor_cluster", backgroundColor = styleInterval(0, c( 'white','gray')))
    
  },options = list(pageLength = 50), escape = FALSE, rownames = FALSE)
  
  #output table for FOCALS ###
  
  output$split_focal <- renderDataTable({
    print("in renderDT")
    fulldf<-filt_db()
    if(is_null(fulldf)){return(NULL)}
    fulldf<-fulldf%>%filter(clust_id %in% click_val() )
    pdts<-paste0(fulldf$neighbor_isolate,".1", collapse = "; ")
    fulldf<-fulldf%>%mutate(focal_pid= ifelse(focal_type=="Pathogen.cl.1.0",1,0))%>%
      select(contains('focal'),-focal_run_acc)%>%select(neighbor_cluster, focal_biosample, focal_type, focal_age, focal_source, focal_loc, focal_strain, focal_date, everything(),focal_pid)%>%unique()%>%rename(Age_in_days=focal_age)
    focal_report(fulldf)
    
    fulldf$neighbor_cluster<-createPDSLink(pdts,fulldf$neighbor_cluster)
    datatable(fulldf, options = list( pageLength= 10, scrollX = TRUE, dom ='tip'), escape = FALSE,
              rownames = FALSE)%>%formatStyle( "focal_type","focal_pid", backgroundColor = styleEqual(c(1,0), c( '#ef8a62','#67a9cf')))#formatStyle(columns = "neighbor_cluster", backgroundColor = styleInterval(0, c( 'white','gray')))
    
  })
  
  #output table for NEIGHBORS ##
  output$split_neighbors <- renderDataTable({
    print("in renderDT")
    fulldf<-filt_db()
    if(is_null(fulldf)){return(NULL)}
    
    fulldf<-fulldf%>%filter(clust_id %in% click_val() )%>%
      mutate(pid= ifelse(Package=="Pathogen.cl.1.0",1,0))%>%
      select( neighbor_biosample_acc, days, Package, isolation_source, geo_loc_name, collection_date2, strain,alleles_different, strain, everything(),cl_count,env_count,c_count,sub_group, -clust_id, -neighbor_cluster,-contains('focal') )%>%
      unique()%>%arrange(days)%>%arrange(match(Package, "Pathogen.env.1.0"))%>%select(-neighbor_cluster,!contains('focal'))%>%rename(Age_in_days=days)
    neighbors_report(fulldf)
    fulldf$neighbor_biosample_acc<-createBiosampleLink(fulldf$neighbor_biosample_acc,fulldf$neighbor_biosample_acc)
    fulldf$neighbor_isolate<-createPDSLink(fulldf$neighbor_isolate,fulldf$neighbor_isolate)
    fulldf<-fulldf[!duplicated(fulldf$neighbor_biosample_acc),]
    datatable(fulldf, options = list( pageLength= 10, scrollX = TRUE, dom ='tip'),escape = FALSE,
              rownames = FALSE)%>%formatStyle( "Package","pid", backgroundColor = styleEqual(c(1,0), c( '#ef8a62','#67a9cf')))
    
  })
  output$downloadReport<-downloadHandler(
    filename = function(){
      paste(click_val(),".csv", sep = "")
    },
    content = function(file){
      write.csv(rbind(focal_report(),neighbors_report()), file,row.names = FALSE)
      #write.csv(neighbors_report(), file, row.names = FALSE, append = TRUE)
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)
