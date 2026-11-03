# Function to classify text

suppressWarnings(transformers <- import("transformers"))
suppressWarnings(tensorflow <- import("tensorflow"))
suppressWarnings(library(DBI))
suppressWarnings(library(RPostgreSQL))
suppressWarnings(library(dplyr))
suppressWarnings(library(glue))
suppressWarnings(library(dbplyr))
#load model and tokenizer
model <- transformers$TFAutoModelForSequenceClassification$from_pretrained("fine_tuned_scibert_v2/")
tokenizer <- transformers$AutoTokenizer$from_pretrained("fine_tuned_scibert_v2/")
classify_text <- function(df,text_col) {
  text<-df[[text_col]]
  # Tokenize the input text
  tokens <- tokenizer(
    text,
    padding = "max_length",
    truncation = TRUE,
    max_length = as.integer(128),
    return_tensors = "tf"
  )
  # Make predictions
  predictions <- model$predict(list(
    input_ids = tokens$data$input_ids,
    attention_mask = tokens$data$attention_mask
  ))$logits
  
  # Convert logits to probabilities
  probabilities <- tensorflow$nn$softmax(predictions)$numpy()
  
  # Get the predicted label (0 or 1)
  # predicted_label <- which.max(probabilities) - 1
  predicted_labels <- apply(probabilities, 1, which.max) - 1  # Adjust indexing
  
  # return(list(
  #   text = text,
  #   probabilities = probabilities,
  #   predicted_label = predicted_label
  # ))
  # return(predicted_label)
  df$bert <- predicted_labels
  return(df)
} 

##create cluster link 
createPDSLink <- function(val,val2) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/pathogens/isolates/#%s" target="_blank" class="btn btn-primary btn-sm">%s</a>',val,val2)  
}
createBiosampleLink <- function(val, val2) {
  sprintf('<a href="https://www.ncbi.nlm.nih.gov/biosample/%s" target="_blank" class="btn btn-primary btn-sm">%s</a>',val, val2)
}

#Get biosample function
get_samn_info2<-function(x){
  # #docs<- entrez_fetch(db='biosample',id=x,rettype ='xml')
  temp<-GET(x)
  print(temp$status_code)
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
  
  node_count<-length(biosamplenodes)
  if(length(biosamplenodes)==0)
  {return (NULL)}
  print(node_count)
  # df4_Ids$submisson_date<-as.POSIXct(df4_Ids$submission_date, format=("%Y-%m-%d"))
  df4_Ids$Sample_name<-as.character(df4_Ids$Sample_name)
  
  
  
  df4_Ids<-df4_Ids%>%drop_na(attrs)
  
  df_all<-cbind(df4_Ids, df_attrs)%>%select(which(!duplicated(colnames(.))))
  #df_all<- subset(df_all, select = -c(Links, access, Ids, id, Attributes, Status, Models))
  return(df_all)
}

#add class for tutorial
add_class <- function(x, class) {
  x$attribs <- append(x$attribs, list(class = class))
  x
}

