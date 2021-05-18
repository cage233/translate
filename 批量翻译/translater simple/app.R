library(shiny)
library(openssl)
library(httr)
library(RCurl)

json_to_csv<-function(json){
  email<-"495165378@qq.com"
  url="https://json-csv.com/api/getcsv"
  params<-list(
    'email'= email,
    'json'= json
  )
  html<-POST(url,body = params, encode = "form")
  mycondition<-content(html)
  mycondition
}

baidutraslate<-function(q,a,b){
  #GET SIGN
  q<-enc2utf8(q)
  id<-"20190130000260341"
  key<-"ZK5yT4K7GJCp7QPhOwir"
  salt<-"1435660288"
  str<-paste(id,q,salt,key,sep="")
  sign<-md5(str)
  #GET URL
  url<-paste("http://api.fanyi.baidu.com/api/trans/vip/translate?q=",q,"&from=",a,"&to=",b,"&appid=",id,"&salt=1435660288&sign=",sign,sep="")
  html<-getURL(url,.encoding ="utf-8")
  #GET RESULT
  out0<-json_to_csv(html)[,c(3,4)]
}

baidutraslate_pro<-function(input,a,b){
  out<-NULL
  for (i in 1:nrow(input)) {
    q<-as.character(input[i,1])
    a<-a
    b<-b
    out0<-baidutraslate(q,a,b)
    Sys.sleep(0.5)
    out<-rbind(out,out0)
  }
  out
}

ui<-fluidPage(
  #头部UI
  titlePanel("Translate"),
  
  
  sidebarPanel(fileInput("file", label = h3("1.Input your File to be translated")),
                tags$hr(),
                div(h3("2.Select language")),
                selectInput("a", label = h3("Input"), 
                            choices = list("Auto" = "auto", "English" = "en", "Chinese" = "zh"), 
                            selected = 1),
                selectInput("b", label = h3("Output"), 
                            choices = list("Auto" = "auto", "English" = "en", "Chinese" = "zh"), 
                            selected = 1),
                tags$hr(),
                actionButton("action", label = "Translate"),
                downloadButton("downloadData", "Download")
    
  ),

  mainPanel(
    
    tableOutput("table")
    
  )
)

server <- function(input, output, session) {
  rawdata<- reactive({
    read.csv(input$file$datapath, sep="")
  })
  
  out<-  reactive({
    input$action
    baidutraslate_pro(rawdata(),input$a,input$b)
  })
  
  
  output$table <- renderTable({
    req(input$action)
    out()
  })
  
output$downloadData <- downloadHandler(
    filename = function() {
      paste(Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(out(), file, row.names = FALSE)
    }
  )
}

shiny::shinyApp(ui, server)
