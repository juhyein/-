setwd("C:/Users/JU HYE IN/Desktop/프로젝트/shiny")
load("tutoring.RData")
load("entire.RData")

library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)

ui<-fluidPage(
  titlePanel("청소년의 도서와 사교육비"),
  
  sidebarLayout(
    sidebarPanel(
      h5("관심있는 도서분류 기준을 선택하세요"),
      br(),
      selectInput("cate",
                  label="관심있는 장르를 선택하세요",
                  choices = c("총류","철학","종교","사회과학","순수과학","응용과학","예술","언어","문학","역사"),
                  selected = "문학"),
      
      radioButtons("sex",
                   label = "성별을 선택하세요",
                   choices = c("남", "여"),
                   selected = "남"),
      img(src = "book.png", height = 150, width = 300),
      
      br(),
      h5("사교육비 파악을 위해 원하는 기준을 선택하세요"),
      selectInput("loc",
                  label = "지역규모를 선택하세요",
                  choices = c("대도시", "중소도시","읍면지역"),
                  selected = "대도시"),
      selectInput("yrs",
                  label = "원하는 연도를 선택하세요",
                  choices = c(2014, 2015,2016,2017),
                  selected = 2014)
    ),
    
    mainPanel(
      fluidRow(
        column(8,
          helpText("2017년을 기준으로 수집된 인기도서대출 목록을 기반으로 한 도서입니다.")
        ),
      ),
      
      
      fluidRow(
        column(12,
               dataTableOutput("df"))
      ),
      
      fluidRow(
        column(12,
               plotOutput("myplot"))
      )
    )
  )
)

server<-function(input, output){
  
  output$df <-renderDataTable({
    entire %>% 
      filter(isbn==input$cate) %>% 
      filter(sex==input$sex) %>% 
      transmute("인기도서"=title, "대출건수"=counts) %>% head(100)
    })
  
  output$myplot <- renderPlot({
    
    varx<-switch(input$loc,
                 "대도시" = 1,
                 "중소도시" = 2,
                 "읍면지역" = 3)

    
    tutoring %>% 
      filter((year == input$yrs)&(location==varx)) %>% 
      group_by(grade) %>% 
      summarise(med=median(fee)) %>% 
      ggplot(aes(x=grade ,y=med))+
      geom_col(aes(color=grade, fill=grade),alpha=0.4)+
      theme_wsj()+
      scale_fill_wsj(labels=c("상위", "중상위", "중위", "중하위", "하위"))+
      scale_color_wsj()+
      labs(fill="성적")+
      ggtitle("성적별 연간 사교육비 중간값")+
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5, size = 15), 
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 7),
            plot.subtitle =element_text(hjust = 0.5, size = 10))+
      guides(color=FALSE)
  })
  
}

shinyApp(ui=ui, server = server)







