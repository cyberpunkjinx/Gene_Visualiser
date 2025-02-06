library(ggplot2) #do i need all these? probably not
library(bslib)
library(shiny)
library(dplyr)
library(tidyr)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(rsconnect)
library(ggthemes)
library(ggiraph)
library(wikifacts)
library(cowsay)
df <- read_excel("nerd ass data - Copy.xlsx", 
                 sheet = "Sheet1") #import data frame
gathered_df_1 <- gather(df,"VNormalized","5dGestNormalized","10dGestNormalized","15dGestNormalized","0dLacNormalized","5dLacNormalized","10dLacNormalized","12hInvNormalized","24hInvNormalized","48hInvNormalized","72hInvNormalized","96hInvNormalized",
                      key = "timepoint",
                      value = "expression") #tidies data by gathering needeed columns
gathered_df_2 <- gather(df, "lac/12","lac/24","lac/48","lac/72", key = "fctimepoint", value = "fcvalue")

level_order <- c("VNormalized","5dGestNormalized","10dGestNormalized","15dGestNormalized",
                 "0dLacNormalized","5dLacNormalized","10dLacNormalized","12hInvNormalized",
                 "24hInvNormalized","48hInvNormalized","72hInvNormalized","96hInvNormalized") #used for the ggplot to order the x axis correctly


# Define UI
ui <- page_sidebar(
  title = "Gene Expression Visualiser",
  sidebar = sidebar(
    selectInput(
      "gene",
      label = "Select a gene!",
      choices = distinct(gathered_df_1,Common) #sidebar code
    ),
    helpText("If a graph has multiple lines or looks a bit strange, this means the selected gene has multiple entries in the microarray 
             dataset. I tried to fix it the best I could by grouping results by their GenBank accession number, but that didn't always work."),
    helpText(htmlOutput("keytext")) #displays the text explaining abbreviations
  ),
  
  navset_pill( #makes it tabbed
    nav_panel( # first tab  outputs lineplot
      title = textOutput("selected_gene"),
      girafeOutput("linePlot"),
    ),
    nav_panel( #second tab outputs gene information
      title = "Gene Information",
      card(
        textOutput("selected_gene_description"),
        verbatimTextOutput("wikiinfo"),
      "If nothing shows up or you get an error, you'll have to google it :(",
      )
    ),
    nav_panel(#third tab outputs fold change data
      title = "Fold change",
      card(
        height = 250,
      selectInput(
        "timepoint",
        label = "Select a timepoint!",
        choices = distinct(gathered_df_2,fctimepoint)),
      textOutput("selected_timepoint_text")
      ),
    ),
    footer = "Made by Jinx Foggon for his MSci project! :3c", #credits me :3c
  ),
)

# Define server logic
server <- function(input, output) {
  selected_gene_data <- reactive({
    gathered_df_1 %>% filter(Common == input$gene)
  })#filters the dataset reactively based on the input gene selected
  
  genename_cap <- reactive({
    str_to_upper(gsub("[;].*]","",input$gene))
  }) #makes the genename uppercase to mimic human gene nomenclature for wikipedia searching of genes
  #add the regex filtering here (see comments line 78 onwards)
  
  selected_tp_data <- reactive({
    gathered_df_2 %>% filter(fctimepoint == input$timepoint, Common == input$gene)
  }) #filter function for the fold change value
  
  output$selected_gene <- renderText(paste(input$gene,"Expression Levels")) #titles the first tab with the gene name
  
  output$selected_timepoint <- renderText(paste(selected_tp_data()$fcvalue))
  
  output$selected_timepoint_text <- renderText(paste("At the timepoint", input$timepoint, "the relative fold change in gene expression is", selected_tp_data()$fcvalue[1] ))
    
  output$selected_gene_description <- renderText(paste((selected_gene_data()$Description[1])))
    
  output$keytext <- renderUI({
    HTML(paste("Key:","V = Virgin","Gest = Gestating","Lac = Lactating","Inv = Involution", sep = "<br/>"))
  }) #had to use HTML to wrap the text properly
  
  ascii <- reactive({ 
    say(wiki_define(genename_cap()),by = "cat", type = "string")
  }) #the most pointless three hours of my life.
  #generates ascii art of a cat saying the gene informaition from wikipedia
  #potential fix later: wiki searching renders an error more times than not, 
  #need to exclude anything after ';' or ':' for genes with multiple names
  #add a regex function to filter the entry before its passed into wiki_define
  #so only the first gene is passed through
  #also figure out what the true/false error is :(
  
  output$wikiinfo <- renderText({
    paste(ascii(), collapse = "\n")
  }) #cowsay does NOT like rendering in places that arent the console so this fixes that
  
  output$linePlot <- renderGirafe({ #worlds messiest graph code
    gg_point <- ggplot(selected_gene_data(), aes(x = factor(timepoint, levels = level_order), y = expression, tooltip = expression, data_id = expression )) +
      geom_line(aes(group = Genbank), color = "#F5A9B8", size = 1.5) + #draws the stupid line
      geom_point_interactive(size = 2.5,shape = 16) + #draws the stupid point
      ylim(0,NA)+
      labs(x = "Timepoint", y = "Expression Levels") + 
      theme(panel.border = element_rect(fill = "transparent", # Needed to add the border, for some reason
                                        color = "#5BCEFA",#border colour
                                        linewidth = 4)) +
      theme(panel.background = element_rect(fill = "#FFFFFF"))+ #graph bg colour
      theme(panel.grid = element_line(color = "#d9d9d9", #grid line colour
                                      linewidth = 0.75,
                                      linetype = 1))+
      theme(panel.grid.minor = element_blank())+ #i don't know but its required to make it work
      theme(axis.text.x = element_text(angle=45, vjust=.5, hjust=.5))#rotates the stupid text
    
    girafe(
      ggobj = gg_point
    ) #turns the ggplot into a girafe object so its interactive
    
  })
}

# Run the stupid application 
shinyApp(ui = ui, server = server)

