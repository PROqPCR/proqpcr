if(!require("shinydashboard")){
  install.packages("shinydashboard",dependencies = TRUE);
  library("shinydashboard");
}
if(!require("DT")){
  install.packages("DT",dependencies = TRUE);
  library("DT");
}
if(!require("RColorBrewer")){
  install.packages("RColorBrewer",dependencies = TRUE);
  library("RColorBrewer");
}
if(!require("plotly")){
  install.packages("plotly",dependencies = TRUE);
  library("plotly");
}

colors<-brewer.pal.info
names.col = row.names(colors)
color2 = colors (distinct = TRUE)
color2 = color2[order(color2)]
choice1 = "NULL"
choice2 = "NULL"
choice3 = "NULL"
choice4 = NULL #ucondi


header <- dashboardHeader(title="PROqPCR",disable=TRUE)

sidebar <- dashboardSidebar(disable=TRUE)


#1f8abf, #0077b3, #80d0d0
body <- dashboardBody(
  tags$style(type="text/css", 
             #largeur de la navbar
             ".container {
             width: 1170px;
             }",
             #position de "application qPCR
             ".navbar-brand {
             padding: 16.5px 18px 20.5px;
             }",
             "body {padding-top: 50px;
             font-family: Open Sans,Helvetica Neue,Helvetica,Arial,sans-serif;
             font-size: 16px;
             line-height: 1.42857;
             color: #666;}",
             "h1 {
             text-align: center;}",
             "h5 {
             text-align: center;}",
             "div.displayed{
             display: block;
             margin-left: auto;
             margin-right: auto;
             }",
             #Helvetica Neue,Helvetica,Arial,sans-serif
             "div.head {
             font-family: Arial,sans-serif,calibri;
             font-size:16px;
             text-align: left;
             max-width: 900px;
             margin: 0px auto;}",
             "div.aid {
             color: black;
             font-family: Arial,sans-serif,calibri;
             font-size:16px;}",
             "p {
             font-size:16px;}",
             #couleur de la navbar
             ".navbar {
             background-color: #80D0D0;
             border-color: #1a75a2;}",
             ".navbar .navbar-brand{
             color: #ffffff;}",
             ".navbar-brand {
             font-size: 19px;
             }",
             #couleur de l'arrrière plan body
             ".content-wrapper {
             background-color: #ffffff;
             }",
             #couleur du texte de la navbar mais ne marche pas !!
             ".navbar .navbar-nav > .active > a,
             .navbar .navbar-nav > .active > a:focus,
             .navbar .navbar-nav > .active > a:hover {
             background-color: rgb(0, 119, 179);
             color: black;
             }",
             ".navbar .navbar-nav > li > a {
             color: #fff;
             }",
             #couleur de "application qPCR"
             ".navbar .navbar-brand:hover,
             .navbar .navbar-brand:focus {
             color: #ffffff;
             }"),
  
  navbarPage(title="PROqPCR", position = "fixed-top",
             tabPanel("Introduction", icon=icon("book"),
                      # Application introduction with pics
                      fluidRow(
                        
                        h1("PROqPCR : PROcessing of qRT-PCR data", style = "color:black"),
                        h5("Developed by Mathilde Sautreuil", style = "color:grey"),
                        h5("contact : mathilde.sautreuil@gmail.com", style = "color:grey"),
                        hr(),
                        div(class = "head", p("This application enables the processing of qRT-PCR data. From Ct and Efficiency files 
                                              for each gene, a serie of steps of analysis is performed to getting several plots.
                                              The different successive steps of analysis are the : "),
                            tags$ul(
                              tags$li(" average on  the technical replicates"),
                              withMathJax(tags$li(" calculation the normalized relative quantity for one gene given for each biological replicate (for more information go to Help)")),
                              tags$li(" average on the biological replicates"),
                              tags$li(" calculation the log-ratio for all the compared conditions.")
                            ),
                            p("For each gene, we get therefore in the end a value of normalized and averaged Ct 
                              on the technical and biological replicates per condition."),
                            p("The tables of results are downloadable at each step. Differents plots are available : "),
                            tags$ul(
                              tags$li("Barplot of conditions for a given gene"),
                              tags$li("Barplot of genes for a given condition"),
                              tags$li("Comparison of barplots for choose genes in all the conditions.")
                            ),
                            p("We can note the Ct files are the output files got when the PCR instrument 
                              is realizing the calculation of Cycle threshold. Cycle threshold (Ct) is defined as 
                              the fractional PCR cycle number at which the reporter fluorescence is greater than 
                              the threshold (3-5 times of the standard deviation of the signal noise above background). 
                              We can note the Efficiency files are the output files got when the PCR instrument is running 
                              serial dilutions with 5-log dilutions."),
                            p("Moreover, the tab \"RNA-Seq\" allows users to compare the results of qRT-PCR with that of RNA-Seq. In this goal, 
                              differents plots are available : "),
                            tags$ul(
                              tags$li("Correlation plot between the log-ratio of qRT-PCR and that of RNA-Seq"),
                              tags$li("Barplot of conditions for a given gene between the two experiments.")
                            ),
                            h4("Test files : ", a("These files enable to test the application", href="https://drive.google.com/open?id=0ByHqXW2f91u8YWxEVzB5RnFLZlk")),
                            style = "color:black"),
                        
                        h2("Tutorial Step-by-step: ",
                           br(), br(), tags$video(src="PROqPCR_VF.mp4", type="video/mp4",
                                            width ="600px", height = "350px", controls="controls"),
                           style="text-align: center;"),
                        hr()
                        
                            ),
                      fluidRow(
                        p("PROqPCR: "),
                        div(class = "displayed",
                            column(12,
                                   img(src = "graph/graph11.png", style='width:100%; max-width: 780px;'),
                                   img(src = "graph/graph12.png", style='width:100%; max-width: 780px;'), 
                                   img(src = "graph/graph13.png", style='width:100%; max-width: 780px;')), 
                            column(12,
                                   img(src = "graph/graph1.png", style='width:100%; max-width: 780px;'), 
                                   img(src = "graph/graph2.png", style='width:100%; max-width: 780px;')), 
                            style="text-align: center;")
                      ),
                      
                      hr()
                      
                      
             ),
             tabPanel("Parameters", icon=icon("file-excel-o"),
                      fluidRow(
                        box(  title="Technical help", color="teal", width = 12, solidHeader = FALSE,
                              div(class="aid",
                                  p("To start, the files in xlsx format with the predifined names of 
                                    columns must be selected (To know if your files are corrects, go 
                                    to the tab Help.)"),
                                  p("To use the application, you must choose the parameters below : "),
                                  tags$ul(
                                    # tags$li("Sélectionner le nom du dossier contenant les fichiers à analyser"),
                                    tags$li("Selectize the number of conditions"),
                                    tags$li("Selectize the number of technical replicates"),
                                    tags$li("Selectize the number of biological replicates"),
                                    tags$li("Selectize the Ct files"),
                                    tags$li("Selectize the Efficiency files"),
                                    tags$li("Selectize the chosen reference genes"),
                                    tags$li("Selectize the conditions for the calculation of log-ratio")
                                  )
                                  
                                  )
                        )
             ),
             fluidRow(
               infoBoxOutput("boxCond"),
               infoBoxOutput("boxRepT"),    
               infoBoxOutput("boxRepB")
             ),
             
             
             
             box(  title="Inputs", status="primary", solidHeader = TRUE,
                   #textInput('in3', 'Entrez le nom des gènes de référence', value = "gènes de référence"),
                   selectInput('in4', 'Number of conditions', choices = 1:10, selectize = TRUE),
                   selectInput('in5', 'Number of technical replicates', choices = 1:10, selectize = TRUE),
                   selectInput('in6', 'Number of biological replicates', choices = 1:10, selectize = TRUE),
                   fileInput('in1', 'Ct files', choice1 , multiple=TRUE),
                   #selectInput('in1', 'Choix des fichiers pour les Ct', choice1 , multiple=TRUE, selectize=TRUE),
                   fileInput('in2', 'Efficiency files', choice2 , multiple=TRUE),
                   #selectInput('in2', 'Choix des fichiers pour les Efficacités', choice2 , multiple=TRUE, selectize=TRUE),
                   selectInput('in3', 'Reference genes', choice3 , multiple=TRUE, selectize=TRUE),
                   #                                selectInput('in7', 'Comparaison de toutes les conditions', choices = c(TRUE,FALSE), selectize = TRUE),
                   #                                selectInput('in8', 'Nom de la condition témoin', choice4, selectize = TRUE)
                   selectInput('lR', 'Compared conditions : ', choice3, multiple = TRUE, selectize = TRUE) 
             ),
             
             box(title="Inputs", status="primary", 
                 
                 #                              helpText('Dossier séléctionné :'),
                 #                              verbatimTextOutput('out0'),
                 helpText('Number of conditions :'),
                 verbatimTextOutput('out4'),
                 
                 helpText('Number of technical replicates :'),
                 verbatimTextOutput('out5'),
                 
                 helpText('Number of biological replicates :'),
                 verbatimTextOutput('out6'),
                 
                 
                 helpText('Informations on the selected files :'),
                 verbatimTextOutput('out1'),
                 
                 helpText('Informations on the selected files :'),
                 verbatimTextOutput('out2'),
                 
                 helpText('Selected genes :'),
                 verbatimTextOutput('out3'),
                 
                 helpText('Compared conditions :'),
                 verbatimTextOutput('outlR')
                 ,
                 
                 helpText('Warning this genes are present more one time :') ,
                 verbatimTextOutput('out8')
             )
             
  ),
  navbarMenu("Analysis", icon=icon("cogs"),
             
             tabPanel("Technical help",
                      box(title="Technical help", color="teal", width = 12, solidHeader = FALSE,
                          div(class="aid",
                              p("Access at each step of the analysis and download the intermediary tables.")
                              
                          )
                      )
             ),
             tabPanel("Ct data",
                      helpText("This table contains all the Ct for each gene (in row) following 
                               the conditions, the technical and biological replicates (in column)."),
                      dataTableOutput(outputId="table1"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataCt', 'Download', class = "download"),
                      tags$head(tags$style(".download{background-color:#80d0d0;} .download{color: white;}"))
                      ),
             tabPanel("Efficiency data",
                      helpText("This table contains all the efficiencies for each gene (in row) following 
                               the conditions, the technical and biological replicates (in column)."),
                      dataTableOutput(outputId="table2"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataE', 'Download', class = "download")
                      ),
             tabPanel("Mean of technical replicates",
                      helpText("This table contains the mean on the technical replicates of Ct for each gene (in row)."),
                      dataTableOutput(outputId="table3"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataRepTech', 'Download')
             ),
             tabPanel("Reference genes",
                      helpText("These tables correspond to obtained tables dans \"Ct data\" and 
                               \"Efficiency data\" for chosen reference genes."),
                      dataTableOutput(outputId="table4"),
                      dataTableOutput(outputId="table5"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      )
                      #                                       downloadButton('downloadDataE', 'Download')
             ),
             tabPanel("Normalization",
                      helpText("This table contain all the normalized Ct for each gene (in row)."),
                      dataTableOutput(outputId="table6"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataNorm', 'Download')
             ),
             tabPanel("Mean of biological replicates",
                      helpText("This table contains the normalized and averaged on the technical
                               and biological replicates Ct for each gene (in row) following the 
                               conditions (in column)."),
                      dataTableOutput(outputId="table7"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataRepBio', 'Download')
                      ),
             tabPanel("Calculation of log-ratios",
                      helpText("This table contains all the log-ratios of each gene (in row) 
                               for each condition (in column)."),
                      br(),
                      #                                       box(title = "LogRatio", status = "primary", 
                      #                                           solidHeader = TRUE, width = 12,    
                      # #                                           selectInput('in3', 'Choix des gènes de référence', choice3 , multiple=TRUE, selectize=TRUE),
                      #                                           
                      #                                           selectInput('lR', 'Condtion à comparer : ', choice3, multiple = TRUE, selectize = TRUE) 
                      #                                       ),
                      #                                       br(),
                      dataTableOutput(outputId="table8"),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      downloadButton('downloadDataLogRatio', 'Download')
                      )
             
             ),
  navbarMenu("Plots", icon = icon("bar-chart"),
             
             tabPanel("Technical help",
                      box(title="Technical help", color="teal", width = 12, solidHeader = FALSE,
                          div(class="aid",
                              p("Explore the results from several plots downloadables in png format."),    
                              p("For more information on the choice of colors available, you can go to the tab Help.")
                              
                          )
                      )
             ),  
             tabPanel("1. for a gene",
                      box(title = "Parameters of graph", status = "primary", 
                          solidHeader = TRUE, width = 12,      
                          selectInput('in9', 'Choice of gene : ', choice3, selectize=TRUE),
                          selectInput('col1', 'Choice the colors for the graph :', names.col, selectize = TRUE),
                          sliderInput('slide1', 'Scale', min = 0, max = 100, value = 50)
                      ),
                      br(),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      br(),
                      box(title = "Barplot of conditions for a given gene", status = "primary", solidHeader = TRUE, width = 12,
                          plotOutput("plot1")
                      ),
                      br(),
                      downloadButton('downloadPlot1', 'Download')
             ),
             tabPanel("2. for a condition",
                      box(title = "Parameters of graph", status = "primary", 
                          solidHeader = TRUE, width = 12,    
                          selectInput('in10', 'Choice of the condition : ', choice3, selectize=TRUE),
                          selectInput('col2', 'Choice of the colors for the graph : ', choices = color2, selectize = TRUE),
                          sliderInput('slide2', 'Scale', min = 0, max = 100, value = 50)
                      ),
                      br(),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      br(),
                      box(title = "Barplot of genes for a given condition", status = "primary", solidHeader = TRUE, width = 12,
                          plotOutput("plot2")
                      ),
                      downloadButton('downloadPlot2', 'Download')
             ),
             tabPanel("3. for several genes",
                      box(title = "Parameters of graph", status = "primary", 
                          solidHeader = TRUE, width = 12,    
                          selectInput('in11', 'Choice of the genes : ', choice3, multiple = TRUE, selectize=TRUE),
                          selectInput('col3', 'Choice of the colors for the graph : ', names.col, selectize = TRUE),
                          sliderInput('slide3', 'Scale', min = 0, max = 100, value = 50)
                      ),
                      br(),
                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                       tags$img(src="loading_circle.gif")
                      ),
                      br(),
                      br(),
                      box(title = "Comparison of barplots for given genes in all conditions", status = "primary", solidHeader = TRUE, width = 12,
                          plotOutput("plot3")
                      ),
                      downloadButton('downloadPlot3', 'Download')
             )
  ),
  tabPanel("RNA-Seq", icon = icon("line-chart"),
           box(title="Technical help", color="teal", width = 12, solidHeader = FALSE,
               div(class="aid",
                   p("The \"Analysis\" part must have previously performed before to carry out the \"RNA-Seq\" part,
                     this part is optional."),
                   br(),
                   p("The input file must contain at least the columns \"Name\", \"log2FoldChange\",
                     \"baseMean\" and \"padj\" and be in csv format."),
                   p("If the file contains the colums \"baseMeanA\" and \"baseMeanB\", check the box."),
                   br(),
                   p("In this part two graphs are available : the first is a correlation plot the chosen 
                     conditions of log-ratio got with the qRT-PCR experience in according to the log-ration
                     got with the RNA-Seq experience, the second graph is a barplot representing the biological
                     mean got by a qRT-PCR and RNA-Seq experince in each chosen conditoin for a predifined gene"),
                   p("To get these two graphs, you must : "),    
                   tags$ul(
                     # tags$li("Sélectionner le nom du dossier contenant les fichiers à analyser"),
                     tags$li("enter the conditions present in the RNA-Seq file"),
                     tags$li("load the RNA-Seq file"),
                     tags$li("check if the columns \"baseMeanA\" and \"baseMeanB\" are pesents in the RNA-Seq file ")
                   )
                   )
               
                   ),
           # tags$script('$( "#RNA-help" ).on( "click", function() { this.value = null; });'),
           
           # infoBoxOutput("boxRNA"),
           box(title = "Selectize the file from a RNA-Seq experience", status = "primary", 
               solidHeader = TRUE, width = 12,   
               selectInput('inCondRna', 'Compared conditions in the file : ', choice3, 
                           multiple = FALSE, selectize=TRUE),
               fileInput('inRnaSeq', 'RNA-Seq file', choice1 , multiple=FALSE),
               checkboxInput("checkbox", "baseMeanA and baseMeanB", FALSE)
               
               
           ),
           br(),
           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                            tags$img(src="loading_circle.gif")
           ),
           br(),
           br(),
           # box(title = "Table RNAseq", status = "primary", solidHeader = TRUE, width = 12,
           #     dataTableOutput(outputId="tableRNAseq")
           # ),
           box(title = "Correlation plot of log-ratio", status = "primary", solidHeader = TRUE, width = 12,
               plotlyOutput("RNAseq")
           ),
           box(title = "Barplot of biological mean", status = "primary", solidHeader = TRUE, width = 12,
               selectInput('inGene', 'Choice of gene : ', choice3, selectize=TRUE),
               selectInput('colRNA', 'Choice of the colors for the graph : ', names.col, selectize = TRUE),
               plotOutput("RNA1"),
               downloadButton('downloadPlotRNA1', 'Download') 
           )
           
  ),
  tabPanel("Help", icon = icon("question"),
           div(class = "head", 
               h3("Introduction : "),
               p("This application enables the processing of qRT-PCR data. From Ct and Efficiency files 
                 for each gene, a serie of steps of analysis is performed to getting several plots.
                 For example, for a gene in 3 different conditions with 2 technical replicates and 
                 4 biological replicates. The different successive steps of analysis are : "),
               tags$ul(
                 withMathJax(tags$li("the average on the technical replicates : $$ \\overline{Ct} = \\frac{Ct_1 + Ct_2}{2} $$")),
                 #                             tags$li("the average on the technical replicates : $$ MoyTech = \\frac{RepTech1 + RepTech2}{2} $$"),
                 withMathJax(tags$li("the calculation of the normalized relative quantity for one gene given for each biological replicate :
                                     $$ NRQ = \\frac{(1+E_{G_i})^{-\\overline{Ct}_{i}}}{[\\prod_{j=1}^{n} (1+E_{Rj})^{\\overline{Ct}_{R_{j}}}]^{-1/2}} $$")),
                 #                             withMathJax(tags$li(" normalize \\(\\Delta Ct\\) (to add) with n reference genes :
                 #                                                     $$ \\Delta Ct = \\frac{E_{G_i}^{Ct}}{(\\prod_{j=1}^{n} E^{Ct}_{Rj})^{1/2}} $$")),
                 #                             tags$li("the average on the biological replicates : $$ MoyBio = \\frac{RepBio1 + RepBio2 + RepBio3 + RepBio4}{4} $$"),
                 tags$li("the average on the biological replicates : $$ \\overline{NRQ_{m,k}} = \\frac{NRQ_{m,1} + NRQ_{m,2} + NRQ_{m,3} + NRQ_{m,4}}{4} $$"),
                 tags$li("the calculation of the log-ratio for all the compared conditions.")
                 ),
               p("For each gene, we get therefore at the end a value of Ct, which is normalized and averaged on the technical 
                 and biological replicates per condition."),
               p("The tables of results are downloadable at each step. Differents plots are available (the graphs are plotted 
                 from averaged and normalized Ct for each gene) : "),
               tags$ul(
                 tags$li("Barplot of conditions for a given gene"),
                 tags$li("Barplot of genes for a given condition"),
                 tags$li("Comparison of barplots for chosen genes in all the conditions.")
               ),  
               hr(),
               h3("Technical help : "),
               br(),
               h4("Header of Ct and Efficiency files : "),
               p("The Ct files must have at least 3 columns called \"Sample Name\", \"Target Name\" et \"CT\".
                 The names of columns must be at the line 43 and the data must begin line 44."),
               p("In the column \"Sample Name\" the condition should be followed by the number of 
                 replicate without spaces or special characters (e.g. Fus1 corresponds at the condition \"fus\" of first replicate).
                 In the column \"Target Name\"  the gene should be written without spaces"),
               br(),
               img(src = "headFilesCt.png", style='width:120%; max-width: 780px;'),
               br(),
               br(),
               p("The Efficiency files must have four columns called \"Sample Name\", \"Target Name\", \"CT\" et \"Slope\".
                 The names of columns must be line 37 and the data must begin line 38."),    
               p("In the column \"Sample Name\" the condition should be written (e.g. Fus corresponds at the condition \"fus\").
                 In the column \"Target Name\" the gene should be written without spaces and the same way in the CT files and Efficiency files."),
               br(),
               img(src = "headFilesE.png", style='width:100%; max-width: 780px;'),
               br(),
               h4("Parameters : "),
               p("To start, the files in xlsx format with the predifined names of 
                 columns must be selected (To know if your files are corrects, go 
                 to the tab Help.)"),
               p("To use the application, you must choose the parameters below : "),
               tags$ul(
                 # tags$li("Sélectionner le nom du dossier contenant les fichiers à analyser"),
                 tags$li("Selectize the number of conditions"),
                 tags$li("Selectize the number of technical replicates"),
                 tags$li("Selectize the number of biological replicates"),
                 tags$li("Selectize the Ct files"),
                 tags$li("Selectize the Efficiency files"),
                 tags$li("Selectize the chosen reference genes"),
                 tags$li("Selectize the conditions for the calculation of log-ratio")
                 
                 
               ),
               br(),
               h4("Analysis : "),
               p("Access at each step of the analysis and download the intermediary tables."),
               br(),
               h4("Plots : "),
               p("Explore the results from several plots downloadables in png format."),    
               p("The choice of the available colors for the plots are : "),
               img(src = "colorGgplot2.png"),
               br(),
               h4("RNA-Seq : "),
               p("The \"Analysis\" part must have previously performed before to carry out the \"RNA-Seq\" part,
                 this part is optional."),
               p("It compare the results of RNA-Seq with the results of qRT-PCR"),    
               br(),
               p("The input file must contain at least the columns \"Name\", \"log2FoldChange\",
                                  \"baseMean\" and \"padj\" and be in csv format."),
               p("If the file contains the colums \"baseMeanA\" and \"baseMeanB\", check the box."),
               br(),
               p("In this part two graphs are available : the first is a correlation plot the chosen 
                                                            conditions of log-ratio got with the qRT-PCR experience in according to the log-ration
                                                            got with the RNA-Seq experience, the second graph is a barplot representing the biological
                                                            mean got by a qRT-PCR and RNA-Seq experince in each chosen conditoin for a predifined gene"),
               p("To get these two graphs, you must : "),    
               tags$ul(
                 # tags$li("Sélectionner le nom du dossier contenant les fichiers à analyser"),
                 tags$li("enter the conditions present in the RNA-Seq file"),
                 tags$li("load the RNA-Seq file"),
                 tags$li("check if the columns \"baseMeanA\" and \"baseMeanB\" are pesents in the RNA-Seq file ")
               )
               
               
               , style = "color:black")
               ),
  tabPanel("References", icon = icon("graduation-cap"),
           tags$iframe(style="width:100%; height:600px; scrolling=yes; text-align: center",
                       src = "https://www.dropbox.com/s/0l72jmfbkk7fttd/Poster_PROqPCR.pdf?raw=1")
  )
  # ,
  #     tabPanel("Publications", icon = icon("graduation-cap")
  #             )
  
               )
  )



dashboardPage(title = "Application qRT-PCR", header, sidebar, body)
