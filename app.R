#load data here when published....

library(GGally)
library(shiny)
library(shinyjs)
library(shinyBS)
library(DT)
library(tibble)
library(leaflet)
library(ggVennDiagram)
library(shinyWidgets)
library(htmltools)
library(ggpubr)
library(factoextra)
library(tidyr)
#library(lessR)
#library(plotly)



###load spruce
spruce<-readRDS('spruce.final.RDS')

spruce$HbBV_C13_gain<-spruce$HbBV_C13_gain*-1
spruce$AbBV_C13_gain<-spruce$AbBV_C13_gain*-1

spruce$Site<-factor(spruce$Site)
spruce$ID<-as.character(spruce$ID)
spruce$Family<-as.character(spruce$Family)

spruce.bv.index<-c(29,31,33,35,37)
spruce.bv.nice.names<-c("DBH30","HT30","WD","RES2015","C13")
spruce.bv.names<-colnames(spruce)[spruce.bv.index]

spruce.A.bv.index<-spruce.bv.index+10
spruce.A.bv.names<-colnames(spruce)[spruce.A.bv.index]

spruce.phen.index<-c(15,18,21,23,25)
spruce.phen.names<-colnames(spruce)[spruce.phen.index]

spruce.gen.gain.index<-c(49:53)
spruce.ped.gain.index<-c(54:58)

spruce.bv.names
spruce.A.bv.names
spruce.phen.names
spruce.bv.nice.names
colnames(spruce[spruce.gen.gain.index])
colnames(spruce[spruce.ped.gain.index])
###



###load pine###
pine<-readRDS('pine.final.RDS')

pine$BV_G.C13b.s_gain<-pine$BV_G.C13b.s_gain*-1
pine$BV_A.C13b.s_gain<-pine$BV_A.C13b.s_gain*-1
pine$Hs8.BV_HT30_gain<-pine$Hs8.BV_HT30_gain+1
pine$Hs8.BV_DBH30_gain<-pine$Hs8.BV_DBH30_gain+1

pine$Site<-factor(pine$Site)
pine$ID<-as.character(pine$ID)
pine$Family<-as.character(pine$Family)

pine.bv.index<-c(31,33,35,37,39,41,43,45)
colnames(pine)[pine.bv.index]

pine.bv.nice.names<-c("DBH30","HT30","WGR36","WD","MFA","DECL","C13","MPB")
pine.bv.names<-colnames(pine)[pine.bv.index]

pine.A.bv.index<-pine.bv.index+16
pine.A.bv.names<-colnames(pine)[pine.A.bv.index]

pine.phen.index<-c(21:28)
pine.phen.names<-colnames(pine)[pine.phen.index]

pine.gen.gain.index<-c(63:70)
pine.ped.gain.index<-c(71:78)

pine.bv.names
pine.A.bv.names
pine.phen.names
pine.bv.nice.names
colnames(pine[pine.gen.gain.index])
colnames(pine[pine.ped.gain.index])


###
#env

soilGRIDS.data <- readRDS('soilGRIDS.data.RDS')
clim.data <- readRDS('clim.data.long.RDS')


##for legacy##
pine.msmt<-readRDS('pine.MSMT.wide.bvs.RDS')
spruce.msmt<-readRDS('pine.MSMT.wide.bvs.RDS')
##


ui<-fluidPage(
  
  navbarPage(title = div(img(src='resfor-logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
             #theme = "journal",
             windowTitle="RESFOR Selection Tool",
             
             tabPanel('Home',
                      titlePanel("Information"),
                      
                      
                      
                      fixedRow(
                        column(6,
                               "",
                               p(),
                               p("This interactive tool is designed using the R-shiny platform, which combines the computational power of R with the
                                interactivity of websites. The tool allows end users of the RES-FOR Project to easily and
                                visually explore the data and results produced from the quantitative genomics aspect of RES-FOR study. The tool also serves as a source
                                of information regarding the analyses and phenotypes used in the RES-FOR study. Development of the tool is ongoing and will be generalized to any data source. Development
                                will continue with feedback from internal RES-FOR researchers in the meantime.")
                        ),
                        column(6,
                               "",
                               leafletOutput("mymap"),
                               p(),
                        )
                      ),
                      fixedRow(
                        column(12,
                               "",
                               
                               div(img(src = "sponsors.PNG", height = 587, width = 922), style="text-align: center;")
                               
                        )
                      )
                      
                      
                      
                      
                      
                      
                      
             ),
             
             
             
             tabPanel("Data Viewer",
                      titlePanel("Data Viewer"),
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(
                            'input.dataset === "Lodgepole Pine"',
                            checkboxGroupInput("show_vars1", "Columns to show:",
                                               names(pine)[1:28], selected = names(pine)[c(1,3,5)])
                          ),
                          conditionalPanel(
                            'input.dataset === "White Spruce"',
                            checkboxGroupInput("show_vars2", "Columns to show:",
                                               names(spruce)[c(1:15,18,21,23,25)], selected = names(spruce)[c(1,3,6)])
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            id = 'dataset',
                            tabPanel("Lodgepole Pine", dataTableOutput("mytable1")),
                            tabPanel("White Spruce", dataTableOutput("mytable2"))
                          )
                        )
                      )
                      
             ),
             
             tabPanel("Environment",
                      titlePanel("Environment Information"),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          selectInput("input_species_environment", "Species",
                                      c("White Spruce","Lodgepole Pine")
                          ),
                          
                          #Climate date range
                          uiOutput("ui_climate_dates")
                          ,
                          
                          #Climate variables annual
                          uiOutput("ui_climate_vars_annual")
                          ,
                          
                          #Climate variables
                          uiOutput("ui_climate_vars_seasonal")
                          ,
                          
                          #Climate variables
                          uiOutput("ui_climate_vars_monthly")
                          ,
                          
                          #Soil variables
                          uiOutput("ui_soil_vars")
                          ,
                          
                          #Soil depths
                          uiOutput("ui_soil_depths")
                          ,
                          
                          #Visualize option
                          radioButtons("radio.env.visualize", label = ("Visualize Site Relationships"),
                                       choices = list("No visualization" = 1, 
                                                      "Soil only" = 2, 
                                                      "Climate only" = 3,
                                                      "Soil + Climate combined" = 4), 
                                       selected = 1),
                          
                          #submit parameters button
                          actionButton("goButton_environment", "Submit")
                          ,
                          #refresh button
                          actionButton('refresh_environment',"Refresh"),
                          
                        ),
                        mainPanel(
                          
                          fixedRow(
                            column(6,
                                   "",
                                   #PCA plot
                                   plotOutput(outputId = "env_PCA_plot")
                            ),
                            column(6,
                                   "",
                                   #kernel plot
                                   plotOutput(outputId = "env_kernel_plot")
                            )
                          ),
                          
                          fixedRow(
                            column(6,
                                   "",
                                   #h4('Data Preview'),
                                   #textOutput("table_title_env"),
                                   uiOutput(outputId = "table_title_env"),
                                   uiOutput("table.env.data")
                            ),
                            column(6,
                                   "",
                                   #h4('Download Data'),
                                   #output$download_title_env
                                   uiOutput(outputId = "download_title_env"),
                                   uiOutput(outputId = "downloadData.environment")
                                   
                            ),
                            
                          )
                        )
                      )
             ),
             
             
             
             
             
             
             
             
             
             navbarMenu("Data Visualization",
                        tabPanel("Distributions",
                                 titlePanel("Distributions"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     
                                     selectInput("input_dist_species", "Species",
                                                 c("White Spruce","Lodgepole Pine"), selected = "White Spruce"),
                                     
                                     # This outputs the dynamic UI1 component
                                     uiOutput("ui_dist_1"),
                                     
                                     # This outputs the dynamic UI2 component
                                     uiOutput("ui_dist_2"),
                                     
                                     uiOutput("ui_dist_3"),
                                     
                                     
                                   ),
                                   mainPanel(
                                     
                                     plotOutput(outputId = "main_plot_distributions")
                                     
                                   )
                                 )
                                 
                        ),
                        
                        
                        
                        
                        tabPanel("Trait-trait Relationships",
                                 titlePanel("Trait-trait Relationships"),
                                 # Sidebar layout with input and output definitions ----
                                 sidebarLayout(
                                   
                                   # Sidebar panel for inputs ----
                                   sidebarPanel(
                                     
                                     selectInput("input_type", "Species",
                                                 c("White Spruce","Lodgepole Pine")
                                     ),
                                     # This outputs the dynamic UI1 component
                                     uiOutput("ui1")
                                     ,
                                     # This outputs the dynamic UI2 component
                                     uiOutput("ui2")
                                   ),
                                   
                                   # Main panel for displaying outputs ----
                                   mainPanel(
                                     
                                     plotOutput(outputId = "main_plot")
                                   )
                                 )
                        ),
                        
                        tabPanel("Site-trait Relationships",
                                 titlePanel("Site-trait Relationships"),
                                 #Sidebar layout with input and output definitions ----
                                 sidebarLayout(
                                   
                                   # Sidebar panel for inputs ----
                                   sidebarPanel(
                                     
                                     selectInput("input_type7", "Species",
                                                 c("Lodgepole Pine","White Spruce")#, selected = "Lodgepole Pine")
                                     ),
                                     # This outputs the dynamic UI1 component
                                     uiOutput("ui8")
                                     ,
                                     # This outputs the dynamic UI1 component
                                     uiOutput("ui9")
                                   ),
                                   
                                   # Main panel for displaying outputs ----
                                   mainPanel(
                                     
                                     plotOutput(outputId = "main_plot7")
                                   )
                                 )
                        ),
                        
                        tabPanel("Scatterplot Zoom",
                                 
                                 titlePanel("Scatterplot Zoom"),
                                 # Sidebar layout with input and output definitions ----
                                 sidebarLayout(
                                   
                                   # Sidebar panel for inputs ----
                                   sidebarPanel(
                                     
                                     selectInput("input_type3", "Species",
                                                 c("White Spruce","Lodgepole Pine")
                                     ),
                                     # This outputs the dynamic UI component
                                     uiOutput("ui6")
                                     ,
                                     # This outputs the dynamic UI component
                                     uiOutput("ui7")
                                     
                                   ),
                                   mainPanel(
                                     
                                     plotOutput(outputId = "main_plot2", 
                                                click = "plot1_click",
                                                brush = brushOpts(
                                                  id = "plot1_brush"
                                                )),
                                     
                                     h4("Points near click"),
                                     verbatimTextOutput("click_info"),
                                     
                                     
                                     h4("Brushed points"),
                                     verbatimTextOutput("brush_info")
                                     
                                   )
                                   
                                 )
                        )
             ),
             
             #backwards selections placeholder
             tabPanel("Backwards Selections",
                      titlePanel("Backwards Selections"),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          selectInput("species_input_backwards", "Species",
                                      c("White Spruce","Lodgepole Pine")
                          ),
                          
                          #site selection
                          uiOutput("ui_backwards_sites")
                          ,
                          #average across selected sites
                          uiOutput("ui_avg_trait_within_site")
                          ,
                          #,
                          #trait selection for index
                          uiOutput("ui_backwards_traits")
                          ,
                          #selection index weights
                          uiOutput("ui_backwards_trait_weights")
                          ,
                          #submit parameters button
                          actionButton("goButton_backwards", "Submit")
                          ,
                          #refresh button
                          actionButton('refresh',"Refresh"),
                          
                        ),
                        mainPanel(
                          
                          
                          
                          fixedRow(
                            column(6,
                                   "",
                                   #base population histogram
                                   plotOutput(outputId = "backwards_family_base_Plot")
                            ),
                            column(6,
                                   "",
                                   #selected population histogram
                                   plotOutput(outputId = "backwards_family_selected_Plot")
                            )
                          ),
                          fixedRow(
                            column(6,
                                   "",
                                   
                                   #h4('Selection Intensity'),
                                   uiOutput(outputId = "backwards_selection_intensity_title"),
                                   
                                   #slider for selection intensity
                                   uiOutput("slider.selection.backwards")
                            ),
                            column(6,
                                   "",
                                   
                                   
                                   #h4('Expected genetic gain (%)'),
                                   uiOutput(outputId = "backwards_gain_title"),
                                   
                                   tableOutput("tableGain_backwards"),
                                   
                                   #h4('Number of families'),
                                   uiOutput(outputId = "backwards_Nfam_title"),
                                   
                                   tableOutput("tableNfam_backwards")
                            )
                          ),
                          fixedRow(
                            column(6,
                                   "",
                                   #h4('Selections Preview'),
                                   uiOutput(outputId = "backwards_selection_preview_title"),
                                   
                                   uiOutput("table.candidate.backwards")
                            ),
                            column(6,
                                   "",
                                   
                                   #h4('Download Selections'),
                                   #downloadButton('downloadData.backwards', label = "Download")
                                   
                                   uiOutput(outputId = "backwards_download_title"),
                                   uiOutput(outputId = "downloadData.backwards"),
                                   
                                   
                            ),
                            column(6,
                                   "",
                                   
                                   
                                   #h4('Pedigree Comparison'),
                                   #actionButton("go2", "Go"),
                                   
                                   uiOutput(outputId = "backwards_ped_compare_title"),
                                   uiOutput(outputId = "backwards_ped_compare_button"),
                                   
                                   #bsModal("modalPedigree", "Pedigree Comparison Plot", "go2", size = "large",plotOutput("plot2"),downloadButton('downloadPlot2', 'Download'))
                                   
                            ),
                            column(6,
                                   "",
                                   
                                   
                                   #h4('Site-Site Stability Plot'),
                                   #actionButton("go", "Go"),
                                   
                                   uiOutput(outputId = "backwards_stability_title"),
                                   uiOutput(outputId = "backwards_stability_button"),
                                   
                                   #bsModal("modalStability", "Family Means x Site Stability Plot", "go", size = "large",plotOutput("plot"),downloadButton('downloadPlot', 'Download'))
                                   
                                   
                                   
                            ),
                            
                          )
                        )
                      )
             ),
             
             
             tabPanel("Fowards Selections",
                      titlePanel("Forwards Selections"),
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          selectInput("input_type2", "Species",
                                      c("White Spruce","Lodgepole Pine")
                          ),
                          # This outputs the dynamic UI1 component
                          uiOutput("ui3")
                          ,
                          # This outputs the dynamic UI1 component
                          uiOutput("ui4")
                          ,
                          #selectInput("num", "select number of inputs", choices = seq(1,10,1)),
                          uiOutput("ui5")
                          ,
                          
                          numericInput('coancestry', 'Maximum number of selections per family', 5, step = 1, min = 1, max = 100),
                          
                          
                          actionButton("goButton", "Submit")
                          ,
                          #refresh button
                          actionButton('refresh_forward',"Refresh"),
                          
                        ),
                        mainPanel(
                          
                          
                          
                          fixedRow(
                            column(6,
                                   "",
                                   plotOutput(outputId = "distPlot")
                            ),
                            column(6,
                                   "",
                                   plotOutput(outputId = "distPlot2")
                            )
                          ),
                          fixedRow(
                            column(6,
                                   "",
                                   #h4('Selection Intensity'),
                                   uiOutput(outputId = "forwards_selection_intensity_title"),
                                   uiOutput("slider.selection")
                            ),
                            column(6,
                                   "",
                                   #h4('Expected genetic gain (%)'),
                                   uiOutput(outputId = "forwards_gain_title"),
                                   tableOutput("tableGain"),
                                   #h4('Status Number (Ns)'),
                                   uiOutput(outputId = "forwards_Nfam_title"),
                                   tableOutput("tableNs")
                            )
                          ),
                          fixedRow(
                            column(6,
                                   "",
                                   #downloadButton('downloadData', label = "Download"),
                                   #h4('Selections Preview'),
                                   uiOutput(outputId = "forwards_selection_preview_title"),
                                   uiOutput("table.candidate")
                            ),
                            column(6,
                                   "",
                                   #h4('Download Selections'),
                                   uiOutput(outputId = "forwards_download_title"),
                                   #downloadButton('downloadData', label = "Download"),
                                   
                                   uiOutput(outputId = "downloadData.forwards"),
                                   #h3('Selections'),
                                   #uiOutput("table.candidate")
                            ),
                            column(6,
                                   "",
                                   
                                   
                                   #h4('Pedigree Comparison'),
                                   #actionButton("go2", "Go"),
                                   
                                   uiOutput(outputId = "forwards_ped_compare_title"),
                                   uiOutput(outputId = "forwards_ped_compare_button"),
                                   
                                   #bsModal("modalPedigree", "Pedigree Comparison Plot", "go2", size = "large",plotOutput("plot2"),downloadButton('downloadPlot2', 'Download'))
                                   
                            )
                            
                          )
                        )
                      )
             ),
             navbarMenu("Help",
                        tabPanel("Trait descriptions",
                                 titlePanel("Help > Trait descriptions"),
                                 
                                 h4("HT30"),
                                 p('Total tree height measured at age 30.'),
                                 HTML('<br>'),
                                 
                                 h4('DBH30'),
                                 p('Diameter at breast height measured at age 30.'),
                                 HTML('<br>'),
                                 
                                 h4('WWD'),
                                 p('WWD: The weighted wood density (WWD) represents the average wood density of the whole tree. Wood density was 
                     calculated individually for each tree ring using X-ray densitometry which reports relative wood density profiles 
                     at a resolution of 0.0254 mm. These measurements were averaged for each tree weighted by tree ring width so the 
                     average would give a better representation of the whole tree wood density rather than the average of individual rings.'),
                                 HTML('<br>'),
                                 
                                 h4('LDECL'),
                                 p("The decline index (LDECL) shows the growth reduction experienced over a tree's lifetime due to drought and
                     other stresses using dendrochronology methods. It is calculated as the ratio between the average growth over the
                     5-year period of maximum growth (1997-2001) and the average growth of the last five years (2012-2016). High decline
                     values represent trees that showed very rapid growth in their younger stages but experienced severe growth reductions,
                     while trees with low decline values showed a constant growth over the whole period."),
                                 HTML('<br>'),
                                 div(img(src = "ldecl.png"), style="text-align: left;"),
                                 HTML('<br>'),
                                 p('Figure: The figure represents two growth chronologies (yearly basal area increment) of one tree with high decline (red) and
                     one tree with low decline (blue). '),
                                 HTML('<br>'),
                                 
                                 h4('WGR36'),
                                 p('Normalized western gall rust rank. Larger values indicate increased infection.'),
                                 HTML('<br>'),
                                 
                                 h4('MPB_rank_resistant'),
                                 
                                 HTML('<p>Collection, interpretation, and importance of phenotypic defense chemistry traits</p>
                   <p>We extracted the major class of chemicals associated with pine defense against mountain pine
                   beetle (<i>Dendroctonus ponderosae</i>) from all RES-FOR selected lodgepole pine at all sites. With
                   these 15 identified and quantified monoterpene compounds from each tree phloem tissue sample,
                   we performed insect and fungi bioassays in the lab from a subset of families that ranged in
                   chemical concentration and composition. <br><br>
                   From these bioassays, we found three major chemicals
                   that elicited a negative response from mountain pine beetle: the proportion of limonene, and
                   concentrations of &beta;-phellandrene and &gamma;-terpinene. We applied these thresholds of chemical
                   components to rank all RES-FOR trees for their suitability for mountain pine beetle infestation
                   (MPB_rank_resistant = equivalent to most to least resistant trees) (Table 1). <br><br>
                   The trait
                   MPB_rank_resistant was derived from chemicals associated with impacting just a single life
                   stage of mountain pine beetle (i.e., host acceptance). The 7 monoterpene compounds, as well as
                   total monoterpene concentration are traits being needed by mountain pine beetle for attraction
                   (i.e., &alpha;-pinene, myrcene, terpinolene) or are associated with being toxic to MPB (limonene, &beta;-phellandrene, total monoterpenes).
                   The association of these chemicals with western gall rust (WGR) infection is not yet known.</p>'),
                                 
                                 HTML('
                   <table border="1" style="height: 308px; width: 1020px;">
                        <tbody>
                        <tr>
                        <td style="width: 1002px;" colspan="3"> Table 1. Definition of mountain pine beetle resistance ranking.</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"><strong>Relative resistance to MPB</strong></td>
                        <td style="width: 590.83px;"><strong>Categories of resistance</strong></td>
                        <td style="width: 186.17px;"><strong>Rank (MPB_rank_resistant)</strong></td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"><strong>Most resistant</strong></td>
                        <td style="width: 590.83px;">high proportion limonene (&ge;23.5%)* and high &beta;-phellandrene (&ge;4079 ng/mg fw)</td>
                        <td style="width: 186.17px;">1</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"></td>
                        <td style="width: 590.83px;">high proportion limonene (&ge;23.5%)*</td>
                        <td style="width: 186.17px;">2</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"></td>
                        <td style="width: 590.83px;">high &gamma;-terpinene (&ge;16.93 ng/mg fw) and high &beta;-phellandrene (&ge;4079 ng/mg fw)</td>
                        <td style="width: 186.17px;">3</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"></td>
                        <td style="width: 590.83px;">high &beta;-phellandrene (&ge;4079 ng/mg fw)</td>
                        <td style="width: 186.17px;">4</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"></td>
                        <td style="width: 590.83px;">high &gamma;-terpinene (&ge;16.93 ng/mg fw)</td>
                        <td style="width: 186.17px;">5</td>
                        </tr>
                        <tr>
                        <td style="width: 225px;"><strong>Least resistant</strong></td>
                        <td style="width: 590.83px;">low all</td>
                        <td style="width: 186.17px;">6</td>
                        </tr>
                        <tr>
                        <td style="width: 1002px;" colspan="3"> * Proportion of total monoterpenes (without &beta;-phellandrene) that is limonene.</td>
                        </tr>
                        </tbody>
                        </table> 
                        '),
                                 HTML('<br>'),
                                 
                                 
                                 
                                 h4('C13'),
                                 
                                 HTML('&delta;13C stands for stable carbon isotope ratio. Stable carbon isotopes are carbon atoms with different masses. Two major types of carbon atoms 
                   naturally exist in the atmosphere: 12C, and 13C. CO2 molecules made of different isotopes have different diffusion rates and different reaction rates 
                   with photosynthetic enzymes, causing stable isotope compositions of plant tissues to deviate from that of the atmosphere. This difference is called 
                   carbon isotope discrimination (&Delta;), and under approximation it increases with the ratio of CO2 concentration inside leaf over atmospheric CO2 concentration
                   (ci/ca). ci/ca, in turn, is proportional to the ratio of photosynthetic rate over transpiration, a ratio known as instantaneous water use efficiency 
                   (WUEins). A plant with higher WUEins can photosynthesize more while losing less water; therefore, WUEins have been considered a trait indicating drought 
                   resistance. When comparing plants grown at the same locations and have similar heights (not as different as a tree compared to a grass), one can use &delta;13C
                   (of plant tissues) to approximate &Delta;. <br><br>
                   A few notes about &delta;13C. First, the relationships between &delta;13C and &Delta;, as well as between &Delta; and ci/ca, are approximate; these relationships might not
                   hold true when comparing plants grown at different elevations, or between different plant tissues or plant species with different photosynthetic pathways 
                   (C3, C4, CAM). Second, water use efficiency is one of multiple traits influencing drought resistance. For instance, a plant can have high water use 
                   efficiency but a water transporting system that is more inclined to dysfunction when facing drought. 
                   <br><br>
                   Reference: Cernusak, Lucas A., et al. "Environmental and physiological determinants of carbon isotope discrimination in terrestrial plants." New Phytologist 200.4 (2013): 950-965.')
                                 
                        ),
                        tabPanel('Trait-trait relationships',
                                 titlePanel("Trait-trait relationships help"),
                                 p('The trait-trait relationships tool allows users to observe the relationships between  breeding values for the various traits measured in the RESFOR study. Users can select the species and progeny sites they are interested in, then select the traits they are interested in. The tool will display an informative plot showing scatterplots, distributions, and the overall correlation and within site correlations between the user selected traits. This tool is useful for understanding the relationships between traits prior to performing selections.')
                                 
                        ),
                        tabPanel('Site-site correlation',
                                 titlePanel("Site-trait correlation help"),
                                 p('The site-trait relationships tool is similar to the trait-trait relationship tool, except it treats each environment (site or progeny trial) as a separate trait. This allows users to observe the genotype x environment or phenotype x environment interactions for the various traits measured in the RESFOR study.   ')
                                 
                        ),
                        tabPanel('Site-site correlation',
                                 titlePanel("Site-trait correlation help"),
                                 p('The scatterplot zoom tool allows users to explore the relationship between <i>two traits</i> in more detail. The tool plots two traits, with colors representing environments, and allows the user to draw or select points in the scatterplot to identify trees of interest. These trees may be favorable deviants (i.e. correlation breakers), that end users wish to identify. Correlation breakers are data points which do not follow the general trend of the observed data. For example, trees with high wood density and high growth rate, i.e. traits that are typically negatively correlated.  ')
                                 
                        ),
                        tabPanel('Selections',
                                 titlePanel("Selections help"),
                                 p('Currently, the selections tool allows the user to base selection decisions on a simple index of trait values. Users select the traits they are interested in and provide a weighting for each trait. The distribution of the weighted index is shown, and the user is asked to input a selection cut-off value and restriction on co-ancestry. The tool then provides a list of individuals, status number, and expected genetic gain to the user based on the selection criteria provided.  ')
                                 
                        )
                        
                        
                        
             )
  ))



# Define server function

server<-function(input, output, session) {
  
  observeEvent(input$refresh_forward,{
    session$reload()
    return()
  })
  
  
  
  ## MAP PANEL ##
  
  points <- data.frame(Name=c('Calling Lake','Red Earth','Carson Lake','Judy Creek','Virginia Hills','Swan Hills','Timeau'),
                       Long=c(-113.09,-115.19,-115.34,-115.34,-115.51,-115.30,-115.18), 
                       Lat=c(55.16,56.34,54.34,54.24,54.28,54.40,54.41), 
                       elev=c(640,518,1003,1097,1127,1033,1064), 
                       Col=factor(c('red','red','red','blue','blue','blue','blue')),
                       Species=factor(c('White Spruce','White Spruce','White Spruce','Lodgepole Pine','Lodgepole Pine','Lodgepole Pine','Lodgepole Pine')),
                       spp=c(paste(sep = "<br/>",
                                   "<b>Calling Lake</b>",
                                   "Elev. 640m",
                                   "Spp. White Spruce",
                                   "CPP Region: E"),
                             paste(sep = "<br/>",
                                   "<b>Red Earth</b>",
                                   "Elev. 518m",
                                   "Spp. White Spruce",
                                   "CPP Region: D1"),
                             paste(sep = "<br/>",
                                   "<b>Carson Lake</b>",
                                   "Elev. 1006m",
                                   "Spp. White Spruce",
                                   "CPP Region: D"),
                             
                             paste(sep = "<br/>",
                                   "<b>Judy Creek</b>",
                                   "Elev. 1097m",
                                   "Spp. Lodgepole Pine",
                                   "CPP Region: C"),
                             paste(sep = "<br/>",
                                   "<b>Virginia Hills</b>",
                                   "Elev. 1127m",
                                   "Spp. Lodgepole Pine",
                                   "CPP Region: C"),
                             paste(sep = "<br/>",
                                   "<b>Swan Hills</b>",
                                   "Elev. 1033m",
                                   "Spp. Lodgepole Pine",
                                   "CPP Region: C"),
                             paste(sep = "<br/>",
                                   "<b>Timeau</b>",
                                   "Elev. 1064m",
                                   "Spp. Lodgepole Pine",
                                   "CPP Region: C")
                             
                             
                       )) 
  
  pal <- colorFactor("viridis", domain = points$Species)
  
  #Spruce
  # Calling Lake (A)	55.16	113.09, 640m
  # Red Earth (C)	56.34	115.19, 518m
  # Carson Lake (D)	54.34	115.34, 1003m
  
  #Pine
  # Judy Creek (A) 54.24, 115.34, 1097m
  # Virginia Hills (B) 54.28, 115.51, 1127m
  # Swan Hills (C) 54.40, 115.30, 1033m
  # Timeau (D) 54.41, 115.18, 1064m
  
  output$mymap <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.Terrain,#OpenTopoMap,#Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(-114.6, 55.3, zoom = 7) %>%
      #addLegend(position = "topright", colors = points()$Col, labels = points()$Species
      addLegend(position = "topright", pal = pal, values = points$Species
      ) %>%
      addCircleMarkers(data = points, label = ~Name, popup = ~spp, color = ~pal(Species), opacity = 1)#~Col)
    
  })
  
  ##
  
  ##PANEL 1 - DATA VIEWER##
  
  output$mytable1 <- renderDataTable(
    datatable(pine[,input$show_vars1, drop = T],
              filter = "top",
              options = list(orderClasses = TRUE, autoWidth = TRUE, columnDefs = list(list(width = '10px'))))
  )
  
  output$mytable2 <- renderDataTable(
    datatable(spruce[,input$show_vars2, drop = T],  
              filter = "top",
              options = list(orderClasses = TRUE, autoWidth = TRUE, columnDefs = list(list(width = '10px'))))
  )
  
  ##
  
  ##DISTRIBUTION PANEL##  
  
  
  #produce trait checkbox based on species selection (distributions panel) 
  output$ui_dist_1 <- renderUI({
    #if (is.null(input$input_dist_species))
    # return()
    
    switch(input$input_dist_species,
           
           "White Spruce" = selectInput("dist_dynamic_trait", "Trait:", 
                                        choices=spruce.bv.names, multiple = FALSE,selected = NULL),
           
           "Lodgepole Pine" = selectInput("dist_dynamic_trait", "Trait:", 
                                          choices=pine.bv.names, multiple = FALSE,selected = NULL)
    )
  })
  
  #design factor checkbox
  output$ui_dist_2 <- renderUI({
    # if (is.null(input$input_dist_species))
    #   return()
    
    switch(input$input_dist_species,
           "White Spruce" = checkboxInput("design_fac_dist", label = "Include design factor?", value = FALSE),
           
           "Lodgepole Pine" = checkboxInput("design_fac_dist", label = "Include design factor?", value = FALSE))
    
  })
  
  #design factor drop down (distributions panel) 
  output$ui_dist_3 <- renderUI({
    
    # if (is.null(input$input_dist_species))
    #   return()
    
    if (isTRUE(input$design_fac_dist)){
      
      switch(input$input_dist_species,
             "White Spruce" = selectInput("dist_dynamic_design", "Design factor:", 
                                          choices=c("Site","Provenance"),selected = 'Site'),
             
             "Lodgepole Pine" = selectInput("dist_dynamic_design", "Design factor:", 
                                            choices=c("Site","Provenance"),selected = 'Site'))
      
    }
  })
  
  
  
  #distribution plots
  output$main_plot_distributions <- renderPlot({
    
    # if (is.null(input$input_dist_species))
    #   return()
    
    if (input$design_fac_dist==FALSE){
      
      #if(length(nchar(input$dist_dynamic_trait)>1)!=0){
      
      #if(nchar(input$dist_dynamic_trait)>1){
      
      switch(input$input_dist_species,
             
             "Lodgepole Pine" = ggplot(pine, aes_string(x=input$dist_dynamic_trait)) +
               geom_histogram(aes(y=..density..),alpha=0.5)+geom_density(), 
             
             "White Spruce"= ggplot(spruce, aes_string(x=input$dist_dynamic_trait)) +
               geom_histogram(aes(y=..density..),alpha=0.5)+geom_density())
      #}
      #}
    } else {
      
      if (input$design_fac_dist==TRUE){
        
        switch(input$input_dist_species,
               
               #if(input$input_dist_species=="Lodgepole Pine") trait.dist.vars<-pine.bv.names[which(input$dist_dynamic_trait%in%pine.bv.nice.names)],
               
               #if(input$input_dist_species=="White Spruce") trait.dist.vars<-spruce.bv.names[which(input$dist_dynamic_trait%in%spruce.bv.nice.names)],
               
               
               "Lodgepole Pine" = ggplot(pine, aes_string(x=input$dist_dynamic_trait, color=input$dist_dynamic_design)) +
                 geom_histogram(alpha=0.5, aes_string(fill=input$dist_dynamic_design))+
                 #geom_vline(data=mu, aes_string(xintercept=grp.mean, color=input$dist_dynamic_design),
                 #           linetype="dashed") +
                 theme(legend.position="top"),
               
               "White Spruce" = ggplot(pine, aes_string(x=input$dist_dynamic_trait, color=input$dist_dynamic_design)) +
                 geom_histogram( alpha=0.5, aes_string(fill=input$dist_dynamic_design))+
                 #geom_vline(data=mu, aes_string(xintercept=grp.mean, color=input$dist_dynamic_design),
                 #           linetype="dashed") +
                 theme(legend.position="top")
               
               
        )
      } 
      
    }
    
    
    
  })
  
  
  
  
  
  
  
  
  
  ##
  
  ##PANEL 2##
  
  #environment checkbox trait-trait correlations panel 
  output$ui1 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "White Spruce" = checkboxGroupInput("dynamic1", "Sites",
                                               choices = as.character(unique(spruce$Site)),
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic1", "Sites",
                                                 choices = as.character(unique(pine$Site)),
                                                 selected = "")
    )
  })
  
  #trait checkbox  trait-trait correlations panel 
  output$ui2 <- renderUI({
    if (is.null(input$input_type))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type,
           "White Spruce" = checkboxGroupInput("dynamic2", "Traits",
                                               choices = colnames(spruce)[c(spruce.phen.index,spruce.bv.index,spruce.A.bv.index,spruce.gen.gain.index,spruce.ped.gain.index)],
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic2", "Traits",
                                                 choices = colnames(pine)[c(pine.phen.index,pine.bv.index,pine.A.bv.index,pine.gen.gain.index,pine.ped.gain.index)],
                                                 selected = "")
    )
  })
  
  
  
  #main plot for trait-trait correlations
  output$main_plot <- renderPlot({
    if (is.null(input$input_type) | is.null(input$dynamic2) | length(input$dynamic2)<2 | is.null(input$dynamic1))
      return()
    
    if (!is.null(input$input_type) | !is.null(input$dynamic2) | length(input$dynamic2)>1){
      df_subset <- reactive({
        
        if(input$input_type=="White Spruce") data2 <- subset(pine, Site %in% input$dynamic1)
        if(input$input_type=="Lodgepole Pine") data2 <- subset(pine, Site %in% input$dynamic1)
        
        return(data2)
      })
    }
    
    
    switch(input$input_type,
           
           "Lodgepole Pine" = ggpairs(df_subset()[,input$dynamic2], aes(color = factor(df_subset()[,'Site']))),
           
           "White Spruce" =  ggpairs(df_subset()[,input$dynamic2], aes(color = factor(df_subset()[,'Site'])))
    )
    
  })
  
  ####
  
  ### PANEL 3 SITE-TRAIT RELATIONSHIPS ###
  
  #environment checkbox 
  output$ui8 <- renderUI({
    if (is.null(input$input_type7))
      return()
    
    # Depending on input$input_type7, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type7,
           "White Spruce" = checkboxGroupInput("dynamic8", "Sites",
                                               choices = as.character(unique(spruce$Site)),
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic8", "Sites",
                                                 choices = as.character(unique(pine$Site)),
                                                 selected = "")
    )
  })
  
  #trait checkbox 
  output$ui9 <- renderUI({
    if (is.null(input$input_type7))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type7,
           "White Spruce" = checkboxGroupInput("dynamic9", "Traits",
                                               choices = unique(gsub('.{5}$', '', colnames(spruce.msmt)[3:ncol(spruce.msmt)])),
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic9", "Traits",
                                                 choices = unique(gsub('.{5}$', '', colnames(spruce.msmt)[3:ncol(spruce.msmt)])),
                                                 selected = "")
    )
  })
  
  #main plot for trait-trait correlations
  output$main_plot7 <- renderPlot({
    if (is.null(input$input_type7) | is.null(input$dynamic8) | is.null(input$dynamic9))
      return()
    
    if (!is.null(input$input_type7) | !is.null(input$dynamic9) | length(input$dynamic8)>1 | length(input$dynamic9)==1){
      df_subset.msmt <- reactive({
        data2<-pine.msmt
        toMatch<-c('ID',input$dynamic8)
        data2<-data2[,grep(paste(toMatch,collapse="|"),colnames(pine.msmt))] #subset SITE
        
        toMatch<-c('ID',input$dynamic9)
        data2<-data2[,grep(paste(toMatch,collapse="|"),colnames(data2))] #then subset TRAIT
        
        
        #data2 <- tidyr::spread(data2,site,input$dynamic9)
        #data2<-aggregate(data2[,ncol(data2)] ~ mum + site, data = pine, FUN= "mean" ) #family means?
        return(data2)
      })
    }
    
    switch(input$input_type7,
           
           "Lodgepole Pine" = ggpairs(df_subset.msmt()[,-1]),
           
           "White Spruce" =  ggpairs(df_subset.msmt()[,-1])
    )
    
  })
  
  
  ###
  
  ### PANEL 4 SCATTER ZOOM ###
  
  #environment checkbox 
  output$ui6 <- renderUI({
    if (is.null(input$input_type3))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type3,
           "White Spruce" = checkboxGroupInput("dynamic5", "Sites",
                                               choices = as.character(unique(spruce$Site)),
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic5", "Sites",
                                                 choices = as.character(unique(pine$Site)),
                                                 selected = "")
    )
  })
  
  #trait checkbox 
  output$ui7 <- renderUI({
    if (is.null(input$input_type3))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type3,
           "White Spruce" = checkboxGroupInput("dynamic6", "Traits",
                                               choices = colnames(spruce)[c(spruce.phen.index,spruce.bv.index,spruce.A.bv.index,spruce.gen.gain.index,spruce.ped.gain.index)],
                                               selected = ""),
           
           "Lodgepole Pine" = checkboxGroupInput("dynamic6", "Traits",
                                                 choices = colnames(pine)[c(pine.phen.index,pine.bv.index,pine.A.bv.index,pine.gen.gain.index,pine.ped.gain.index)],
                                                 selected = "")
    )
  })
  
  #main plot for scatterplot zoom
  output$main_plot2 <- renderPlot({
    if (is.null(input$input_type3) | is.null(input$dynamic6) | length(input$dynamic6)<2 | length(input$dynamic6)>2)
      return()
    
    if (!is.null(input$input_type3) | !is.null(input$dynamic6) | length(input$dynamic6)==2){
      
      df_subset <- reactive({
        
        if(input$input_type3=="White Spruce") data2 <- spruce[which(spruce$Site%in%input$dynamic5),] 
        
        if(input$input_type3=="Lodgepole Pine") data2 <- pine[which(pine$Site%in%input$dynamic5),] 
        
        #subset(pine, site %in% input$dynamic5 & colnames(pine)%in%input$dynamic6)
        
        
        return(data2)
      })
    }
    
    switch(input$input_type3,
           
           "Lodgepole Pine" = ggplot(df_subset(), aes_string(x = colnames(pine)[which(colnames(pine)%in%input$dynamic6[1])], y = colnames(pine)[which(colnames(pine)%in%input$dynamic6[2])], color='Site')) + geom_point() + labs(fill="Site",x= input$dynamic6[1],y=input$dynamic6[2]) + scale_colour_manual(values=c("blue", "purple", "orange", "green")),
           
           "White Spruce" =  ggplot(df_subset(), aes_string(x = input$dynamic6[1], y = input$dynamic6[2], color='Site'), aes(color = factor(df_subset()[,'Site']))) + geom_point()+ labs(fill = "Site", x= input$dynamic6[1],y=input$dynamic6[2])
    ) 
  })
  
  
  #to subset dataset based on selections
  df_subset2 <- reactive({
    
    if(input$input_type3=="White Spruce") a <- spruce[which(spruce$Site%in%input$dynamic5),] 
    
    if(input$input_type3=="Lodgepole Pine") a <- pine[which(pine$Site%in%input$dynamic5),] 
    
    #a <- pine[which(pine$site%in%input$dynamic5),]
    
    #pine[which(pine$site%in%input$dynamic5),]
    #subset(pine, site %in% input$dynamic5 & colnames(pine)%in%input$dynamic6)
    return(a)
  })
  
  
  #Scatter plot zoom
  output$click_info <- renderPrint({
    if (is.null(input$input_type) | is.null(input$dynamic6) | length(input$dynamic6)<2 | length(input$dynamic6)>3)
      return()
    
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(na.omit(df_subset2()[,c('ID','Site',input$dynamic6)]), input$plot1_click, addDist = FALSE)
    #nearPoints(df_subset2()[,c('ID','site',input$dynamic6)], input$plot1_click, addDist = FALSE)
    
  })
  
  output$brush_info <- renderPrint({
    if (is.null(input$input_type) | is.null(input$dynamic6) | length(input$dynamic6)<2 | length(input$dynamic6)>3)
      return()
    #if (length(input$dynamic6)>2)
    
    
    brushedPoints(na.omit(df_subset2()[,c('ID','Site',input$dynamic6)]), input$plot1_brush)
    #brushedPoints(df_subset2()[,c('ID','site',input$dynamic6)], input$plot1_brush)
    
  })
  
  ####
  
  ### PANEL 5 SELECTIONS ###
  
  output$ui3 <- renderUI({
    if (is.null(input$input_type2))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type2,
           "White Spruce" = checkboxGroupInput("dynamic3", "Sites",
                                               choices = as.character(unique(spruce$Site)),#c("JUDY" = "JUDY",
                                               #"VIRG" = "VIRG",
                                               #"SWAN" = "SWAN",
                                               #"TIME" = "TIME"),
                                               
                                               selected = ""
           ),
           "Lodgepole Pine" = checkboxGroupInput("dynamic3", "Sites",
                                                 choices = as.character(unique(pine$Site)),#c("JUDY" = "JUDY",
                                                 #"VIRG" = "VIRG",
                                                 #"SWAN" = "SWAN",
                                                 #"TIME" = "TIME"),
                                                 selected = ""
           )
    )
  })
  
  output$ui4 <- renderUI({
    if (is.null(input$input_type2))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_type2,
           "White Spruce" = checkboxGroupInput("dynamic4", "Traits",
                                               choices = spruce.bv.nice.names,#c("HT30" = "HT30",
                                               #"DBH30" = "DBH30",
                                               #"NSW" = "NSW",
                                               #"MFA" = "MFA"),
                                               
                                               selected = ""
           ),
           "Lodgepole Pine" = checkboxGroupInput("dynamic4", "Traits",
                                                 choices = pine.bv.nice.names,#c("HT30" = "HT30",
                                                 #"DBH30" = "DBH30",
                                                 #"NSWGR" = "NSWGR",
                                                 #"WWD" = "WWD"),
                                                 selected = ""
           )
    )
  })
  
  output$ui5 <- renderUI({
    if (is.null(input$dynamic4) )
      return()
    
    num <- as.integer(length(input$dynamic4))
    
    lapply(1:num, function(i) {
      numericInput(paste0("Weight", i), label = paste(unique(input$dynamic4)[i],'Weight',sep=' '), value = 0,min = -1,max=1,step=0.05)
    })  
  })
  
  
  #subset data based on input parameters
  select.subset <- eventReactive(input$goButton, {
    if(input$goButton>=1){
      
      #subset pine by site + traits
      
      if(length(input$dynamic3)==0 | length(input$dynamic4)==0) return()
      
      weights<-c()
      
      #multiply traits by their weighting
      for(qq in 1:length(unique(input$dynamic4))){
        
        weights[qq]<-input[[paste0("Weight", qq)]]
        
      }
      
      if(sum(weights)==0) return() #if weights all equal zero
      
      # print(input$dynamic4)
      # print(which(pine.bv.nice.names%in%input$dynamic4))
      # print(pine.bv.names[which(pine.bv.nice.names%in%input$dynamic4)])
      
      if(input$input_type2=="Lodgepole Pine") {
        
        trait.name.temp<-pine.bv.names[which(pine.bv.nice.names%in%input$dynamic4)]
        data3<-pine[which(pine$Site%in%input$dynamic3),c('ID','Family',trait.name.temp)] 
        data3<-data3[complete.cases(data3),] #remove NA
        
        trait.name.ped.temp<-pine.A.bv.names[which(pine.bv.nice.names%in%input$dynamic4)]
        data.ped<-pine[which(pine$Site%in%input$dynamic3),c('ID','Family',trait.name.ped.temp)] 
        data.ped<-data.ped[complete.cases(data.ped),] #remove NA
        
      }
      
      if(input$input_type2=="White Spruce") {
        
        trait.name.temp<-spruce.bv.names[which(spruce.bv.nice.names%in%input$dynamic4)]
        data3<-spruce[which(spruce$Site%in%input$dynamic3),c('ID','Family',trait.name.temp)] 
        data3<-data3[complete.cases(data3),] #remove NA
        
        trait.name.ped.temp<-spruce.A.bv.names[which(spruce.bv.nice.names%in%input$dynamic4)]
        data.ped<-spruce[which(spruce$Site%in%input$dynamic3),c('ID','Family',trait.name.ped.temp)] 
        data.ped<-data.ped[complete.cases(data.ped),] #remove NA
        
      }
      
      
      #data3<-pine[which(pine$Site%in%input$dynamic3),c('ID','Family',)] 
      
      weights<-c()
      #multiply traits by their weighting
      for(qq in 1:length(unique(trait.name.temp))){
        
        data3[,unique(trait.name.temp)[qq]]<-data3[,unique(trait.name.temp)[qq]]*input[[paste0("Weight", qq)]]
        
        data.ped[,unique(trait.name.ped.temp)[qq]]<-data.ped[,unique(trait.name.ped.temp)[qq]]*input[[paste0("Weight", qq)]]
        
        weights[qq]<-input[[paste0("Weight", qq)]]
      }
      
      #sum traits
      #data3<-na.omit(data3) #remove NA
      # print(head(data3))
      # print(head(data.ped))
      
      
      data3$index<-rowSums(as.data.frame(data3[,trait.name.temp]),na.rm = T) #how to deal with NAs??
      data.ped$index<-rowSums(as.data.frame(data.ped[,trait.name.ped.temp]),na.rm = T) #how to deal with NAs??
      
      #apply coancestry restriction
      if(!is.null(input$coancestry) | input$coancestry!=0 | input$coancestry<=max(table(pine$Family))){
        
        print(paste('apply coancestry restriction ',input$coancestry,sep=''))
        
        require(data.table)
        d <- data.table(data3, key="index")
        data3 <- as.data.frame(d[, tail(.SD, input$coancestry), by=Family])
        
        d <- data.table(data.ped, key="index")
        data.ped <- as.data.frame(d[, tail(.SD, input$coancestry), by=Family])
      }
      
      data3<-data3[order(-data3$index,data3$Family),]
      data.ped<-data.ped[order(-data.ped$index,data.ped$Family),]
      
      # print('test ped')
      # print(head(data3))
      # print(head(data.ped))
      
      
      return(list(data3=data3,data.ped=data.ped,weights=weights))
    }
  })
  
  
  #index histogram
  output$distPlot <- renderPlot({
    if(is.null(select.subset()$data3))
      return()
    
    x    <- select.subset()$data3
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x$index, col = "#75AADB", border = "white",
         xlab = "Weighted Index (BV)",
         main = "Base Population")
    
    abline(v=input$selection.integer, col="red", lwd=3)
    
  })
  
  output$distPlot2 <- renderPlot({
    
    if(is.null(select.subset()$data3))
      return()
    
    x2 <- select.subset()$data3
    
    x2 <- x2[which(x2$index>input$selection.integer),]
    
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x2$index, col = "#75AADB", border = "white",
         xlab = "Weighted Index (BV)",
         main = "Selected Population")
    
    abline(v=mean(x2$index), col="red", lwd=3)
    
  })
  
  
  #expected gain table  
  output$tableGain <- renderTable({
    if (is.null(input$dynamic4) | is.null(select.subset()$data3))
      return()
    
    x5<-select.subset()$data3
    x5<-x5[order(x5$index,decreasing = T),]
    x5<-x5[which(x5$index>input$selection.integer),]
    
    # temp.gen<-select.subset()$data3
    # temp.gen<-temp.gen[order(temp.gen$index,decreasing = T),]
    # temp.gen<-temp.gen[which(temp.gen$index>input$selection.integer),]
    # 
    # temp.ped<-select.subset()$data.ped
    # temp.ped<-temp.ped[order(temp.ped$index,decreasing=T),]
    # temp.ped<-temp.ped[1:nrow(temp.gen),]
    
    if(input$input_type2=="White Spruce") {
      
      
      # temp.gen.gain<-spruce[which(spruce$ID%in%temp.gen$ID),c('ID',colnames(spruce)[spruce.gen.gain.index[which(spruce.bv.names%in%colnames(temp.gen))]])]
      # temp.ped.gain<-spruce[which(spruce$ID%in%temp.ped$ID),c('ID',colnames(spruce)[spruce.gen.gain.index[which(spruce.bv.names%in%colnames(temp.gen))]])]
      # 
      # df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F)
      # df.gain<-cbind(df.gain,rbind(colMeans(as.data.frame(temp.gen.gain[,-1])),colMeans(as.data.frame(temp.ped.gain[,-1]))))
      # 
      # colnames(df.gain)[2:ncol(df.gain)]<-spruce.bv.nice.names[which(spruce.bv.names%in%colnames(temp.gen))]
      
      
      temp.gen.gain<-spruce[which(spruce$ID%in%x5$ID),c('ID',colnames(spruce)[spruce.gen.gain.index[which(spruce.bv.names%in%colnames(x5))]])]
      temp.ped.gain<-spruce[which(spruce$ID%in%x5$ID),c('ID',colnames(spruce)[spruce.ped.gain.index[which(spruce.bv.names%in%colnames(x5))]])]
      df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F)
      
      
      
      df.gain<-cbind(df.gain,rbind(colMeans(as.data.frame(temp.gen.gain[,-1])),colMeans(as.data.frame(temp.ped.gain[,-1]))))
      
      colnames(df.gain)[2:ncol(df.gain)]<-spruce.bv.nice.names[which(spruce.bv.names%in%colnames(x5))]
      
    }
    
    if(input$input_type2=="Lodgepole Pine") {
      
      # temp.gen.gain<-pine[which(pine$ID%in%temp.gen$ID),c('ID',colnames(pine)[pine.gen.gain.index[which(pine.bv.names%in%colnames(temp.gen))]])]
      # temp.ped.gain<-pine[which(pine$ID%in%temp.ped$ID),c('ID',colnames(pine)[pine.gen.gain.index[which(pine.bv.names%in%colnames(temp.gen))]])]
      # 
      # df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F)
      # df.gain<-cbind(df.gain,rbind(colMeans(as.data.frame(temp.gen.gain[,-1])),colMeans(as.data.frame(temp.ped.gain[,-1]))))
      # 
      # colnames(df.gain)[2:ncol(df.gain)]<-pine.bv.nice.names[which(pine.bv.names%in%colnames(temp.gen))]
      
      
      temp.gen.gain<-pine[which(pine$ID%in%x5$ID),c('ID',colnames(pine)[pine.gen.gain.index[which(pine.bv.names%in%colnames(x5))]])]
      temp.ped.gain<-pine[which(pine$ID%in%x5$ID),c('ID',colnames(pine)[pine.ped.gain.index[which(pine.bv.names%in%colnames(x5))]])]
      
      df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F)
      
      df.gain<-cbind(df.gain,rbind(colMeans(as.data.frame(temp.gen.gain[,-1])),colMeans(as.data.frame(temp.ped.gain[,-1]))))
      
      
      
      colnames(df.gain)[2:ncol(df.gain)]<-pine.bv.nice.names[which(pine.bv.names%in%colnames(x5))]
      
    }
    
    
    
    
    
    
    
    # num2 <- colMeans(x5[input$dynamic4]) #as.integer(length(input$dynamic4))
    # num2<-num2/select.subset()$weights
    # df.gain<-as.data.frame(matrix(num2,1,length(input$dynamic4)))
    # print(df.gain)
    # names(df.gain)<-input$dynamic4
    
    
    return(df.gain)
    #data.frame(ID=c(0,0))
    
    # data.frame(lapply(1:num2, function(i) {
    #   input[[paste0("n_input_", i)]]
    # }))
  })
  
  output$tableNs <- renderTable({
    if (is.null(input$dynamic4) | is.null(select.subset()$data3))
      return()
    
    #num2 <- as.integer(length(input$dynamic4))
    x3<-select.subset()$data3
    x3 <- x3[which(x3$index>input$selection.integer),]
    
    df.Ns<-data.frame(N.fam=length(unique(x3$Family)))
    
    #Ns = 0.5 / mean(ped.info$matIBD.K[which(rownames(ped.info$matIBD.K)%in%x3$ID),which(colnames(ped.info$matIBD.K)%in%x3$ID)])
    #df.Ns<-as.data.frame(matrix(round(Ns,0),1,1))
    #names(df.Ns)<-'Ns'
    
    return(df.Ns)
    
  })
  
  #selected samples table
  output$table.candidate <- renderTable({
    #if (is.null(input$dynamic4) )
    #  return()
    
    #give subset of selected trees
    if (is.null(select.subset())) 
      return()
    
    x4<-select.subset()$data3
    x4 <- x4[which(x4$index>input$selection.integer),]
    colnames(x4)[ncol(x4)]<-'Index'
    
    print(head(x4))
    
    
    if(input$input_type2=="White Spruce") {
      
      
      temp.gen.gain2<-spruce[which(spruce$ID%in%x4$ID),c('Family','ID',colnames(spruce)[spruce.gen.gain.index[which(spruce.bv.names%in%colnames(x4))]])]
      temp.gen.gain2<-temp.gen.gain2[order(match(temp.gen.gain2$ID,x4$ID)),]
      
      colnames(x4)<-c('Family','ID',spruce.bv.nice.names[which(spruce.bv.names%in%colnames(x4))],'Index')   #set friendly names
      
      print(head(temp.gen.gain2))
      
      print('printing x4')
      print(head(x4))
      
      temp.gen.gain2<-data.frame(Family=x4$Family,ID=x4$ID,temp.gen.gain2[,3:ncol(temp.gen.gain2)],Index=x4$Index,stringsAsFactors = F)
      
      colnames(temp.gen.gain2)<-colnames(x4)
      
      print('temp.gen.gain2')
      print(head(temp.gen.gain2))
      x4<-temp.gen.gain2
      
      if(nrow(x4)>=20) return(x4[1:20,])
      if(nrow(x4<20)) return(x4)
      
      
    }
    
    #colnames(x4)<-c('ID',spruce.bv.nice.names[which(spruce.bv.names%in%colnames(x4))],'Index')
    
    #print(head(x4))
    
    #print(head(temp.gen.gain2))
    
    if(input$input_type2=="Lodgepole Pine") {
      
      temp.gen.gain2<-pine[which(pine$ID%in%x4$ID),c('Family','ID',colnames(pine)[pine.gen.gain.index[which(pine.bv.names%in%colnames(x4))]])]
      temp.gen.gain2<-temp.gen.gain2[order(match(temp.gen.gain2$ID,x4$ID)),]
      
      colnames(x4)<-c('Family','ID',pine.bv.nice.names[which(pine.bv.names%in%colnames(x4))],'Index')   #set friendly names
      
      print(head(temp.gen.gain2))
      
      print('printing x4')
      print(head(x4))
      
      temp.gen.gain2<-data.frame(Family=x4$Family,ID=x4$ID,temp.gen.gain2[,3:ncol(temp.gen.gain2)],Index=x4$Index,stringsAsFactors = F)
      
      colnames(temp.gen.gain2)<-colnames(x4)
      print(head(temp.gen.gain2))
      x4<-temp.gen.gain2
      
      if(nrow(x4)>=20) return(x4[1:20,])
      if(nrow(x4<20)) return(x4)
      
    }
    # 
    # df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F)
    # df.gain<-cbind(df.gain,rbind(colMeans(temp.gen.gain[,-1]),colMeans(temp.ped.gain[,-1])))
    # colnames(df.gain)[2:ncol(df.gain)]<-spruce.bv.nice.names[which(spruce.bv.names%in%colnames(x5))]
    
    
    
    
    
    
    
    
    
    
    
    
    
    #head(select.subset()$data3)
    
    
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
  
  output$slider.selection<-renderUI({
    
    if(is.null(select.subset()$data3))
      return()
    xx    <- select.subset()$data3
    
    sliderInput("selection.integer", "",
                min = round(min(xx$index),1), max = round(max(xx$index),1),
                value = round(mean(xx$index),1))
    
  })
  
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste('selections-', Sys.Date(), '.csv', sep='')
  #   },
  #   content = function(con) {
  #     write.csv(select.subset()$data3, con, quote = F,row.names = F,col.names = T)
  #   }
  # )
  
  
  #data download button
  output$output.data.forwards <- downloadHandler(
    filename = function() {
      paste('forwards.selections-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(select.subset()$data3[which(select.subset()$data3$index>input$selection.integer),], con, quote = F,row.names = F,col.names = T)
    }
  )
  
  
  output$downloadData.forwards <- renderUI({
    
    if (is.null(select.subset())) 
      return()
    
    downloadButton('output.data.forwards', 'Download')
    
  })
  
  
  
  output$forwards_selection_intensity_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Selection Intensity","</h4>"))
  })
  
  output$forwards_gain_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Expected genetic gain (%)","</h4>"))
  }) 
  
  output$forwards_Nfam_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Number of families","</h4>"))
  }) 
  
  output$forwards_selection_preview_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Selections Preview","</h4>"))
  }) 
  
  output$forwards_download_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Download Selections","</h4>"))
  }) 
  
  output$forwards_ped_compare_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Pedigree Comparison","</h4>"))
  }) 
  
  output$forwards_ped_compare_title <- renderText({ 
    if (is.null(select.subset())) 
      return()
    HTML(paste0("<h4>","Pedigree Comparison","</h4>"))
  }) 
  
  
  
  
  
  
  
  output$forwards_ped_compare_button <- renderUI({
    
    if (is.null(select.subset()))
      return()
    
    tagList(
      bsModal("modalPedigree_forward", "Pedigree Comparison Plot", "go_pedCompare_forward", size = "large",plotOutput("PedCompare_forward_plot"),downloadButton('downloadPedComparePlot_forward', 'Download')),
      actionButton("go_pedCompare_forward", "Go")
    )
    
  })
  
  output$downloadPedComparePlot_forward <- downloadHandler(
    filename = "Shinyplot_GenPedCompare.png",
    content = function(file) {
      png(file)
      plotPedComparePlot_forward()
      dev.off()
    })
  
  
  output$PedCompare_forward_plot <- renderPlot({
    
    if(is.null(select.subset()$data3))
      return()
    
    gen.sel.BV<-select.subset()$data3
    gen.sel.BV<-gen.sel.BV[order(gen.sel.BV$index,decreasing = T),]
    gen.sel.BV<-gen.sel.BV[which(gen.sel.BV$index>input$selection.integer),]
    
    ped.sel.BV<-select.subset()$data.ped
    ped.sel.BV<-ped.sel.BV[order(ped.sel.BV$index,decreasing=T),]
    ped.sel.BV<-ped.sel.BV[1:nrow(gen.sel.BV),]
    
    gen.ped<-list(Genomic=unique(gen.sel.BV$ID),Pedigree=unique(ped.sel.BV$ID))
    
    return(ggVennDiagram(gen.ped)+ggtitle('*Similarity in selections made by genomic and pedigree pedigree information.'))
    
  })
  
  
  plotPedComparePlot_forward <- function(){
    
    if(is.null(select.subset()$data3))
      return()
    
    gen.sel.BV<-select.subset()$data3
    gen.sel.BV<-gen.sel.BV[order(gen.sel.BV$index,decreasing = T),]
    gen.sel.BV<-gen.sel.BV[which(gen.sel.BV$index>input$selection.integer),]
    
    ped.sel.BV<-select.subset()$data.ped
    ped.sel.BV<-ped.sel.BV[order(ped.sel.BV$index,decreasing=T),]
    ped.sel.BV<-ped.sel.BV[1:nrow(gen.sel.BV),]
    
    gen.ped<-list(Genomic=unique(gen.sel.BV$ID),Pedigree=unique(ped.sel.BV$ID))
    
    return(ggVennDiagram(gen.ped)+ggtitle('*Similarity in selections made by genomic and pedigree pedigree information.'))
    
    
  }
  
  
  
  ####environment###
  ###load data here when published....###
  
  
  #Refresh button
  observeEvent(input$refresh_environment,{
    session$reload()
    return()
  })
  
  #climate vars
  
  #climate annual
  output$ui_climate_vars_annual <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_species_environment,
           "White Spruce" = pickerInput("clim.vars.annual", "Climate Variables (Annual)",
                                        choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Annual" & clim.data$Variable3=="Direct")])),
                                                       Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Annual" & clim.data$Variable3=="Derived")]))
                                        ),
                                        selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           ),
           "Lodgepole Pine" = pickerInput("clim.vars.annual", "Climate Variables (Annual)",
                                          choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Annual" & clim.data$Variable3=="Direct")])),
                                                         Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Annual" & clim.data$Variable3=="Derived")]))
                                          ),
                                          selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           )
           
           
           
    )
  })
  
  #climate seasonal
  output$ui_climate_vars_seasonal <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_species_environment,
           "White Spruce" = pickerInput("clim.vars.seasonal", "Climate Variables (Seasonal)",
                                        choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Seasonal" & clim.data$Variable3=="Direct")])),
                                                       Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Seasonal" & clim.data$Variable3=="Derived")]))
                                        ),
                                        selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           ),
           "Lodgepole Pine" = pickerInput("clim.vars.seasonal", "Climate Variables (Seasonal)",
                                          choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Seasonal" & clim.data$Variable3=="Direct")])),
                                                         Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Seasonal" & clim.data$Variable3=="Derived")]))
                                          ),
                                          selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           )
    )
  })
  
  #climate monthly
  output$ui_climate_vars_monthly <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_species_environment,
           "White Spruce" = pickerInput("clim.vars.monthly", "Climate Variables (Monthly)",
                                        choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Monthly" & clim.data$Variable3=="Direct")])),
                                                       Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="White Spruce" & clim.data$Variable2=="Monthly" & clim.data$Variable3=="Derived")]))
                                        ),
                                        selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           ),
           "Lodgepole Pine" = pickerInput("clim.vars.monthly", "Climate Variables (Monthly)",
                                          choices = list(Direct=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Monthly" & clim.data$Variable3=="Direct")])),
                                                         Derived=as.character(unique(clim.data$Variable1[which(clim.data$Species=="Lodgepole Pine" & clim.data$Variable2=="Monthly" & clim.data$Variable3=="Derived")]))
                                          ),
                                          selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           )
    )
  })
  
  #climate dates
  output$ui_climate_dates <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    switch(input$input_species_environment,
           
           # "White Spruce" = selectInput(
           #                   inputId =  "dateRange_clim", 
           #                   label = "Select time period:", 
           #                   choices = 2014:2019),
           #              
           #  "Lodgepole Pine" = selectInput(
           #                  inputId =  "dateRange_clim", 
           #                  label = "Select time period:", 
           #                  choices = 2014:2020)
           
           "White Spruce" =  dateRangeInput('dateRange_clim',
                                            label = HTML(paste0('Climate Variables ',a(href = 'http://climatebc.ca/downloads/help.pdf', '(?)'),' <br/> Date range input: yyyy')),
                                            
                                            start = paste0(min(clim.data$Year[clim.data$Species=="White Spruce"]),"-01-01"), end = paste0(max(clim.data$Year[clim.data$Species=="White Spruce"]),"-12-31"),
                                            min=paste0(min(clim.data$Year[clim.data$Species=="White Spruce"]),"-01-01"), max=paste0(max(clim.data$Year[clim.data$Species=="White Spruce"]),"-12-31"),
                                            format = "yyyy", startview = "year",autoclose = T
           ),
           "Lodgepole Pine" =  dateRangeInput('dateRange_clim',
                                              label = HTML(paste0('Climate Variables ',a(href = 'http://climatebc.ca/downloads/help.pdf', '(?)'),' <br/> Date range input: yyyy')),
                                              start = paste0(min(clim.data$Year[clim.data$Species=="Lodgepole Pine"]),"-01-01"), end = paste0(max(clim.data$Year[clim.data$Species=="Lodgepole Pine"]),"-12-31"),
                                              min=paste0(min(clim.data$Year[clim.data$Species=="Lodgepole Pine"]),"-01-01"), max=paste0(max(clim.data$Year[clim.data$Species=="Lodgepole Pine"]),"-12-31"),
                                              format = "yyyy", startview = "year",autoclose = T
           )
    )
  })
  
  #soil variables
  output$ui_soil_vars <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_species_environment,
           "White Spruce" = pickerInput("soil.vars", HTML(paste0('Soil Variables ',a(href = 'https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean', '(?)'))),
                                        
                                        choices = as.character(unique(soilGRIDS.data$Variable[which(soilGRIDS.data$Species=="White Spruce")])),
                                        selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           ),
           "Lodgepole Pine" = pickerInput("soil.vars", HTML(paste0('Soil Variables ',a(href = 'https://www.isric.org/explore/soilgrids/faq-soilgrids#What_do_the_filename_codes_mean', '(?)'))),
                                          choices = as.character(unique(soilGRIDS.data$Variable[which(soilGRIDS.data$Species=="Lodgepole Pine")])),
                                          selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           )
    )
  })
  
  #soil depths
  output$ui_soil_depths <- renderUI({
    if (is.null(input$input_species_environment))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$input_species_environment,
           "White Spruce" = pickerInput("soil.depths", "Soil Depths",
                                        choices = as.character(unique(soilGRIDS.data$Depth[which(soilGRIDS.data$Species=="White Spruce")])),
                                        selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           ),
           "Lodgepole Pine" = pickerInput("soil.depths", "Soil Depths",
                                          choices = as.character(unique(soilGRIDS.data$Depth[which(soilGRIDS.data$Species=="Lodgepole Pine")])),
                                          selected = "", multiple=TRUE,options = list(`actions-box` = TRUE)
           )
    )
  })
  
  #subset soil/climate data based on input parameters
  
  
  
  select.subset.env <- eventReactive(input$goButton_environment, {
    
    if(input$goButton_environment>=1){
      
      ##user input checks
      
      
      if(input$radio.env.visualize==4){
        
        if(is.null(input$clim.vars.annual) & is.null(input$clim.vars.seasonal) & is.null(input$clim.vars.monthly))  return()
        
        if(is.null(input$soil.vars) | is.null(input$soil.depths))  return()
        
      }
      
      if(input$radio.env.visualize==2){
        
        if(is.null(input$soil.vars) | is.null(input$soil.depths))  return()
        
      }
      
      if(input$radio.env.visualize==3){
        
        if(is.null(input$clim.vars.annual) & is.null(input$clim.vars.seasonal) & is.null(input$clim.vars.monthly))  return()
        
      }
      
      
      ##
      
      
      
      #subset by species/soil var/soil depth
      
      if(input$input_species_environment=="Lodgepole Pine") {
        
        if(!is.null(input$soil.vars) & !is.null(input$soil.depths)) {
          
          temp<-soilGRIDS.data[which(soilGRIDS.data$Species=="Lodgepole Pine" & soilGRIDS.data$Variable%in%input$soil.vars & soilGRIDS.data$Depth%in%input$soil.depths),]#; unique(temp$Depth); unique(temp$Variable)
          temp$Type<-"Soil"
        }
        
        if(is.null(input$soil.vars) & is.null(input$soil.depths)) temp<-NULL
        
        
        if(!is.null(input$clim.vars.annual) | !is.null(input$clim.vars.seasonal) | !is.null(input$clim.vars.monthly)) {
          
          temp2<-clim.data[which(clim.data$Species=="Lodgepole Pine"),]
          dates.year.sel<-seq(from=as.numeric(substr(input$dateRange_clim[1], start = 1, stop = 4)), to=as.numeric(substr(input$dateRange_clim[2], start = 1, stop = 4)))
          temp2<-temp2[which(temp2$Year%in%dates.year.sel),]
          
          clim.var.hold<-c()
          if(!is.null(input$clim.vars.annual)) clim.var.hold<-input$clim.vars.annual
          if(!is.null(input$clim.vars.seasonal)) clim.var.hold<-input$clim.vars.seasonal
          if(!is.null(input$clim.vars.monthly)) clim.var.hold<-input$clim.vars.monthly
          
          temp2<-temp2[which(temp2$Variable1%in%clim.var.hold),]
          temp2$Type<-"Climate"
          
        }
        
        if(is.null(input$clim.vars.annual) & is.null(input$clim.vars.seasonal) & is.null(input$clim.vars.monthly)) temp2<-NULL
        
        
      }
      
      if(input$input_species_environment=="White Spruce") {
        
        #temp<-soilGRIDS.data[which(soilGRIDS.data$Species=="White Spruce" & soilGRIDS.data$Variable%in%input$soil.vars & soilGRIDS.data$Depth%in%input$soil.depths),]#; unique(temp$Depth); unique(temp$Variable)
        if(!is.null(input$soil.vars) & !is.null(input$soil.depths)) {
          
          temp<-soilGRIDS.data[which(soilGRIDS.data$Species=="White Spruce" & soilGRIDS.data$Variable%in%input$soil.vars & soilGRIDS.data$Depth%in%input$soil.depths),]#; unique(temp$Depth); unique(temp$Variable)
          temp$Type<-"Soil"
        }
        
        if(is.null(input$soil.vars) & is.null(input$soil.depths)) temp<-NULL
        
        
        if(!is.null(input$clim.vars.annual) | !is.null(input$clim.vars.seasonal) | !is.null(input$clim.vars.monthly)) {
          
          temp2<-clim.data[which(clim.data$Species=="White Spruce"),]
          dates.year.sel<-seq(from=as.numeric(substr(input$dateRange_clim[1], start = 1, stop = 4)), to=as.numeric(substr(input$dateRange_clim[2], start = 1, stop = 4)))
          temp2<-temp2[which(temp2$Year%in%dates.year.sel),]
          
          clim.var.hold<-c()
          if(!is.null(input$clim.vars.annual)) clim.var.hold<-c(clim.var.hold,input$clim.vars.annual)
          if(!is.null(input$clim.vars.seasonal)) clim.var.hold<-c(clim.var.hold,input$clim.vars.seasonal)
          if(!is.null(input$clim.vars.monthly)) clim.var.hold<-c(clim.var.hold,input$clim.vars.monthly)
          
          temp2<-temp2[which(temp2$Variable1%in%clim.var.hold),]
          temp2$Type<-"Climate"
          
        }
        
        if(is.null(input$clim.vars.annual) & is.null(input$clim.vars.seasonal) & is.null(input$clim.vars.monthly)) temp2<-NULL
      }
      
      #convert to wide & merge soil & clim. if needed
      
      if(input$radio.env.visualize==1) temp5<-NULL
      
      if(input$radio.env.visualize!=1){
        
        if(!is.null(temp) & input$radio.env.visualize%in%c(2,4)){
          
          temp3<-temp
          temp3$Variable<-paste0(temp3$Variable,'_',temp3$Depth)
          temp3<-temp3[,c("Species","Site","Variable","Value")]
          temp3<-spread(data = temp3, key = Variable, value = Value)
          
        } 
        if(!is.null(temp2) & input$radio.env.visualize%in%c(3,4)){
          
          #if(is.null() & is.null() & is.null())
          
          temp4<-temp2
          temp4$Variable<-paste0(temp4$Variable1,'_',temp4$Year,'_',temp4$Variable2)
          temp4<-temp4[,c("Species","Site","Variable","Value")]
          temp4<-spread(data = temp4, key = Variable, value = Value)
          
        }
        
        if(input$radio.env.visualize==4){
          
          temp3<-temp3[order(match(temp3$Site,temp4$Site)),]
          temp5<-cbind(temp3,temp4[,-c(1,2)])
          # print("temp5")
          #print(temp5)
          
        }
        
        if(input$radio.env.visualize==2){
          temp5<-temp3
        }
        
        if(input$radio.env.visualize==3){
          temp5<-temp4
        }
        
      }
      
      
      # print(temp)
      # print(input$dateRange_clim)
      # print(input$clim.vars.annual)
      # print(input$clim.vars.seasonal)
      # print(input$clim.vars.monthly)
      # print(input$radio.env.visualize)
      # [1] "2000-03-02" "2006-08-23"
      # [1] "MAT" "TD" 
      # [1] "Tmin_at" "PPT_at" 
      # [1] "Tmax05" "Tmax09"
      # [1] "3"
      #print(temp5)
      
      
      #combine long formats for output
      if(!is.null(temp2) & is.null(temp)){
        temp6<-temp2
        temp6$Variable<-paste0(temp6$Variable1,'_',temp6$Year)
        temp6<-temp6[,c("Site","Variable","Value")]
        temp8<-temp6
      }
      if(!is.null(temp) & is.null(temp2)){
        temp7<-temp
        temp7$Variable<-paste0(temp7$Variable,'_',temp7$Depth)
        temp7<-temp7[,c("Site","Variable","Value")]
        temp8<-temp7
      }
      if(!is.null(temp) & !is.null(temp2)){
        temp6<-temp2
        temp6$Variable<-paste0(temp6$Variable1,'_',temp6$Year)
        temp6<-temp6[,c("Site","Variable","Value","Type")]
        temp7<-temp
        temp7$Variable<-paste0(temp7$Variable,'_',temp7$Depth)
        temp7<-temp7[,c("Site","Variable","Value","Type")]
        temp8<-rbind(temp7,temp6)
      }
      if(is.null(temp) & is.null(temp2)){
        temp8<-NULL
      }
      
      return(list(data=temp,data2=temp2,data3=temp5,data4=temp8))
    }
  })
  
  
  
  #env PCA plot
  
  output$env_PCA_plot <- renderPlot({
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data3)) 
      return()
    
    #temp<-select.subset.env()$data #convert to wide format
    #temp<-soilGRIDS.data[which(soilGRIDS.data$Species=="White Spruce"),] #debug
    # temp$Variable<-paste0(temp$Variable,'_',temp$Depth)
    # temp<-temp[,-which(colnames(temp)=='Depth')]
    # temp<-spread(data = temp, key = Variable, value = Value)
    # rownames(temp)<-temp$Site
    # temp<-temp[,-c(1,2)]
    #print(temp) #debug
    
    
    
    temp<-select.subset.env()$data3
    rownames(temp)<-temp$Site
    temp<-temp[,-c(1,2)]
    
    #check for number of variables (min, max) and variances (!=0)
    monomorph<-which(apply(temp,2,function(x) length(unique(x)))==1)
    
    if(length(monomorph)>0)  temp<-as.data.frame(temp[,-monomorph])
    
    if(ncol(temp)<2) {
      
      pca.error.text<- paste("PCA Error. p < 2")
      pca.error.plot<-ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = pca.error.text) + 
        theme_void()
      
      return(pca.error.plot) 
    } 
    #end check
    
    res.pca <- prcomp(temp, scale = TRUE)
    
    #fviz_eig(res.pca) #scree
    
    pca_plot<-fviz_pca_ind(res.pca,
                           col.ind = "cos2", # Color by the quality of representation
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                           repel = TRUE     # Avoid text overlapping
    )
    
    return(pca_plot)
    
  })
  
  #kernel matrix representation
  output$env_kernel_plot <- renderPlot({
    
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data3)) 
      return()
    
    
    W<-select.subset.env()$data3
    rownames(W)<-W$Site
    W<-W[,-c(1,2)]
    
    #check for number of variables (min, max) and variances (!=0)
    monomorph<-which(apply(W,2,function(x) length(unique(x)))==1)
    
    if(length(monomorph)>0)  W<-as.data.frame(W[,-monomorph])
    
    if(ncol(W)<2) {
      
      kernel.error.text<- paste("PCA Error. p < 2")
      kernel.error.plot<-ggplot() + 
        annotate("text", x = 4, y = 25, size=8, label = kernel.error.text) + 
        theme_void()
      
      return(kernel.error.plot) 
    } 
    #end check
    
    
    W<-as.matrix(W)
    W<-scale(W)
    W<-W/sqrt(ncol(W))
    WW<-tcrossprod(W) 
    WW<-WW/mean(diag(WW))
    
    #lessR::corReorder(WW,dendrogram = F)
    #image(WW)
    
    diag(WW)<-mean(WW[upper.tri(WW,diag = F)])
    
    x<-rownames(WW); y<-rownames(WW); temp<-expand.grid(X=x,Y=y,stringsAsFactors = F)
    
    temp$value<-WW[cbind(match(temp$X,rownames(WW)),match(temp$Y,rownames(WW)))]
    
    temp <- temp %>%
      mutate(text = paste0("x: ", X, "\n", "y: ", Y, "\n", "Value: ",round(value,2)))
    
    kernel_plot<-ggplot(temp, aes(X, Y, fill= value, text=text)) + 
      geom_tile() +
      theme(axis.title = element_blank())
    
    #ggplotly(p, tooltip="text")
    return(kernel_plot)
  })
  
  #selected samples table
  output$table.env.data <- renderTable({
    
    
    #give subset of selected trees
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data4)) 
      return()
    
    
    x.env <- select.subset.env()$data4
    
    if(nrow(x.env)>=20) { 
      x.env[1:20,]
    } else {
      x.env
    }
    #head(select.subset.backwards()$data.backwards)
    
    
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
  output$table_title_env <- renderText({ 
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data4)) 
      return()
    
    HTML(paste0("<h4>","Environment Data Preview","</h4>"))
    #h4("Environment Data Preview")
    
  })
  
  output$download_title_env <- renderText({ 
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data4)) 
      return()
    
    HTML(paste0("<h4>","Download Data","</h4>"))
    #h4("Environment Data Preview")
    
  })
  
  #data download button
  output$output.data.environment <- downloadHandler(
    filename = function() {
      paste('env_data-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(select.subset.env()$data4, con, quote = F,row.names = F,col.names = T)
    }
  )
  
  output$downloadData.environment <- renderUI({
    
    if (is.null(select.subset.env()) | is.null(select.subset.env()$data4)) 
      return()
    
    downloadButton('output.data.environment', 'Download Data')
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ####backwards selections
  observeEvent(input$refresh,{
    session$reload()
    return()
  })
  
  
  ### PANEL 6 BACKWARDS SELECTIONS ###
  
  #site selection
  output$ui_backwards_sites <- renderUI({
    if (is.null(input$species_input_backwards))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$species_input_backwards,
           "White Spruce" = checkboxGroupInput("sites.backwards", "Sites",
                                               choices = as.character(unique(spruce$Site)),
                                               selected = ""
           ),
           "Lodgepole Pine" = checkboxGroupInput("sites.backwards", "Sites",
                                                 choices = as.character(unique(pine$Site)),
                                                 selected = ""
           )
    )
  })
  
  output$ui_avg_trait_within_site <- renderUI({
    if (is.null(input$species_input_backwards))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$species_input_backwards,
           
           "White Spruce" = checkboxInput("avg.within.site", label = "Average by site?", value = FALSE),
           "Lodgepole Pine" = checkboxInput("avg.within.site", label = "Average by site?", value = FALSE)
           
           
    )
  })
  
  
  #trait selection
  output$ui_backwards_traits <- renderUI({
    if (is.null(input$species_input_backwards))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$species_input_backwards,
           "White Spruce" = checkboxGroupInput("traits.backwards", "Traits",
                                               choices = spruce.bv.nice.names,
                                               selected = ""
           ),
           "Lodgepole Pine" = checkboxGroupInput("traits.backwards", "Traits",
                                                 choices = pine.bv.nice.names,
                                                 selected = ""
           )
    )
  })
  
  #index weightings
  output$ui_backwards_trait_weights <- renderUI({
    if (is.null(input$traits.backwards) )
      return()
    
    num <- as.integer(length(input$traits.backwards))
    
    lapply(1:num, function(i) {
      numericInput(paste0("Weight", i), label = paste(unique(input$traits.backwards)[i],'Weight',sep=' '), value = 0,min = -1,max=1,step=0.05)
    })  
  })
  
  #subset data based on input parameters
  select.subset.backwards <- eventReactive(input$goButton_backwards, {
    if(input$goButton_backwards>=1){
      
      if(length(input$traits.backwards)==0 | length(input$sites.backwards)==0)
        return()
      
      weights2<-c()
      
      #multiply traits by their weighting
      for(qq in 1:length(input$traits.backwards)){
        
        weights2[qq]<-input[[paste0("Weight", qq)]]
        
      }
      
      if(sum(weights2)==0) return()
      
      #subset by species
      
      if(input$species_input_backwards=="Lodgepole Pine") {
        
        #average across all selected sites - genomic
        #regular BV
        trait.temp<-which(pine.bv.nice.names%in%input$traits.backwards)
        trait.temp<-colnames(pine)[pine.bv.index[trait.temp]]
        data.backwards<-pine[which(pine$Site%in%input$sites.backwards),c('ID','Family','Site',trait.temp)] 
        data.backwards<-data.backwards[complete.cases(data.backwards),] #remove NA
        data.backwards<-data.backwards[which(!data.backwards$Family%in%names(which(sort(table(data.backwards$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards[,-c(1:3)], list(data.backwards$Family), mean) #mean by family
        colnames(temp)<-c('Family',trait.temp)
        data.backwards1<-temp; rm(temp)
        
        print(paste('break 1'))
        
        #genetic gain BV
        gen.gain.names<-colnames(pine)[pine.gen.gain.index[which(pine.bv.nice.names%in%input$traits.backwards)]]
        data.backwards.gen.gain<-pine[which(pine$Site%in%input$sites.backwards),c('ID','Family','Site',gen.gain.names)] 
        data.backwards.gen.gain<-data.backwards.gen.gain[complete.cases(data.backwards.gen.gain),] #remove NA
        data.backwards.gen.gain<-data.backwards.gen.gain[which(!data.backwards.gen.gain$Family%in%names(which(sort(table(data.backwards.gen.gain$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.gen.gain[,-c(1:3)], list(data.backwards.gen.gain$Family), mean) #mean by family
        colnames(temp)[1]<-'Family'; data.backwards.gen.gain1<-temp; rm(temp)
        
        print(paste('break 2'))
        
        #average families within selected sites - genomic
        #regular bv
        temp<-data.backwards
        
        print(head(temp))
        
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        
        print(head(temp))
        
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        
        colnames(temp)<-c("Family","Site",trait.temp)
        
        print(head(temp))
        print(trait.temp)
        
        
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',trait.temp)] #reorder
        data.backwards2<-temp; rm(temp)
        
        print(paste('break 3'))
        
        #genetic gain bv
        temp<-data.backwards.gen.gain
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",gen.gain.names)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',gen.gain.names)] #reorder
        data.backwards.gen.gain2<-temp; rm(temp)
        
        print(paste('break 4'))
        
        ###average across all selected sites -pedigree
        #regular bv
        trait.temp2<-which(pine.bv.nice.names%in%input$traits.backwards)
        trait.temp2<-colnames(pine)[pine.A.bv.index[trait.temp2]]
        data.backwards.A<-pine[which(pine$Site%in%input$sites.backwards),c('ID','Family','Site',trait.temp2)] 
        data.backwards.A<-data.backwards.A[complete.cases(data.backwards.A),] #remove NA
        data.backwards.A<-data.backwards.A[which(!data.backwards.A$Family%in%names(which(sort(table(data.backwards.A$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.A[,-c(1:3)], list(data.backwards.A$Family), mean) #mean by family
        colnames(temp)<-c('Family',trait.temp2) 
        data.backwards.A1<-temp; rm(temp)
        
        print(paste('break 5'))         
        
        #genetic gain bv
        ped.gain.names<-colnames(pine)[pine.ped.gain.index[which(pine.bv.nice.names%in%input$traits.backwards)]]
        data.backwards.ped.gain<-pine[which(pine$Site%in%input$sites.backwards),c('ID','Family','Site',ped.gain.names)] 
        data.backwards.ped.gain<-data.backwards.ped.gain[complete.cases(data.backwards.ped.gain),] #remove NA
        data.backwards.ped.gain<-data.backwards.ped.gain[which(!data.backwards.ped.gain$Family%in%names(which(sort(table(data.backwards.ped.gain$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.ped.gain[,-c(1:3)], list(data.backwards.ped.gain$Family), mean) #mean by family
        colnames(temp)[1]<-'Family'; data.backwards.ped.gain1<-temp; rm(temp)
        
        print(paste('break 6'))
        
        #average families within selected sites - pedigree
        #regular bv
        temp<-data.backwards.A
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",trait.temp2)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',trait.temp2)] #reorder
        data.backwards.A2<-temp; rm(temp)
        
        print(paste('break 7'))
        
        #genetic gain bv
        temp<-data.backwards.ped.gain
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",ped.gain.names)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',ped.gain.names)] #reorder
        data.backwards.ped.gain2<-temp; rm(temp)
        
        print(paste('break 8'))
        
        ###
        
        
        
      }
      
      if(input$species_input_backwards=="White Spruce") {
        
        #average across all selected sites - genomic
        #regular BV
        trait.temp<-which(spruce.bv.nice.names%in%input$traits.backwards)
        trait.temp<-colnames(spruce)[spruce.bv.index[trait.temp]]
        data.backwards<-spruce[which(spruce$Site%in%input$sites.backwards),c('ID','Family','Site',trait.temp)] 
        data.backwards<-data.backwards[complete.cases(data.backwards),] #remove NA
        data.backwards<-data.backwards[which(!data.backwards$Family%in%names(which(sort(table(data.backwards$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards[,-c(1:3)], list(data.backwards$Family), mean) #mean by family
        colnames(temp)<-c('Family',trait.temp)
        data.backwards1<-temp; rm(temp)
        
        print(paste('break 1'))
        
        #genetic gain BV
        gen.gain.names<-colnames(spruce)[spruce.gen.gain.index[which(spruce.bv.nice.names%in%input$traits.backwards)]]
        data.backwards.gen.gain<-spruce[which(spruce$Site%in%input$sites.backwards),c('ID','Family','Site',gen.gain.names)] 
        data.backwards.gen.gain<-data.backwards.gen.gain[complete.cases(data.backwards.gen.gain),] #remove NA
        data.backwards.gen.gain<-data.backwards.gen.gain[which(!data.backwards.gen.gain$Family%in%names(which(sort(table(data.backwards.gen.gain$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.gen.gain[,-c(1:3)], list(data.backwards.gen.gain$Family), mean) #mean by family
        colnames(temp)[1]<-'Family'; data.backwards.gen.gain1<-temp; rm(temp)
        
        print(paste('break 2'))
        
        #average families within selected sites - genomic
        #regular bv
        temp<-data.backwards
        
        print(head(temp))
        
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        
        print(head(temp))
        
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        
        colnames(temp)<-c("Family","Site",trait.temp)
        
        print(head(temp))
        print(trait.temp)
        
        
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',trait.temp)] #reorder
        data.backwards2<-temp; rm(temp)
        
        print(paste('break 3'))
        
        #genetic gain bv
        temp<-data.backwards.gen.gain
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",gen.gain.names)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',gen.gain.names)] #reorder
        data.backwards.gen.gain2<-temp; rm(temp)
        
        print(paste('break 4'))
        
        ###average across all selected sites -pedigree
        #regular bv
        trait.temp2<-which(spruce.bv.nice.names%in%input$traits.backwards)
        trait.temp2<-colnames(spruce)[spruce.A.bv.index[trait.temp2]]
        data.backwards.A<-spruce[which(spruce$Site%in%input$sites.backwards),c('ID','Family','Site',trait.temp2)] 
        data.backwards.A<-data.backwards.A[complete.cases(data.backwards.A),] #remove NA
        data.backwards.A<-data.backwards.A[which(!data.backwards.A$Family%in%names(which(sort(table(data.backwards.A$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.A[,-c(1:3)], list(data.backwards.A$Family), mean) #mean by family
        colnames(temp)<-c('Family',trait.temp2) 
        data.backwards.A1<-temp; rm(temp)
        
        print(paste('break 5'))         
        
        #genetic gain bv
        ped.gain.names<-colnames(spruce)[spruce.ped.gain.index[which(spruce.bv.nice.names%in%input$traits.backwards)]]
        data.backwards.ped.gain<-spruce[which(spruce$Site%in%input$sites.backwards),c('ID','Family','Site',ped.gain.names)] 
        data.backwards.ped.gain<-data.backwards.ped.gain[complete.cases(data.backwards.ped.gain),] #remove NA
        data.backwards.ped.gain<-data.backwards.ped.gain[which(!data.backwards.ped.gain$Family%in%names(which(sort(table(data.backwards.ped.gain$Family))<5))),] #min 5 obs / family
        temp<-aggregate(data.backwards.ped.gain[,-c(1:3)], list(data.backwards.ped.gain$Family), mean) #mean by family
        colnames(temp)[1]<-'Family'; data.backwards.ped.gain1<-temp; rm(temp)
        
        print(paste('break 6'))
        
        #average families within selected sites - pedigree
        #regular bv
        temp<-data.backwards.A
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",trait.temp2)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',trait.temp2)] #reorder
        data.backwards.A2<-temp; rm(temp)
        
        print(paste('break 7'))
        
        #genetic gain bv
        temp<-data.backwards.ped.gain
        temp$ID2<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[which(!temp$ID2%in%names(which(sort(table(temp$ID2))<4))),] #min 4 obs / family_site
        temp<-temp[,-ncol(temp)]
        temp<-aggregate(temp[,-c(1:3)], list(temp$Family,temp$Site), mean) #mean by family within site
        colnames(temp)<-c("Family","Site",ped.gain.names)
        temp$ID<-paste0(temp$Family,'_',temp$Site)
        temp<-temp[c('ID','Family','Site',ped.gain.names)] #reorder
        data.backwards.ped.gain2<-temp; rm(temp)
        
        print(paste('break 8'))
        
        ###
        
        
        
      }
      
      weights2<-c()
      
      #multiply traits by their weighting
      for(qq in 1:length(unique(trait.temp))){
        
        data.backwards1[,unique(trait.temp)[qq]]<-data.backwards1[,unique(trait.temp)[qq]]*input[[paste0("Weight", qq)]]
        data.backwards2[,unique(trait.temp)[qq]]<-data.backwards2[,unique(trait.temp)[qq]]*input[[paste0("Weight", qq)]]
        
        data.backwards.A1[,unique(trait.temp2)[qq]]<-data.backwards.A1[,unique(trait.temp2)[qq]]*input[[paste0("Weight", qq)]]
        data.backwards.A2[,unique(trait.temp2)[qq]]<-data.backwards.A2[,unique(trait.temp2)[qq]]*input[[paste0("Weight", qq)]]
        
        
        weights2[qq]<-input[[paste0("Weight", qq)]]
        
        print(qq)
        
      }
      
      print(paste('break 9'))     
      
      #sum traits to calculate index
      if(length(trait.temp)==1){
        
        #genomic
        data.backwards1$Index<-data.backwards1[,trait.temp]
        data.backwards2$Index<-data.backwards2[,trait.temp]
        
        #pedigree
        data.backwards.A1$Index<-data.backwards.A1[,trait.temp2]
        data.backwards.A2$Index<-data.backwards.A2[,trait.temp2]
        
      }
      print(paste('break 10'))        
      if(length(trait.temp)>1){  
        #genomic
        data.backwards1$Index<-rowSums(data.backwards1[,trait.temp],na.rm = T) 
        data.backwards2$Index<-rowSums(data.backwards2[,trait.temp],na.rm = T)
        
        #pedigree
        data.backwards.A1$Index<-rowSums(data.backwards.A1[,trait.temp2],na.rm = T) 
        data.backwards.A2$Index<-rowSums(data.backwards.A2[,trait.temp2],na.rm = T)
        
      }
      
      if(input$avg.within.site==FALSE){
        data.backwards<-data.backwards1
      }
      if(input$avg.within.site==TRUE){
        data.backwards<-data.backwards2
      }
      
      return(list(data.backwards=data.backwards,
                  data.backwards2=data.backwards2,
                  data.backwards.A1=data.backwards.A1,
                  data.backwards.A2=data.backwards.A2,
                  gen.gain1=data.backwards.gen.gain1,
                  gen.gain2=data.backwards.gen.gain2,
                  ped.gain1=data.backwards.ped.gain1,
                  ped.gain2=data.backwards.ped.gain2,
                  weights=weights2))
    }
  })
  
  #index histogram
  output$backwards_family_base_Plot <- renderPlot({
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    
    x    <- select.subset.backwards()$data.backwards
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x$Index, col = "#75AADB", border = "white",
         xlab = "Weighted Index (Family BV)",
         main = "Base Population")
    
    abline(v=input$selection.integer, col="red", lwd=3)
    
  })
  
  #selected histogram
  output$backwards_family_selected_Plot <- renderPlot({
    
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    
    x2 <- select.subset.backwards()$data.backwards
    x2 <- x2[which(x2$Index>input$selection.integer),]
    
    
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x2$Index, col = "#75AADB", border = "white",
         xlab = "Weighted Index (Family BV)",
         main = "Selected Population")
    
    abline(v=mean(x2$Index), col="red", lwd=3)
    
  })
  
  # #expected gain table  
  output$tableGain_backwards <- renderTable({
    
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    
    #check if avg within sites
    if(input$avg.within.site==TRUE){
      
      gen.sel.BV <- select.subset.backwards()$data.backwards
      gen.sel.BV <- gen.sel.BV[which(gen.sel.BV$Index>input$selection.integer),]
      gen.sel.BV<-select.subset.backwards()$gen.gain2[which(select.subset.backwards()$gen.gain2$ID%in%gen.sel.BV$ID),]
      
      # ped.sel.BV <- select.subset.backwards()$data.backwards.A2
      # ped.sel.BV <- ped.sel.BV[order(ped.sel.BV$Index,decreasing = T),]
      # ped.sel.BV <- ped.sel.BV[1:nrow(gen.sel.BV),]
      # ped.sel.BV <- select.subset.backwards()$ped.gain2[which(select.subset.backwards()$ped.gain2$ID%in%ped.sel.BV$ID),]
      
      ped.sel.BV <- select.subset.backwards()$ped.gain2[which(select.subset.backwards()$ped.gain2$ID%in%gen.sel.BV$ID),]
      
      #get columns of traits & calculate column means
      df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F) #use nice names
      
      df.gain<-cbind(df.gain, rbind(colMeans(gen.sel.BV[4:ncol(gen.sel.BV)]),
                                    colMeans(ped.sel.BV[4:ncol(ped.sel.BV)])))
      
      colnames(df.gain)[2:ncol(df.gain)]<-input$traits.backwards
      
      return(df.gain)
      
      
    }
    
    if(input$avg.within.site==FALSE){
      
      gen.gain.IDs <- select.subset.backwards()$data.backwards
      gen.gain.IDs <- gen.gain.IDs[which(gen.gain.IDs$Index>input$selection.integer),]
      gen.gain.IDs <- gen.gain.IDs$Family
      
      print(paste('printing gen gain IDs'))
      print(head(gen.gain.IDs))
      
      gen.sel.BV<-select.subset.backwards()$gen.gain1
      gen.sel.BV<-gen.sel.BV[which(gen.sel.BV$Family%in%gen.gain.IDs),]
      
      print(head(gen.sel.BV))
      
      # ped.gain.IDs <- select.subset.backwards()$data.backwards.A1
      # ped.gain.IDs <- ped.gain.IDs[order(ped.gain.IDs$Index,decreasing = T),]
      # ped.gain.IDs <- ped.gain.IDs[1:nrow(gen.sel.BV),]
      # ped.gain.IDs <- ped.gain.IDs$Family
      # ped.sel.BV <- select.subset.backwards()$ped.gain1
      # ped.sel.BV <- ped.sel.BV[which(ped.sel.BV$Family%in%ped.gain.IDs),]
      
      ped.sel.BV <- select.subset.backwards()$ped.gain1
      ped.sel.BV <- ped.sel.BV[which(ped.sel.BV$Family%in%gen.gain.IDs),]
      
      #get columns of traits & calculate column means
      df.gain<-data.frame(Analysis=c("Genomic","Pedigree"),stringsAsFactors = F) #use nice names
      
      print(head(gen.sel.BV))
      print(head(ped.sel.BV))
      
      df.gain<-cbind(df.gain,rbind(colMeans(gen.sel.BV[2:ncol(gen.sel.BV)]),colMeans(ped.sel.BV[2:ncol(ped.sel.BV)]))) ##### should be column 3???????
      
      colnames(df.gain)[2:ncol(df.gain)]<-input$traits.backwards
      
      return(df.gain)
      
    }
    
  })
  
  #number of families selected table
  output$tableNfam_backwards <- renderTable({
    if (is.null(input$traits.backwards) | is.null(select.subset.backwards()$data.backwards))
      return()
    
    df.Ns<-select.subset.backwards()$data.backwards
    df.Ns <- df.Ns[which(df.Ns$Index>input$selection.integer),]
    df.Ns<-as.data.frame(matrix(round(length(unique(df.Ns$Family)),0)))
    names(df.Ns)<-'N.Fam'
    
    return(df.Ns)
    
  })
  
  #selected samples table
  output$table.candidate.backwards <- renderTable({
    #if (is.null(trait.temp) )
    #  return()
    
    #give subset of selected trees
    if (is.null(select.subset.backwards())) 
      return()
    
    x4 <- select.subset.backwards()$data.backwards
    x4 <- x4[which(x4$Index>input$selection.integer),]
    x4 <- x4[order(-x4$Index),]
    
    print(paste('x4'))
    print(head(x4))
    
    if(input$avg.within.site==TRUE){
      
      x5 <- select.subset.backwards()$gen.gain2
      x5 <- x5[which(x5$ID%in%x4$ID),]
      x5 <- x5[order(match(x5$ID,x4$ID)),]
      x5 <- cbind(x5,x4[,ncol(x4)])
      
      colnames(x5)<-colnames(x4)
      
      print(paste('x5'))
      print(head(x5))
    }
    if(input$avg.within.site==FALSE){
      
      x5 <- select.subset.backwards()$gen.gain1
      x5 <- x5[which(x5$Family%in%x4$Family),]
      x5 <- x5[order(match(x5$Family,x4$Family)),]
      x5 <- cbind(x5,x4[,ncol(x4)])
      
      colnames(x5)<-colnames(x4)
      
      print(paste('x5'))
      print(head(x5))
    }
    
    
    if(input$species_input_backwards=="White Spruce") {
      colnames(x5)[which(colnames(x5)%in%spruce.bv.names)]<-spruce.bv.nice.names[which(spruce.bv.names%in%colnames(x5))]# add nice names!!
      
    }
    if(input$species_input_backwards=="Lodgepole Pine") {
      colnames(x5)[which(colnames(x5)%in%pine.bv.names)]<-pine.bv.nice.names[which(pine.bv.names%in%colnames(x5))]# add nice names!!
      
    }
    
    if(nrow(x5)>=20) { 
      x5[1:20,]
    } else {
      x5
    }
    #head(select.subset.backwards()$data.backwards)
    
    
  }, 'include.rownames' = FALSE
  , 'include.colnames' = TRUE
  , 'sanitize.text.function' = function(x){x}
  )
  
  #selection intensity slider
  output$slider.selection.backwards<-renderUI({
    
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    xx    <- select.subset.backwards()$data.backwards
    
    sliderInput("selection.integer", "",
                min = round(min(xx$Index),1), max = round(max(xx$Index),1),
                value = round(mean(xx$Index),1))
    
  })
  
  
  # randomVals <- eventReactive(input$go, { #????
  #   runif(50)
  # })
  
  plotStability <- function(){
    
    #add check for monomorphic site... e.g. 'error 1 site in selections'
    
    if(length(unique(select.subset.backwards()$data.backwards2$Site))==1){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Error: Only 1 Site represented in selections."),
           cex = 1.6, col = "black")
    }
    
    if(length(unique(select.subset.backwards()$data.backwards2$Site))>1){
      
      selected.fams<-select.subset.backwards()$data.backwards$Family[which(select.subset.backwards()$data.backwards$Index>input$selection.integer)]
      #print(selected.fams)
      
      temp.plot<-select.subset.backwards()$data.backwards2[which(select.subset.backwards()$data.backwards2$Family%in%selected.fams),]
      #print(temp.plot)
      #print(select.subset.backwards()$data.backwards2)
      #print(unique(select.subset.backwards()$data.backwards2$Family))
      #print(unique(select.subset.backwards()$data.backwards2$Site))
      
      temp.plot$Family<-as.character(temp.plot$Family); temp.plot$Site<-as.character(temp.plot$Site)
      #print(temp.plot)
      
      return(interaction.plot(temp.plot$Site, temp.plot$Family, temp.plot$Index, fixed = TRUE, xlab = "Site", ylab = "Weighted Index (Family BV)", main='*Only represents selections observed in all selected testing environments.', legend = FALSE ))
      
    }
    
    
    
    
  }
  
  output$Stability_plot <- renderPlot({
    
    #add check for monomorphic site... e.g. 'error 1 site in selections'
    
    if(length(unique(select.subset.backwards()$data.backwards2$Site))==1){
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
      text(x = 0.5, y = 0.5, paste("Error: Only 1 Site represented in selections."),
           cex = 1.6, col = "black")
    }
    
    if(length(unique(select.subset.backwards()$data.backwards2$Site))>1){
      
      selected.fams<-select.subset.backwards()$data.backwards$Family[which(select.subset.backwards()$data.backwards$Index>input$selection.integer)]
      #print(selected.fams)
      
      temp.plot<-select.subset.backwards()$data.backwards2[which(select.subset.backwards()$data.backwards2$Family%in%selected.fams),]
      #print(temp.plot)
      #print(select.subset.backwards()$data.backwards2)
      #print(unique(select.subset.backwards()$data.backwards2$Family))
      #print(unique(select.subset.backwards()$data.backwards2$Site))
      
      temp.plot$Family<-as.character(temp.plot$Family); temp.plot$Site<-as.character(temp.plot$Site)
      #print(temp.plot)
      
      return(interaction.plot(temp.plot$Site, temp.plot$Family, temp.plot$Index, fixed = TRUE, xlab = "Site", ylab = "Weighted Index (Family BV)", main='*Only represents selections observed in all selected testing environments.', legend = FALSE ))
      
    }
    
  })
  
  output$downloadStabilityPlot <- downloadHandler(
    filename = "Shinyplot_FamStability.png",
    content = function(file) {
      png(file)
      plotStability()
      dev.off()
    })
  
  plotPedCompare <- function(){
    
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    
    #check if avg within sites
    if(input$avg.within.site==TRUE){
      
      gen.sel.BV <- select.subset.backwards()$data.backwards
      gen.sel.BV <- gen.sel.BV[which(gen.sel.BV$Index>input$selection.integer),]
      
      ped.sel.BV <- select.subset.backwards()$data.backwards.A2
      ped.sel.BV <- ped.sel.BV[order(ped.sel.BV$Index,decreasing = T),]
      ped.sel.BV <- ped.sel.BV[1:nrow(gen.sel.BV),]
      
      gen.ped<-list(Genomic=unique(gen.sel.BV$ID),Pedigree=unique(ped.sel.BV$ID))
      
      return(ggVennDiagram(gen.ped))
      
    }
    if(input$avg.within.site==FALSE){
      
      gen.sel.BV <- select.subset.backwards()$data.backwards
      gen.sel.BV <- gen.sel.BV[which(gen.sel.BV$Index>input$selection.integer),]
      
      ped.sel.BV <- select.subset.backwards()$data.backwards.A1
      ped.sel.BV <- ped.sel.BV[order(ped.sel.BV$Index,decreasing = T),]
      ped.sel.BV <- ped.sel.BV[1:nrow(gen.sel.BV),]
      
      gen.ped<-list(Genomic=unique(gen.sel.BV$Family),Pedigree=unique(ped.sel.BV$Family))
      
      return(ggVennDiagram(gen.ped))
      
    }
    
  }
  
  output$PedCompare_plot <- renderPlot({
    
    if(is.null(select.subset.backwards()$data.backwards))
      return()
    
    #check if avg within sites
    if(input$avg.within.site==TRUE){
      
      gen.sel.BV <- select.subset.backwards()$data.backwards
      gen.sel.BV <- gen.sel.BV[which(gen.sel.BV$Index>input$selection.integer),]
      
      ped.sel.BV <- select.subset.backwards()$data.backwards.A2
      ped.sel.BV <- ped.sel.BV[order(ped.sel.BV$Index,decreasing = T),]
      ped.sel.BV <- ped.sel.BV[1:nrow(gen.sel.BV),]
      
      gen.ped<-list(Genomic=unique(gen.sel.BV$ID),Pedigree=unique(ped.sel.BV$ID))
      
      return(ggVennDiagram(gen.ped)+ggtitle('*Similarity in selections made by genomic and pedigree pedigree information.'))
      
    }
    if(input$avg.within.site==FALSE){
      
      gen.sel.BV <- select.subset.backwards()$data.backwards
      gen.sel.BV <- gen.sel.BV[which(gen.sel.BV$Index>input$selection.integer),]
      
      ped.sel.BV <- select.subset.backwards()$data.backwards.A1
      ped.sel.BV <- ped.sel.BV[order(ped.sel.BV$Index,decreasing = T),]
      ped.sel.BV <- ped.sel.BV[1:nrow(gen.sel.BV),]
      
      gen.ped<-list(Genomic=unique(gen.sel.BV$Family),Pedigree=unique(ped.sel.BV$Family))
      
      return(ggVennDiagram(gen.ped)+ggtitle('*Similarity in selections made by genomic and pedigree information.'))
      
      
      
    }
    
    
  })
  
  output$downloadPedComparePlot <- downloadHandler(
    filename = "Shinyplot_GenPedCompare.png",
    content = function(file) {
      png(file)
      plotPedCompare()
      dev.off()
    })
  
  output$backwards_selection_intensity_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Selection Intensity","</h4>"))
  })
  
  output$backwards_gain_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Expected genetic gain (%)","</h4>"))
  }) 
  
  output$backwards_Nfam_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Number of families","</h4>"))
  }) 
  
  output$backwards_selection_preview_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Selections Preview","</h4>"))
  }) 
  
  output$backwards_download_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Download Selections","</h4>"))
  }) 
  
  output$backwards_ped_compare_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Pedigree Comparison","</h4>"))
  }) 
  
  output$backwards_stability_title <- renderText({ 
    if (is.null(select.subset.backwards())) 
      return()
    HTML(paste0("<h4>","Site-Site Stability Plot","</h4>"))
  }) 
  
  
  
  #data download button
  output$output.data.backwards <- downloadHandler(
    filename = function() {
      paste('backwards.selections-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(select.subset.backwards()$data.backwards[which(select.subset.backwards()$data.backwards$Index>input$selection.integer),], con, quote = F,row.names = F,col.names = T)
    }
  )
  
  
  output$downloadData.backwards <- renderUI({
    
    if (is.null(select.subset.backwards())) 
      return()
    
    downloadButton('output.data.backwards', 'Download')
    
  })
  
  output$backwards_ped_compare_button <- renderUI({
    
    if (is.null(select.subset.backwards())) 
      return()
    
    tagList(
      bsModal("modalPedigree", "Pedigree Comparison Plot", "go_pedCompare", size = "large",plotOutput("PedCompare_plot"),downloadButton('downloadPedComparePlot', 'Download')),
      actionButton("go_pedCompare", "Go")
    )
    
  })
  
  output$backwards_stability_button <- renderUI({
    
    if (is.null(select.subset.backwards())) 
      return()
    tagList(
      bsModal("modalStability", "Family Means x Site Stability Plot", "go_Stability", size = "large",plotOutput("Stability_plot"),downloadButton('downloadStabilityPlot', 'Download')),
      actionButton("go_Stability", "Go")
      
    )
    
    
    
  })
  
  
  
  
  
  
  
  
  
  ####
  
  
}



# Create Shiny object
shinyApp(ui = ui, server = server)








