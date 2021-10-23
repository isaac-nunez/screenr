#Este documento será el que ya use para ScreenR.
#Pendientes importantes:
#1. Decidir cuáles serán las variables que incluiré en el tamizaje del adulto sano
#2. Diseño general de la apariencia
#3. Cuál será el sitio donde se haga el hosting de la app?

library(ggplot2)
library(shiny)
library(tidyverse)
library(vroom)

###################
#Guía para tamizaje en diabetes general https://care.diabetesjournals.org/content/44/Supplement_1/S40.full-text.pdf
#Guía para tamizaje en diabetes cardiovascular https://care.diabetesjournals.org/content/44/Supplement_1/S125.full-text.pdf
#Guía para tamizaje CV https://www.ahajournals.org/doi/epub/10.1161/CIR.0000000000000678


###################
tamizajes_metabolicos <- function(diabetes, edad, sexo, cv, imc, has, tabaco, tab){
  metabolicos <- tibble(enfermedad = c("-Diabetes mellitus tipo 2 (comorbilidades): ", 
                                       "-Diabetes mellitus tipo 2 (tamizajes clínicos): ",
                                       "-Diabetes mellitus tipo 2 (tamizajes de laboratorio): ",
                                       "-Diabetes mellitus tipo 2 (prevención CV primaria): ",
                                       "-Diabetes mellitus tipo 2 (prevención CV secundaria): ",
                                       "-Riesgo cardiovascular: ",
                                       "-Sobrepeso/Obesidad: ",
                                       "-Tabaquismo: "),
                        indicacion = c(
                        case_when(
                        diabetes == "No"~"",
                        diabetes == "Sí"&has=="No"& imc<=25 ~ "Buscar HAS y sobrepeso/obesidad en cada visita, exploración oftalmológica anual, 
                        buscar MAFLD si química hepática anormal",
                        diabetes == "Sí"&has=="Sí"& imc<=25 ~ "Buscar sobrepeso/obesidad en cada visita, exploración oftalmológica anual, 
                        buscar MAFLD si química hepática anormal",
                        diabetes == "Sí"&has=="Sí"& imc>25 ~ "Exploración oftalmológica anual, buscar MAFLD si química hepática anormal"
                        ),
                        case_when(diabetes == "No"~"",
                                  diabetes == "Sí"~ "Exploración exhaustiva de pies anual (más frecuente si tiene complicaciones documentadas), 
                                  buscar acantosis y lipodistrofia en sitio de inyección (si usa insulina)"),
                        case_when(diabetes == "No"~"",
                                  diabetes == "Sí"~"Cada 3 meses: Hba1c. Anual: perfil de lípidos, relación alb/cr en orina, química hepática, 
                                  creatinina y TFG, potasio si usa IECA/ARA2/diurético"),
                        case_when(diabetes == "No"~"",
                                  diabetes == "Sí"&cv =="Sí"~"",
                                  diabetes == "Sí"&cv =="No"&edad>=40&edad<=75&has=="No"&imc<30&tabaco=="No"~"Estatinas con intensidad moderada",
                                  diabetes == "Sí"&cv =="No"&edad>=20&edad<=39&(has=="Sí"|imc>=30|tabaco=="Sí")~"Considerar estatinas con intensidad moderada, aspirina como decisión individualizada",
                                  diabetes == "Sí"&cv =="No"&edad>=40&edad<=75&(has=="Sí"|imc>=30|tabaco=="Sí")~"Considerar estatinas con alta intensidad, aspirina como decisión individualizada"
                                  ),
                        case_when(diabetes == "No"~"",
                                  diabetes == "Sí"&cv =="No"~"",
                                  diabetes == "Sí"&cv =="Sí"~"Estatinas con alta intensidad, aspirina, considerar iSGLT-2 o agonista GLP-1, considerar fibrato si triglicéridos >=500"
                                  ),
                        case_when(diabetes == "Sí"|cv == "Sí" ~"",
                                  (diabetes == "No"&cv == "No")&edad >=40&edad<=75 ~"Si colesterol LDL >190 iniciar estatina de alta intensidad, si menor calcular riesgo CV a 10 años e iniciar 
                                  estatina con moderada intensidad si riesgo ≥7.5% y <20% (alternativa es medir calcio coronario para decidir), alta intensidad si riesgo >20%"),
                        case_when(imc>25&imc<35~"Cambios en estilo de vida, considerar enviar a nutrición clínica",
                                  imc>35~"Considerar enviar a clínica de obesidad",
                                  imc<25~""),
                        case_when(tabaco == "Sí"~"Medidas para suspender tabaco, considerar enviar a clínica",
                                  tabaco == "No"~"")
                        ),
                        referencia = c(rep("Standards of Medical Care in Diabetes 2021. Diabetes Care 2021;44(Suppl. 1):S40–S52 y S125-S150.
                                           Disponible en: https://care.diabetesjournals.org/content/44/Supplement_1", times = 5),
                                       rep("2019 ACC/AHA Guideline on the Primary Prevention of Cardiovascular Disease. Circulation. 2019;140:e596–e646.
                                           Disponible en: https://www.ahajournals.org/doi/10.1161/CIR.0000000000000678", times = 3))) %>% filter(indicacion!="")
  
  if_else(tab==1,
  paste(str_c(metabolicos$enfermedad, metabolicos$indicacion), collapse = "\n"),
  paste(str_c(c("-Diabetes mellitus tipo 2: ", "-Riesgo cardiovascular, sobrepeso/obesidad y tabaquismo: "), metabolicos$referencia)[1:2], collapse = "\n"))
}


###################
#Función para determinar si le toca o no tamizaje para cuestiones específicas a sexo
tamizajes_sexo <- function(edad, sexo, tab){
  por_sexo <- tibble(tamizaje=c("-Osteoporosis: "),
                     indicacion=c(case_when(
                       edad>=65&sexo=="Femenino"~"Densitometría ósea de columna y fémur",
                       edad>=70&sexo=="Masculino"~"Densitometría ósea de columna y fémur",
                       (edad>=35&edad<65)&sexo=="Femenino"~"Densitometría ósea de columna y fémur si postmenopausia y FRAX elevado, antecedente de fractura por fragilidad u otro factor de riesgo",
                       (edad<70&sexo=="Masculino")|(edad<35&sexo=="Femenino")~"Densitometría ósea de columna y fémur en caso de uso crónico de esteroide 
                       (>=5mg prednisona por 3 meses) u otro factor fuertemente asociado")),
                     referencia = "Screening for Osteoporosis to Prevent Fractures: US Preventive Services Task Force Recommendation Statement. JAMA. 2018;319(24):2521-2531.
                     Disponible en: https://jamanetwork.com/journals/jama/fullarticle/2685995")
  
  if_else(tab == 1,
  paste(str_c(por_sexo$tamizaje, por_sexo$indicacion), collapse = "\n"),
  paste(str_c(por_sexo$tamizaje, por_sexo$referencia), collapse = "\n"))
}

###################
#Los tamizajes oncológicos específicos a sexo van dentro de tamizajes oncológicos
#Guía de CA Cu https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/cervical-cancer-screening
#Guía CA colon https://journals.lww.com/ajg/fulltext/2021/03000/acg_clinical_guidelines__colorectal_cancer.14.aspx
#Guía CA de mama https://www.cdc.gov/cancer/breast/pdf/breast-cancer-screening-guidelines-508.pdf
#Guía CA de pulmón https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/lung-cancer-screening
#Guía de CA de próstata https://www.uspreventiveservicestaskforce.org/uspstf/recommendation/prostate-cancer-screening

tamizajes_oncologicos <- function(edad, sexo, tabaco, tab){
  oncologicos <- tibble(cancer=c("-Cervicouterino: ", "-Colon: ", "-Mama: ", "-Pulmón: ", "-Próstata: "),
                        indicacion=c(case_when(sexo == "Femenino"&(edad<21|edad>65)~"",
                                               sexo == "Femenino"&edad>=21&edad<=29~"Citología cervical cada 3 años",
                                               sexo == "Femenino"&edad>29&edad<=65~"Citología cervical cada 3 años, captura de híbridos cada 5 años, o ambas cada 5 años",
                                                       sexo == "Masculino"~""),
                                     case_when(edad>=45&edad<=75~"Tamizaje anual con FIT o cada 10 años con colonoscopia",
                                               edad>75~"Individualizar decisión de tamizar",
                                               edad<45~"Colonoscopia a los 40 años o 10 años previo a familiar con diagnóstico más temprano solo si un familiar de primer grado 
                                               con pólipo avanzado o CA colon <60 años o 2 familiares con CA colon cualquier edad. Repetir cada 5 años "),
                                             case_when(sexo == "Femenino"&edad >=40 & edad <50~"Individualizar decisión de iniciar tamizaje, mastografía anual en caso de iniciar",
                                                       sexo == "Femenino"&edad>=50 & edad <=55~"Mastografía anual",
                                                       sexo == "Femenino"&edad>55&edad<=75~"Mastografía anual o cada dos años según hallazgos previos",
                                                       sexo == "Femenino"&(edad<40|edad>75)~"",
                                                      sexo == "Masculino"~""),
                                             case_when(tabaco == "Sí"&edad>=50 & edad <=80~"TAC de tórax de baja dosis anual si consumo >=20 paquetes-año, no realizar si más de 15 años sin fumar o expectativa de vida corta",
                                                       tabaco == "Sí"&edad<50|edad>80~"",
                                                       tabaco == "No"~""),
                                             case_when(sexo == "Femenino"~"",
                                                       sexo == "Masculino"&edad>=55&edad<=70~"Decisión de tamizar deberá ser consensuada con el paciente",
                                                       sexo == "Masculino"&edad<55|edad>70~"")
                                     ),
                        referencia = c("Screening for cervical cancer: U.S. Preventive Services Task Force recommendation statement. U.S. Preventive Services Task Force. JAMA 2018;320:674–86. 
                                       Disponible en: https://jamanetwork.com/journals/jama/fullarticle/2697704",
                                       "ACG Clinical Guidelines: Colorectal Cancer Screening 2021. Am J Gastroenterol 2021;116:458–479.
                                       Disponible en: https://journals.lww.com/ajg/fulltext/2021/03000/acg_clinical_guidelines__colorectal_cancer.14.aspx",
                                       "Breast cancer screening guideline synopsis. CDC, 2021. 
                                       Disponible en: https://www.cdc.gov/cancer/breast/pdf/breast-cancer-screening-guidelines-508.pdf",
                                       "Screening for Lung Cancer US Preventive Services Task Force Recommendation Statement. JAMA. 2021;325(10):962-970. 
                                       Disponible en: https://jamanetwork.com/journals/jama/fullarticle/2777244",
                                       "Screening for Prostate Cancer US Preventive Services Task Force Recommendation Statement. JAMA. 2018;319(18):1901-1913.
                                       Disponible en: https://jamanetwork.com/journals/jama/fullarticle/2680553")) %>% filter(indicacion!="")
 if_else(tab == 1, 
  paste(str_c(oncologicos$cancer, oncologicos$indicacion), collapse = "\n"),
  paste(str_c(oncologicos$cancer, oncologicos$referencia), collapse = "\n"))
}

###################
#Función para determinar si le toca o no tamizaje para VIH o alguna otra infección. OJO: TAMIZAJE
tamizajes_infectologicos <- function(x, tab){
  infecciosos <- tibble(infeccion=c("-VIH: ", "-Hepatitis B: ", "-Hepatitis C: ",
                                    "-Tuberculosis: ", "-Sífilis: ", "-Clamidia/Gonorrea: "),
                        indicacion=c(if_else(x>=18&x<=65, "Al menos una prueba en la vida en quienes no tienen factores de riesgo", ""),
                                     "",
                                     if_else(as.numeric(format(Sys.time(), "%Y"))-x>=1945&
                                               as.numeric(format(Sys.time(), "%Y"))-x<=1965,
                                             "Prueba de anticuerpos en quienes nacieron entre 1945-1965", ""),
                                     "", "", ""),
                        referencia = c("Screening for HIV Infection: US Preventive Services Task Force Recommendation Statement.JAMA. 2019 Jun 18;321(23):2326-2336. 
                                       Disponible en: https://jamanetwork.com/journals/jama/fullarticle/2735345",
                                       "", 
                                       "CDC Recommendations for Hepatitis C Screening Among Adults — United States, 2020. 
                                       MMWR Recomm Rep 2020;69(No. RR-2):1–17. Disponible en: https://www.cdc.gov/mmwr/volumes/69/rr/rr6902a1.htm",
                                       rep("", times = 3))) %>% filter(indicacion!="")
  if_else(tab ==1,
  paste(str_c(infecciosos$infeccion, infecciosos$indicacion), collapse = "\n"),
  paste(str_c(infecciosos$infeccion, infecciosos$referencia), collapse = "\n"))
}

#1945-1965
#Función para determinar si le toca o no una vacuna determinada
#Obtenido tal cual de la página de la CDC (https://www.cdc.gov/vaccines/schedules/hcp/imz/adult.html)
edad <- function(x){
  case_when(x>=18&x<=26~"18-26",
            x>=27&x<=49~"27-49",
            x>=50&x<=64~"50-64",
            x>=65~ ">=65")
}

tamizajes_vacunas <- function(x, dm2, cv, tab){
 age <- edad(x)
  vacunacion <- tibble(vacuna = c("-SARS-CoV-2: ", "-Influenza: ","-Tétanos/difteria/pertusis: ", "-Triple viral: ",
                               "-Varicela: ", "-Zoster: ", "-VPH: ", "-Neumococo 13V: ",
                               "-Neumococo 23V: ", "-Hepatitis A:", "-Hepatitis B: ",
                               "-Meningococo cuádruple: ", "-Meningococo B: ",
                               "H. influenzae tipo B: "),
                   indicacion = c("Una, dos o tres dosis dependiendo del esquema y de la disponibilidad de boosters", 
                   "Una dosis anual", 
                                  "Refuerzo cada 10 años, en caso de embarazo o en caso de herida",
                                  ifelse(age == ">=65","",
                                         "Una dosis en caso de no tener antecedente de infección/vacunación"),
                                  "Dos dosis con 4 semanas de separación en caso de no tener antecedente de infección/vacunación",
                                  if_else(x>=50, "Dos dosis con 2-6 meses de separación",
                                          ""), 
                                  if_else(x>=18&x<=45, "Tres dosis (0, 2, 6 meses) en caso de no haberse ya vacunado", ""),
                                  if_else(x>= 65, "Es posible administrar dosis única, 
                           dar un año previo a neumococo 23V en caso de que se quieran dar ambas", ""),
                                  if_else(x>= 65, "Dosis única, si dada previo a los 65 años repetir 5 años después, dar al menos
                           un año después de neumococo 13V", ""),
                                  "", #Hepatitis A
                                  if_else(x<60&(dm2=="Sí"|cv == "Sí"), "2-3 dosis dependiendo del esquema en caso de no tener evidencia de inmunidad",
                                          ""),
                                  "", #meningococo A  C, W, Y)
                   "",#Meningococo B
                   ""),
                   referencias = c(rep("Esquema de vacunación para población adulta sana, Estados Unidos, 2021. CDC, 2021.
                                       Disponible en: https://www.cdc.gov/vaccines/schedules/hcp/imz/adult.html#note-mening", times = 14))) %>% filter(indicacion != "") #H influenzae B
  if_else(tab == 1,
  paste(str_c(vacunacion$vacuna, vacunacion$indicacion), collapse = "\n"),
  paste(str_c("Todas las recomendaciones fueron obtenidas de: ", vacunacion$referencias)[1], collapse = "\n"))

}

#LISTA DE COMORBILIDADES PARA LA VERSIÓN EXTENDIDA DE LA APP
comorbilidades <- c("CA de mama", "CA de colon", "Melanoma", "CA cervico-uterino", "CA gástrico", "CA renal de células claras", #Oncológicos
                    "CA de pulmón",#Oncológicos
                    "Lupus eritematoso generalizado", "Artritis reumatoide", "Dermatomiositis", "Sx de Sjogren", #Reuma
                    "Esclerosis sistémica", "Polimiositis", "Vasculitis ANCA", "Vasculitis de grandes vasos", "Polarteritis nodosa",#Reuma
                    "Anemia ferropénica", "Anemia falciforme", "Talasemia", "Trombocitopenia inmune primaria", "Anemia hemolítica autoinmune",#Hemato no-CA
                    "Linfoma difuso células grandes B", "Linfoma de Hodgkin", "Leucemia mieloide aguda", "Leucemia linfoblástica aguda", #Hemato CA
                    "Leucemia promielocítica", "Mieloma múltiple", #Hemato CA
                    "VIH", "Hepatitis B", "Hepatitis C", "Sífilis", #Infecto
                    "Cirrosis Child A", "Cirrosis Child B", "Cirrosis Child C", "Hepatitis autoinmune", "Colangitis biliar primaria",#Gastro
                    "Colangitis esclerosante primaria", "Colitis ulcerativa crónica inespecífica", "Enfermedad de Crohn",#Gastro
                    "Gastroparesia diabética", "Trasplante hepático (protocolo previo)", "Trasplante hepático (post-op)", #Gastro
                    "Enfermedad renal crónica KDIGO V", "Enfermedad renal crónica KDIGO III-IV", "Enfermedad por cambios mínimos",#Nefro
                    "Glomerulonefritis focal y segmentaria", "Glomerulonefritis membranosa", "Nefritis lúpica",#Nefro
                    "Trasplante renal (protocolo previo)", "Trasplante renal (post-op)",#Nefro
                    "Insuficiencia cardiaca crónica", "Enfermedad coronaria crónica", "Infarto agudo al miocardio (antecedente)",#Cardio
                    "Estenosis aórtica clínicamente significativa", "Hipertensión arterial pulmonar", "Hipertensión pulmonar (no grupo 1)",#Cardio
                    "Migraña crónica", "Esclerosis lateral amiotrófica", "Epilepsia", "Evento vascular cerebral (antecedente)", "Miastenia gravis",#Neuro
                    "Esclerosis múltiple", "Esclerosis lateral amiotrófica", "Esclerosis tuberosa",#Neuro
                    "Diabetes mellitus tipo 1", "Acromegalia", "Prolactinoma", "Síndrome de ovario poliquístico", "Hipotiroidismo",#Endocrino
                    "Hipertiroidismo", "Neoplasia endócrina múltiple", "CA papilar de tiroides", "CA medular de tiroides",#Endocrino
                    "Sx poliglandular autoinmune", #Endocrino
                    "Asma", "EPOC",
                    "Ninguna")#Neumo


#Objeto que me permite insertar los bulletpoints
bp <- "\U2022"

#theme=bs_theme(bootswatch = "sandstone"),

ui <- fluidPage(
  titlePanel("ScreenR: una herramienta para el seguimiento de rutina del paciente adulto"),
  sidebarLayout(
    sidebarPanel(tabsetPanel(type = "tabs",
                             tabPanel(title = "V. estándar",
  numericInput("edad", "Edad", 18, min = 18, max = 120),
  selectInput("sexo", "Sexo biológico", choices = c("Femenino", "Masculino")),
  numericInput("peso", "Peso (kg)", 70, min = 20, max = 500),
  numericInput("talla", "Talla (cm)", 160, min = 30, max = 250),
  radioButtons("dm2", "Diabetes mellitus tipo 2", choices = c("Sí", "No")),
  radioButtons("has", "Hipertensión", choices = c("Sí", "No")),
  radioButtons("cv", "Antecedente de evento cardiovascular", choices = c("Sí", "No")),
  radioButtons("tabaco", "Antecedente de tabaquismo", choices = c("Sí", "No")),
  actionButton("boton", "Aplicar")),
  tabPanel(title = "V. extendida",
           numericInput("edad", "Edad", 18, min = 18, max = 120),
           selectInput("sexo", "Sexo biológico", choices = c("Femenino", "Masculino")),
           numericInput("peso", "Peso (kg)", 70, min = 20, max = 500),
           numericInput("talla", "Talla (cm)", 160, min = 30, max = 250),
           radioButtons("dm2", "Diabetes mellitus tipo 2", choices = c("Sí", "No")),
           radioButtons("has", "Hipertensión", choices = c("Sí", "No")),
           radioButtons("cv", "Antecedente de evento cardiovascular", choices = c("Sí", "No")),
           radioButtons("tabaco", "Antecedente de tabaquismo", choices = c("Sí", "No")),
           "Para comorbilidades borrar el contenido de la casilla y comenzar a teclear",
           selectInput("comorbilidad_1", "Comorbilidad 1", choices = comorbilidades, selectize = T),
           selectInput("comorbilidad_2", "Comorbilidad 2", choices = comorbilidades, selectize = T),
           selectInput("comorbilidad_3", "Comorbilidad 3", choices = comorbilidades, selectize = T),
           actionButton("boton", "Aplicar"))), width = 2),
  mainPanel(tabsetPanel(type = "tabs",
    tabPanel(title="Recomendaciones",
            br(),
    textOutput("sexo2"),
    tags$head(tags$style("#sexo2{color: black;
                                 font-size: 22px;
                                 font-weight: bold
                                 }"
    )
    ),
    br(),#Espacio vacío
    br(),#Espacio vacío
    textOutput("metabolico"),
    tags$head(tags$style("#metabolico{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
    )
    ),
    verbatimTextOutput("tami_metab2", placeholder=F),
    tags$head(tags$style("#tami_metab2{color: black;
                                 font-size: 14px
                                 }"
    )
    ),
    textOutput("tami_sexo"),
    tags$head(tags$style("#tami_sexo{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
    )
    ),
    verbatimTextOutput("tami_sexo2", placeholder=F),
    tags$head(tags$style("#tami_sexo2{color: black;
                                 font-size: 14px
                                 }"
    )
    ),
    textOutput("oncologicos"),
    tags$head(tags$style("#oncologicos{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
    )
    ),
    verbatimTextOutput("onco2", placeholder=F),
    tags$head(tags$style("#onco2{color: black;
                                 font-size: 14px
                                 }"
    )
    ),
    textOutput("infectologicos"),
    tags$head(tags$style("#infectologicos{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
    )
    ),
    verbatimTextOutput("infecto2", placeholder=F),
    tags$head(tags$style("#infecto2{color: black;
                                 font-size: 14px
                                 }"
    )
    ),
    textOutput("vacunas"),
    tags$head(tags$style("#vacunas{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
    )
    ),
    verbatimTextOutput("vacunas2", placeholder=F),
    tags$head(tags$style("#vacunas2{color: black;
                                 font-size: 14px
                                 }"
    )
    )),
    tabPanel(title="Referencias", 
             br(),
             textOutput("sexo3"),
             tags$head(tags$style("#sexo3{color: black;
                                 font-size: 22px;
                                 font-weight: bold
                                 }"
             )
             ),
             br(),#Espacio vacío
             br(),#Espacio vacío
             textOutput("metabolico2"),
             tags$head(tags$style("#metabolico2{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
             )
             ),
             verbatimTextOutput("tami_metab_tab2", placeholder=F),
             tags$head(tags$style("#tami_metab_tab2{color: black;
                                 font-size: 14px
                                 }"
            )
            ),
            textOutput("tami_sexo3"),
            tags$head(tags$style("#tami_sexo3{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
            )
            ),
           verbatimTextOutput("tami_sexo_tab2", placeholder=F),
           tags$head(tags$style("#tami_sexo_tab2{color: black;
                                 font-size: 14px
                                 }"
           )
           ),
            textOutput("oncologicos2"),
            tags$head(tags$style("#oncologicos2{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
            )
            ),
           verbatimTextOutput("onco_tab2", placeholder=F),
           tags$head(tags$style("#onco_tab2{color: black;
                                 font-size: 14px
                                 }"
           )
           ),
            textOutput("infectologicos2"),
            tags$head(tags$style("#infectologicos2{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
            )
            ),
           verbatimTextOutput("infecto_tab2", placeholder=F),
           tags$head(tags$style("#infecto_tab2{color: black;
                                font-size: 14px
                                }"
           )
           ),
            textOutput("vacunas3"),
            tags$head(tags$style("#vacunas3{color: black;
                                 font-size: 18px;
                                 font-weight: bold
                                 }"
            )
            ),
            verbatimTextOutput("vacunas_tab2"),
            tags$head(tags$style("#vacunas_tab2{color: black;
                                font-size: 14px
                                 }"
            )
            )
            )), width = 10)))
             
             

#############

server <- function(input, output, session){
  ###########PRIMERA PESTAÑA
  #Lo primero es para el título introductorio acorde a sexo (la o el paciente)
  sexo <- eventReactive(input$boton, {input$sexo})
  output$sexo2 <- renderText({if_else(sexo()=="Femenino",
                                      print("Los tamizajes/intervenciones recomedados para la paciente son los siguientes:"),
                                      print("Los tamizajes/intervenciones recomedados para el paciente son los siguientes:"))})#Para uso en la primera pestaña
  output$sexo3 <- renderText({if_else(sexo()=="Femenino",
                                      print("Las referencias de donde se obtuvieron las recomendaciones para la paciente fueron las siguientes:"),
                                      print("Las referencias de donde se obtuvieron las recomendaciones para el paciente fueron las siguientes:"))})#Para uso en la segunda pestaña
  
  #Lo siguiente es para el título introductorio del tamizaje metabólico
  metabolico <- eventReactive(input$boton, {"Metabólicos"})
  output$metabolico <- renderText({paste(bp, metabolico())})#Para uso en la primera pestaña
  output$metabolico2 <- renderText({paste(bp, metabolico())})#Para uso en la segunda pestaña
  
  
  #diabetes, edad, sexo, cv, imc, has, tabaco)
  #Lo siguiente es propiamente lo que incluiría el tamizaje metabólico de acuerdo a edad, sexo, IMC y comorbilidades
  imc <- eventReactive(input$boton, {input$peso/(input$talla/100*input$talla/100)})
  
  tami_metab <- eventReactive(input$boton, {tamizajes_metabolicos(input$dm2, input$edad, input$sexo, input$cv, imc(), input$has, input$tabaco, 1)})
  output$tami_metab2 <- renderText({paste(tami_metab())})#Para uso en la primera pestaña

  tami_metab_tab2 <- eventReactive(input$boton, {tamizajes_metabolicos(input$dm2, input$edad, input$sexo, input$cv, imc(), input$has, input$tabaco, 2)})
  output$tami_metab_tab2 <- renderText({paste(tami_metab_tab2())})#Para uso en la segunda pestaña
  
  
  #Lo siguiente es para el título introductorio del tamizaje de acuerdo a sexo
  tami_sexo <- eventReactive(input$boton, {"Tamizajes de acuerdo a sexo"})
  output$tami_sexo <- renderText({paste(bp, tami_sexo())})#Para uso en la primera pestaña
  output$tami_sexo3 <- renderText({paste(bp, tami_sexo())})#Para uso en la segunda pestaña
  
  #Lo siguiente es para el contenido del tamizaje de acuerdo a sexo
  tami_sexo2 <- eventReactive(input$boton, {tamizajes_sexo(input$edad, input$sexo, 1)})#pestaña 1
  output$tami_sexo2 <- renderText({paste(tami_sexo2())})#pestaña 1

  tami_sexo_tab2 <- eventReactive(input$boton, {tamizajes_sexo(input$edad, input$sexo, 2)})#pestaña 2
  output$tami_sexo_tab2 <- renderText({paste(tami_sexo_tab2())})#pestaña 2
  
  #Lo siguiente es para el título introductorio del tamizaje oncológico
  oncologicos <- eventReactive(input$boton, {"Tamizajes oncológicos"})
  output$oncologicos <- renderText({paste(bp, oncologicos())})#Para uso en la primera pestaña
  output$oncologicos2 <- renderText({paste(bp, oncologicos())})#Para uso en la segunda pestaña
  
  #Lo siguiente es para el contenido del tamizaje oncológico
  tamiz_onco <- eventReactive(input$boton, {tamizajes_oncologicos(input$edad, input$sexo, input$tabaco, 1)})#pestaña 1
  output$onco2 <- renderText({paste(tamiz_onco())})#pestaña 1
  
  tamiz_onco_tab2 <- eventReactive(input$boton, {tamizajes_oncologicos(input$edad, input$sexo, input$tabaco, 2)})#pestaña 1
  output$onco_tab2 <- renderText({paste(tamiz_onco_tab2())})#pestaña 1
  
  #Lo siguiente es para el título introductorio de los tamizajes infectológicos
  infectologicos <- eventReactive(input$boton, {"Tamizajes infectológicos"})
  output$infectologicos <- renderText({paste(bp, infectologicos())})#Para uso en la primera pestaña
  output$infectologicos2 <- renderText({paste(bp, infectologicos())})#Para uso en la segunda pestaña
  
  #Lo siguiente es para el contenido del tamizaje infectológico
  tamiz_infecto <- eventReactive(input$boton, {tamizajes_infectologicos(input$edad, 1)})
  output$infecto2 <- renderText({paste(tamiz_infecto())})
  
  tamiz_infecto_tab2 <- eventReactive(input$boton, {tamizajes_infectologicos(input$edad, 2)})#Pestaña 2
  output$infecto_tab2 <- renderText({paste(tamiz_infecto_tab2())})#Pestaña 2
  
  #Lo siguiente es para el título introductorio de las vacunas
  vacunas <- eventReactive(input$boton, {"Vacunas"})
  output$vacunas <- renderText({paste(bp, vacunas())})#Para uso en la primera pestaña
  output$vacunas3 <- renderText({paste(bp, vacunas())})#Para uso en la segunda pestaña
  
  #Lo siguiente es para el contenido del tamizaje de las vacunas
  tamiz_vacunas <- eventReactive(input$boton, {tamizajes_vacunas(input$edad, input$dm2, input$cv, 1)})#Pestaña 1
  output$vacunas2 <- renderText({paste(tamiz_vacunas())})
  
  tamiz_vacunas_tab2 <- eventReactive(input$boton, {tamizajes_vacunas(input$edad, input$dm2, input$cv, 2)})#Pestaña 1
  output$vacunas_tab2 <- renderText({paste(tamiz_vacunas_tab2())})
  
  ##########SEGUNDA PESTAÑA
  
}
#
shinyApp(ui,server)

