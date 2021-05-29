library(shiny)
library(miniUI)

ui <-shinyUI(fluidPage(
    titlePanel("Predict Swiss Fertility"),
    sidebarLayout(
        #contains input sent to server
        sidebarPanel(
            sliderInput("sliderMPG", "What is the Fertility rate?:",            #get a slider input called 'sliderMPG'
                        min = 0,
                        max = 90,
                        value = 10), #start value
            #two checkboxes for whether it will show that particular model output
            checkboxInput("showMode11","Show/Hide Model 1",value=T),            #showModel11 is an input
            submitButton("Submit")                                               #this allows a delayed reactivity so that you have to click a submit button on UI to re-run calc, otherwise it always calculated automatically once you change the input value on the slider. 
        ),
        
        # contains output
        mainPanel(
            plotOutput("plot1"),                                                #look in server for plot labeled plot1 to display
            
            h3("Predicted Swiss Fertility from Model 1:"),
            textOutput("pred1")                                           #output from prediction in server labelled pred1
            
        )
    )
))


server <- function(input, output) {
    
    
    model11<-lm(Fertility ~ Agriculture , data = swiss) #red line
    
    #get prediction for model 1
    model1pred<-reactive({               #since we are doing calcs based on inputted values from UI, it needs to be reactive
        mpgInput<-input$sliderMPG        #sliderMPG is what we named the sliderInput value in the UI
        predict(model11,data.frame(Agriculture=mpgInput)) #predict using new value input from slider
    })
    
    
    
    #create outputs
    output$plot1 <- renderPlot({           #remember UI is looking for a plot called 'plot1'
        mpgInput<-input$sliderMPG           #grab the input from the slider
        plot(swiss$Agriculture,swiss$Fertility,xlab = "Agriculture", ylab="Fertility")  #plot all mpg vs hp
        #{abline(model11,col="red")}
        model12lines<-predict(model11,data.frame(Agriculture=0:90))
        
        lines(0:90,model12lines,col="blue")
    })
    
    output$pred1<-renderText({model1pred()}) #need the parentheses after 'model1pred' bc it is from a reactive expression. if you dont have the parenthesis it will return the function, not the number/points
    
    
}

shinyApp(ui = ui, server = server)