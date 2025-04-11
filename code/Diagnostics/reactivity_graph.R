#Reactivity Graph
#Last edited 4/11 by Catalina
#NOTES: This script is for looking at the apps reactivity graph
#here's a resource on this package https://mastering-shiny.org/reactive-graph.html

#This is a really cool tool.

#Run this first line before starting the app
reactlog::reactlog_enable()

#start the app and play around with it triggering the reactive elements that you want analyzed
#close the app

#run this next line of code to see the reactivity graph
#it will pop up in your browser
shiny::reactlogShow()
