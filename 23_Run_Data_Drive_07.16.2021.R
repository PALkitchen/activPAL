
Events_Files_To_Process_folder_location <- "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Events_Files_To_Process\\"

Temp_Output_folder_location <- "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Temp_Output\\"

Confirmed_Output_folder_location <- "S:\\PT\\IMSLAB\\IMSL RESEARCH PROJECTS\\SPARX III\\SPARX III CORE\\SPARX DATA ANALYSIS\\SPARX ACTIVPAL PROCESSING\\AP_Processing_Directory\\Confirmed_Output\\"

activpal.process.folder(Events_Files_To_Process_folder_location, Temp_Output_folder_location)

Last_Batched_Ids <- activpal.process.folder(Events_Files_To_Process_folder_location, Temp_Output_folder_location)

prepare.ex.times(Temp_Output_folder_location)

make.index.file(Events_Files_To_Process_folder_location, Temp_Output_folder_location)

individual.chart.overlay(Temp_Output_folder_location)


## run after adjusting estimate exercise times ------------------------------------------------
apSummary(Events_Files_To_Process_folder_location, Confirmed_Output_folder_location)
