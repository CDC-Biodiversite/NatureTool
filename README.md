---
output:
  html_document: default
---
# NatureTool
Tool to assess biodiversity impacts and dependencies for financial portfolios.

## Input data
Before running the NatureTool, please fill in an input file. A pre-filled template named *"NatureTool_Input_File_Template.xlsx"* is available in the *"Input files"* folder

## Run the NatureTool
Steps to run the NatureTool

1. Please go to the *"Assessment"* folder and open the file *"Run_NatureTool.Rmd"*.

2. Then, on the top right side of the code, click on the click-down menu next to *"Knit"* and select *"Knit with parameters"*.

3. In the box, select or drag and drop the previously filled input file. 

4. Click on the *"Knit"* button. You can follow the computation progress in the *"Render"* cell at the bottom of the code. The code ends running when an *.html* page pops up. 

5. An excel file with results is saved in the "Assessment" folder, named *"Current_date_NatureTool_results.xlsx"*.
 The file contains the following sheets:
 
  - **portfolio_impacts**: Impacts of each ISIN in the portfolio, broken down by 
 
      - *Scope* (Scope 1, Scope 2, Upstream Scope 3)
 
      - *Pressure* (Land use, Climate change, Fragmentation, Encroachment, Atmospheric nitrogen depositions, Ecotoxicity, Wetland conversion, Hydrological disturbance due to direct water use, Hydrological disturbance due to climate change, Land use in the catchment of wetlands, Land use in the catchment of rivers)
 
      - *Realm* (Terrestrial, Aquatic)
 
      - *Accounting category* (Static, Dynamic)
 
  - **portfolio_dependencies_average**: Average dependency scores for each ecosystem service as well as the aggregated average dependency score for each ISIN in the portfolio, broken down by Scope.
 
  - **portfolio_dependencies_critical**: Critical dependency scores for each ecosystem service as well as the aggregated average dependency score for each ISIN in the portfolio, broken down by Scope.


