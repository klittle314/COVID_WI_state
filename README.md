# COVID_WI_state
Simple displays of the gating criteria measures for the State of Wisconsin Covid-19 response.

## Background

The State of Wisconsin Department of Public Health WI DHS) has outlined gating criteria to move from the current shelter in place order.

Click [here](https://www.dhs.wisconsin.gov/covid-19/prepare.htm) to read more about the proposal from WI DHS.

The WI DHS team proposes to use linear regression to determine changes to their gating measures sufficient to move to more open phase of social interaction.   Simple data displays and control charts are offered as a complement to regression slopes to guide daily review and action cycles. 

The [file](https://github.com/klittle314/COVID_WI_state/blob/master/Notes%20on%20criteria%20for%20decision%20making%20rev%20May%201..pdf) 'Notes on criteria for decision making rev May..pdf' outlines issues in the regression approach and describes control chart alternatives.

If you are familiar with the R language and are able to run RMarkdown files, the Rmarkdown file in this repository the file plots raw data on Covid-19 testing in Wisconsin and also shows three control chart options to view the percent positive tests. 

The [markdown file](https://github.com/klittle314/COVID_WI_state/blob/master/Data%20Displays%20for%20DHS_Gating%20Criteria.Rmd) automatically grabs the daily data from the State of Wisconsin and offers my updated recommendations for DHS decision-makers.

The repository also has the files for a Shiny web app:  use the global.R, ui.R, and server.R files, along with the helper.R file for an interactive app that shows county level data.

**Want more information?**  Email [Kevin Little](mailto:klittle@iecodesign.com?subject=[GitHub]%20COVID_WI_state).


