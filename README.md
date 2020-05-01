# COVID_WI_state
Examination of state COVID related data

## Background

The State of Wisconsin Department of Public Health WI DHS) has outlined gating criteria to move from the current shelter in place order.

Click [here](https://www.dhs.wisconsin.gov/covid-19/prepare.htm) to read more about the proposal from WI DHS.

The WI DHS team proposes to use linear regression to determine changes to their gating measures sufficient to move to more open phase of social interaction.   Control charts are offered as an alternative for analysis.

The [file](https://github.com/klittle314/COVID_WI_state/blob/master/Notes%20on%20criteria%20for%20decision%20making%2027%20April%202020.pdf) 'Notes on criteria for decision making.pdf' outlines issues in the regression approach and describes control chart alternatives.

If you are familiar with the R language and are able to run RMarkdown files, the Rmarkdown file in this repository the file plots the raw data on Covid-19 testing in Wisconsin and also shows three control chart options to view the percent positive tests. 

The markdown file does not yet automatically access data from the [DHS data webpage](https://data.dhsgis.wi.gov/datasets/covid-19-historical-data-table/data?where=%20(GEO%20%3D%20%27County%27%20OR%20GEO%20%3D%20%27State%27)%20).  

Use the CSV file in the same local directory as the RMarkdown file to view the graphs.

**Want more information?**  Email [Kevin Little](mailto:klittle@iecodesign.com?subject=[GitHub]%20COVID_WI_state).


