### Overview

(Note that the Reed Senior Thesis has been updated and can be found [here](http://github.com/ismayc/thesisdown).)

The **reedtemplates** package (formerly named **rticles** but changed due to ambiguity with the package at `rstudio/rticles`) includes a set of [R Markdown](http://rmarkdown.rstudio.com) templates that enable authoring of journal and conference submissions, e-books, class projects, and a variety of other reports.  Currently, this package only includes a template for creating a senior thesis at Reed College. If you'd like to see the PDF document the template produces, it can be found [here](https://github.com/ismayc/reedtemplates/blob/master/inst/rmarkdown/templates/reed_thesis/skeleton/skeleton.pdf).

Under the hood, LaTeX templates are used to ensure that documents conform precisely to submission standards. At the same time, composition and formatting can be done using lightweight [markdown](http://rmarkdown.rstudio.com/authoring_basics.html) syntax, and **R** code and its output can be seamlessly included using [knitr](http://yihui.name/knitr/).

Using **reedtemplates** has some prerequisites which are described below. To compile PDF documents using **R**, you are going to need to have LaTeX installed.  It can be downloaded for Windows at <http://http://miktex.org/download> and for Mac at <http://tug.org/mactex/mactex-download.html>.  Follow the instructions to install the necessary packages after downloading the (somewhat large) installer files.  You may need to install a few extra LaTeX packages on your first attempt to knit as well.

### Using reedtemplates from Chester's GitHub

To use **reedtemplates** from RStudio:

1) Install the latest [RStudio](http://www.rstudio.com/products/rstudio/download/).

2) Install the **reedtemplates** package: 

```S
install.packages("devtools")
devtools::install_github("ismayc/reedtemplates")
```

3) Use the **New R Markdown** dialog to create an article from one of the templates:

![New R Markdown](reed_template.png)
    
    
### Using reedtemplates outside of RStudio

1) Install [pandoc](http://johnmacfarlane.net/pandoc/) using the [instructions for your platform](https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md).

2) Install the **rmarkdown** and **reedtemplates** packages:

```S
install.packages("rmarkdown")
devtools::install_github("ismayc/reedtemplates")
```
    
3) Use the `rmarkdown::draft` function to create articles:

```S
rmarkdown::draft("MyThesis.Rmd", template = "reed_thesis", package = "reedtemplates")
```

