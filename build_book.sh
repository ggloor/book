rm _main.Rmd
rm codaseq-book.Rmd

Rscript --quiet -e  'bookdown::render_book("index.Rmd", "bookdown::html_book", config_file = "_config.yml")'

