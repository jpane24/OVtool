all: README.md

clean:
	Rscript -e 'suppressWarnings(file.remove("README.md", "vignettes/OVtool.md"))'

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

README.md : vignettes/OVtool.Rmd
#	echo "Rendering the OVtool vignette"
	Rscript -e 'rmarkdown::render("vignettes/OVtool.Rmd", output_format = "md_document", output_options = list(pandoc_args = c("-t", "commonmark")))'
#	echo "Correcting image paths"
#	sed -i -- 's,../inst,inst,g' vignettes/shinyjs.md
	Rscript -e 'file <- gsub("\\\.\\\./inst", "inst", readLines("vignettes/OVtool.md")); writeLines(file, "vignettes/OVtool.md")'#	echo "Correcting paths to other reference vignettes"
	Rscript -e 'file <- gsub("\\\((.*)\\\.([rR]md)","(vignettes/\\1.\\2", readLines("vignettes/OVtool.md")); writeLines(file, "vignettes/OVtool.md")'
#	echo "Copying output to README.md"
#	cp vignettes/shinyjs.md README.md
	Rscript -e 'file.copy("vignettes/OVtool.md", "README.md", overwrite = TRUE)'
	Rscript -e 'suppressWarnings(file.remove("vignettes/OVtool.md"))'
