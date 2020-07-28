all: README.md

clean:
	Rscript -e 'suppressWarnings(file.remove("README.md", "vignettes/OVtool.md"))'

.PHONY: all clean
.DELETE_ON_ERROR:
.SECONDARY:

README.md : vignettes/OVtool.Rmd
	echo "Rendering the OVtool vignette"
	Rscript -e 'rmarkdown::render("vignettes/OVtool.Rmd", output_format = "md_document", output_options = list(pandoc_args = c("-t", "commonmark")))'
	echo "Copying output to README.md"
	cp vignettes/OVtool.md README.md
	cp -R vignettes/inst .
	Rscript -e 'suppressWarnings(file.remove("vignettes/OVtool.md"))'
