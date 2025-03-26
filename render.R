# Render index.html
rmarkdown::render("index.Rmd",
                  #rmarkdown::html_document(toc = TRUE, toc_float = TRUE),
                  output_dir = "docs")

# Copy data folder
file.copy("data", "docs", recursive = TRUE)

# Render scripts for sessions

folders <- dir(pattern = "^0[0-9]")

## Render tex files
for (folder in folders) {

  setwd(folder)

  fnames <- dir(pattern = "\\.tex$")

  for (fname in fnames) {
    tinytex::pdflatex(fname,
      pdf_file = paste0("../docs/", folder, "/", gsub("tex$", "pdf", fname))
    )
  }

  setwd("..")

}

## Render R scripts
for (folder in folders) {

  fnames <- dir(path = folder, pattern = "\\.R$", full.names = TRUE)

  for (fname in fnames) {

    if (grepl("exercise", fname)) {
      code_download <- FALSE
    } else {
      code_download <- TRUE
    }
    subfolder <- gsub("(^0[0-9].*)\\/.*", "\\1", fname)
    rmarkdown::render(fname,
                      rmarkdown::html_document(code_download = code_download),
                      output_dir = paste("docs", subfolder, sep = "/"))
  }

}

