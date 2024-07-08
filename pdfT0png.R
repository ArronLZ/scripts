main = FALSE

if (!main) {
  cat("check_pack("pdftools")", "\n", 
      "pdfTOpng(pdf_folder = pdf_folder, png_output_folder = png_output_folder)", "\n")
}

# function --------------------
# Load the pdftools package
check_pack <- function(pack) {
  if (!requireNamespace(pack, quietly = TRUE)) {
    install.packages(pack)
  } else {
    library(pack, character.only = TRUE)
  }
}

pdfTOpng <- function(pdf_folder, pdf_files, png_output_folder) {
  # Check if either pdf_folder or pdf_files is set, but not both
  if (!xor(missing(pdf_folder), missing(pdf_files))) {
    stop("Either pdf_folder or pdf_files must be set, but not both.")
  }
  # Get the PDF files list
  if (!missing(pdf_folder)) {
    pdf_files <- list.files(pdf_folder, pattern = ".pdf$", full.names = TRUE)
  } else {
    pdf_files <- pdf_files
    # Check if the provided PDF files are valid
    lapply(pdf_files, function(f) {
      # Check if the file is a PDF file
      if ( endsWith(f, ".pdf") ) {
        stop(paste0("The provided file's suffix is not .pdf : ", f))
      }
      # Check if the file exists
      if ( !file.exists(f) ) {
        stop(paste0("The provided PDF file does not exist: ", f))
      }
    })
  }
  # Create the PNG output directory if it doesn't exist
  if (!dir.exists(png_output_folder)) {
    dir.create(png_output_folder)
  }
  # convert each PDF file to PNG ------------------------------------------
  # Loop through each PDF file
  for (pdf_file in pdf_files) {
    # Get the number of pages in the PDF file
    pdf <- pdftools::pdf_info(pdf_file)
    num_pages <- pdf$pages
    # Loop through each page in the PDF file
    for (page in 1:num_pages) {
      supressWarnings({
        png_output_file <- file.path(png_output_folder, 
                                     paste0(tools::file_path_sans_ext(basename(pdf_file)), 
                                            "_page", page, ".png"))
        pdftools::pdf_convert(pdf_file, format = "png", pages = page, 
                              dpi = 400, filenames = png_output_file)
      })
      cat("Converted page", page, "of", pdf_file, "to", png_output_file, "\n")
    }
  }
  cat("\n", "PDF to PNG conversion complete! output dir: ", 
      png_output_folder, "\n")
}
# /function --------------------


if (main) {
# command: Rscript pdfT0png.R <pdf_directory> <png_output_directory>
# command args settings: --------------------
# Check if required arguments are provided
if (length(commandArgs(trailingOnly = TRUE)) != 2) {
  cat("Usage: Rscript pdfT0png.R <pdf_directory> <png_output_directory>\n")
  quit(status = 1)
}

# Get the arguments
pdf_folder <- commandArgs(trailingOnly = TRUE)[1]
png_output_folder <- commandArgs(trailingOnly = TRUE)[2]

# Check if the PDF directory exists and contains PDF files
if (!file.exists(pdf_folder) || length(list.files(pdf_folder, pattern = ".pdf$", full.names = TRUE)) == 0) {
  cat("Error: PDF directory does not exist or does not contain PDF files.\n")
  quit(status = 1)
}
# /command args settings: -------------------
}

# main ----------------
# Load the pdftools package
if (main) {
  check_pack("pdftools")
  pdfTOpng(pdf_folder = pdf_folder, png_output_folder = png_output_folder)
}
