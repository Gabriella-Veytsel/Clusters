library(pdftools)
library(magick)

convert_pdfs_to_pngs <- function(input_folders, output_folder, dpi = 300) {
  # Ensure the output folder exists
  if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)
  
  # Loop through each specified input folder
  for (input_folder in input_folders) {
    # List all PDF files in the current folder
    pdf_files <- list.files(input_folder, pattern = "\\.pdf$", full.names = TRUE)
    
    # Create a subfolder in the output directory for this input folder
    subfolder_name <- basename(normalizePath(input_folder))
    subfolder_path <- file.path(output_folder, subfolder_name)
    if (!dir.exists(subfolder_path)) dir.create(subfolder_path, recursive = TRUE)
    
    # Loop through each PDF and convert to PNG
    for (pdf in pdf_files) {
      # Extract filename without extension
      filename <- tools::file_path_sans_ext(basename(pdf))
      
      # Read the PDF and convert to an image
      img <- image_read_pdf(pdf, density = dpi)  # Read PDF at specified DPI
      
      # Save each page as a PNG
      for (i in seq_along(img)) {
        output_path <- file.path(subfolder_path, paste0(filename, "_page", i, ".png"))
        image_write(img[i], path = output_path, format = "png")
      }
    }
    
    cat("Processed:", input_folder, "\n")
  }
  
  cat("Conversion completed!\n")
}

rabies_main_figures_pdf <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 2 - Rabies/Figures PDF/Main Figures"
rabies_main_figures_png <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 2 - Rabies/Figures PNG/Main Figures"  # Output directory
convert_pdfs_to_pngs(rabies_main_figures_pdf, rabies_main_figures_png)

clusters_main_figures_pdf <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 3 - Clusters/Figures PDF/Main Figures"
clusters_main_figures_png <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 3 - Clusters/Figures PNG/Main Figures"  # Output directory
convert_pdfs_to_pngs(clusters_main_figures_pdf, clusters_main_figures_png)

clusters_supp_figures_pdf <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 3 - Clusters/Figures PDF/Supplementary Figures"
clusters_supp_figures_png <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 3 - Clusters/Figures PNG/Supplementary Figures"  # Output directory
convert_pdfs_to_pngs(clusters_supp_figures_pdf, clusters_supp_figures_png)

ww_main_figures_pdf <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 4 - Wastewater/Figures PDF/Main Figures"
ww_main_figures_png <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 4 - Wastewater/Figures PNG/Main Figures"  # Output directory
convert_pdfs_to_pngs(ww_main_figures_pdf, ww_main_figures_png)

ww_supp_figures_pdf <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 4 - Wastewater/Figures PDF/Supplementary Figures"
ww_supp_figures_png <- "C:/Users/gev25289/Desktop/OneDrive_2025-03-13/Lab Review/Veytsel Chapter 4 - Wastewater/Figures PNG/Supplementary Figures"  # Output directory
convert_pdfs_to_pngs(ww_supp_figures_pdf, ww_supp_figures_png)
