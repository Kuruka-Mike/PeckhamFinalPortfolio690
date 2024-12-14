# Install required packages
install.packages("pdftools")
install.packages("magick")

library(pdftools)
library(magick)

# Path to your PDF files
pdf_files <- c(
  "www/CoverLetter_Portfolio.pdf",
  "www/Peckham_SewerAI_Resume.pdf",
  "www/ReferencesExample.pdf"
)

# Generate thumbnails
output_thumbnails <- c("www/cover_letter_thumbnail.png", 
                       "www/resume_thumbnail.png", 
                       "www/references_thumbnail.png")

for (i in seq_along(pdf_files)) {
  # Convert the first page of each PDF to a PNG thumbnail
  img <- pdf_render_page(pdf_files[i], page = 1, dpi = 150)
  magick_img <- image_read(img)
  
  # Resize the image for thumbnail size
  thumbnail <- image_resize(magick_img, "100x150")
  image_write(thumbnail, output_thumbnails[i])
}
