#!/bin/zsh

# Directory containing the PDF files
directory="/Users/pracz/Github/RaczRebrus2024cont/uesz"

# Loop through all PDF files in the directory
for pdf_file in "$directory"/*.pdf; do
    # Get the base name of the file (without extension)
    base_name=$(basename "$pdf_file" .pdf)
    
    # Convert the PDF to a text file
    pdftotext "$pdf_file" "$directory/$base_name.txt"
done