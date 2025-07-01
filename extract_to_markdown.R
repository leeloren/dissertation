#!/usr/bin/env Rscript
# Extract sections from HTML and save as markdown files
# This script extracts sections from dissertation.html and converts them to markdown files

# Load required libraries
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

# Source the required functions
source("extract_sections_from_googledoc.R")
source("convert-html-to-markdown.R")

#' Extract a section and save as markdown
#' 
#' @param section_name The name of the section to extract
#' @param output_md_file The output markdown file path
#' @param html_file The source HTML file (default: "temp_files/dissertation.html")
#' @param verbose Whether to print status messages (default: TRUE)
extract_section_to_markdown <- function(section_name, output_md_file, html_file = "temp_files/dissertation.html", verbose = TRUE) {
  # Create temporary HTML file name
  temp_html_file <- paste0("temp_files/temp_", gsub("[^a-zA-Z0-9]", "_", section_name), ".html")
  
  if (verbose) {
    cat("Extracting section:", section_name, "\n")
  }
  
  # Extract the section to temporary HTML file
  tryCatch({
    extract_section_by_name(
      html_file = html_file,
      section_name = section_name,
      output_file = temp_html_file,
      clean_html = FALSE,
      verbose = FALSE
    )
    
    if (verbose) {
      cat("Converting to markdown...\n")
    }
    
    # Convert HTML to markdown
    converted_markdown <- convert_html_to_markdown(temp_html_file)
    
    # Save the markdown
    writeLines(converted_markdown, output_md_file, useBytes = TRUE)
    
    # Clean up temporary HTML file
    if (file.exists(temp_html_file)) {
      file.remove(temp_html_file)
    }
    
    if (verbose) {
      cat("Saved markdown to:", output_md_file, "\n\n")
    }
    
  }, error = function(e) {
    cat("Error extracting section '", section_name, "':", e$message, "\n")
    
    # Clean up temporary HTML file if it exists
    if (file.exists(temp_html_file)) {
      file.remove(temp_html_file)
    }
  })
}

#' Extract a subsection and save as markdown
#' 
#' @param section_name The name of the subsection to extract
#' @param heading_level The heading level (e.g., "h2", "h3")
#' @param output_md_file The output markdown file path
#' @param parent_section Optional parent section to search within
#' @param html_file The source HTML file (default: "temp_files/dissertation.html")
#' @param verbose Whether to print status messages (default: TRUE)
extract_subsection_to_markdown <- function(section_name, heading_level, output_md_file, parent_section = NULL, html_file = "temp_files/dissertation.html", verbose = TRUE) {
  # Create temporary HTML file name
  temp_html_file <- paste0("temp_files/temp_", gsub("[^a-zA-Z0-9]", "_", section_name), ".html")
  
  if (verbose) {
    cat("Extracting subsection:", section_name, "at level", heading_level, "\n")
    if (!is.null(parent_section)) {
      cat("Within parent section:", parent_section, "\n")
    }
  }
  
  # Extract the subsection to temporary HTML file
  tryCatch({
    extract_subsection_by_name(
      html_file = html_file,
      section_name = section_name,
      heading_level = heading_level,
      parent_section = parent_section,
      output_file = temp_html_file,
      clean_html = FALSE,
      verbose = FALSE
    )
    
    if (verbose) {
      cat("Converting to markdown...\n")
    }
    
    # Convert HTML to markdown
    converted_markdown <- convert_html_to_markdown(temp_html_file)
    
    # Save the markdown
    writeLines(converted_markdown, output_md_file, useBytes = TRUE)
    
    # Clean up temporary HTML file
    if (file.exists(temp_html_file)) {
      file.remove(temp_html_file)
    }
    
    if (verbose) {
      cat("Saved markdown to:", output_md_file, "\n\n")
    }
    
  }, error = function(e) {
    cat("Error extracting subsection '", section_name, "':", e$message, "\n")
    
    # Clean up temporary HTML file if it exists
    if (file.exists(temp_html_file)) {
      file.remove(temp_html_file)
    }
  })
}

#' Extract all main sections to markdown files
#' 
#' @param output_dir Directory to save markdown files (default: "temp_files")
#' @param html_file The source HTML file (default: "temp_files/dissertation.html")
extract_all_sections_to_markdown <- function(output_dir = "temp_files", html_file = "temp_files/dissertation.html") {
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created directory:", output_dir, "\n")
  }
  
  # Define the sections to extract (based on your current QMD files)
  sections <- list(
    list(name = "Abstract", file = "googledoc_abstract.md"),
    list(name = "Introduction", file = "googledoc_intro.md"),
    list(name = "1. On Digital Editing", file = "googledoc_chapter1.md"),
    list(name = "2.1 An Approach to Editing Version T, Part 1", file = "googledoc_chapter2-1.md"),
    list(name = "2.2 An Approach to Editing Version T, Part 2", file = "googledoc_chapter2-2.md"),
    list(name = "3. A Cross-Copy Close Reading of the T Portraits", file = "googledoc_chapter3.md")
  )
  
  cat("Extracting all main sections to markdown files...\n\n")
  
  for (section in sections) {
    output_file <- file.path(output_dir, section$file)
    extract_section_to_markdown(section$name, output_file, html_file)
  }
  
  cat("All sections extracted to:", output_dir, "\n")
}


# run the script
extract_all_sections_to_markdown()
