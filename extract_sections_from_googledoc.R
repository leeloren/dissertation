#!/usr/bin/env Rscript
# HTML Section Extractor for Dissertation (R Version)
# Extracts specific sections from dissertation.html based on heading IDs or section names.

# Load required libraries
library(rvest)
library(xml2)
library(dplyr)
library(stringr)

#' Extract a section from HTML file by section name
#' 
#' @param html_file Path to the HTML file
#' @param section_name The name of the section (e.g., 'Abstract', 'Introduction')
#' @param output_file Output file path (optional). If NULL, prints to console
#' @param clean_html Logical. If TRUE (default), creates clean HTML with styling. If FALSE, returns raw HTML as-is from the file.
#' @param verbose Logical. If TRUE (default), prints status messages. If FALSE, suppresses output.
#' @return The extracted HTML content as a character string
extract_section_by_name <- function(html_file, section_name, output_file = NULL, clean_html = TRUE, verbose = TRUE) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # Find the heading with the specified text (case insensitive)
  headings <- html_content %>% html_nodes("h1")
  
  heading <- NULL
  for (h in headings) {
    text <- h %>% html_text() %>% str_trim()
    # Strip comment references like [f], [g], etc. for matching
    text_clean <- str_replace_all(text, "\\[[a-zA-Z]\\]", "")
    text_clean <- str_squish(text_clean)
    
    if (str_detect(tolower(text_clean), tolower(section_name))) {
      heading <- h
      break
    }
  }
  
  if (is.null(heading)) {
    stop(paste("Error: Section '", section_name, "' not found.", sep = ""))
  }
  
  # Get the section title
  section_title <- heading %>% html_text() %>% str_trim()
  
  # Find all content until the next h1 heading
  extracted_content <- extract_content_until_next_heading(html_content, heading)
  
  # Create output HTML based on clean_html parameter
  if (clean_html) {
    final_html <- create_clean_html(section_title, extracted_content)
  } else {
    # Return raw HTML as-is
    final_html <- extracted_content
  }
  
  # Save or print
  if (!is.null(output_file)) {
    writeLines(final_html, output_file, useBytes = TRUE)
    if (verbose) {
      cat("Section '", section_title, "' extracted to '", output_file, "'\n", sep = "")
    }
  } else {
    if (verbose) {
      cat(final_html)
    }
  }
  
  return(final_html)
}

#' List all available sections in the HTML file
#' 
#' @param html_file Path to the HTML file
list_available_sections <- function(html_file) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # Find all h1 headings
  headings <- html_content %>% html_nodes("h1")
  
  cat("Available sections:\n")
  cat(paste(rep("-", 50), collapse = ""), "\n")
  
  for (i in seq_along(headings)) {
    title <- headings[i] %>% html_text() %>% str_trim()
    heading_id <- headings[i] %>% html_attr("id")
    if (is.na(heading_id)) heading_id <- "No ID"
    
    # Strip comment references for display
    title_clean <- str_replace_all(title, "\\[[a-zA-Z]\\]", "")
    title_clean <- str_squish(title_clean)
    
    cat(sprintf("%2d. %s\n", i, title))
    if (title != title_clean) {
      cat(sprintf("    Clean: %s\n", title_clean))
    }
    cat(sprintf("    ID: %s\n", heading_id))
    cat("\n")
  }
}

#' Extract content from a heading until the next h1 heading, including footnotes
#' 
#' @param html_content Parsed HTML content
#' @param start_heading The starting heading element
#' @return HTML content as a character string
extract_content_until_next_heading <- function(html_content, start_heading) {
  # Get all h1 headings
  all_headings <- html_content %>% html_nodes("h1")
  heading_texts <- sapply(all_headings, function(h) html_text(h) %>% str_trim())
  start_heading_text <- html_text(start_heading) %>% str_trim()
  start_index <- which(tolower(heading_texts) == tolower(start_heading_text))
  if (length(start_index) == 0) {
    return(as.character(start_heading))
  }
  # Find the next h1 heading
  end_index <- if (start_index < length(all_headings)) start_index + 1 else NA
  # Get all elements in document order
  all_elements <- html_content %>% html_nodes("body") %>% xml_children()
  # Find the start and end positions by matching heading text
  start_pos <- NA
  end_pos <- NA
  for (i in seq_along(all_elements)) {
    el <- all_elements[[i]]
    if (xml_name(el) == "h1" && str_trim(html_text(el)) == start_heading_text && is.na(start_pos)) {
      start_pos <- i
    } else if (!is.na(end_index) && xml_name(el) == "h1" && str_trim(html_text(el)) == heading_texts[end_index]) {
      end_pos <- i - 1
      break
    }
  }
  if (is.na(start_pos)) {
    return(as.character(start_heading))
  }
  if (is.na(end_pos)) {
    end_pos <- length(all_elements)
  }
  # Extract the main content
  extracted_elements <- all_elements[start_pos:end_pos]
  
  # Deduplicate content to prevent the same text from appearing multiple times
  seen_content <- character(0)
  unique_elements <- list()
  
  for (element in extracted_elements) {
    element_text <- xml_text(element)
    element_text_clean <- str_squish(element_text)
    
    # Skip empty elements
    if (element_text_clean == "") {
      next
    }
    
    # Normalize the text for better comparison (remove extra spaces, normalize quotes, etc.)
    element_text_normalized <- str_replace_all(element_text_clean, "\\s+", " ")
    element_text_normalized <- str_trim(element_text_normalized)
    
    # Check if we've seen this content before (using normalized text)
    if (!(element_text_normalized %in% seen_content)) {
      seen_content <- c(seen_content, element_text_normalized)
      unique_elements <- c(unique_elements, list(element))
    }
  }
  
  extracted_content <- paste(sapply(unique_elements, as.character), collapse = "\n")
  
  # Find footnote references in the extracted content
  footnote_refs <- character(0)
  for (element in extracted_elements) {
    footnote_links <- element %>% html_nodes("a[href*='#ftnt']")
    for (link in footnote_links) {
      href <- html_attr(link, "href")
      footnote_num <- stringr::str_extract(href, "\\d+")
      if (!is.na(footnote_num)) {
        footnote_refs <- c(footnote_refs, footnote_num)
      }
    }
  }
  footnote_refs <- unique(sort(as.numeric(footnote_refs)))
  # Extract footnote definitions for the referenced footnotes
  if (length(footnote_refs) > 0) {
    footnote_definitions <- character(0)
    for (footnote_num in footnote_refs) {
      footnote_anchor <- html_content %>% html_nodes(paste0("a[id='ftnt", footnote_num, "']"))
      if (length(footnote_anchor) > 0) {
        footnote_element <- footnote_anchor[[1]]
        # Traverse up to the parent div
        current_parent <- xml_parent(footnote_element)
        footnote_div <- NULL
        while (!is.na(current_parent)) {
          if (xml_name(current_parent) == "div") {
            if (length(xml_find_all(current_parent, paste0(".//a[@id='ftnt", footnote_num, "']"))) > 0) {
              footnote_div <- current_parent
              break
            }
          }
          next_parent <- tryCatch(xml_parent(current_parent), error = function(e) NA)
          if (is.na(next_parent)) {
            break
          }
          current_parent <- next_parent
        }
        if (!is.null(footnote_div) && !is.na(footnote_div)) {
          # Get all content from the footnote div, but exclude the anchor element
          footnote_children <- xml_children(footnote_div)
          
          # Filter out the anchor element (the one with the footnote number)
          content_children <- list()
          for (child in footnote_children) {
            # Skip if this is the anchor element
            if (xml_name(child) == "a" && xml_attr(child, "id") == paste0("ftnt", footnote_num)) {
              next
            }
            # Skip if this is a p element that contains only the anchor
            if (xml_name(child) == "p") {
              p_children <- xml_children(child)
              if (length(p_children) == 1 && xml_name(p_children[[1]]) == "a" && 
                  xml_attr(p_children[[1]], "id") == paste0("ftnt", footnote_num)) {
                next
              }
              # Also skip if this p element contains the anchor and nothing else meaningful
              p_text <- xml_text(child)
              p_text_clean <- str_replace_all(p_text, paste0("\\[", footnote_num, "\\]"), "")
              p_text_clean <- str_squish(p_text_clean)
              if (p_text_clean == "") {
                next
              }
            }
            content_children <- c(content_children, list(child))
          }
          
          # Convert the filtered content to HTML
          footnote_html_content <- paste(sapply(content_children, as.character), collapse = "")
          footnote_html_content <- str_replace_all(footnote_html_content, "\\[[a-zA-Z]{1,3}\\]", "")
          # Remove any remaining footnote number references
          footnote_html_content <- str_replace_all(footnote_html_content, paste0("\\[", footnote_num, "\\]"), "")
          footnote_html_content <- str_squish(footnote_html_content)
          
          footnote_def <- paste0('<div class="footnote" id="ftnt', footnote_num, '_def">',
                               '<sup>[', footnote_num, ']</sup> ', footnote_html_content, '</div>')
          footnote_definitions <- c(footnote_definitions, footnote_def)
        }
      }
    }
    if (length(footnote_definitions) > 0) {
      extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
      extracted_content <- paste0(extracted_content, "\n\n<div class='footnotes'>\n",
                                 paste(footnote_definitions, collapse = "\n"), "\n</div>")
    } else {
      extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
    }
  } else {
    extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
  }
  return(extracted_content)
}

#' Create clean HTML with proper structure and styling
#' 
#' @param title Section title
#' @param content HTML content
#' @return Clean HTML as a character string
create_clean_html <- function(title, content) {
  # Parse the content to clean it up
  content_soup <- read_html(content)
  
  # Remove any existing style tags
  style_tags <- content_soup %>% html_nodes("style")
  for (style in style_tags) {
    xml_remove(style)
  }
  
  # Clean up the content
  clean_content <- as.character(content_soup)
  
  # Create the final HTML
  html_template <- sprintf('<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>%s - The Life of Saint Mary the Egyptian</title>
    <style>
        body {
            font-family: "Times New Roman", serif;
            line-height: 1.6;
            max-width: 800px;
            margin: 0 auto;
            padding: 20px;
            background-color: #ffffff;
        }
        h1 {
            text-align: center;
            font-size: 24px;
            font-weight: bold;
            margin-bottom: 30px;
            color: #000000;
        }
        h2 {
            font-size: 18px;
            font-weight: bold;
            margin-top: 25px;
            margin-bottom: 15px;
            color: #000000;
        }
        p {
            text-indent: 36pt;
            margin-bottom: 15px;
            font-size: 12pt;
            text-align: left;
        }
        .keywords {
            font-weight: bold;
            margin-top: 20px;
        }
        .keywords span {
            font-weight: normal;
        }
        em {
            font-style: italic;
        }
        sup {
            vertical-align: super;
            font-size: smaller;
        }
        table {
            border-collapse: collapse;
            margin: 20px 0;
            width: 100%%;
        }
        td {
            border: 1px solid #000;
            padding: 10px;
            vertical-align: top;
        }
    </style>
</head>
<body>
%s
</body>
</html>', title, clean_content)
  
  return(html_template)
}

#' Extract a subsection from HTML file by section name and heading level
#' 
#' @param html_file Path to the HTML file
#' @param section_name The name of the subsection (e.g., 'Methods: Authoring in Quarto')
#' @param heading_level The heading level to search for (e.g., 'h2', 'h3', 'h4', etc.)
#' @param parent_section Optional parent section name to search within (e.g., '2.2 An Approach to Editing Version T, Part 2')
#' @param output_file Output file path (optional). If NULL, prints to console
#' @param clean_html Logical. If TRUE (default), creates clean HTML with styling. If FALSE, returns raw HTML as-is from the file.
#' @param verbose Logical. If TRUE (default), prints status messages. If FALSE, suppresses output.
#' @return The extracted HTML content as a character string
extract_subsection_by_name <- function(html_file, section_name, heading_level = "h2", parent_section = NULL, output_file = NULL, clean_html = TRUE, verbose = TRUE) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # If parent_section is specified, first extract that section
  if (!is.null(parent_section)) {
    # Find the parent section first
    parent_headings <- html_content %>% html_nodes("h1")
    parent_heading <- NULL
    
    for (h in parent_headings) {
      text <- h %>% html_text() %>% str_trim()
      # Strip comment references like [f], [g], etc. for matching
      text_clean <- str_replace_all(text, "\\[[a-zA-Z]\\]", "")
      text_clean <- str_squish(text_clean)
      
      if (str_detect(tolower(text_clean), tolower(parent_section))) {
        parent_heading <- h
        break
      }
    }
    
    if (is.null(parent_heading)) {
      stop(paste("Error: Parent section '", parent_section, "' not found.", sep = ""))
    }
    
    # Extract content from parent section
    parent_content <- extract_content_until_next_heading(html_content, parent_heading)
    # Parse the parent content to search within it
    html_content <- read_html(parent_content)
  }
  
  # Find the subsection heading with the specified text and level
  headings <- html_content %>% html_nodes(heading_level)
  
  heading <- NULL
  for (h in headings) {
    text <- h %>% html_text() %>% str_trim()
    # Strip comment references like [f], [g], etc. for matching
    text_clean <- str_replace_all(text, "\\[[a-zA-Z]\\]", "")
    text_clean <- str_squish(text_clean)
    
    if (str_detect(tolower(text_clean), tolower(section_name))) {
      heading <- h
      break
    }
  }
  
  if (is.null(heading)) {
    stop(paste("Error: Subsection '", section_name, "' at level '", heading_level, "' not found.", sep = ""))
  }
  
  # Get the section title
  section_title <- heading %>% html_text() %>% str_trim()
  
  # Find all content until the next heading at the same level or higher
  extracted_content <- extract_content_until_next_same_level_heading(html_content, heading, heading_level)
  
  # Create output HTML based on clean_html parameter
  if (clean_html) {
    final_html <- create_clean_html(section_title, extracted_content)
  } else {
    # Return raw HTML as-is
    final_html <- extracted_content
  }
  
  # Save or print
  if (!is.null(output_file)) {
    writeLines(final_html, output_file, useBytes = TRUE)
    if (verbose) {
      cat("Subsection '", section_title, "' extracted to '", output_file, "'\n", sep = "")
    }
  } else {
    if (verbose) {
      cat(final_html)
    }
  }
  
  return(final_html)
}

#' List all available subsections within a section or at a specific heading level
#' 
#' @param html_file Path to the HTML file
#' @param heading_level The heading level to list (e.g., 'h2', 'h3', 'h4', etc.)
#' @param parent_section Optional parent section name to search within
list_available_subsections <- function(html_file, heading_level = "h2", parent_section = NULL) {
  # Check if file exists
  if (!file.exists(html_file)) {
    stop(paste("Error: File '", html_file, "' not found.", sep = ""))
  }
  
  # Read and parse HTML
  html_content <- read_html(html_file)
  
  # If parent_section is specified, first extract that section
  if (!is.null(parent_section)) {
    # Find the parent section first
    parent_headings <- html_content %>% html_nodes("h1")
    parent_heading <- NULL
    
    for (h in parent_headings) {
      text <- h %>% html_text() %>% str_trim()
      # Strip comment references like [f], [g], etc. for matching
      text_clean <- str_replace_all(text, "\\[[a-zA-Z]\\]", "")
      text_clean <- str_squish(text_clean)
      
      if (str_detect(tolower(text_clean), tolower(parent_section))) {
        parent_heading <- h
        break
      }
    }
    
    if (is.null(parent_heading)) {
      stop(paste("Error: Parent section '", parent_section, "' not found.", sep = ""))
    }
    
    # Extract content from parent section
    parent_content <- extract_content_until_next_heading(html_content, parent_heading)
    # Parse the parent content to search within it
    html_content <- read_html(parent_content)
    
    cat("Available subsections in '", parent_section, "' at level ", heading_level, ":\n", sep = "")
  } else {
    cat("Available sections at level ", heading_level, ":\n", sep = "")
  }
  
  cat(paste(rep("-", 70), collapse = ""), "\n")
  
  # Find all headings at the specified level
  headings <- html_content %>% html_nodes(heading_level)
  
  if (length(headings) == 0) {
    cat("No headings found at level ", heading_level, "\n")
    return()
  }
  
  for (i in seq_along(headings)) {
    title <- headings[i] %>% html_text() %>% str_trim()
    heading_id <- headings[i] %>% html_attr("id")
    if (is.na(heading_id)) heading_id <- "No ID"
    
    # Strip comment references for display
    title_clean <- str_replace_all(title, "\\[[a-zA-Z]\\]", "")
    title_clean <- str_squish(title_clean)
    
    cat(sprintf("%2d. %s\n", i, title))
    if (title != title_clean) {
      cat(sprintf("    Clean: %s\n", title_clean))
    }
    cat(sprintf("    ID: %s\n", heading_id))
    cat("\n")
  }
}

#' Extract content from a heading until the next heading at the same level or higher
#' 
#' @param html_content Parsed HTML content
#' @param start_heading The starting heading element
#' @param heading_level The level of the starting heading (e.g., 'h2', 'h3')
#' @return HTML content as a character string
extract_content_until_next_same_level_heading <- function(html_content, start_heading, heading_level) {
  # Extract the numeric level from heading_level (e.g., 'h2' -> 2)
  current_level <- as.numeric(str_extract(heading_level, "\\d+"))
  
  # Create a selector for headings at the same level or higher (lower numbers)
  # For h2, we want to stop at h1 or h2; for h3, we want to stop at h1, h2, or h3
  stop_levels <- paste0("h", 1:current_level, collapse = ", ")
  
  # Get all headings at the same level or higher
  all_stop_headings <- html_content %>% html_nodes(stop_levels)
  
  # Find the text of our starting heading
  start_heading_text <- html_text(start_heading) %>% str_trim()
  
  # Find the position of our starting heading
  start_index <- NA
  for (i in seq_along(all_stop_headings)) {
    if (str_trim(html_text(all_stop_headings[i])) == start_heading_text) {
      start_index <- i
      break
    }
  }
  
  if (is.na(start_index)) {
    return(as.character(start_heading))
  }
  
  # Find the next heading at the same level or higher
  end_index <- if (start_index < length(all_stop_headings)) start_index + 1 else NA
  
  # Get all elements in document order
  all_elements <- html_content %>% html_nodes("body") %>% xml_children()
  
  # Find the start and end positions by matching heading text
  start_pos <- NA
  end_pos <- NA
  
  for (i in seq_along(all_elements)) {
    el <- all_elements[[i]]
    element_name <- xml_name(el)
    
    # Check if this is our starting heading
    if (element_name == heading_level && str_trim(html_text(el)) == start_heading_text && is.na(start_pos)) {
      start_pos <- i
    } else if (!is.na(end_index) && !is.na(start_pos)) {
      # Check if this is the next heading at same level or higher
      next_heading_text <- str_trim(html_text(all_stop_headings[end_index]))
      if (str_detect(stop_levels, element_name) && str_trim(html_text(el)) == next_heading_text) {
        end_pos <- i - 1
        break
      }
    }
  }
  
  if (is.na(start_pos)) {
    return(as.character(start_heading))
  }
  if (is.na(end_pos)) {
    end_pos <- length(all_elements)
  }
  
  # Extract the main content
  extracted_elements <- all_elements[start_pos:end_pos]
  
  # Deduplicate content to prevent the same text from appearing multiple times
  seen_content <- character(0)
  unique_elements <- list()
  
  for (element in extracted_elements) {
    element_text <- xml_text(element)
    element_text_clean <- str_squish(element_text)
    
    # Skip empty elements
    if (element_text_clean == "") {
      next
    }
    
    # Normalize the text for better comparison
    element_text_normalized <- str_replace_all(element_text_clean, "\\s+", " ")
    element_text_normalized <- str_trim(element_text_normalized)
    
    # Check if we've seen this content before
    if (!(element_text_normalized %in% seen_content)) {
      seen_content <- c(seen_content, element_text_normalized)
      unique_elements <- c(unique_elements, list(element))
    }
  }
  
  extracted_content <- paste(sapply(unique_elements, as.character), collapse = "\n")
  
  # Find footnote references in the extracted content
  footnote_refs <- character(0)
  for (element in extracted_elements) {
    footnote_links <- element %>% html_nodes("a[href*='#ftnt']")
    for (link in footnote_links) {
      href <- html_attr(link, "href")
      footnote_num <- stringr::str_extract(href, "\\d+")
      if (!is.na(footnote_num)) {
        footnote_refs <- c(footnote_refs, footnote_num)
      }
    }
  }
  footnote_refs <- unique(sort(as.numeric(footnote_refs)))
  
  # Extract footnote definitions for the referenced footnotes
  if (length(footnote_refs) > 0) {
    footnote_definitions <- character(0)
    for (footnote_num in footnote_refs) {
      footnote_anchor <- html_content %>% html_nodes(paste0("a[id='ftnt", footnote_num, "']"))
      if (length(footnote_anchor) > 0) {
        footnote_element <- footnote_anchor[[1]]
        # Traverse up to the parent div
        current_parent <- xml_parent(footnote_element)
        footnote_div <- NULL
        while (!is.na(current_parent)) {
          if (xml_name(current_parent) == "div") {
            if (length(xml_find_all(current_parent, paste0(".//a[@id='ftnt", footnote_num, "']"))) > 0) {
              footnote_div <- current_parent
              break
            }
          }
          next_parent <- tryCatch(xml_parent(current_parent), error = function(e) NA)
          if (is.na(next_parent)) {
            break
          }
          current_parent <- next_parent
        }
        if (!is.null(footnote_div) && !is.na(footnote_div)) {
          # Get all content from the footnote div, but exclude the anchor element
          footnote_children <- xml_children(footnote_div)
          
          # Filter out the anchor element (the one with the footnote number)
          content_children <- list()
          for (child in footnote_children) {
            # Skip if this is the anchor element
            if (xml_name(child) == "a" && xml_attr(child, "id") == paste0("ftnt", footnote_num)) {
              next
            }
            # Skip if this is a p element that contains only the anchor
            if (xml_name(child) == "p") {
              p_children <- xml_children(child)
              if (length(p_children) == 1 && xml_name(p_children[[1]]) == "a" && 
                  xml_attr(p_children[[1]], "id") == paste0("ftnt", footnote_num)) {
                next
              }
              # Also skip if this p element contains the anchor and nothing else meaningful
              p_text <- xml_text(child)
              p_text_clean <- str_replace_all(p_text, paste0("\\[", footnote_num, "\\]"), "")
              p_text_clean <- str_squish(p_text_clean)
              if (p_text_clean == "") {
                next
              }
            }
            content_children <- c(content_children, list(child))
          }
          
          # Convert the filtered content to HTML
          footnote_html_content <- paste(sapply(content_children, as.character), collapse = "")
          footnote_html_content <- str_replace_all(footnote_html_content, "\\[[a-zA-Z]{1,3}\\]", "")
          # Remove any remaining footnote number references
          footnote_html_content <- str_replace_all(footnote_html_content, paste0("\\[", footnote_num, "\\]"), "")
          footnote_html_content <- str_squish(footnote_html_content)
          
          footnote_def <- paste0('<div class="footnote" id="ftnt', footnote_num, '_def">',
                               '<sup>[', footnote_num, ']</sup> ', footnote_html_content, '</div>')
          footnote_definitions <- c(footnote_definitions, footnote_def)
        }
      }
    }
    if (length(footnote_definitions) > 0) {
      extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
      extracted_content <- paste0(extracted_content, "\n\n<div class='footnotes'>\n",
                                 paste(footnote_definitions, collapse = "\n"), "\n</div>")
    } else {
      extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
    }
  } else {
    extracted_content <- paste(sapply(extracted_elements, as.character), collapse = "\n")
  }
  
  return(extracted_content)
}

#' Simple wrapper for extracting subsections with common usage patterns
#' 
#' @param html_file Path to the HTML file
#' @param section_name The name of the subsection to extract
#' @param level The heading level (1-6, defaults to 2)
#' @param parent Optional parent section name to search within
#' @param output_file Output file path (optional)
#' @param verbose Whether to print status messages (default TRUE)
#' @return The extracted HTML content as a character string
#' 
#' @examples
#' # Extract an h2 subsection
#' extract_subsection("dissertation.html", "Methods: Authoring in Quarto")
#' 
#' # Extract an h3 subsection  
#' extract_subsection("dissertation.html", "Fetishism and Idolatry", level = 3)
#' 
#' # Extract subsection within a parent section
#' extract_subsection("dissertation.html", "The Portraits", parent = "2.2 An Approach")
extract_subsection <- function(html_file, section_name, level = 2, parent = NULL, output_file = NULL, verbose = TRUE) {
  heading_level <- paste0("h", level)
  extract_subsection_by_name(html_file, section_name, heading_level, parent, output_file, verbose = verbose)
}

#' Simple wrapper for listing subsections with common usage patterns
#' 
#' @param html_file Path to the HTML file
#' @param level The heading level to list (1-6, defaults to 2)
#' @param parent Optional parent section name to search within
#' 
#' @examples
#' # List all h2 subsections
#' list_subsections("dissertation.html")
#' 
#' # List all h3 subsections
#' list_subsections("dissertation.html", level = 3)
#' 
#' # List subsections within a parent section
#' list_subsections("dissertation.html", parent = "2.2 An Approach")
list_subsections <- function(html_file, level = 2, parent = NULL) {
  heading_level <- paste0("h", level)
  list_available_subsections(html_file, heading_level, parent)
}