# Function to convert HTML to clean markdown with formatting using XML parsing
convert_html_to_markdown <- function(html_file) {
  if (!file.exists(html_file)) {
    stop(paste("HTML file '", html_file, "' not found.", sep = ""))
  }
  
  library(xml2)
  library(stringr)
  
  # Read HTML file with UTF-8 encoding
  html_content <- read_html(html_file, encoding = "UTF-8")
  
  # Remove Google Docs comment references before processing
  comment_refs <- xml_find_all(html_content, ".//sup[a[starts-with(@href, '#cmnt')]]")
  for (ref in comment_refs) {
    xml_remove(ref)
  }
  
  # Also remove any remaining comment references in the text using regex
  html_text <- as.character(html_content)
  html_text <- str_replace_all(html_text, "<sup><a[^>]*href=\"#cmnt[^>]*>\\[[a-zA-Z]\\]</a></sup>", "")
  html_content <- read_html(html_text)
  
  # Initialize table_seen_content at the top so it is always defined
  table_seen_content <- character(0)
  
  # Helper: is italic style?
  is_italic <- function(node) {
    style <- xml_attr(node, "style")
    !is.na(style) && str_detect(style, "font-style:italic")
  }
  # Helper: is superscript style?
  is_super <- function(node) {
    style <- xml_attr(node, "style")
    !is.na(style) && str_detect(style, "vertical-align:super")
  }
  # Convert a node to markdown, but don't wrap italics here
  to_md_leaf <- function(node) {
    style <- xml_attr(node, "style")
    text <- xml_text(node)
    if (!is.na(style)) {
      if (str_detect(style, "vertical-align:super")) {
        return(paste0("^", text, "^"))
      }
    }
    return(text)
  }
  # Main function: group adjacent italics, keep superscript inside
  convert_paragraph <- function(p) {
    children <- xml_children(p)
    out <- character(0)
    i <- 1
    while (i <= length(children)) {
      node <- children[[i]]
      
      # Check if this is a sup element with footnote
      if (xml_name(node) == "sup") {
        # Look for footnote link inside sup
        footnote_link <- xml_find_first(node, ".//a[contains(@href, '#ftnt')]")
        if (!is.na(footnote_link)) {
          # Extract footnote number from href or text
          href <- xml_attr(footnote_link, "href")
          footnote_num <- str_extract(href, "\\d+")
          if (is.na(footnote_num)) {
            # Fallback to text content
            footnote_text <- xml_text(footnote_link)
            footnote_num <- str_extract(footnote_text, "\\d+")
          }
          if (!is.na(footnote_num)) {
            out <- c(out, paste0("[^", footnote_num, "]"))
            i <- i + 1
            next
          }
        }
      }
      
      if (is_italic(node)) {
        # Start italic group
        italic_group <- character(0)
        while (i <= length(children) && is_italic(children[[i]])) {
          n <- children[[i]]
          # If also superscript, convert to ^text^
          if (is_super(n)) {
            italic_group <- c(italic_group, paste0("^", xml_text(n), "^"))
          } else {
            italic_group <- c(italic_group, xml_text(n))
          }
          i <- i + 1
        }
        out <- c(out, paste0("*", paste0(italic_group, collapse=""), "*"))
      } else {
        # Not italic
        if (is_super(node)) {
          out <- c(out, paste0("^", xml_text(node), "^"))
        } else {
          out <- c(out, xml_text(node))
        }
        i <- i + 1
      }
    }
    paste0(out, collapse = "")
  }
  
  # Function to convert heading to markdown
  convert_heading <- function(heading) {
    level <- as.numeric(str_extract(xml_name(heading), "\\d+"))
    if (is.na(level)) level <- 1
    
    # Use the same logic as convert_paragraph to extract heading text
    children <- xml_children(heading)
    if (length(children) == 0) {
      heading_text <- xml_text(heading)
    } else {
      heading_text <- convert_paragraph(heading)
    }
    heading_text <- str_squish(heading_text)
    
    # Create markdown heading with appropriate number of #
    paste0(paste(rep("#", level), collapse = ""), " ", heading_text)
  }
  
  # Process all elements in document order
  text_content <- character(0)
  footnote_definitions <- character(0)
  
  # Get all elements in document order
  all_elements <- xml_find_all(html_content, ".//p | .//h1 | .//h2 | .//h3 | .//h4 | .//h5 | .//h6")
  
  # Track if we've seen the first h1
  first_h1_skipped <- FALSE
  
  # Helper to normalize text for deduplication
  normalize_for_dedup <- function(txt) {
    txt <- str_replace_all(txt, "\\*|_", "") # remove asterisks and underscores
    txt <- str_replace_all(txt, "<br>", " ") # treat <br> as space
    txt <- str_replace_all(txt, "\\n", " ") # treat newlines as space
    txt <- str_replace_all(txt, "\\s+", " ") # collapse whitespace
    str_trim(txt)
  }

  # Track seen content to prevent duplicates
  seen_content <- character(0)
  
  # Create a list to store content with table insertions
  final_content <- list()
  content_index <- 1
  
  # Detect all tables and convert them to markdown tables
  table_md <- character(0)
  table_positions <- integer(0)
  all_tables <- xml_find_all(html_content, ".//table")
  
  for (table_idx in seq_along(all_tables)) {
    current_table <- all_tables[[table_idx]]
    
    # Find the position of this table in the document
    # We'll use the position of the first paragraph before this table
    table_position <- 0
    prev_elements <- xml_find_all(current_table, "preceding::p | preceding::h1 | preceding::h2 | preceding::h3 | preceding::h4 | preceding::h5 | preceding::h6")
    if (length(prev_elements) > 0) {
      table_position <- length(prev_elements)
    }
    
    rows <- xml_find_all(current_table, ".//tr")
    if (length(rows) > 0) {
      # Get the first row (should be the only row in this case)
      first_row <- rows[[1]]
      cells <- xml_find_all(first_row, ".//td")
      
      if (length(cells) == 2) {
        # Get all p elements from each cell
        left_cell <- cells[[1]]
        right_cell <- cells[[2]]
        
        left_ps <- xml_find_all(left_cell, ".//p")
        right_ps <- xml_find_all(right_cell, ".//p")
        
        # Convert each p element to markdown
        left_rows <- sapply(left_ps, function(p) {
          convert_paragraph(p)
        })
        right_rows <- sapply(right_ps, function(p) {
          convert_paragraph(p)
        })
        
        # Clean up empty rows and normalize
        left_rows <- str_squish(left_rows)
        right_rows <- str_squish(right_rows)
        
        # Remove completely empty rows
        left_rows <- left_rows[left_rows != ""]
        right_rows <- right_rows[right_rows != ""]
        
        # Special case: Remove the blank row with "**" from this specific table
        # This is the table with the quote about "Nos conteurs se fourvoient"
        if (length(left_rows) > 0 && length(right_rows) > 0) {
          # Check if this looks like the specific table we want to fix
          if (any(str_detect(left_rows, "Il sunt del cunte forsveié")) && 
              any(str_detect(right_rows, "Nos conteurs se fourvoient"))) {
            # Remove the row that contains just "**"
            left_rows <- left_rows[left_rows != "**"]
            right_rows <- right_rows[right_rows != "**"]
          }
        }
        
        # Add ALL table content to seen_content for deduplication
        all_table_content <- c(left_rows, right_rows)
        for (content in all_table_content) {
          if (content != "") {
            seen_content <- c(seen_content, normalize_for_dedup(content))
          }
        }
        
        # Also add the raw text content to catch duplicates that might appear as separate elements
        for (p in c(left_ps, right_ps)) {
          raw_text <- xml_text(p)
          if (raw_text != "") {
            seen_content <- c(seen_content, normalize_for_dedup(raw_text))
          }
        }
        
        # Determine the maximum number of rows needed
        max_rows <- max(length(left_rows), length(right_rows))
        
        # Pad the shorter column with empty strings
        while (length(left_rows) < max_rows) {
          left_rows <- c(left_rows, "")
        }
        while (length(right_rows) < max_rows) {
          right_rows <- c(right_rows, "")
        }
        
        # Build the markdown table
        table_rows <- character(0)
        for (i in 1:max_rows) {
          table_row <- paste0("| ", left_rows[i], " | ", right_rows[i], " |")
          table_rows <- c(table_rows, table_row)
        }
        
        # Add separator row after the first row
        ncol <- 2
        sep_row <- paste0("| ", paste(rep("---", ncol), collapse = " | "), " |")
        current_table_md <- paste0(table_rows[1], "\n", sep_row, "\n", paste(table_rows[-1], collapse = "\n"))
        
        # Add CSS class to the table using Quarto table attributes
        current_table_md <- paste0("::: {.quote-table}\n", current_table_md, "\n:::")
        
        # Add this table to the collection with its position
        table_md <- c(table_md, current_table_md)
        table_positions <- c(table_positions, table_position)
      }
    }
  }
  
  for (element_idx in seq_along(all_elements)) {
    element <- all_elements[[element_idx]]
    element_name <- xml_name(element)
    
    # Check if we need to insert a table at this position
    if (length(table_positions) > 0) {
      for (table_idx in seq_along(table_positions)) {
        if (table_positions[table_idx] == element_idx - 1) {
          # Insert table at this position
          final_content[[content_index]] <- table_md[table_idx]
          content_index <- content_index + 1
        }
      }
    }
    
    if (element_name == "p") {
      # Check if this paragraph is part of a footnote definition
      # If so, skip it in the main text processing
      is_footnote_content <- FALSE
      tryCatch({
        parent <- xml_parent(element)
        while (!is.na(parent)) {
          if (xml_name(parent) == "div" && xml_attr(parent, "id") != "" && 
              str_detect(xml_attr(parent, "id"), "^ftnt\\d+_def$")) {
            # This paragraph is part of a footnote definition, skip it
            is_footnote_content <- TRUE
            break
          }
          parent <- xml_parent(parent)
        }
      }, error = function(e) {
        # If xml_parent fails, assume it's not footnote content
        is_footnote_content <- FALSE
      })
      
      if (is_footnote_content) {
        # Skip this paragraph as it's part of a footnote
        next
      }
      
      # Check if this is a blockquote (has margin-left:36pt styling)
      style <- xml_attr(element, "style")
      is_blockquote <- FALSE
      if (!is.na(style)) {
        # Check for margin-left:36pt which indicates a blockquote in footnotes
        is_blockquote <- str_detect(style, "margin-left.*36pt") || 
                        str_detect(style, "margin-left.*36")
      }
      
      # Process paragraph
      md <- convert_paragraph(element)
      md <- str_squish(md)
      if (md != "" && md != "**") {
        # Check if we've seen this content before
        md_normalized <- normalize_for_dedup(md)
        if (!(md_normalized %in% seen_content)) {
          seen_content <- c(seen_content, md_normalized)
          
          # Format as blockquote if detected
          if (is_blockquote) {
            # Split into lines and add > to each line
            lines <- str_split(md, "\n")[[1]]
            blockquote_lines <- sapply(lines, function(line) {
              if (str_trim(line) != "") {
                paste0("> ", line)
              } else {
                ">"
              }
            })
            md <- paste(blockquote_lines, collapse = "\n")
          }
          
          final_content[[content_index]] <- md
          content_index <- content_index + 1
        }
      }
    } else if (str_detect(element_name, "^h\\d+$")) {
      # Process heading
      if (element_name == "h1" && !first_h1_skipped) {
        # Skip the first h1
        first_h1_skipped <- TRUE
        next
      }
      md <- convert_heading(element)
      if (md != "" && md != "**") {
        # Check if we've seen this heading before
        md_normalized <- normalize_for_dedup(md)
        if (!(md_normalized %in% seen_content)) {
          seen_content <- c(seen_content, md_normalized)
          final_content[[content_index]] <- md
          content_index <- content_index + 1
        }
      }
    }
  }
  
  # Check if we need to insert any remaining tables at the end
  if (length(table_positions) > 0) {
    for (table_idx in seq_along(table_positions)) {
      if (table_positions[table_idx] >= length(all_elements)) {
        # Insert table at the end
        final_content[[content_index]] <- table_md[table_idx]
        content_index <- content_index + 1
      }
    }
  }
  
  # Helper function to convert a single element (span, etc.) to markdown, recursively
  convert_single_element <- function(element) {
    element_name <- xml_name(element)
    children <- xml_children(element)
    
    # Detect blockquote style for <p> or <span> at the top level
    if (element_name == "p" || element_name == "span") {
      style <- xml_attr(element, "style")
      is_blockquote <- FALSE
      if (!is.na(style)) {
        has_border <- str_detect(style, "border.*solid") || 
                     str_detect(style, "border.*style.*solid") ||
                     str_detect(style, "border.*width.*1pt") ||
                     str_detect(style, "border.*width.*1px")
        has_background <- str_detect(style, "background.*color") && 
                         !str_detect(style, "background.*color.*transparent")
        is_blockquote <- has_border || has_background
      }
      
      if (is_blockquote) {
        # Convert to blockquote
        if (length(children) > 0) {
          child_content <- sapply(children, convert_single_element)
          content <- paste(child_content, collapse = "")
          # Split into lines and prefix each with >
          lines <- str_split(content, "\n")[[1]]
          blockquote_lines <- sapply(lines, function(line) {
            if (str_trim(line) == "") return("")
            paste0("> ", line)
          })
          return(paste(blockquote_lines, collapse = "\n"))
        } else {
          text_content <- xml_text(element)
          if (str_trim(text_content) == "") return("")
          lines <- str_split(text_content, "\n")[[1]]
          blockquote_lines <- sapply(lines, function(line) {
            if (str_trim(line) == "") return("")
            paste0("> ", line)
          })
          return(paste(blockquote_lines, collapse = "\n"))
        }
      }
    }
    
    # Handle different element types
    if (element_name == "span") {
      style <- xml_attr(element, "style")
      is_italic <- FALSE
      is_bold <- FALSE
      
      if (!is.na(style)) {
        is_italic <- str_detect(style, "font-style.*italic")
        is_bold <- str_detect(style, "font-weight.*700") || str_detect(style, "font-weight.*bold")
      }
      
      # Process children recursively
      if (length(children) > 0) {
        child_content <- sapply(children, convert_single_element)
        content <- paste(child_content, collapse = "")
      } else {
        content <- xml_text(element)
      }
      
      # Apply formatting
      if (is_italic && is_bold) {
        return(paste0("***", content, "***"))
      } else if (is_italic) {
        return(paste0("*", content, "*"))
      } else if (is_bold) {
        return(paste0("**", content, "**"))
      } else {
        return(content)
      }
    } else if (element_name == "sup") {
      # Handle superscript
      if (length(children) > 0) {
        child_content <- sapply(children, convert_single_element)
        content <- paste(child_content, collapse = "")
      } else {
        content <- xml_text(element)
      }
      return(paste0("^", content, "^"))
    } else if (element_name == "a") {
      # Handle links
      href <- xml_attr(element, "href")
      if (length(children) > 0) {
        child_content <- sapply(children, convert_single_element)
        content <- paste(child_content, collapse = "")
      } else {
        content <- xml_text(element)
      }
      
      if (!is.na(href) && href != "") {
        return(paste0("[", content, "](", href, ")"))
      } else {
        return(content)
      }
    } else if (element_name == "p") {
      # Handle paragraphs
      # Check if this is a blockquote (has margin-left:36pt styling)
      style <- xml_attr(element, "style")
      is_blockquote <- FALSE
      if (!is.na(style)) {
        # Check for margin-left:36pt which indicates a blockquote in footnotes
        is_blockquote <- str_detect(style, "margin-left.*36pt") || 
                        str_detect(style, "margin-left.*36")
      }
      
      if (length(children) > 0) {
        child_content <- sapply(children, convert_single_element)
        content <- paste(child_content, collapse = "")
      } else {
        content <- xml_text(element)
      }
      
      # If this is a blockquote, prefix each line with '> '
      if (is_blockquote) {
        lines <- str_split(content, "\n")[[1]]
        blockquote_lines <- sapply(lines, function(line) {
          if (str_trim(line) == "") return("")
          paste0("> ", line)
        })
        return(paste(blockquote_lines, collapse = "\n"))
      } else {
        return(content)
      }
    } else {
      # For other elements, process children recursively
      if (length(children) > 0) {
        child_content <- sapply(children, convert_single_element)
        return(paste(child_content, collapse = ""))
      } else {
        return(xml_text(element))
      }
    }
  }

  # Look for footnote definitions in the HTML
  # Only look for footnote definitions that were extracted by the section extraction function
  footnote_def_divs <- xml_find_all(html_content, ".//div[contains(@id, '_def')]")
  for (def_div in footnote_def_divs) {
    footnote_id <- xml_attr(def_div, "id")
    # Only process if it's a footnote definition (starts with ftnt and ends with _def)
    if (str_detect(footnote_id, "^ftnt\\d+_def$")) {
      footnote_num <- str_extract(footnote_id, "\\d+")
      if (!is.na(footnote_num)) {
        # Extract content from the footnote definition div, preserving HTML formatting
        # Get all child elements of the footnote div (skip the sup element with the number)
        footnote_children <- xml_children(def_div)
        
        # Convert each child element using the same logic as the main text
        footnote_content_parts <- character(0)
        for (child in footnote_children) {
          if (xml_name(child) == "sup") {
            # Skip the footnote number sup element
            next
          } else {
            # Convert the element using the helper function
            md <- convert_single_element(child)
            if (md != "") {
              footnote_content_parts <- c(footnote_content_parts, md)
            }
          }
        }
        
        # Simple approach: process each part and ensure proper separation
        processed_paragraphs <- character(0)
        
        for (i in seq_along(footnote_content_parts)) {
          part <- str_trim(footnote_content_parts[i])
          if (part == "") next
          
          # Check if this part contains blockquotes
          if (str_detect(part, "> ")) {
            # Split the part into lines and process each line
            lines <- str_split(part, "\n")[[1]]
            current_text <- character(0)
            
            for (line in lines) {
              line <- str_trim(line)
              if (line == "") {
                # Empty line - add current text as paragraph if not empty
                if (length(current_text) > 0) {
                  text <- paste(current_text, collapse = " ")
                  if (i == 1 && length(processed_paragraphs) == 0) {
                    processed_paragraphs <- c(processed_paragraphs, text)
                  } else {
                    processed_paragraphs <- c(processed_paragraphs, paste0("    ", text))
                  }
                  current_text <- character(0)
                }
              } else if (str_detect(line, "^> ")) {
                # Blockquote line - add current text as paragraph first, then add blockquote
                if (length(current_text) > 0) {
                  text <- paste(current_text, collapse = " ")
                  if (i == 1 && length(processed_paragraphs) == 0) {
                    processed_paragraphs <- c(processed_paragraphs, text)
                  } else {
                    processed_paragraphs <- c(processed_paragraphs, paste0("    ", text))
                  }
                  current_text <- character(0)
                }
                # Add blockquote with proper indentation
                if (i == 1 && length(processed_paragraphs) == 0) {
                  processed_paragraphs <- c(processed_paragraphs, line)
                } else {
                  processed_paragraphs <- c(processed_paragraphs, paste0("    ", line))
                }
              } else {
                # Regular text line
                current_text <- c(current_text, line)
              }
            }
            
            # Add any remaining text
            if (length(current_text) > 0) {
              text <- paste(current_text, collapse = " ")
              if (i == 1 && length(processed_paragraphs) == 0) {
                processed_paragraphs <- c(processed_paragraphs, text)
              } else {
                processed_paragraphs <- c(processed_paragraphs, paste0("    ", text))
              }
            }
          } else {
            # Regular paragraph without blockquotes
            if (i == 1 && length(processed_paragraphs) == 0) {
              processed_paragraphs <- c(processed_paragraphs, part)
            } else {
              processed_paragraphs <- c(processed_paragraphs, paste0("    ", part))
            }
          }
        }
        
        # Rejoin paragraphs with proper spacing
        footnote_content <- paste(processed_paragraphs, collapse = "\n\n")
        
        # Remove Google Docs comment references like [as], [at], [e], etc. (robust)
        footnote_content <- str_replace_all(footnote_content, "\\[[a-zA-Z]{1,3}\\]", "")
        footnote_content <- str_squish(footnote_content)
        
        # Remove the footnote number from the beginning if it appears there
        # This handles cases where the number is still in the content
        footnote_content <- str_replace(footnote_content, paste0("^\\[", footnote_num, "\\]\\s*"), "")
        footnote_content <- str_squish(footnote_content)
        
        # Step 1: Remove any spaces just inside the asterisks
        footnote_content <- str_replace_all(footnote_content, "\\*\\s+([^*]+?)\\s+\\*", "*\\1*")
        footnote_content <- str_replace_all(footnote_content, "\\*\\s+([^*]+?)\\*", "*\\1*")
        footnote_content <- str_replace_all(footnote_content, "\\*([^*]+?)\\s+\\*", "*\\1*")
        # Step 2: Add a space before/after *italic* if adjacent to a letter/number
        footnote_content <- str_replace_all(footnote_content, "([a-zA-Z0-9])\\*([^*]+)\\*", "\\1 *\\2*")
        footnote_content <- str_replace_all(footnote_content, "\\*([^*]+)\\*([a-zA-Z0-9])", "*\\1* \\2")
        footnote_content <- str_replace_all(footnote_content, "\\s+", " ")
        footnote_content <- str_trim(footnote_content)
        
        footnote_definitions <- c(footnote_definitions, paste0("[^", footnote_num, "]: ", footnote_content))
      }
    }
  }
  
  # Deduplicate footnote definitions before appending
  footnote_definitions <- unique(footnote_definitions)
  
  # Combine into markdown
  markdown_content <- paste(final_content, collapse = "\n\n")
  
  # Remove any old-style footnote definitions that might have been included in the content
  # These appear as [49], [50], etc. at the end of the document
  markdown_content <- str_replace_all(markdown_content, "\\n\\[\\d+\\]\\s+[^\\n]+\\n", "\n")
  
  # Add footnote definitions at the end
  if (length(footnote_definitions) > 0) {
    markdown_content <- paste0(markdown_content, "\n\n", paste(footnote_definitions, collapse = "\n"))
  }
  
  return(markdown_content)
}