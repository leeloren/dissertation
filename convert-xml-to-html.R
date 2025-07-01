#function to create html from xml
convert_xml_to_html <- function(xml_file, 
                                version_name = c("minimal", "intermediate", "extensive"),
                                min_line = NULL, 
                                max_line = NULL) {

    # Check if the version name is valid
    version_name <- match.arg(version_name)

    # Read the XML content
    xml_file <- read_xml(xml_file)

    # Extract both <l>, <cb>, and <pb> elements
    lines <- xml_find_all(xml_file, "//text//l | //text//cb | //text//pb")

    # Initialize an empty list to store the processed lines
    processed_lines <- list()

    # Initialize line counter
    line_count <- 0
    # Track if we're within our desired verse range
    in_desired_range <- FALSE

    # Function to process each element
    process_line <- function(line) {
    
    # Only increment line count for verse lines
    if (xml_name(line) == "l") {
        line_count <<- line_count + 1
        
        # Check if we're in the desired range (verses 613-702)
        if (is.null(min_line) && is.null(max_line)) { #min and max line are NULL
            in_desired_range <<- TRUE
        } else if (is.null(min_line) && !is.null(max_line) && line_count <= max_line) { #min line is NULL, max line is not NULL, and line is within range
            in_desired_range <<- TRUE
        } else if (!is.null(min_line) && is.null(max_line) && line_count >= min_line) { #min line is not NULL, max line is NULL, and line is within range
            in_desired_range <<- TRUE
        } else if (!is.null(min_line) && !is.null(max_line) && line_count >= min_line && line_count <= max_line) { #min and max line are not NULL, and line is within range
            in_desired_range <<- TRUE
        } else {
            in_desired_range <<- FALSE
            return(NULL) # Skip lines outside our range
        }
        
        # Add line number div every 5th line
        line_number <- if (line_count %% 5 == 0) {
        sprintf("<div class='line-number'>%d</div>", line_count)
        } else {
        ""
        }
    } else {
        line_number <- ""
        # For non-line elements, if we're not in range, skip them
        if (!in_desired_range) {
        return(NULL)
        }
    }
    
    # Process emph elements (decorative initials)
    emph_elements <- xml_find_all(line, ".//emph")
    for (emph in emph_elements) {
        # Get the initial letter text
        initial_letter <- xml_text(emph)
        
        # Get the size attribute (n) and style attribute (rend)
        size_attr <- xml_attr(emph, "n")
        style_attr <- xml_attr(emph, "rend")
        
        # Create the HTML for the styled initial
        styled_initial <- sprintf('<span class="decorative-initial %s" data-size="%s">%s</span>', 
                                style_attr, size_attr, initial_letter)
        
        # Replace the emph element with the styled HTML
        xml_text(emph) <- styled_initial
    }
    

    if (version_name == "minimal") {

        # Find all <choice> elements within the line
        choices <- xml_find_all(line, ".//choice")

        for (choice in choices) {
            # Extract the text from the <orig> tag
            orig_text <- xml_text(xml_find_first(choice, ".//orig"))
            # Remove all children and set text directly
            xml_remove(xml_children(choice))
            xml_text(choice) <- paste0(orig_text, " ")
        }

        # Now we want to remove any <reg> elements entirely
        reg_elements <- xml_find_all(line, ".//reg")
        xml_remove(reg_elements)

    } else if (version_name == "intermediate") {
        # Find all <choice> elements within the line
        choices <- xml_find_all(line, ".//choice")

        for (choice in choices) {
            abbr_exists <- xml_find_first(choice, ".//abbr")
            expan_exists <- xml_find_first(choice, ".//expan")
            
            if (!is.na(abbr_exists) && !is.na(expan_exists)) {
                # First check if there's an intermediate tag
                intermediate_node <- xml_find_first(choice, ".//expan/intermediate")
                if (!is.na(intermediate_node)) {
                    # If intermediate exists, use its text
                    text_to_use <- xml_text(intermediate_node)
                } else {
                    # If no intermediate, use reg text
                    text_to_use <- xml_text(xml_find_first(choice, ".//expan/reg"))
                }
                xml_remove(xml_children(choice))
                xml_text(choice) <- paste0(text_to_use, " ")
            } else {
                orig_text <- xml_text(xml_find_first(choice, ".//orig"))
                xml_remove(xml_children(choice))
                xml_text(choice) <- paste0(orig_text, " ")
            }
        }
        # Now we want to remove any <reg> elements entirely
        reg_elements <- xml_find_all(line, ".//reg")
        xml_remove(reg_elements)

    } else if (version_name == "extensive") {

        # Now we want to remove any <orig> elements entirely
        orig_elements <- xml_find_all(line, ".//orig")
        xml_remove(orig_elements)

        # Find all <choice> elements within the line
        choices <- xml_find_all(line, ".//choice")
        for (choice in choices) {
            # Extract the text from the <reg> tag
            reg_text <- xml_text(xml_find_first(choice, ".//reg"))
            # Remove all children and set text directly
            xml_remove(xml_children(choice))
            xml_text(choice) <- paste0(" ", reg_text, " ")
        }

    }
    
    # Replace <lb/> tags with HTML line break
    lb_elements <- xml_find_all(line, ".//lb")
    for (lb in lb_elements) {
        xml_set_text(lb, "")
    }
    
    # Return line with number if applicable
    return(sprintf("<div style='line-height: 1.2;'>%s%s</div>", 
                line_number, xml_text(line)))
    }

    # Process each line and wrap with <div class='column-break'> at each <cb/>
    for (line in lines) {
    processed_line <- process_line(line)
    if (!is.null(processed_line)) {
        if (xml_name(line) == "cb" && in_desired_range) {
        # Add a closing </div> for the previous column and an opening <div> for the new column
        processed_lines <- c(processed_lines, "</div>", "<div class='column-break'>")
        } else {
        # Process the line and add it to the current column
        processed_lines <- c(processed_lines, processed_line)
        }
    }
    }

    # Process lines and wrap in edition-text div
    formatted_text <- paste(c("<div class='edition-text'>", processed_lines, "</div>"), collapse = "\n")
    cat(formatted_text)
}


