





#' Title
#'
#' @param project_slug 
#' @param project_name 
#' @param project_contact 
#' @param target_directory 
#'
#' @return
#' @export
#'
#' @examples
init_project <- function(project_slug = "",
                         project_name = "",
                         project_contact = "",
                         target_directory = "."){
  
  # If we're in package development, grab it locally, else look it up from
  # installed package:
  yaml_path <- "inst/project_structure.yaml"
  
  project_structure_yaml <- ifelse(
    
    file.exists(yaml_path), 
    yaml_path, 
    system.file(yaml_path, package = "toolbox.mjk")
    
  ) |> yaml::read_yaml()
  
  
  recurse_over_project_structure_yaml(
    target_directory, 
    project_structure_yaml, 
    names(project_structure_yaml),
    project_slug,
    project_name,
    project_contact
  )
  
  
  message("Project setup complete.")
  return(invisible(TRUE))
  
}






#' Title
#'
#' @param current_dir 
#' @param a_list 
#' @param list_names 
#' @param project_slug 
#' @param project_name 
#' @param project_contact 
#' @param verbose 
#'
#' @return
#' @export
#'
#' @examples
recurse_over_project_structure_yaml <- function(current_dir,
                                                a_list,
                                                list_names,
                                                project_slug = "",
                                                project_name = "",
                                                project_contact = "",
                                                verbose = TRUE){
  
  # Maybe make current_dir =====================================================
  
  if(!dir.exists(current_dir)){
    
    dir.create(current_dir)
    
    message(glue::glue("✅ Created directory:  {current_dir}"))
    
  } else {
    
    message(glue::glue("⏩ Exists (skipping):  {current_dir}"))
    
  }
  
  # Go through list of current_dir contents ====================================
  
  walk2(a_list, list_names, \(list_i, name_i){
    
    theoretical_path <- glue::glue("{current_dir}/{stringr::str_replace_all(name_i, '📄|📁', '')}")
    
    ## If it's a subfolder, do recursively ---------------------------
    
    if(stringr::str_starts(name_i, "📁")){
      
      recurse_over_project_structure_yaml(
        theoretical_path,
        list_i, 
        names(list_i),
        project_slug,
        project_name,
        project_contact
      )
      
    }
    
    ## If it's a file, maybe create ----------------------------------
    
    if(stringr::str_starts(name_i, "📄")){
      
      if(!file.exists(theoretical_path)){
        
        list_i[[1]] |> 
          
          stringr::str_replace_all(
            
            c(
              "$PROJECT_SLUG" = project_slug,
              "$PROJECT_NAME" = project_name,
              "$PROJECT_CONTACT" = project_contact
            )
            
          ) |> 
          
          writeLines(theoretical_path)
        
        message(glue::glue("✅ Created file:       {current_dir}"))
        
      } else {
        
        message(glue::glue("⏩ Exists (skipping):  {current_dir}"))

      }
      
    }
    
  })
  
  return(invisible(TRUE))
  
}

