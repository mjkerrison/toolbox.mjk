

#' List tables by schema from a database connection.
#'
#' @description AFAIK this kind of thing is quite language / infra-dependent.
#'   This is intended for Microsoft SQL Server.
#'
#' @param conn_obj
#'
#' @return
#' @export
#'
#' @examples
dbListTablesBySchema <- function(conn_obj){
  
  # Tables
  db_tables <- DBI::dbGetQuery(conn_obj, glue::glue("

  select schema_name(t.schema_id) as schema_name,
    t.name as table_name
  from sys.tables t
  order by table_name;"
                                                    
  )) %>% setNames(c("Schema", "Obj_Name")) %>%
    
    mutate(Type = "Table")
  
  
  # Views
  db_views <- DBI::dbGetQuery(conn_obj, glue::glue("

  SELECT
    TABLE_SCHEMA,
    TABLE_NAME
  FROM INFORMATION_SCHEMA.VIEWS;"
                                                   
  )) %>% setNames(c("Schema", "Obj_Name")) %>%
    
    mutate(Type = "View")
  
  
  bind_rows(db_tables, db_views) %>%
    as_tibble() %>%
    return()
  
  
}


# dbListSchemas <- function(conn_obj){
#
#   dbGetQuery(conn_obj,
#              "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")
#
# }
