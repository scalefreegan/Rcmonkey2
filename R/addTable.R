#' Add table to existing SQLite database.
#'
#' \code{addTable} adds a table to an existing
#' SQLite database from a tab-delimited file,
#' e.g., Microbes Online genome annotation file
#'
#' @param db_path Location of the SQLite database
#' @param table_name Name for new table
#' @param tab_file Tab-delimited file to add to database
#'
#' @return NULL (modified SQLite database)
#'
#' @examples 
#' 
#'  @export
addTable <- function( db_path = "", table_name = "", tab_file = "" ) {
	library( "RSQLite" )
	if (db_path == "" || tab_file == "" || table_name == "") {
		cat("You need to specific an existing database with 
			*db* parameter 
			and 
			tab-delimited annotation file  with 
			*tab_file* parameter
			table name with 
			*table_name* parameter\n")
		return(NULL)
	} else {
		dbconn <- dbConnect( SQLite( ), db_path )
		m_table <- read.delim(tab_file,sep="\t")
		dbWriteTable(dbconn, name = table_name, value = m_table)
	}
	dbDisconnect( dbconn )
}