#' Get bicluster conditions from SQLite database.
#'
#' \code{getConditions} retrieves conditions from a 
#' user specified bicluster at any iteration
#' of cMonkey algorithm
#'
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#'
#' @return Vector of conditions in the bicluster
#' 
#'  @export
getConditions <- function( db_path = "", bicluster = 1, iteration = "max" ) {
	if ( is.character( db_path ) ) {
		db_path <- load_cMonkey( db_path, verbose = F )
	}
	if ( iteration=="max" ) {
		iteration <- as.numeric( dbGetQuery( db_path, "SELECT MAX(iteration) FROM column_members;" ) )
	}
	to_r <- dbGetQuery( db_path, paste(
		"SELECT column_names.name FROM column_names 
		INNER JOIN column_members 
		ON column_names.order_num = column_members.order_num
		WHERE column_members.iteration = ", iteration,
		" AND column_members.cluster = ", bicluster, ";", sep="" ) )
	dbDisconnect( db_path )
	return( c( to_r[[1]] ) )
}