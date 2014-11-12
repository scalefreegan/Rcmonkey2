#' Get bicluster genes from SQLite database.
#'
#' \code{getGenes} retrieves genes from a 
#' user specified bicluster at any iteration
#' of cMonkey algorithm
#'
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#'
#' @return Vector of genes in the bicluster
#'
#'  @export
getGenes <- function( db_path = "", bicluster = 1, iteration = "max" ) {
	if ( is.character( db_path ) ) {
		db_path <- load_cMonkey( db_path, verbose = F )
	}
	if ( iteration=="max" ) {
		iteration <- as.numeric( dbGetQuery( db_path, "SELECT MAX(iteration) FROM row_members;" ) )
	} 
	to_r <- dbGetQuery( db_path, paste(
		"SELECT row_names.name FROM row_names 
		INNER JOIN row_members 
		ON row_names.order_num = row_members.order_num
		WHERE row_members.iteration = ", iteration,
		" AND row_members.cluster = ", bicluster, ";", sep="" ) )
	dbDisconnect( db_path )
	return( c( to_r[[1]] ) )
}