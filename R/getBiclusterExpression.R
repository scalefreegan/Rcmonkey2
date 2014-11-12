#' Get bicluster expression values from SQLite database.
#'
#' \code{getBiclusterExpression} retrieves expression values (genes, conditions) from a 
#' user specified bicluster and/or motif number at any iteration
#' of cMonkey algorithm
#'
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#' @param ratios Path to tab-delimited file or variable containing gene expression data
#'
#' @return Matrix containing gene expression values for each gene and condition in the bicluster
#' @examples 
#' getBiclusterExpression( db_path = "/path/to/database", bicluster = 10, ratios = "/path/to/expression/data" )
#' getBiclusterExpression( db_path = "/path/to/database", bicluster = 100, iteration = 1002, ratios = expression_data_var )
#' 
#'  @export
getBiclusterExpression <- function( db_path = "", bicluster = 1, iteration = "max", ratios = "" ) {
	#' 
	if ( is.character ( ratios ) ) {
		# load ratios if not supplied
		cat("Loading ratios...\n")
		cat("* If you supply them, this will be faster! *\n")
		if ( ratios == "" ) {
			if ( file.exists ( "ratios.tsv.gz" ) ) {
				ratios <- read.table("ratios.tsv.gz")
			} else if ( file.exists ( "./out/ratios.tsv.gz" ) ) {
				ratios <- read.table("./out/ratios.tsv.gz")
			} else {
				cat("Could not find ratios file. Please specify path.")
				return(NULL)
			}
		}
		
	}
	genes <- getGenes( db_path = db_path, bicluster = bicluster, iteration = iteration)
	conds <- getConditions( db_path = db_path, bicluster = bicluster, iteration = iteration)
	to_r <- ratios[ genes, conds ]
	return( to_r )
}