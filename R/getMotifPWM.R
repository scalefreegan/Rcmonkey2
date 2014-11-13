#' Get bicluster motif PWMs from SQLite database.
#'
#' \code{getMotifPWM} retrieves motif PWM(s) from a 
#' user specified bicluster and/or motif number at any iteration
#' of cMonkey algorithm
#'
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#' @param motif_num Number of the motif to return. Defaults to all motifs 
#' (assuming # motifs per bicluster < 10) 
#'
#' @return List containing $pwm and $evalue for each motif requested
#'
#'  @export
getMotifPWM <- function( db_path = "", bicluster = 1, iteration = "max", motif_num = seq( 1, 10 ) ) {
	if ( is.character( db_path ) ) {
		db_path <- load_cMonkey( db_path, verbose = F )
	}
	if ( iteration=="max" ) {
		iteration <- as.numeric( dbGetQuery( db_path, "SELECT MAX(iteration) FROM row_members;" ) )
	}
	motif_num = paste( "(", paste( motif_num, collapse = "," ), ")", sep = "")
	to_r <- dbGetQuery( db_path, paste(
		"SELECT motif_pssm_rows.row, motif_pssm_rows.a, motif_pssm_rows.c,
		motif_pssm_rows.g, motif_pssm_rows.t, motif_infos.motif_num, motif_infos.evalue
		FROM motif_pssm_rows 
		JOIN motif_infos 
		ON motif_pssm_rows.motif_info_id = motif_infos.ROWID
		WHERE motif_infos.iteration = ", iteration,
		" AND motif_infos.cluster = ", bicluster, 
		" AND motif_infos.motif_num IN ", motif_num, ";", sep="" ) )
	to_r_r <- lapply( unique( to_r$motif_num ), function( i ) {
		sub_m <- to_r[ to_r$motif_num == i, ]
		motif_sub <- list( )
		row_order <- order( to_r$row )
		pwm <- cbind( to_r$a, to_r$c, to_r$g, to_r$t )[ row_order, ]
		colnames(pwm) <- c( "A", "C", "G", "T" )
		motif_sub$pwm <- t( pwm )
		motif_sub$eval <- unique( sub_m$evalue )
		return( motif_sub )
		})
	names( to_r_r ) <- unique( to_r$motif_num )
	dbDisconnect( db_path )
	return(to_r)
}