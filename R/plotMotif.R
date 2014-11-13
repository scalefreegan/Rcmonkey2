#' Plot cMonkey PSSM using seqLogo.
#'
#' \code{plotMotif} plots a PWM
#'
#' @param pwm A PWM of the form:
#' @param pwm A PWM of the form:
#'
#' @return Plot of the motif
#'
#'  @export
plotMotif <- function( pwm = NULL, db_path = "", bicluster = 1, iteration = "max", 
	motif_num = 1, ic.scale = TRUE, file = NULL) {
	library( seqLogo )
	if ( is.character( db_path ) ) {
		db_path <- load_cMonkey( db_path, verbose = F )
	}
	if ( iteration=="max" ) {
		iteration <- as.numeric( dbGetQuery( db_path, "SELECT MAX(iteration) FROM row_members;" ) )
	}
	if ( pwm == NULL ) {
		pwm_o <- getMotifPWM( db_path = db_path, bicluster = bicluster, 
			iteration = iteration, motif_num = motif_num )
		pwm <- pwm_o[ [ 1 ] ]$pwm
		eval <- pwm_o[ [ 1 ] ]$eval
	}
	p <- makePWM( pwm, ic.scale = ic.scale )
	if ( !is.null( file ) ) {
		pdf( file )
	}
	seqLogo(p, ic.scale=ic.scale)
	if ( !is.null( file ) ) {
		dev.off()
	}
}