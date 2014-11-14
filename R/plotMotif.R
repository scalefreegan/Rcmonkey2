#' Plot cMonkey PWM using seqLogo.
#'
#' \code{plotMotif} plots a PWM
#'
#' @param pwm A PWM of the form:
#'  \tabular{rrrrrrrrr}{
#'   \tab 1 \tab 2 \tab 3 \tab 4 \tab 5 \tab 6 \tab 7 \tab 8\cr
#'  A \tab 0 \tab 0 \tab 0.23 \tab 0 \tab 1 \tab 0.77 \tab 0.56 \tab 1\cr
#'  C \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0 \tab 0.44 \tab 0\cr
#'  G \tab 1 \tab 1 \tab 0.77 \tab 0.11 \tab 0 \tab 0.23 \tab 0 \tab 0\cr
#'  T \tab 0 \tab 0 \tab 0 \tab 0.89 \tab 0 \tab 0 \tab 0 \tab 0\cr
#' }
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#' @param motif_num Number of the motif to plot 
#' @param ic.scale Scale by information content
#' @param file Path to save pdf to file
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
	if ( is.null( pwm ) ) {
		pwm_o <- getMotifPWM( db_path = db_path, bicluster = bicluster, 
			iteration = iteration, motif_num = motif_num )
		pwm <- pwm_o[[1]]$pwm
		eval <- pwm_o[[1]]$eval
	}
	p <- makePWM( pwm )
	if ( !is.null( file ) ) {
		pdf( file )
	}
	seqLogo(p, ic.scale=ic.scale)
	if ( !is.null( file ) ) {
		dev.off()
	}
	dbDisconnect( db_path )
}