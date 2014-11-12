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
	p <- makePWM( m, ic.scale = ic.scale )
	if ( !is.null( file ) ) {
		pdf( file )
	}
	seqLogo(p, ic.scale=ic.scale)
}