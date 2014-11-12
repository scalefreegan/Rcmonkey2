#' Load cMonkey database.
#'
#' \code{load_cMonkey} Loads the SQLite databases produced from a
#' cMonkey2 (aka python cMonkey) run
#'
#' @param path Location of the SQLite database
#' @param verbose Should location of database be written to console
#'
#' @return Connection to SQLite database
#'
#' @examples 
#' dbconn <- load_cMonkey("path/to/your/database")
#' 
#'  @export
load_cMonkey <- function( path = "", verbose = T) {
	library( "RSQLite" )
	# assumes the you are in the standard
	# cMonkey ./out dir if a path is not
	# specified
	if ( nchar( path ) != 0 ) {
		# path is specified
		# load it
		db_loc <- path
		if( verbose ){ cat( paste( "Found SQLite database at",db_loc,"\n",sep =" " ) ) }
		dbconn <- dbConnect( SQLite( ), db_loc )
		return( dbconn )	
		} else if ( file.exists( paste( getwd( ), "/cmonkey_run.db", sep="" ) ) ) {
			db_loc <- paste( getwd( ), "/cmonkey_run.db", sep="" )
			if( verbose ){ cat( paste( "Found SQLite database at", db_loc,"\n", sep =" ") ) }
			dbconn <- dbConnect( SQLite( ), db_loc )
			return( dbconn )
		} else if( file.exists( paste( getwd( ), "/out/cmonkey_run.db", sep="" ) ) ) {
				db_loc <- paste( getwd( ), "/out/cmonkey_run.db", sep="" )
				if( verbose ){ cat( paste( "Found SQLite database at", db_loc, "\n", sep =" " ) ) }
				dbconn <- dbConnect( SQLite( ), db_loc )
				return( dbconn ) 
		} else {
			cat( "ERROR: Cannot find SQLite database.\n" )
			return( NULL )
		}
}