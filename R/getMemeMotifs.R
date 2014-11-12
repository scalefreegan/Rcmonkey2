#' Get bicluster MEME motif info from SQLite database.
#'
#' \code{getgetMemeMotifs} retrieves MEME output  from a 
#' user specified bicluster and/or motif number at any iteration
#' of cMonkey algorithm
#'
#' @param db_path Location of the SQLite database
#' @param bicluster Bicluster number, integer
#' @param iteration Iteration of bicluster to retrieve, defaults to last (final) iteration 
#' @param motif_num Number of the motif to return. Defaults to all motifs 
#' (assuming # motifs per bicluster < 10) 
#'
#' @return data.frame containing information about the locations and properties of discovered motifs by MEME
#'
#'  @export
getMemeMotifs <- function( db_path = "", bicluster = 1, iteration = "max", motif_num = seq( 1, 10 ) ) {
	if ( is.character( db_path ) ) {
		db_path <- load_cMonkey( db_path, verbose = F )
	}
	if ( iteration=="max" ) {
		iteration <- as.numeric( dbGetQuery( db_path, "SELECT MAX(iteration) FROM row_members;" ) )
	} 
	motif_num = paste( "(", paste( motif_num, collapse = "," ), ")", sep = "")
	to_r <- dbGetQuery( db_path, paste(
		"SELECT microbes_online.sysName, microbes_online.name, 
		microbes_online.start AS gene_start, microbes_online.stop AS gene_stop, microbes_online.strand, 
		meme_motif_sites.start AS motif_start, meme_motif_sites.reverse, meme_motif_sites.pvalue, 
		meme_motif_sites.flank_left, meme_motif_sites.seq, meme_motif_sites.flank_right, 
		motif_infos.motif_num,motif_infos.evalue
		FROM meme_motif_sites 
		JOIN motif_infos 
		ON meme_motif_sites.motif_info_id = motif_infos.ROWID
		JOIN microbes_online
		ON meme_motif_sites.seq_name = microbes_online.accession
		WHERE motif_infos.iteration = ", iteration,
		" AND motif_infos.cluster = ", bicluster,
		" AND motif_infos.motif_num IN ", motif_num, ";", sep="" ) )
	dbDisconnect( db_path )
	return(to_r)
}