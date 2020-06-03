#' Kinetic pressure
#
#' Compute the pressure resulting from a low-density gas in contact with a rigid three dimensional boundary.
#
#' @param ng, gas density in m^{-3}
#' @param Tg, average gas temperature in Kelvin
#
#' @return Gas pressure arising from kinetic interaction of a low density gas obeying Boltzmann statistics in three dimensions.
#
#' @depends constants
#
#' @author Peter Norgard
#
kinetic_pressure = function(ng, Tg)
{
	# Extract CODATA value for Boltzmann constant [J/K]
	kb = with(constants::syms, k);	
	pg = ng*Tg*kb;
	return(pg);
}
