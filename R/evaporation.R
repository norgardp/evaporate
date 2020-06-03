#' Evaporation Rate
#' 
#' Compute the evaporation rate of material from a surface.
#
#' @param pv numeric vector; vapor pressure [Pa] at which to evaluate the evaporation
#' @param p0 numeric value; pressure [Pa] adjacent to evaporating surface
#' @param T numeric vector; surface temperature [K] of the evaporating material
#' @param M numeric value; molar mass [kg/mol] of the evaporating material
#' @param a numeric value; sticking coefficient [unitless] with range [0,1] (default = 1)
#
#' @value evaporation() returns a numeric vector representing the particle flux at the surface boundary [#/m^2/s]
#
#' @author Peter Norgard
#
#' @examples T_eval = seq(from=300, to=500, by=20);
#' @examples k = c(1000, -20, 0.002);
#' @examples pv = vapor_pressure(T_eval, k);
#' @examples evaporation(pv, 10, T_eval, 150.2);
#
evaporation = function(pv, p0, Tval, M, a = 1)
{
	# CODATA values for universal constants
	R = with(constants::syms, R);
	Na = with(constants::syms, Na);
	
	# Idnetify cases where the vapor pressure is greater than the ambient
	# vacuum pressure; function evaluates to zero if the vapor pressure is
	# less than ambient, since nothing can evaporate.
	ill = which((pv - p0 ) > 0);
	retval = rep(0, length(Tval));
	retval[ill] = ((pv[ill]-p0)*a*Na)/sqrt(2*pi*R*Tval*M); 
	return(retval);
}
