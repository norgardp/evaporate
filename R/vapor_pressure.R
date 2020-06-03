#' Vapor pressure
#' 
#' Compute the vapor pressure of a material using Antoine parameters. This function uses 3-valued or 4-valued Antoine parameters, and returns the vapor pressure in Pascals.
#
#' @param T numeric vector; temperature(s) [K] at which to evaluate the vapor pressure(s)
#' @param k numeric vector; Antoine coefficients
#
#' @values k = c(A, B, C) or k = c(A, B, C, D), where A ... D are the Antoine coefficients for the material under evaluation.
#
#' @author Peter Norgard
#
#' @examples T_eval = seq(from=300, to=500, by=20);
#' @examples k = c(1000, -20, 0.002);
#' @examples vapor_pressure(T_eval, k);
#
vapor_pressure = function(Tval, k)
{
	if(!is.numeric(Tval))
	{
		stop("Error in vapor_pressure(): Temperature is non-numeric");
	}
	
	if((length(k) < 3) | (length(k) > 4) | !is.numeric(k))
	{
		stop("Error in vapor_pressure(): problem with Antoine coefficient vector")
	}
	

	# ---------------------------------------------------------------
	# Compute the intermediate vapor pressure term; use a fourth 
	# coefficient term if available; output pressure units Pascal
	pconst = 5.006;
	pow = pconst +  k[1] + k[2]/Tval + k[3]*log(Tval, 10)
	if(length(k) == 4)
	{	
		pow = pow + k[4]/Tval^3;
	}
	
	# ---------------------------------------------------------------
	# Return the vapor pressure
	return(10^pow);
}
