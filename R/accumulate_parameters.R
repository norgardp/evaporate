#' Accumulate material properties into a prescribed list format
#' 
#' @param name character string; complete name of the material
#' @param sname character string; shortened name (e.g. atomic symbol, chemical name, etc) of the material
#' @param ac numeric vector; Antoine coefficents for vapor pressure calculations
#' @param acr numeric vector; temperature range [K] over which the Antoine coefficents are valid
#' @param Mm numeric vector; molar mass [kg/mol] of the material
#
#' @values The Antoine coefficients are used in a polynomial function to approximate the vapor pressure of a material. The coefficients are a vector of three or four numbers, depending on the material. The coefficients are usually valid over the range some temperature range.
#' @values The temperature range is a length-two vector, for instance t.range = c(350, 500), over which the Antoine coefficients will be evaluated. The evaluation range should be limited such that in [T.low, T.high], T.low < T.high. It is often the case that T.low is the freezing point and T.high is the boiling point of the material.
#' @values The function returns a list-type result containing the appropriately packaged data.
#
#' @author Peter Norgard
accumulate_parameters = function(name, sname, ac, acr, Mm)
{
	if(!is.character(name) | !is.character(sname))
	{
		stop("Error in accumulate_parameters(): non-character entry for name/short name");
	}
	
	if( (length(ac) < 3) | (length(ac) > 4) )
	{
		stop("Error in accumulate_parameters(): incorrect number of Antoine parameters");
	}
	
	if(!(length(acr) == 2))
	{
		stop("Error in accumulate_parameters(): incorrect dimension for Antoine parameter temperature range");
	}
	
	theList = list(
		name = name,
		short_name = sname,
		antoine = ac,
		antoine_range = acr,
		Mm = Mm);
	return(theList);
}
