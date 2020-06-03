#' Particle rate out
#' 
#' Compute the outflux rate of particles due to an aperture.
#
#' @param ng numeric value; gas density [m^-3}]
#' @param Tg numeric vector; gas temperature [K]
#' @param A_orifice numeric value; surface area of exit orifice [m^2]
#' @param mat list; list of material properties for exiting substance
#
#' @author Peter Norgard
#
particle_rate_out = function(ng, Tg, A_orfice, mat)
{
	v_average = average_velocity(ng, Tg, mat$Mm);
	return(A_orfice * v_average);
}
