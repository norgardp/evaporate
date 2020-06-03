#' Particle rate in
#' 
#' Compute the influx rate of particles due to material evaporation.
#
#' @param ng numeric value; gas density [m^-3}]
#' @param Ts numeric value; surface temperature [K]
#' @param Tg numeric vector; gas temperature [K]
#' @param A_surface numeric value; surface area of evaporating substance [m^2]
#' @param mat list; list of material properties for evaporating substance
#
#' @author Peter Norgard
#
#
particle_rate_in = function(ng, Ts, Tg, A_surface, mat)
{
	pa = kinetic_pressure(ng, Tg);
	pv = vapor_pressure(Ts, mat$antoine);
	return(A_surface * evaporation(pv, pa, Ts, mat$Mm));
}
