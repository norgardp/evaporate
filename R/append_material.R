#' Append Material
#' 
#' Append material properties to the end of a list-of-lists containing specific properties of materials to be evaluated.
#
#' @param main_list list of lists; the main list of lists onto which the sub-list is to be appended
#' @param sub_list list; a sub-list containing one-for-one data under equivalent names
#
#' @values The function will compare the names of the parameters within the main and sub-lists for a one-to-one comparison. If the lists are different in any way, the function will fail.
#' @values Upon successful completion, the function will return a new list of lists with the appended sub-list.
#
#' @example new_list = append_material(old_list, sublist);
#' @example theList = append_material(theList, sublist); 
#
#' @author Peter Norgard
#
append_material = function(master, element)
{
	# compare elements to make sure the lists types are equivalent
	temp = lapply(sort(names(sm)), '==', sort(unique(names(physical_properties))));
	isgood = 0;
	for(i in 1:length(temp))
	{
		if(temp[[i]][i] == TRUE)
		{
			isgood = isgood + 1;
		}
	}
	if(isgood != length(temp))
	{
		stop("Error in append_material(): attempting to append improper list type")
	}
	
	master = append(master, element);
	return(master);
}
