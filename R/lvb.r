lvb = function(age,linf,k,t0){

	length = linf * (1 - exp(-k * (age - t0)))

	return(length)
}