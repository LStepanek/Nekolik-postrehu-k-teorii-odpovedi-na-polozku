###############################################################################
###############################################################################
###############################################################################

## nastavuji pracovní složku --------------------------------------------------

setwd(choose.dir())


## ---------------------------------------------------------------------------

###############################################################################

## logistická funkce ----------------------------------------------------------

p_of_success <- function(theta, a, b, c){
    
    # '''
    # vrací pro respondenta s parametrem schopnosti theta pravděpodobnost p,
    # že správně odpoví na danou položku charakterizovanou parametry a, b, c
    # '''
    
	c + (1 - c) / (1 + exp(-a * (theta - b)))

}


## ----------------------------------------------------------------------------

###############################################################################

## vracím diagram logistické funkce pro různá theta ---------------------------

jpeg(
    filename = "logisticka_funkce.jpg",
	width = 7,
	height = 5,
	units = "in",
	res = 600
)

a <- 1.00
b <- 0.00
c <- 0.25
delta <- 1.50
theta <- seq(-4, 4, length = 10000)

par(mar = c(4, 4, 1, 1))

plot(
    x = theta,
	y = p_of_success(theta, a, b, c),
	type = "l",
	xlim = c(-3, 3),
	ylim = c(0, 1),
	xlab = expression(italic(theta)),
	ylab = expression(italic(p))	
)

segments(
    x0 = median(theta) - delta,
	x1 = median(theta) + delta,
	y0 = p_of_success(
	          median(theta), a, b, c
			  ) + (median(theta) - delta) * (a - a * c) / 4,
	y1 = p_of_success(
	          median(theta), a, b, c
			  ) - (median(theta) - delta) * (a - a * c) / 4,
    col = "red"
)

segments(
    x0 = median(theta),
	y0 = -1,
	y1 = p_of_success(
	          median(theta), a, b, c
			  ),
	lty = 2
)

segments(
    x0 = -4,
	x1 = median(theta),
	y0 = p_of_success(
	          median(theta), a, b, c
			  ),
	lty = 2
)

text(
    x = median(theta) + delta - 0.5,
	y = p_of_success(
	          median(theta) + delta - 0.5, a, b, c
			  ) + 0.05,
    labels = expression(italic(a)),
	col = "red"
)

text(
    x = median(theta) + 0.1,
	y = 0,
    labels = expression(italic(b))
)

segments(
    x0 = -4,
	x1 = -2,
	y0 = c,
	lty = 2
)

text(
    x = -3,
	y = c - 0.03,
    labels = expression(italic(c))
)

dev.off()


## ----------------------------------------------------------------------------

###############################################################################

## informační křivka testu ----------------------------------------------------

jpeg(
    filename = "informacni_funkce.jpg",
	width = 7,
	height = 5,
	units = "in",
	res = 600
)

a <- 1.50
b <- 0.00
c <- 0.10
delta <- 1.50
theta <- seq(-4, 4, length = 10000)

par(mar = c(4, 4, 1, 1))

plot(
    x = theta,
	y = a^2 * p_of_success(theta, a, b, c) * (
	        1 - p_of_success(theta, a, b, c)
			),
	type = "l",
	xlim = c(-3, 3),
	ylim = c(0, 1),
	xlab = expression(italic(theta)),
	ylab = expression(paste(italic(I), "(", theta, ")", sep = ""))#,
	#main = expression(paste(
	#           "Informační funkce 2PL modelu pro ",
	#		   italic(a), " = 1.5, ",
	#		   italic(b), " = 0.0, ",
    #           italic(c), " = 0.1",	   
	#		   sep = ""
	#		   ))
)

text(
    x = 1.6,
	y = 0.98,
	labels = expression(paste(
	           "2PL model, ",
			   italic(a), " = 1.5, ",
			   italic(b), " = 0.0, ",
               italic(c), " = 0.1",	   
			   sep = ""
			   ))
)

dev.off()



## ----------------------------------------------------------------------------

###############################################################################
###############################################################################
###############################################################################



