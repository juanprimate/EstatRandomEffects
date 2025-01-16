test_that("efeito-verossimilhanca26.1_2", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	REML<-TRUE	    #restricted likelihood estimate
	input <- list(dataset=dataset,varY=varY,REML=REML)

	#output<-lmerRandomList(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)
	#
	output<-lmerRandomListSemItera(input)
	expect_equal(class(output),"list", tolerance = 1e-06,
	ignore_attr = FALSE)
	#
	#input$covariavel='batch'
	#output<-lmerRandomListComUma(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)	
})
test_that("efeito-verossimilhanca26.1_2", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	REML<-TRUE	    #restricted likelihood estimate
	input <- list(dataset=dataset,varY=varY,REML=REML)

	#output<-lmerRandomList(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)
	#
	out<-list(covarianceMatrixFit = structure(list(Factors = c("batch", 
	"employee", "Residual"), `Standard deviation` = c("0.72190", 
	"1.24403", "0.49414")), row.names = c("X", "X.1", "X.2"), class = "data.frame"), 
		randFitAnovaFit = structure(c(4, 3, 3, -84.0289995559482, 
		-152.716938981142, -120.290150593468, 176.057999111896, 311.433877962284, 
		246.580301186935, NA, 137.375878850388, 72.5223020750388, 
		NA, 1, 1, NA, 9.97897560473113e-32, 1.65154976503463e-17), dim = c(3L, 
		6L), dimnames = list(c("<none>", "employee", "batch"), c("npar", 
		"logLik", "AIC", "LRT", "Df", "Pr(>Chisq)"))), contestFitResp = structure(list(
			Estimate = 27.2477777777787, `Std. Error` = 0.631737229400994, 
			df = 6.13125474611779, `t value` = 43.131505489418, lower = 25.7099586795015, 
			upper = 28.7855968760559, `Pr(>|t|)` = 7.54539200840282e-09), row.names = c(NA, 
		-1L), class = "data.frame"), contest1DFitResp = structure(list(
			Estimate = 27.2477777777787, `Std. Error` = 0.631737229400994, 
			df = 6.13125474611779, `t value` = 43.131505489418, `Pr(>|t|)` = 7.54539200840282e-09), class = "data.frame", row.names = c(NA, 
		-1L)), contestMDFitResp = structure(list(`Sum Sq` = 454.234955237551, 
			`Mean Sq` = 454.234955237551, NumDF = 1L, DenDF = 6.13125474611779, 
			`F value` = 1860.3267657837, `Pr(>F)` = 7.54539200840282e-09), class = "data.frame", row.names = c(NA, 
		-1L)), confintFitIntervalResp = structure(c(0.414814179400747, 
		0.67179903574567, 0.426453258375663, 25.9181759152381, 1.4767588716499, 
		2.50036191435928, 0.581877440628647, 28.577380062795), dim = c(4L, 
		2L), dimnames = list(c("sd_(Intercept)|batch", "sd_(Intercept)|employee", 
		"sigma", "(Intercept)"), c("2.5 %", "97.5 %"))), vcovLmerResp = new("dpoMatrix", 
			Dim = c(1L, 1L), Dimnames = list("(Intercept)", "(Intercept)"), 
			x = 0.399091927011245, uplo = "U", factors = list(correlation = new("corMatrix", 
				sd = 0.631737229400994, Dim = c(1L, 1L), Dimnames = list(
					"(Intercept)", "(Intercept)"), x = 1, uplo = "U", 
				factors = list()))), VarCorrLmerResp = structure(list(
			grp = c("batch", "employee", "Residual"), var1 = c("(Intercept)", 
			"(Intercept)", NA), vcov = c(0.521134082796718, 1.54761626351972, 
			0.244169445708209), sdcor = c(0.721896171756519, 1.24403225983883, 
			0.494135048046796)), class = "data.frame", row.names = c(NA, 
		-3L)))
	output<-lmerRandomListSemItera(input)

	expect_equal(output,out, tolerance = 1e-06,
	ignore_attr = FALSE)
	#
	#input$covariavel='batch'
	#output<-lmerRandomListComUma(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)	
})
test_that("efeito-verossimilhanca26.1_3", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	REML<-TRUE	    #restricted likelihood estimate
	input <- list(dataset=dataset,varY=varY,REML=REML)

	#output<-lmerRandomList(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)
	#

	out<-  list(covarianceMatrixFit = structure(list(Factors = c("batch", 
  "employee", "Residual"), `Standard deviation` = c("0.72190", 
  "1.24403", "0.49414")), row.names = c("X", "X.1", "X.2"), class = "data.frame"), 
      randFitAnovaFit = structure(c(4, 3, 3, -84.0289995559481, 
      -152.716938981142, -120.290150593468, 176.057999111896, 311.433877962284, 
      246.580301186935, NA, 137.375878850388, 72.5223020750389, 
      NA, 1, 1, NA, 9.97897560473099e-32, 1.6515497650346e-17), dim = c(3L, 
      6L), dimnames = list(c("<none>", "employee", "batch"), c("npar", 
      "logLik", "AIC", "LRT", "Df", "Pr(>Chisq)"))), contestFitResp = structure(list(
          Estimate = 27.2477777777784, `Std. Error` = 0.631737229319205, 
          df = 6.13125473757555, `t value` = 43.1315054950017, 
          lower = 25.7099586791939, upper = 28.7855968763629, `Pr(>|t|)` = 7.54539215941474e-09), row.names = c(NA, 
      -1L), class = "data.frame"), contest1DFitResp = structure(list(
          Estimate = 27.2477777777784, `Std. Error` = 0.631737229319205, 
          df = 6.13125473757555, `t value` = 43.1315054950017, 
          `Pr(>|t|)` = 7.54539215941474e-09), class = "data.frame", row.names = c(NA, 
      -1L)), contestMDFitResp = structure(list(`Sum Sq` = 454.234955364045, 
          `Mean Sq` = 454.234955364045, NumDF = 1L, DenDF = 6.13125473757555, 
          `F value` = 1860.32676626536, `Pr(>F)` = 7.54539215941474e-09), class = "data.frame", row.names = c(NA, 
      -1L)), confintFitIntervalResp = structure(c(0.414814179518708, 
      0.671799035764914, 0.426453258401055, 25.9181759152138, 1.4767588707481, 
      2.5003644206975, 0.581877439662271, 28.577380062796), dim = c(4L, 
      2L), dimnames = list(c("sd_(Intercept)|batch", "sd_(Intercept)|employee", 
      "sigma", "(Intercept)"), c("2.5 %", "97.5 %"))), vcovLmerResp = new("dpoMatrix", 
          Dim = c(1L, 1L), Dimnames = list("(Intercept)", "(Intercept)"), 
          x = 0.399091926907906, uplo = "U", factors = list(correlation = new("corMatrix", 
              sd = 0.631737229319205, Dim = c(1L, 1L), Dimnames = list(
                  "(Intercept)", "(Intercept)"), x = 1, uplo = "U", 
              factors = list()))), VarCorrLmerResp = structure(list(
          grp = c("batch", "employee", "Residual"), var1 = c("(Intercept)", 
          "(Intercept)", NA), vcov = c(0.521134082762659, 1.54761626303116, 
          0.244169445712986), sdcor = c(0.721896171732929, 1.24403225964247, 
          0.494135048051629)), class = "data.frame", row.names = c(NA, 
      -3L)))  
	output<-lmerRandomListSemItera(input)      
	expect_equal(output,out, tolerance = 1e-06,
	ignore_attr = FALSE)
	#
	#input$covariavel='batch'
	#output<-lmerRandomListComUma(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)	
})
test_that("efeito-verossimilhanca26.1_4", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	REML<-TRUE	    #restricted likelihood estimate
	covariavel='batch'
	input <- list(dataset=dataset,varY=varY,REML=REML,covariavel=covariavel)

	#output<-lmerRandomList(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)
	#

	out<-  'list' 
	#   input$covariavel='batch'
	#output<-lmerRandomListComUma(input)  
	output<-lmerRandomListComUma(input)  
	expect_equal(class(output),out, tolerance = 1e-06,
	ignore_attr = FALSE)
	#input$covariavel='batch'
	#output<-lmerRandomListComUma(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)	
})
test_that("efeito-verossimilhanca26.1_4", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	REML<-TRUE	    #restricted likelihood estimate
	
	input <- list(dataset=dataset,varY=varY,REML=REML)

	#output<-lmerRandomList(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)
	#

	# out<-  'list' 
  input$covariavel='batch'
	#output<-lmerRandomListComUma(input)  
	 
	out<-list(covarianceMatrixFit = structure(list(Factors = c("batch", 
	"Residual"), `Standard deviation` = c("0.6578", "1.2533")), row.names = c("X", 
	"X.1"), class = "data.frame"), randFitAnovaFit = structure(c(3, 
	2, -152.716938981142, -157.917846758013, 311.433877962284, 319.835693516026, 
	NA, 10.4018155537418, NA, 1, NA, 0.00125891475415007), dim = c(2L, 
	6L), dimnames = list(c("<none>", "batch"), c("npar", "logLik", 
	"AIC", "LRT", "Df", "Pr(>Chisq)"))), contestFitResp = structure(list(
		Estimate = 27.2477777777779, `Std. Error` = 0.299280214270158, 
		df = 4.99999964091135, `t value` = 91.0443673806701, lower = 26.4784534785914, 
		upper = 28.0171020769644, `Pr(>|t|)` = 3.03024516600393e-09), row.names = c(NA, 
	-1L), class = "data.frame"), contest1DFitResp = structure(list(
		Estimate = 27.2477777777779, `Std. Error` = 0.299280214270158, 
		df = 4.99999964091135, `t value` = 91.0443673806701, `Pr(>|t|)` = 3.03024516600393e-09), class = "data.frame", row.names = c(NA, 
	-1L)), contestMDFitResp = structure(list(`Sum Sq` = 13019.6397818812, 
		`Mean Sq` = 13019.6397818812, NumDF = 1L, DenDF = 4.99999964091135, 
		`F value` = 8289.07683174642, `Pr(>F)` = 3.03024516600393e-09), class = "data.frame", row.names = c(NA, 
	-1L)), confintFitIntervalResp = structure(c(0.253256333550444, 
	1.08523550935624, 26.6139882536513, 1.30240633204341, 1.46960975276041, 
	27.8815672144669), dim = 3:2, dimnames = list(c("sd_(Intercept)|batch", 
	"sigma", "(Intercept)"), c("2.5 %", "97.5 %"))), vcovLmerResp = new("dpoMatrix", 
		Dim = c(1L, 1L), Dimnames = list("(Intercept)", "(Intercept)"), 
		x = 0.0895686466535914, uplo = "U", factors = list(correlation = new("corMatrix", 
			sd = 0.299280214270158, Dim = c(1L, 1L), Dimnames = list(
				"(Intercept)", "(Intercept)"), x = 1, uplo = "U", 
			factors = list()))), VarCorrLmerResp = structure(list(
		grp = c("batch", "Residual"), var1 = c("(Intercept)", NA), 
		vcov = c(0.432698652733873, 1.57069840781511), sdcor = c(0.657798337436234, 
		1.2532750726856)), class = "data.frame", row.names = c(NA, 
	-2L)))	    
	output<-lmerRandomListComUma(input) 
	expect_equal(output,out, tolerance = 1e-06,
	ignore_attr = FALSE)
	#
	#input$covariavel='batch'
	#output<-lmerRandomListComUma(input)
	#expect_equal(class(output),"list", tolerance = 1e-06,
	#ignore_attr = FALSE)	
})
test_that("efeito-verossimilhanca27", {
	#datos quality
	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
		class = "data.frame")

    varY<-'score'
	# formula <- formulaRandom(dataset,varY)
	REML<-TRUE	    #restricted likelihood estimate
    input<-list(dataset=dataset,varY=varY,REML=REML)

	output<-	list(covarianceMatrixFit = structure(list(Factors = c("employee:batch", 
  "batch", "employee", "Residual"), `Standard deviation` = c("0.15327", 
  "0.71947", "1.24287", "0.47597")), row.names = c("X", "X.1", 
  "X.2", "X.3"), class = "data.frame"), randFitAnovaFit = structure(c(5, 
  4, 4, 4, -83.7419037122538, -108.356767783616, -98.3528812073478, 
  -84.0289995559481, 177.483807424508, 224.713535567232, 204.705762414696, 
  176.057999111896, NA, 49.2297281427245, 29.2219549901882, 0.574191687388776, 
  NA, 1, 1, 1, NA, 2.27674048426924e-12, 6.45436941610685e-08, 
  0.448598042684412), dim = c(4L, 6L), dimnames = list(c("<none>", 
  "employee", "batch", "employee:batch"), c("npar", "logLik", "AIC", 
  "LRT", "Df", "Pr(>Chisq)"))), contestFitResp = structure(list(
      Estimate = 27.2477777777787, `Std. Error` = 0.631284195080375, 
      df = 6.11281727100506, `t value` = 43.1624583509643, lower = 25.7099653350336, 
      upper = 28.7855902205239, `Pr(>|t|)` = 7.85763694077746e-09), row.names = c(NA, 
  -1L), class = "data.frame"), contest1DFitResp = structure(list(
      Estimate = 27.2477777777787, `Std. Error` = 0.631284195080375, 
      df = 6.11281727100506, `t value` = 43.1624583509643, `Pr(>|t|)` = 7.85763694077746e-09), class = "data.frame", row.names = c(NA, 
  -1L)), contestMDFitResp = structure(list(`Sum Sq` = 422.064774697611, 
      `Mean Sq` = 422.064774697611, NumDF = 1L, DenDF = 6.11281727100506, 
      `F value` = 1862.99781089873, `Pr(>F)` = 7.85763694077746e-09), class = "data.frame", row.names = c(NA, 
  -1L)), confintFitIntervalResp = structure(c(0, 0.410005845487949, 
  0.669434006853428, 0.402026565325628, 25.9189910318517, 0.352799813598422, 
  1.47544267452886, 2.49938774708599, 0.574143144824227, 28.5765640582849
  ), dim = c(5L, 2L), dimnames = list(c("sd_(Intercept)|employee:batch", 
  "sd_(Intercept)|batch", "sd_(Intercept)|employee", "sigma", "(Intercept)"
  ), c("2.5 %", "97.5 %"))), vcovLmerResp = new("dpoMatrix", Dim = c(1L, 
  1L), Dimnames = list("(Intercept)", "(Intercept)"), x = 0.398519734958277, 
      uplo = "U", factors = list(correlation = new("corMatrix", 
          sd = 0.631284195080375, Dim = c(1L, 1L), Dimnames = list(
              "(Intercept)", "(Intercept)"), x = 1, uplo = "U", 
          factors = list()))), VarCorrLmerResp = structure(list(
      grp = c("employee:batch", "batch", "employee", "Residual"
      ), var1 = c("(Intercept)", "(Intercept)", "(Intercept)", 
      NA), vcov = c(0.0234901765943228, 0.517635328630026, 1.54473468224099, 
      0.226551406678252), sdcor = c(0.153265053402016, 0.719468782248421, 
      1.24287355842861, 0.475974165977789)), class = "data.frame", row.names = c(NA, 
  -4L)))	
	expect_equal(lmerRandomList(input), output, tolerance = 1e-06,
	ignore_attr = FALSE)
})
