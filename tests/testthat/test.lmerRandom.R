context("LMER Random")

test_that("efeito-verossimilhanca1", {
	resistors <- utils::read.table(header = TRUE,
	    text = 'id part oper mohms
		1 1 1 417
		2 1 2 394
		3 1 3 404
		4 1 1 419
		5 1 2 398
		6 1 3 410
		7 2 1 417
		8 2 2 387
		9 2 3 398
		10 2 1 417
		11 2 2 399
		12 2 3 402
		13 3 1 423
		14 3 2 389
		15 3 3 407
		16 3 1 418
		17 3 2 407
		18 3 3 402
		19 4 1 412
		20 4 2 389
		21 4 3 407
		22 4 1 410
		23 4 2 405
		24 4 3 411
		25 5 1 407
		26 5 2 386
		27 5 3 400
		28 5 1 409
		29 5 2 405
		30 5 3 410
		31 6 1 408
		32 6 2 382
		33 6 3 405
		34 6 1 413
		35 6 2 400
		36 6 3 410
		37 7 1 409
		38 7 2 385
		39 7 3 407
		40 7 1 408
		41 7 2 400
		42 7 3 400
		43 8 1 408
		44 8 2 384
		45 8 3 402
		46 8 1 411
		47 8 2 401
		48 8 3 405
		49 9 1 412
		50 9 2 387
		51 9 3 412
		52 9 1 408
		53 9 2 401
		54 9 3 405
		55 10 1 410
		56 10 2 386
		57 10 3 418
		58 10 1 404
		59 10 2 407
		60 10 3 404'
	)
	resistors <- within(resistors,
		{oper <- as.factor(oper);part <- as.factor(part)})
	mohms.lmer <-
		lme4::lmer(mohms~1 + (1|part) + (1|oper) + (1|part:oper),data=resistors)
	# summary(mohms.lmer)
	# library(languageR)
	# lmerPlotInt.fnc(mohms.lmer)
	#https://github.com/Estatcamp/EstatResidualAnalysis
	# /blob/feature/GoldfeldQuandt/tests/testthat/test.calcGQ.R
	mohms.partonly.lmer <- lme4::lmer(mohms~1 + (1|part),data=resistors)
	mohms.operonly.lmer <- lme4::lmer(mohms~1 + (1|oper),data=resistors)
	mohms.intronly.lmer <- lme4::lmer(mohms~1 + (1|part:oper),data=resistors)
	mohms.nopart.lmer <-
		lme4::lmer(mohms~1 + (1 | oper) + (1 | part:oper),data=resistors)
	mohms.nooper.lmer <-
		lme4::lmer(mohms~1 + (1 | part) + (1 | part:oper),data=resistors)
	mohms.nointr.lmer <-
		lme4::lmer(mohms~1+ (1 | part) + (1 | oper),data=resistors)
	fit1<-RLRsim::exactRLRT(mohms.partonly.lmer,mohms.lmer,mohms.nopart.lmer)
	fit2<-RLRsim::exactRLRT(mohms.operonly.lmer,mohms.lmer,mohms.nooper.lmer)
	fit3<-RLRsim::exactRLRT(mohms.intronly.lmer,mohms.lmer,mohms.nointr.lmer)

	out<-c(NA,fit1[['statistic']],
	fit2[['statistic']],
	fit3[['statistic']])

	output<-lmerTest::ranova(mohms.lmer)

	expect_equal(as.numeric(out), output[['LRT']], tolerance = 1e-06,
	ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca2", {
	dataset <- read.table(header = TRUE,
	    text = 'id part oper mohms
		1 1 1 417
		2 1 2 394
		3 1 3 404
		4 1 1 419
		5 1 2 398
		6 1 3 410
		7 2 1 417
		8 2 2 387
		9 2 3 398
		10 2 1 417
		11 2 2 399
		12 2 3 402
		13 3 1 423
		14 3 2 389
		15 3 3 407
		16 3 1 418
		17 3 2 407
		18 3 3 402
		19 4 1 412
		20 4 2 389
		21 4 3 407
		22 4 1 410
		23 4 2 405
		24 4 3 411
		25 5 1 407
		26 5 2 386
		27 5 3 400
		28 5 1 409
		29 5 2 405
		30 5 3 410
		31 6 1 408
		32 6 2 382
		33 6 3 405
		34 6 1 413
		35 6 2 400
		36 6 3 410
		37 7 1 409
		38 7 2 385
		39 7 3 407
		40 7 1 408
		41 7 2 400
		42 7 3 400
		43 8 1 408
		44 8 2 384
		45 8 3 402
		46 8 1 411
		47 8 2 401
		48 8 3 405
		49 9 1 412
		50 9 2 387
		51 9 3 412
		52 9 1 408
		53 9 2 401
		54 9 3 405
		55 10 1 410
		56 10 2 386
		57 10 3 418
		58 10 1 404
		59 10 2 407
		60 10 3 404'
	)

	varResp<-'mohms'
	Formula<-mohms~(1|part) + (1|oper) + (1|part:oper)
	metodo<-'REML'
	efeito <- 'fixo'

	# Formula<-mohms~part +oper
	# fit<-suppressWarnings(lm(Formula,dados))
	fit<-suppressWarnings(lme4::lmer(Formula,dataset))
	output<-lmerTest::rand(fit)
	expect_equal(output[['LRT']],
	c(NA, 1.44382283906452e-11, 35.5322685223058, -5.79802872380242e-12),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca3", {
	dataset <- read.table(header = TRUE,
	    text = 'id part oper mohms
		1 1 1 417
		2 1 2 394
		3 1 3 404
		4 1 1 419
		5 1 2 398
		6 1 3 410
		7 2 1 417
		8 2 2 387
		9 2 3 398
		10 2 1 417
		11 2 2 399
		12 2 3 402
		13 3 1 423
		14 3 2 389
		15 3 3 407
		16 3 1 418
		17 3 2 407
		18 3 3 402
		19 4 1 412
		20 4 2 389
		21 4 3 407
		22 4 1 410
		23 4 2 405
		24 4 3 411
		25 5 1 407
		26 5 2 386
		27 5 3 400
		28 5 1 409
		29 5 2 405
		30 5 3 410
		31 6 1 408
		32 6 2 382
		33 6 3 405
		34 6 1 413
		35 6 2 400
		36 6 3 410
		37 7 1 409
		38 7 2 385
		39 7 3 407
		40 7 1 408
		41 7 2 400
		42 7 3 400
		43 8 1 408
		44 8 2 384
		45 8 3 402
		46 8 1 411
		47 8 2 401
		48 8 3 405
		49 9 1 412
		50 9 2 387
		51 9 3 412
		52 9 1 408
		53 9 2 401
		54 9 3 405
		55 10 1 410
		56 10 2 386
		57 10 3 418
		58 10 1 404
		59 10 2 407
		60 10 3 404'
	)

	varResp<-'mohms'
	Formula<-mohms~(1|part) + (1|oper) + (1|part:oper)
	metodo<-'REML'

	# Formula<-mohms~part +oper
	# fit<-suppressWarnings(lm(Formula,dados))
	fit<-suppressWarnings(lme4::lmer(Formula,dataset))
	output<-lmerTest::rand(fit)
	expect_equal(output[['LRT']],
	c(NA, 1.44382283906452e-11, 35.5322685223058, -5.79802872380242e-12),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca4", {
	resistors <- utils::read.table(header = TRUE,
	    text = 'id part oper mohms
		1 1 1 417
		2 1 2 394
		3 1 3 404
		4 1 1 419
		5 1 2 398
		6 1 3 410
		7 2 1 417
		8 2 2 387
		9 2 3 398
		10 2 1 417
		11 2 2 399
		12 2 3 402
		13 3 1 423
		14 3 2 389
		15 3 3 407
		16 3 1 418
		17 3 2 407
		18 3 3 402
		19 4 1 412
		20 4 2 389
		21 4 3 407
		22 4 1 410
		23 4 2 405
		24 4 3 411
		25 5 1 407
		26 5 2 386
		27 5 3 400
		28 5 1 409
		29 5 2 405
		30 5 3 410
		31 6 1 408
		32 6 2 382
		33 6 3 405
		34 6 1 413
		35 6 2 400
		36 6 3 410
		37 7 1 409
		38 7 2 385
		39 7 3 407
		40 7 1 408
		41 7 2 400
		42 7 3 400
		43 8 1 408
		44 8 2 384
		45 8 3 402
		46 8 1 411
		47 8 2 401
		48 8 3 405
		49 9 1 412
		50 9 2 387
		51 9 3 412
		52 9 1 408
		53 9 2 401
		54 9 3 405
		55 10 1 410
		56 10 2 386
		57 10 3 418
		58 10 1 404
		59 10 2 407
		60 10 3 404'
	)
	dataset <- within(resistors, {
		oper <- as.factor(oper);part <- as.factor(part)
	})
	dataset<-dataset[, -which(names(dataset) == 'id')]
	varY<-'mohms'
	formula <- formulaRandom(dataset,varY)

	output<-formula
	expect_equal(output,
	{mohms ~ (1 | part) + (1 | oper) + (1 | part:oper)}, tolerance = 1e-06,
	ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca5", {
	resistors <- utils::read.table(header = TRUE,
	    text = 'id part oper mohms
		1 1 1 417
		2 1 2 394
		3 1 3 404
		4 1 1 419
		5 1 2 398
		6 1 3 410
		7 2 1 417
		8 2 2 387
		9 2 3 398
		10 2 1 417
		11 2 2 399
		12 2 3 402
		13 3 1 423
		14 3 2 389
		15 3 3 407
		16 3 1 418
		17 3 2 407
		18 3 3 402
		19 4 1 412
		20 4 2 389
		21 4 3 407
		22 4 1 410
		23 4 2 405
		24 4 3 411
		25 5 1 407
		26 5 2 386
		27 5 3 400
		28 5 1 409
		29 5 2 405
		30 5 3 410
		31 6 1 408
		32 6 2 382
		33 6 3 405
		34 6 1 413
		35 6 2 400
		36 6 3 410
		37 7 1 409
		38 7 2 385
		39 7 3 407
		40 7 1 408
		41 7 2 400
		42 7 3 400
		43 8 1 408
		44 8 2 384
		45 8 3 402
		46 8 1 411
		47 8 2 401
		48 8 3 405
		49 9 1 412
		50 9 2 387
		51 9 3 412
		52 9 1 408
		53 9 2 401
		54 9 3 405
		55 10 1 410
		56 10 2 386
		57 10 3 418
		58 10 1 404
		59 10 2 407
		60 10 3 404'
	)
	dataset <- within(resistors, {
		oper <- as.factor(oper);part <- as.factor(part)
	})
	dataset<-dataset[, -which(names(dataset) == 'id')]
	varY<-'mohms'
	formula <- formulaRandom(dataset,varY)

	fit<-suppressWarnings(lme4::lmer(formula,dataset))
	output<-lmerTest::rand(fit)
	expect_equal(output[['LRT']],
	c(NA, 1.44382283906452e-11, 35.5322685223058, -5.79802872380242e-12),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca6", {
	resistors <- structure(list(id = 1:60, part = structure(c(1L, 1L, 1L, 1L,
		1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
		4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L,
		7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L,
		9L, 9L, 10L, 10L, 10L, 10L, 10L, 10L), .Label = c("1", "2", "3",
		"4", "5", "6", "7", "8", "9", "10"), class = "factor"),
		oper = structure(c(1L,
		2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
		3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
		1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
		2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1",
		"2", "3"), class = "factor"), mohms = c(417L, 394L, 404L, 419L,
		398L, 410L, 417L, 387L, 398L, 417L, 399L, 402L, 423L, 389L, 407L,
		418L, 407L, 402L, 412L, 389L, 407L, 410L, 405L, 411L, 407L, 386L,
		400L, 409L, 405L, 410L, 408L, 382L, 405L, 413L, 400L, 410L, 409L,
		385L, 407L, 408L, 400L, 400L, 408L, 384L, 402L, 411L, 401L, 405L,
		412L, 387L, 412L, 408L, 401L, 405L, 410L, 386L, 418L, 404L, 407L,
		404L)), row.names = c(NA, -60L), class = "data.frame"
	)
	formula<-mohms~1 + (1|part) + (1|oper) + (1|part:oper)
	mohms.lmer <- lme4::lmer(formula,data=resistors)
	# summary(mohms.lmer)
	# library(languageR)
	# lmerPlotInt.fnc(mohms.lmer)
	#https://github.com/Estatcamp/EstatResidualAnalysis/
	# blob/feature/GoldfeldQuandt/tests/testthat/test.calcGQ.R
	mohms.partonly.lmer <- lme4::lmer(mohms~1 + (1|part),data=resistors)
	mohms.operonly.lmer <- lme4::lmer(mohms~1 + (1|oper),data=resistors)
	mohms.intronly.lmer <- lme4::lmer(mohms~1 + (1|part:oper),data=resistors)
	mohms.nopart.lmer <-
	lme4::lmer(mohms~1 + (1 | oper) + (1 | part:oper),data=resistors)
	mohms.nooper.lmer <-
	lme4::lmer(mohms~1 + (1 | part) + (1 | part:oper),data=resistors)
	mohms.nointr.lmer <-
	lme4::lmer(mohms~1+ (1 | part) + (1 | oper),data=resistors)
	fit1<-RLRsim::exactRLRT(mohms.partonly.lmer,mohms.lmer,mohms.nopart.lmer)
	fit2<-RLRsim::exactRLRT(mohms.operonly.lmer,mohms.lmer,mohms.nooper.lmer)
	fit3<-RLRsim::exactRLRT(mohms.intronly.lmer,mohms.lmer,mohms.nointr.lmer)

	out<-c(NA,fit1[['statistic']],
	fit2[['statistic']],
	fit3[['statistic']])

	output<-lmerTest::ranova(mohms.lmer)

    expect_equal(as.numeric(out), output[['LRT']],
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca7", {
	dataset <- structure(list(part = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L,
		2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L,
		4L, 5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 7L, 7L, 7L,
		7L, 7L, 7L, 8L, 8L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 9L, 9L, 10L,
		10L, 10L, 10L, 10L, 10L), .Label = c("1", "2", "3", "4", "5",
		"6", "7", "8", "9", "10"), class = "factor"), oper = structure(c(1L,
		2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L,
		3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L,
		1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L,
		2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), .Label = c("1",
		"2", "3"), class = "factor"), mohms = c(417L, 394L, 404L, 419L,
		398L, 410L, 417L, 387L, 398L, 417L, 399L, 402L, 423L, 389L, 407L,
		418L, 407L, 402L, 412L, 389L, 407L, 410L, 405L, 411L, 407L, 386L,
		400L, 409L, 405L, 410L, 408L, 382L, 405L, 413L, 400L, 410L, 409L,
		385L, 407L, 408L, 400L, 400L, 408L, 384L, 402L, 411L, 401L, 405L,
		412L, 387L, 412L, 408L, 401L, 405L, 410L, 386L, 418L, 404L, 407L,
		404L)), class = "data.frame", row.names = c(NA, -60L))

	# dataset <- resistors
	varY <- 'mohms'
	formula <- formulaRandom(dataset,varY)
	# formula<-mohms~1 + (1|part) + (1|oper) + (1|part:oper)
	mohms.lmer <- lme4::lmer(formula,data=dataset)
	# summary(mohms.lmer)
	# library(languageR)
	# lmerPlotInt.fnc(mohms.lmer)
	#https://github.com/Estatcamp/EstatResidualAnalysis/
	# blob/feature/GoldfeldQuandt/tests/testthat/test.calcGQ.R
	mohms.partonly.lmer <- lme4::lmer(mohms~1 + (1|part),data=dataset)
	mohms.operonly.lmer <- lme4::lmer(mohms~1 + (1|oper),data=dataset)
	mohms.intronly.lmer <- lme4::lmer(mohms~1 + (1|part:oper),data=dataset)
	mohms.nopart.lmer <-
	lme4::lmer(mohms~1 + (1 | oper) + (1 | part:oper),data=dataset)
	mohms.nooper.lmer <-
	lme4::lmer(mohms~1 + (1 | part) + (1 | part:oper),data=dataset)
	mohms.nointr.lmer <-
	lme4::lmer(mohms~1+ (1 | part) + (1 | oper),data=dataset)
	fit1<-RLRsim::exactRLRT(mohms.partonly.lmer,mohms.lmer,mohms.nopart.lmer)
	fit2<-RLRsim::exactRLRT(mohms.operonly.lmer,mohms.lmer,mohms.nooper.lmer)
	fit3<-RLRsim::exactRLRT(mohms.intronly.lmer,mohms.lmer,mohms.nointr.lmer)

	out<-c(NA,fit1[['statistic']],
	fit2[['statistic']],
	fit3[['statistic']])

	output<-lmerTest::ranova(mohms.lmer)

    expect_equal(as.numeric(out), output[['LRT']], tolerance = 1e-06,
	ignore_attr = FALSE)
})

context("Montgomery Cases")

test_that("efeito-verossimilhanca8", {
	#datos montgomery
	dataset<-structure(list(Day = c("p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2"), Operator = c("A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C"), Response = c(21L,
		24L, 20L, 27L, 19L, 23L, 22L, 19L, 24L, 25L, 21L, 18L, 23L, 24L,
		29L, 26L, 20L, 19L, 25L, 19L, 20L, 23L, 21L, 27L, 18L, 21L, 21L,
		17L, 23L, 23L, 20L, 19L, 25L, 24L, 30L, 26L, 20L, 21L, 26L, 19L,
		20L, 24L, 19L, 28L, 19L, 24L, 22L, 18L, 25L, 26L, 20L, 17L, 25L,
		23L, 30L, 25L, 19L, 19L, 25L, 18L, 20L, 24L, 21L, 26L, 18L, 21L,
		24L, 20L, 23L, 25L, 20L, 19L, 25L, 25L, 28L, 26L, 20L, 19L, 24L,
		17L, 19L, 23L, 20L, 27L, 18L, 23L, 22L, 19L, 24L, 24L, 21L, 18L,
		25L, 24L, 31L, 25L, 20L, 21L, 25L, 19L, 21L, 24L, 22L, 28L, 21L,
		22L, 20L, 18L, 24L, 25L, 20L, 19L, 25L, 25L, 30L, 27L, 20L, 23L,
		25L, 17L)), class = "data.frame", row.names = c(NA, -120L)
	)

	varY<-'Response'
	formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	positionVarY<-which(colnames(dataset)==varY)
	colnames(dataset) <- iconv(colnames(dataset), to = "UTF-8")
	varY<-colnames(dataset)[positionVarY]
	for (i in seq(ncol(dataset))){
		if (colnames(dataset)[i] !=varY){
			dataset[,i] <- as.factor(dataset[,i])
		}
	}

	#Converte a resposta p/ num\u00E9rico
	dataset[,varY] <- as.numeric(unclass(dataset[,varY]))
	output <- lme4::lmer(formula,data=dataset,REML)
    expect_equal(class(output), structure("lmerMod", package = "lme4"),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca9", {
	#datos montgomery
	dataset<-structure(list(Day = c("p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2"), Operator = c("A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C"), Response = c(21L,
		24L, 20L, 27L, 19L, 23L, 22L, 19L, 24L, 25L, 21L, 18L, 23L, 24L,
		29L, 26L, 20L, 19L, 25L, 19L, 20L, 23L, 21L, 27L, 18L, 21L, 21L,
		17L, 23L, 23L, 20L, 19L, 25L, 24L, 30L, 26L, 20L, 21L, 26L, 19L,
		20L, 24L, 19L, 28L, 19L, 24L, 22L, 18L, 25L, 26L, 20L, 17L, 25L,
		23L, 30L, 25L, 19L, 19L, 25L, 18L, 20L, 24L, 21L, 26L, 18L, 21L,
		24L, 20L, 23L, 25L, 20L, 19L, 25L, 25L, 28L, 26L, 20L, 19L, 24L,
		17L, 19L, 23L, 20L, 27L, 18L, 23L, 22L, 19L, 24L, 24L, 21L, 18L,
		25L, 24L, 31L, 25L, 20L, 21L, 25L, 19L, 21L, 24L, 22L, 28L, 21L,
		22L, 20L, 18L, 24L, 25L, 20L, 19L, 25L, 25L, 30L, 27L, 20L, 23L,
		25L, 17L)), class = "data.frame", row.names = c(NA, -120L)
	)

	varY<-'Response'
	formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	output<-lmerRandomFit(dataset,varY,formula,REML)
    expect_equal(class(output), structure("lmerMod", package = "lme4"),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca10", {
	#datos montgomery
	dataset<-structure(list(Day = c("p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2"), Operator = c("A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C"), Response = c(21L,
		24L, 20L, 27L, 19L, 23L, 22L, 19L, 24L, 25L, 21L, 18L, 23L, 24L,
		29L, 26L, 20L, 19L, 25L, 19L, 20L, 23L, 21L, 27L, 18L, 21L, 21L,
		17L, 23L, 23L, 20L, 19L, 25L, 24L, 30L, 26L, 20L, 21L, 26L, 19L,
		20L, 24L, 19L, 28L, 19L, 24L, 22L, 18L, 25L, 26L, 20L, 17L, 25L,
		23L, 30L, 25L, 19L, 19L, 25L, 18L, 20L, 24L, 21L, 26L, 18L, 21L,
		24L, 20L, 23L, 25L, 20L, 19L, 25L, 25L, 28L, 26L, 20L, 19L, 24L,
		17L, 19L, 23L, 20L, 27L, 18L, 23L, 22L, 19L, 24L, 24L, 21L, 18L,
		25L, 24L, 31L, 25L, 20L, 21L, 25L, 19L, 21L, 24L, 22L, 28L, 21L,
		22L, 20L, 18L, 24L, 25L, 20L, 19L, 25L, 25L, 30L, 27L, 20L, 23L,
		25L, 17L)), class = "data.frame", row.names = c(NA, -120L)
	)

	varY<-'Response'
	formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	output<-lmerRandomFit(dataset,varY,formula,REML)
    expect_equal(class(output), structure("lmerMod", package = "lme4"),
	tolerance = 1e-06, ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca11", {
	#datos montgomery
	dataset<-structure(list(Day = c("p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1", "p1",
		"p1", "p1", "p1", "p1", "p1", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2", "p2",
		"p2", "p2", "p2"), Operator = c("A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A",
		"A", "A", "A", "A", "A", "A", "A", "A", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B", "B",
		"B", "B", "B", "B", "B", "B", "B", "B", "B", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C", "C",
		"C", "C", "C", "C", "C", "C", "C", "C", "C", "C"), Response = c(21L,
		24L, 20L, 27L, 19L, 23L, 22L, 19L, 24L, 25L, 21L, 18L, 23L, 24L,
		29L, 26L, 20L, 19L, 25L, 19L, 20L, 23L, 21L, 27L, 18L, 21L, 21L,
		17L, 23L, 23L, 20L, 19L, 25L, 24L, 30L, 26L, 20L, 21L, 26L, 19L,
		20L, 24L, 19L, 28L, 19L, 24L, 22L, 18L, 25L, 26L, 20L, 17L, 25L,
		23L, 30L, 25L, 19L, 19L, 25L, 18L, 20L, 24L, 21L, 26L, 18L, 21L,
		24L, 20L, 23L, 25L, 20L, 19L, 25L, 25L, 28L, 26L, 20L, 19L, 24L,
		17L, 19L, 23L, 20L, 27L, 18L, 23L, 22L, 19L, 24L, 24L, 21L, 18L,
		25L, 24L, 31L, 25L, 20L, 21L, 25L, 19L, 21L, 24L, 22L, 28L, 21L,
		22L, 20L, 18L, 24L, 25L, 20L, 19L, 25L, 25L, 30L, 27L, 20L, 23L,
		25L, 17L)), class = "data.frame", row.names = c(NA, -120L)
	)

	varY<-'Response'
	formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	fit<-lmerRandomFit(dataset,varY,formula,REML)


	output<-structure(c(0, 0, 0, 2.88650876419734, 21.7224759310989,
	0.717717691229731,
		0.867379271462575, 1.0207167878154, 3.71927163561311, 23.0608548180667
		), .Dim = c(5L, 2L), .Dimnames = list(c("sd_(Intercept)|Day:Operator",
		"sd_(Intercept)|Operator", "sd_(Intercept)|Day", "sigma", "(Intercept)"
		), c("2.5 %", "97.5 %"))
	)
    expect_equal(confintFitInterval(fit), output, tolerance = 1e-06,
	ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca12", {
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
			class = "data.frame"
	)

	varY<-'score'
	formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	fit<-lmerRandomFit(dataset,varY,formula,REML)
	# VarCorrLmer(fit)

	output<-structure(list(grp = c("employee:batch", "batch", "employee",
		"Residual"), var1 = c("(Intercept)", "(Intercept)", "(Intercept)",
		NA), vcov = c(0.0234901765943228, 0.517635328630026, 1.54473468224099,
		0.226551406678252), sdcor = c(0.153265053402016, 0.719468782248421,
		1.24287355842861, 0.475974165977789)), class = "data.frame",
		row.names = c(NA,
		-4L)
	)
    expect_equal(VarCorrLmer(fit), output, tolerance = 1e-06,
	ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca13", {
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
	REML<-TRUE
	input<-list(dataset=dataset,varY=varY,REML=REML)
	#
	dataset<-input$dataset
	varY<-input$varY
	formula<-formulaRandom(dataset,varY)
	REML<-input$REML

	fit<-lmerRandomFit(dataset,varY,formula,REML)
	covarianceMatrixFit<-covarianceMatrix(fit)
	# randFitAnovaFit<-randFitAnova(fit)
	# contestFitResp<-contestFit(fit)
	# contest1DFitResp<-contest1DFit(fit)
	# contestMDFitResp<-contestMDFit(fit)
	# confintFitIntervalResp<-confintFitInterval(fit)
	# vcovLmerResp<-vcovLmer(fit)
	# VarCorrLmerResp<-VarCorrLmer(fit)
	output<-structure("lmerMod", package = "lme4")
	expect_equal(class(fit), output, tolerance = 1e-06, ignore_attr = FALSE)
})

context("Dados Quality")

test_that("efeito-verossimilhanca14", {
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
	REML<-TRUE
	input<-list(dataset=dataset,varY=varY,REML=REML)
	#
	dataset<-input$dataset
	varY<-input$varY
	formula<-formulaRandom(dataset,varY)
	REML<-input$REML

	fit<-lmerRandomFit(dataset,varY,formula,REML)
	covarianceMatrixFit<-covarianceMatrix(fit)
	# randFitAnovaFit<-randFitAnova(fit)
	L <- diag(length(fixef(fit)))[1, ]
	fit2<-lmerTest::as_lmerModLmerTest(fit)
	# erer<-lmerTest:::contest.lmerMod(fit, L=1, joint=FALSE, confint = TRUE)

	# yy<-suppressWarnings(lmerTest::contest(fit, L, joint=FALSE, confint = T))

	# contest(fit, L=1, joint=FALSE, confint = TRUE)
	# erer<-lmerTest:::contest.lmerMod(fit, L=1, joint=FALSE, confint = TRUE)

	yy<-lmerTest::contest(fit2, L=1, joint=FALSE, confint = TRUE)


	contestFitResp<-contestFit(dataset,varY,formula,REML)
	# contest1DFitResp<-contest1DFit(fit)
	# contestMDFitResp<-contestMDFit(fit)
	# confintFitIntervalResp<-confintFitInterval(fit)
	# vcovLmerResp<-vcovLmer(fit)
	# VarCorrLmerResp<-VarCorrLmer(fit)
	output<-c("data.frame")
	expect_equal(class(contestFitResp), output, tolerance = 1e-06,
	ignore_attr = FALSE)
})

test_that("efeito-verossimilhanca15", {
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
		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L), class = "data.frame")
	varY<-'score'
	# formula <- formulaRandom(dataset,varY)
	REML<-TRUE
	input<-list(dataset=dataset,varY=varY,REML=REML)
	#
	dataset<-input$dataset
	varY<-input$varY
	formula<-formulaRandom(dataset,varY)
	REML<-input$REML

	fit<-lmerRandomFit(dataset,varY,formula,REML)
	covarianceMatrixFit<-covarianceMatrix(fit)

	L <- diag(length(fixef(fit)))[1, ]
	fit2<-lmerTest::as_lmerModLmerTest(fit)

	yy<-lmerTest::contest(fit2, L=1, joint=FALSE, confint = TRUE)

	contestFitResp<-contestFit(dataset,varY,formula,REML)

	output<-c("data.frame")
	expect_equal(class(contestFitResp), output, tolerance = 1e-06,
	ignore_attr = FALSE)
})

# test_that("efeito-verossimilhanca16.1", {
# 	#datos quality
# 	data<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")
# 	varY<-'score'
# 	# formula <- formulaRandom(dataset,varY)
# 	REML<-TRUE
# 	input<-list(data=data,varY=varY,REML=REML)
# 	data=as.data.frame(data)
# 	formula<-formulaRandom(data,varY)
# 	# ########################################
# 	# positionVarY<-which(colnames(data)==varY)
# 	# colnames(data) = iconv(colnames(data), to = "UTF-8")
# 	# varY<-colnames(data)[positionVarY]
# 	# for (i in 1:ncol(data)){
# 	# 	if (colnames(data)[i] !=varY){
# 	# 		data[,i] <- as.factor(data[,i])
# 	# 	}
# 	# }
# 	# #Converte a resposta p/ num\u00E9rico
# 	# # data[,varY] = as.numeric(unclass(data[,varY]))

# 	# data[,which(names(data) == varY)] =
# as.numeric(unclass(data[,which(names(data) == varY)]))
# 	# ########################################


# 	# data[,which(names(data) == varY)]
# 	# -which(names(dataset) == varY)

# 	# data<-dataset

# 	fit<-lme4::lmer(formula=formula,data,REML=REML,na.action=na.omit)
# 	# covarianceMatrixFit<-covarianceMatrix(fit)
# 	rtrt<-lme4::lFormula(formula,data)
# 	erer<-lme4:::checkFormulaData(formula,data)
# 	# tyty<-lmerTest::ranova(fit)
# 	tyty<-lmerTest::rand(fit)

# 	output<-"list"
# 	expect_equal(class(lmerRandom(input)), output, tolerance = 1e-06,
# ignore_attr = FALSE)
# })

# test_that("efeito-verossimilhanca16.1", {
# 	#datos quality
# 	data<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")
# 	data <- as.list(data)
# 	varY<-'score'
# 	REML<-TRUE

#     formulaRandomList<-function(dataset,varY) {
#         # covariables<-colnames(dataset[, -which(names(dataset) == varY)])
#         covariables<-names(dataset)[names(dataset)!=varY]
#         # k<-ncol(dataset[, -which(names(dataset) == varY)])
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }
# 	formula <- formulaRandomList(data,varY)
# 	fit<-lme4::lmer(formula=formula,data=data,REML=REML,na.action=na.omit)

# 		fit2<-lmerTest::as_lmerModLmerTest(fit)
# 		tyty=lmerTest::rand(fit2)

# 	output<-c("anova", "data.frame")
# 	expect_equal(class(tyty), output, tolerance = 1e-06, ignore_attr = FALSE)
# })
# test_that("efeito-verossimilhanca17.1", {
# 	#datos quality
# 	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")

#     varY<-'score'
# 	REML<-TRUE
#     input<-list(dataset=dataset,varY=varY,REML=REML)

#     formulaRandomList<-function(data,varY) {
#         covariables<-names(data)[names(data)!=varY]
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }
# 	data <- as.list(dataset)
# 	formula <- formulaRandomList(data,varY)
# 	fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)

# 		fit<-lmerTest::as_lmerModLmerTest(fit2)
# 		tyty=lmerTest::rand(fit)
# 	output<-c("anova", "data.frame")
# 	expect_equal(class(tyty), output, tolerance = 1e-06, ignore_attr = FALSE)
# })
# test_that("efeito-verossimilhanca18.1", {
# 	#datos quality
# 	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")

#     varY<-'score'
# 	REML<-TRUE
#     input<-list(dataset=dataset,varY=varY,REML=REML)

#     formulaRandomList<-function(data,varY) {
#         covariables<-names(data)[names(data)!=varY]
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }

# 	data <- as.list(dataset)

# 	formula <- formulaRandomList(data,varY)
# 	fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)

# 		fit<-lmerTest::as_lmerModLmerTest(fit2)
# 		tyty=lmerTest::rand(fit)

# 	output<-c("anova", "data.frame")
# 	expect_equal(class(tyty), output, tolerance = 1e-06, ignore_attr = FALSE)
# })
# test_that("efeito-verossimilhanca20.1", {
# 	#datos quality
# 	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")

#     varY<-'score'
# 	REML<-TRUE
#     input<-list(dataset=dataset,varY=varY,REML=REML)

#     formulaRandomList<-function(data,varY) {
#         covariables<-names(data)[names(data)!=varY]
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }
# 	lmerRandomList<-function(input) {
# 		dataset<-input$dataset
# 			data <- as.list(dataset)

# 		varY<-input$varY
# 		formula<-formulaRandomList(data,varY)
# 		REML<-input$REML

#         fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,
# na.action=na.omit)
#         fit<-lmerTest::as_lmerModLmerTest(fit2)

# 		covarianceMatrixFit<-covarianceMatrix(fit)
# 		randFitAnovaFit<-randFitAnova(fit)
# 		contestFitResp<-contestFit(dataset,varY,formula,REML)
# 		contest1DFitResp<-contest1DFit(fit)
# 		contestMDFitResp<-contestMDFit(fit)
# 		confintFitIntervalResp<-confintFitInterval(fit)
# 		vcovLmerResp<-vcovLmer(fit)
# 		VarCorrLmerResp<-VarCorrLmer(fit)

#         return(list(covarianceMatrixFit=covarianceMatrixFit,
# randFitAnovaFit=randFitAnovaFit,contestFitResp=contestFitResp,
# contestMDFitResp=contestMDFitResp,
# confintFitIntervalResp=confintFitIntervalResp,vcovLmerResp=vcovLmerResp,
# VarCorrLmerResp=VarCorrLmerResp))
# 	}

# 	data <- as.list(dataset)

# 	formula <- formulaRandomList(data,varY)
# 	fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)

# 		fit<-lmerTest::as_lmerModLmerTest(fit2)
# 		tyty=lmerTest::rand(fit)

# 	output<-c("anova", "data.frame")
# 	expect_equal(class(tyty), output, tolerance = 1e-06, ignore_attr = FALSE)
# })
# test_that("efeito-verossimilhanca21.1", {
# 	#datos quality
# 	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")

#     varY<-'score'
# 	REML<-TRUE
#     input<-list(dataset=dataset,varY=varY,REML=REML)

#     formulaRandomList<-function(data,varY) {
#         covariables<-names(data)[names(data)!=varY]
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }
# 	lmerRandomList<-function(input) {
# 		dataset<-input$dataset
# 			data <- as.list(dataset)

# 		varY<-input$varY
# 		formula<-formulaRandomList(data,varY)
# 		REML<-input$REML

#         fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,
# na.action=na.omit)
#         fit<-lmerTest::as_lmerModLmerTest(fit2)

# 		# fit<-lmerRandomFit(dataset,varY,formula,REML)
# 		covarianceMatrixFit<-covarianceMatrix(fit)
# 		randFitAnovaFit<-randFitAnova(fit)
# 		contestFitResp<-contestFit(dataset,varY,formula,REML)
# 		contest1DFitResp<-contest1DFit(fit)
# 		contestMDFitResp<-contestMDFit(fit)
# 		confintFitIntervalResp<-confintFitInterval(fit)
# 		vcovLmerResp<-vcovLmer(fit)
# 		VarCorrLmerResp<-VarCorrLmer(fit)

#         return(list(covarianceMatrixFit=covarianceMatrixFit,
# randFitAnovaFit=randFitAnovaFit,contestFitResp=contestFitResp,
# contestMDFitResp=contestMDFitResp,
# confintFitIntervalResp=confintFitIntervalResp,vcovLmerResp=vcovLmerResp,
# VarCorrLmerResp=VarCorrLmerResp))
# 		# return(list(covarianceMatrixFit=covarianceMatrixFit,
# randFitAnovaFit=randFitAnovaFit,contestFitResp=contestFitResp,
# contest1DFitResp=contest1DFitResp,contestMDFitResp=contestMDFitResp,
# confintFitIntervalResp=confintFitIntervalResp,vcovLmerResp=vcovLmerResp))
# 	}
# 	data <- as.list(dataset)

# 	formula <- formulaRandomList(data,varY)
# 	fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,na.action=na.omit)

# 		fit<-lmerTest::as_lmerModLmerTest(fit2)
# 		tyty=lmerTest::rand(fit)

# 	output<-c("anova", "data.frame")
# 	expect_equal(class(tyty), output, tolerance = 1e-06, ignore_attr = FALSE)
# })
# test_that("efeito-verossimilhanca22.1", {
# 	#datos quality
# 	dataset<-structure(list(employee = structure(c(1L, 1L, 1L, 1L, 1L, 1L,
# 	1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L,
# 	2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L,
# 	3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L, 3L,
# 	4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
# 	4L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
# 	5L, 5L, 5L, 5L), .Label = c("1", "2", "3", "4", "5"), class = "factor"),
# 		batch = structure(c(1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L,
# 		4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L,
# 		3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L, 1L, 1L, 2L,
# 		2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L, 6L, 6L, 1L,
# 		1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L, 5L, 5L, 6L,
# 		6L, 6L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 5L,
# 		5L, 5L, 6L, 6L, 6L), .Label = c("B1", "B2", "B3", "B4", "B5",
# 		"B6"), class = "factor"), score = c(27.4, 27.8, 27.3, 25.5,
# 		25.5, 26.4, 26.9, 26.3, 25.1, 25.6, 25.8, 25.5, 25.7, 25.9,
# 		26.5, 25.5, 25, 25.8, 29.6, 30.6, 29.7, 29.3, 28.7, 28.4,
# 		28.5, 28.9, 29.3, 28.3, 28.7, 28.7, 29.5, 29.6, 30.2, 28.9,
# 		30, 28.3, 29.1, 28.3, 29.4, 27, 27.7, 26.7, 27.6, 28.1, 27.4,
# 		28.2, 26.9, 27.9, 27.6, 27.3, 28.4, 26.9, 26.9, 26.5, 29,
# 		28.3, 29.2, 26.6, 26.4, 26.5, 26.6, 26.5, 26.5, 26.8, 26.4,
# 		26.2, 27.1, 28.1, 27.5, 26.8, 26.4, 25.9, 28, 28.5, 26.9,
# 		26.5, 25.6, 25.6, 25.6, 26.5, 26.2, 26.4, 26, 26.3, 26.5,
# 		26.3, 26.3, 25.5, 25.3, 24.9)), row.names = c(NA, -90L),
# class = "data.frame")

#     varY<-'score'
# 	REML<-TRUE
#     input<-list(dataset=dataset,varY=varY,REML=REML)

#     formulaRandomList<-function(data,varY) {
#         covariables<-names(data)[names(data)!=varY]
#         k<-length(covariables)
#         temporal<-NULL
#         for (i in seq(k)) {
#             combina<-as.matrix(combinat::combn(covariables, i))
#             acumula<-NULL
#             for(j in seq(ncol(combina))){
#                 tmp<-combina[,j]
#                 formula2<-paste(tmp,collapse=':')
#                 acumula<-c(acumula,formula2)
#             }
#             temporal<-c(temporal,paste0("(1|",acumula,")", collapse='+'))
#         }
#         return(stats::formula(paste(varY,'~',paste0(temporal,collapse="+"))))
#     }
# 	lmerRandomList<-function(input) {
# 		dataset<-input$dataset
# 			data <- as.list(dataset)

# 		varY<-input$varY
# 		formula<-formulaRandomList(data,varY)
# 		REML<-input$REML

#         fit2<-lme4::lmer(formula=formula,data=dataset,REML=REML,
# na.action=na.omit)
#         fit<-lmerTest::as_lmerModLmerTest(fit2)

# 		covarianceMatrixFit<-covarianceMatrix(fit)
# 		randFitAnovaFit<-randFitAnova(fit)
# 		contestFitResp<-contestFit(dataset,varY,formula,REML)
# 		contest1DFitResp<-contest1DFit(fit)
# 		contestMDFitResp<-contestMDFit(fit)
# 		confintFitIntervalResp<-confintFitInterval(fit)
# 		vcovLmerResp<-vcovLmer(fit)
# 		VarCorrLmerResp<-VarCorrLmer(fit)

#         return(list(covarianceMatrixFit=covarianceMatrixFit,
# randFitAnovaFit=randFitAnovaFit,contestFitResp=contestFitResp,
# contestMDFitResp=contestMDFitResp,
# confintFitIntervalResp=confintFitIntervalResp,vcovLmerResp=vcovLmerResp,
# VarCorrLmerResp=VarCorrLmerResp))
# 	}

# 	output<-"list"
# 	expect_equal(class(lmerRandomList(input)), output, tolerance = 1e-06,
# ignore_attr = FALSE)
# })


test_that("efeito-verossimilhanca25.1", {
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

	output<-list(covarianceMatrixFit = structure(list(Factors = c("employee:batch", 
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

test_that("efeito-verossimilhanca26.1", {
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

	output<-lmerRandomList(input)
	expect_equal(class(output),"list", tolerance = 1e-06,
	ignore_attr = FALSE)
})

