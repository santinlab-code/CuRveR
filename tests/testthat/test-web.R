test_that("preparation retains the historical transformation", {
  d<-data.frame(Time=10:20,A1=seq(.1,1.1,.1),A2=seq(.2,1.2,.1))
  old<-CuRveR::clean_time(d,"Time");old$signal<-"OD"
  expect_equal(CuRveR:::web_prepare(d,"Time",c("A1","A2"),"OD"),CuRveR::format_wellplate_long(old))
  expect_error(CuRveR:::web_prepare(transform(d,A1="bad"),"Time","A1","OD"),"numériques")
  expect_error(CuRveR:::web_prepare(d,"Time","Time","OD"),"distinctes")
})

test_that("numerical functions match the frozen reference", {
  reference<-readRDS(test_path("fixtures","scientific.rds"))
  x<-seq(0,24,length.out=49);y<-.1+.9/(1+exp(-.5*(x-12)))
  set.seed(42);m<-CuRveR::RichardModel$new(x,y);m$optimize(CuRveR::DeOptimizer$new(CuRveR::ordinary_least_squares))
  expect_equal(m$estimated,reference$estimated,tolerance=1e-10)
  expect_equal(m$predict(),reference$predicted,tolerance=1e-10)
  expect_equal(CuRveR::finite_diff_5pt_cent(x,y),reference$derivative)
})

test_that("adapter preserves grouped historical blank correction and fitting", {
  x<-seq(0,24,length.out=49)
  d<-data.frame(Time=x,A1=.1+.9/(1+exp(-.5*(x-12))),A2=.11+.9/(1+exp(-.5*(x-12))),B1=rep(.05,49))
  d<-CuRveR:::web_prepare(d,"Time",c("A1","A2","B1"),"OD")
  cond<-list(Control=list(replicates=c("A1","A2"),blanks="B1"))
  windows<-data.frame(signal="OD",well=c("A1","A2","B1"),left=0,right=24)
  for(loss in c("LAD","OLS")){
    fun<-if(loss=="LAD") CuRveR::least_absolute_deviation else CuRveR::ordinary_least_squares
    old<-d[d$well!="B1",];old$value<-old$value-mean(d$value[d$well=="B1"]);old$condition<-"Control"
    set.seed(42)
    fit<-CuRveR::fit_data(old,c(signal,condition),value,Time,loss_fun=fun,.id=well)
    new<-CuRveR:::web_analyze(d,cond,windows,loss,42L)
    expect_equal(unlist(new$parameters[1,c("p_max","p_min","r_max","s")]),fit$model[[1]]$estimated,tolerance=1e-10)
    expect_equal(new$data$value,old$value)
    expect_equal(new$data$fit,fit$model[[1]]$predict(new$data$Time),tolerance=1e-10)
    expect_equal(new$performance$VEcv,CuRveR:::variance_explained(new$data$value,new$data$fit))
    expect_s3_class(CuRveR:::web_plot(new,"OD","Control"),"ggplot")
    expect_silent(ggplot2::ggplot_build(CuRveR:::web_plot(new,"OD","Control","annotations",c("fit","r_max","s"))))
  }
  expect_error(CuRveR:::web_validate_conditions(list(C=list(replicates="A1",blanks="A1")),unique(d$well)),"distincts")
  expect_error(CuRveR:::web_analyze(d,cond,transform(windows,right=1)),"cinq")
})

test_that("both interfaces construct and modules initialise", {
  expect_s3_class(CuRveR::run_curver(),"shiny.appobj")
  expect_s3_class(CuRveR::run_curver(legacy=TRUE),"shiny.appobj")
  shiny::testServer(CuRveR:::web_import_server,args=list(),{expect_null(data())})
})

test_that("decreasing curves, windows and multiple signals remain reproducible", {
  x<-seq(0,24,length.out=49)
  d<-CuRveR:::web_prepare(data.frame(Time=x,A1=1-.9/(1+exp(-.5*(x-12)))),"Time","A1","Z")
  d<-rbind(d,transform(d,signal="A"))
  c<-list(Control=list(replicates="A1",blanks=character()))
  w<-data.frame(signal=c("Z","A"),well="A1",left=2,right=22)
  set.seed(123);state<-.Random.seed
  a<-CuRveR:::web_analyze(d,c,w,"LAD",42)
  expect_identical(.Random.seed,state)
  b<-CuRveR:::web_analyze(d,c,w,"LAD",42)
  expect_equal(a,b)
  expect_true(all(a$data$Time>=2 & a$data$Time<=22))
  expect_true(all(is.finite(a$data$fit)))
  expect_equal(sort(unique(a$data$signal)),c("A","Z"))
})

test_that("overview uses reversible dual axes without changing measurements", {
  d<-data.frame(Time=rep(0:2,2),well="A1",signal=rep(c("OD","mCh"),each=3),value=c(.2,.4,.6,200,400,600))
  attr(d,"od_signal")<-"OD";attr(d,"fluorescence_signal")<-"mCh"
  original<-d;p<-CuRveR:::web_overview_plot(d)
  built<-ggplot2::ggplot_build(p)
  expect_equal(d,original)
  expect_equal(built$data[[1]]$y,c(.2,.4,.6))
  expect_equal(built$data[[2]]$y,c(.2,.4,.6))
  expect_equal(p$scales$get_scales("y")$secondary.axis$trans(built$data[[2]]$y),c(200,400,600))
  expect_equal(unique(built$data[[1]]$colour),"black")
  expect_equal(unique(built$data[[2]]$colour),"#c62828")
  d$value<-0
  expect_true(all(is.finite(ggplot2::ggplot_build(CuRveR:::web_overview_plot(d))$data[[2]]$y)))
})
