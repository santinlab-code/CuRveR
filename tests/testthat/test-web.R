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
    new<-CuRveR:::web_analyze(d,cond,windows,loss,42L,engine="legacy")
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

test_that("language defaults to English and signal labels are generic", {
  ui<-as.character(CuRveR:::modern_ui())
  expect_match(ui,'id="language"')
  expect_match(ui,'value="en" selected="selected"')
  expect_false(grepl("Feuille signal \\(mCh\\)",ui))
})

test_that("overview aligns nonzero minima and maxima with an invertible axis", {
  d<-data.frame(Time=rep(0:2,2),well="A1",signal=rep(c("OD","mCh"),each=3),value=c(.2,.4,.6,1000,3000,5000))
  attr(d,"od_signal")<-"OD";attr(d,"fluorescence_signal")<-"mCh"
  p<-CuRveR:::web_overview_plot(d);built<-ggplot2::ggplot_build(p)
  expect_equal(range(built$data[[1]]$y),range(built$data[[2]]$y))
  expect_equal(p$scales$get_scales("y")$secondary.axis$trans(built$data[[2]]$y),c(1000,3000,5000))
  expect_equal(d$value,c(.2,.4,.6,1000,3000,5000))
})

test_that("each well independently aligns both axis endpoints", {
  d<-data.frame(Time=rep(0:2,4),well=rep(c("A1","A2"),each=6),signal=rep(rep(c("OD","signal"),each=3),2),value=c(.2,.4,.6,1000,3000,5000,.3,.4,.5,600,900,1200))
  attr(d,"od_signal")<-"OD";attr(d,"fluorescence_signal")<-"signal"
  original<-d
  panels<-CuRveR:::web_overview_panels(d)
  expect_equal(names(panels),c("A1","A2"))
  for(well in names(panels)){
    built<-ggplot2::ggplot_build(panels[[well]])
    expect_equal(range(built$data[[1]]$y),range(built$data[[2]]$y))
    expected<-d$value[d$well==well & d$signal=="signal"]
    expect_equal(panels[[well]]$scales$get_scales("y")$secondary.axis$trans(built$data[[2]]$y),expected)
  }
  expect_s3_class(CuRveR:::web_overview_grid(d),"gTree")
  expect_equal(d,original)
})

test_that("technical triplets follow physical rows and columns", {
  groups<-CuRveR:::web_triplet_conditions(c(paste0("A",12:1),"B1","B3","B4"))
  expect_equal(groups[["A1-A3"]]$replicates,paste0("A",1:3))
  expect_equal(groups[["A10-A12"]]$replicates,paste0("A",10:12))
  expect_equal(groups[["B1-B3"]]$replicates,c("B1","B3"))
  expect_equal(groups[["B4"]]$replicates,"B4")
  expect_true(all(vapply(groups,function(g)length(g$blanks)==0,logical(1))))
  expect_equal(CuRveR:::web_triplet_conditions(NULL),list())
  expect_false(grepl('value="Témoin"',as.character(CuRveR:::web_conditions_ui('c'))))
})

test_that("one global blank is applied to all conditions and excluded from fitting", {
  groups<-CuRveR:::web_triplet_conditions(c(paste0("A",1:9)))
  common<-CuRveR:::web_apply_global_blank(groups,paste0("A",7:9))
  expect_equal(names(common),c("A1-A3","A4-A6"))
  expect_true(all(vapply(common,function(x)identical(x$blanks,paste0("A",7:9)),logical(1))))
  x<-seq(0,24,length.out=49);curve<-.1+.9/(1+exp(-.5*(x-12)))
  d<-expand.grid(Time=x,well=paste0("A",1:9),signal=c("OD","Signal"),stringsAsFactors=FALSE)
  d$value<-ifelse(d$signal=="OD",curve[match(d$Time,x)]+.05,1000*curve[match(d$Time,x)]+100)
  d$value[d$well %in% paste0("A",7:9)]<-ifelse(d$signal[d$well %in% paste0("A",7:9)]=="OD",.05,100)
  original<-d
  windows<-unique(d[c("signal","well")]);windows$left<-0;windows$right<-24
  corrected<-CuRveR:::web_blank_correct(d,paste0("A",7:9),windows)
  expect_equal(d,original)
  expect_equal(corrected$value[corrected$well=="A7"],rep(0,98))
  result<-CuRveR:::web_analyze(d,common,windows)
  expect_false(any(result$data$well %in% paste0("A",7:9)))
  for(signal in c("OD","Signal"))for(condition in names(common)){
    actual<-result$data[result$data$signal==signal & result$data$condition==condition,]
    expected<-corrected[corrected$signal==signal & corrected$well %in% common[[condition]]$replicates,]
    expect_equal(actual$value,expected$value)
  }
  expect_false(grepl('id="c-blanks"',as.character(CuRveR:::web_conditions_ui('c'))))
})

test_that("renaming and editing any group preserves the common blank identity", {
  wells<-paste0("A",1:9);groups<-CuRveR:::web_triplet_conditions(wells)
  edit<-CuRveR:::web_edit_group(groups,"A7-A9","Medium",paste0("A",7:9),"A7-A9",wells)
  expect_equal(edit$blank,"Medium")
  expect_true("Medium" %in% names(edit$groups))
  edit<-CuRveR:::web_edit_group(edit$groups,"Medium","Medium",c("A7","A8"),edit$blank,wells)
  effective<-CuRveR:::web_apply_global_blank(edit$groups,edit$groups[[edit$blank]]$replicates)
  expect_equal(effective[[1]]$blanks,c("A7","A8"))
  expect_false("Medium" %in% names(effective))
  edit<-CuRveR:::web_edit_group(edit$groups,"A1-A3","Treatment",paste0("A",1:4),edit$blank,wells)
  expect_equal(edit$groups$Treatment$replicates,paste0("A",1:4))
  expect_equal(edit$groups[["A4-A6"]]$replicates,c("A5","A6"))
  config<-CuRveR:::web_read_group_config(list(version=2,blank=edit$blank,groups=edit$groups),wells)
  expect_equal(config,edit)
  expect_error(CuRveR:::web_edit_group(edit$groups,"Treatment","Medium","A1",edit$blank,wells),"exists")
})

test_that("time windows stay local unless global application is explicitly checked", {
  d<-expand.grid(Time=0:10,well=c("A1","A2"),signal=c("OD","Signal"),stringsAsFactors=FALSE);d$value<-d$Time
  shiny::testServer(CuRveR:::web_windows_server,args=list(data=shiny::reactive(d)),{
    session$setInputs(signal="OD",well="A1",all=FALSE,bounds_edit=list(signal="OD",well="A1",left=2,right=8,all=FALSE))
    w<-windows();expect_equal(w$left[w$signal=="OD" & w$well=="A1"],2)
    expect_true(all(w$left[!(w$signal=="OD" & w$well=="A1")]==0))
    session$setInputs(signal="OD",well="A2")
    expect_equal(windows(),w)
    session$setInputs(signal="Signal",well="A1",bounds_edit=list(signal="Signal",well="A1",left=3,right=7,all=FALSE))
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A1"],2)
    expect_equal(windows()$left[windows()$signal=="Signal" & windows()$well=="A1"],3)
    session$setInputs(all=TRUE,bounds_edit=list(signal="Signal",well="A1",left=1,right=9,all=TRUE))
    expect_true(all(windows()$left==1));expect_true(all(windows()$right==9))
    session$setInputs(all=FALSE,well="A2",bounds_edit=list(signal="Signal",well="A2",left=4,right=6,all=FALSE))
    expect_equal(sum(windows()$left==4),1)
  })
})

test_that("brushed windows persist immediately without Apply", {
  d<-expand.grid(Time=0:10,well=c("A1","A2"),signal=c("OD","Signal"),stringsAsFactors=FALSE);d$value<-d$Time
  shiny::testServer(CuRveR:::web_windows_server,args=list(data=shiny::reactive(d)),{
    session$setInputs(signal="OD",well="A1",all=FALSE,brush=list(xmin=2,xmax=8))
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A1"],2)
    session$setInputs(well="A2",brush=NULL)
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A2"],0)
    session$setInputs(brush=list(xmin=3,xmax=7))
    session$setInputs(well="A1",brush=NULL)
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A1"],2)
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A2"],3)
    session$setInputs(all=TRUE,brush=list(xmin=1,xmax=9))
    expect_true(all(windows()$left==1));expect_true(all(windows()$right==9))
  })
})

test_that("checking global mode immediately propagates the current saved window", {
  d<-expand.grid(Time=0:10,well=c("A1","A2"),signal=c("OD","Signal"),stringsAsFactors=FALSE);d$value<-d$Time
  shiny::testServer(CuRveR:::web_windows_server,args=list(data=shiny::reactive(d)),{
    session$setInputs(signal="OD",well="A1",all=FALSE,brush=list(xmin=2,xmax=8))
    session$setInputs(all=TRUE,brush=NULL)
    expect_true(all(windows()$left==2));expect_true(all(windows()$right==8))
    session$setInputs(all=FALSE,well="A2",bounds_edit=list(signal="OD",well="A1",left=3,right=7,all=FALSE))
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A1"],3)
    expect_equal(windows()$left[windows()$signal=="OD" & windows()$well=="A2"],2)
  })
  expect_false(grepl('id="w-apply"',as.character(CuRveR:::web_windows_ui('w'))))
})

test_that("complete result data are accessible beyond the old 200-row limit", {
  d<-data.frame(row=1:537,value=seq_len(537)/10)
  pages<-lapply(1:11,function(page)CuRveR:::web_data_page(d,page,50))
  expect_equal(do.call(rbind,pages),d)
  expect_equal(nrow(CuRveR:::web_data_page(d,11,50)),37)
  expect_equal(CuRveR:::web_data_page(d,999,50),pages[[11]])
  expect_equal(CuRveR:::web_data_page(d,NA,NA),pages[[1]])
})

test_that("scaled Richard fitting recovers smooth known curves independent of units", {
  x<-rep(seq(0,24,length.out=49),3)
  for(direction in c(-1,1)) {
    y<-.1+.9/(1+exp(-direction*.5*(x-12)))
    set.seed(42);fit<-CuRveR:::web_fit_richard(x,y,CuRveR::least_absolute_deviation)
    expect_lt(mean(abs(fit$predict()-y)),.005)
    expect_equal(sign(fit$estimated['r_max']),direction,ignore_attr=TRUE)
    set.seed(42);minutes<-CuRveR:::web_fit_richard(x*60,y,CuRveR::least_absolute_deviation)
    expect_equal(minutes$predict(),fit$predict(),tolerance=.005)
    expect_equal(unname(minutes$estimated['r_max']*60),unname(fit$estimated['r_max']),tolerance=.01)
    set.seed(42);large<-CuRveR:::web_fit_richard(x,y*10000,CuRveR::least_absolute_deviation)
    expect_equal(large$predict()/10000,fit$predict(),tolerance=.005)
  }
})
