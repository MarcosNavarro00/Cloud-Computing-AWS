setwd('../../')
source('./R/utils.R')
source('./app.R')
PATH_TO_FILE = "/home/ec2-user/environment/CloudComputingProject/StreamingHistory0.json"

testServer(expr = {
  session$setInputs(zima = 1)
  expect_equal(selected_spotidane$begin_date, date(format(date("2018-12-22"),"%Y-%m-%d")))
  expect_equal(selected_spotidane$end_date, date(format(date("2019-03-20"),"%Y-%m-%d")))
  
  session$setInputs(wiosna = 1)
  expect_equal(selected_spotidane$begin_date, date("2019-03-21"))
  expect_equal(selected_spotidane$end_date, date("2019-06-30"))
  
  session$setInputs(lato = 1)
  expect_equal(selected_spotidane$begin_date, date(format(date("2019-07-01"),"%Y-%m-%d")))
  expect_equal(selected_spotidane$end_date, date(format(date("2019-09-30"),"%Y-%m-%d")))
  
  session$setInputs(jesien = 1)
  expect_equal(selected_spotidane$begin_date, date("2019-10-01"))
  expect_equal(selected_spotidane$end_date, date("2019-12-31"))
  
  session$setInputs(daterange1 = c(date(format(date("2018-12-22"),"%Y-%m-%d")),
                                   date(format(date("2019-03-20"),"%Y-%m-%d"))))
  expect_equal(selected_spotidane$begin_date, date(format(date("2018-12-22"),"%Y-%m-%d")))
  expect_equal(selected_spotidane$end_date, date(format(date("2019-03-20"),"%Y-%m-%d")))
  expect_equal(selected_spotidane$arrow_index, 0)
  
  session$setInputs(pressedKey = 1, key=39)
  expect_equal(selected_spotidane$window, c(FALSE, TRUE, FALSE))
  session$setInputs(pressedKey = 1, key=40)
  expect_equal(selected_spotidane$first_plot, FALSE)
  expect_equal(selected_spotidane$get_range, FALSE)
  expect_equal(selected_spotidane$keep_range, TRUE)
  
  session$setInputs(resetdat = 1)
  expect_equal(selected_spotidane$begin_date, date("2018-12-01"))
  expect_equal(selected_spotidane$end_date, date("2020-01-31"))
  
  expect_equal(selected_spotidane$hover[1], -0.3)
  expect_equal(selected_spotidane$hover[2], -0.3)
  session$setInputs(hover = NULL)
  expect_equal(selected_spotidane$hover[1], -0.3)
  expect_equal(selected_spotidane$hover[2], -0.3)
  session$setInputs(hover_read=F, hover = list(x=2, y=3))
  expect_equal(selected_spotidane$hover[1], -0.3)
  expect_equal(selected_spotidane$hover[2], -0.3)
  
  selected_spotidane$hover_read = T
  session$setInputs(hover = list(x=2, y=3))
  expect_equal(selected_spotidane$hover[1], 2)
  expect_equal(selected_spotidane$hover[2], 3)
  
  session$setInputs(files = list(datapath=c(PATH_TO_FILE), type=c("text/json")))
  expect_true(is.data.frame(spotidane$toBind))
  expect_true(is.data.frame(spotidane$data))
  expect_gt(nrow(spotidane$data), 0)
  expect_equal(colnames(spotidane$data), c("endTime", "artistName", "trackName", "msPlayed"))
  
  expect_type(getRegion(), 'character')
  
  ex_session = list(userData=list(user_id='123e4567-e89b-12d3-a456-556642440000'))
  ex_file_list = update_file_list(NULL, shiny::tags, ex_session, NULL, NULL, NULL)
  expect_equal(ex_file_list$name, 'table')
  expect_equal(length(ex_file_list$attribs), 0)
  })

