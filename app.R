library(DT)
library(lubridate)
library(shiny)
library(stringr)
library(tidyverse)


rank2int <- function(s) as.integer(sprintf(
	"%02d%03d%d",
	match(str_match(s, "^\\D+")[, 1], c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")),
	as.integer(str_match(s, "^\\D+([0-9]+)\\D")[, 2]),
	match(substr(s, nchar(s), nchar(s)), c("e", "w"))
))


server <- function(input, output) {
	banzuke <- reactive({
		withProgress({
			"/home/cervus/sumo-misc-scripts/banzuke" %>% 
				list.files(pattern = "\\.csv", full.names = TRUE) %>% 
				tail(1) %>% 
				read_csv()
		}, message = "Loading banzuke...", value = .5)
	})
	
	# HEADER: yyyy.mm
	output$basho = renderText({
		req(banzuke())
		paste(max(banzuke()$basho), "banzuke")
	})
	
	# remove lower divisions & select relevant columns
	banzuke_to_show <- reactive({
		req(banzuke)
		
		banzuke() %>% 
			mutate(
				rank = ordered(
					rank,
					levels = banzuke() %>% 
						mutate(rank_as_int = rank2int(rank)) %>% 
						arrange(rank_as_int) %>% 
						pull(rank)
				),
				prev = ordered(
					prev,
					levels = banzuke() %>% 
						mutate(prev_as_int = rank2int(prev)) %>% 
						replace_na(replace = list(prev_as_int = 1e+6)) %>% 
						arrange(prev_as_int) %>% 
						pull(prev) %>% 
						unique()
				),
				rank_level = ordered(
					str_extract(rank, "^\\D+"),
					levels = c("Y", "O", "S", "K", "M", "J", "Ms", "Sd", "Jd", "Jk")
				)
			) %>% 
			filter(rank_level < "J") %>% 
			transmute(
				rank,
				rikishi,
				heya,
				shusshin,
				age = as.integer(difftime(
					Sys.Date(),
					dmy(birth_date),
					units = "days"
				)) / 365.25,
				height,
				weight,
				prev_rank = prev,
				prev_result = paste(prev_w, prev_l, sep = "-")
			)
	})
	
	# TABLE: banzuke
	output$banzuke = DT::renderDataTable({
		req(banzuke_to_show)
		
		datatable(
			banzuke_to_show(),
			class = "compact",
			options = list(
				dom = "t",
				lengthChange = FALSE,
				pageLength = nrow(banzuke_to_show())
			)
		) %>% 
			formatRound(columns = c("age"), digits = 0) %>% 
			formatRound(columns = c("height", "weight"), digits = 1)
	}, server = TRUE)
	
	# historical results
	results <- reactive({
		withProgress({
			files <- "/home/cervus/sumo-misc-scripts/results" %>% 
				list.files(pattern = "\\.csv", full.names = TRUE)
			
			do.call(
				rbind,
				lapply(
					files,
					function(f) {
						incProgress(1 / length(files))
						read_csv(f)
					}
				)
			)
		}, message = "Loading historical results...", value = 0)
	})
	
	# Marathonbet odds
	odds <- reactive({
		withProgress({
			"/home/cervus/sumo-odds/odds" %>% 
				list.files(pattern = "\\.csv", full.names = TRUE) %>% 
				tail(1) %>% 
				read_csv() %>% 
				group_by(rikishi1, rikishi2) %>% 
				summarise(
					odds1 =  round(last(odds1, order_by = ts), 2),
					odds2 = round(last(odds2, order_by = ts), 2)
				) %>% 
				ungroup()
		}, message = "Loading odds...", value = .5)
	})
	
	# HEADER: first wrestler's name
	output$shikona1 = renderText({
		req(input$banzuke_rows_selected, results)
		
		shikona <- banzuke_to_show()$rikishi[input$banzuke_rows_selected[1]]
		
		results() %>% 
			filter(
				basho == max(banzuke()$basho),
				rikishi1_shikona == shikona
			) %>% 
			arrange(day) %>% 
			pull(rikishi1_result) %>% 
			tail(1) %>% 
			paste(shikona, .)
	})
	
	# TABLE: first wrestler's performance
	output$rikishi1 = renderTable({
		req(banzuke, results, odds, input$banzuke_rows_selected[1])
		
		results() %>% 
			filter(
				basho == max(banzuke()$basho),
				rikishi1_shikona == banzuke_to_show()$rikishi[input$banzuke_rows_selected[1]]
			) %>% 
			left_join(
				odds() %>% 
					transmute(
						rikishi1_shikona = rikishi1,
						rikishi2_shikona = rikishi2,
						odds1
					)
			) %>%
			left_join(
				odds() %>% 
					transmute(
						rikishi1_shikona = rikishi2,
						rikishi2_shikona = rikishi1,
						odds2
					)
			) %>% 
			transmute(
				day,
				odds = coalesce(odds1, odds2),
				win = recode(rikishi1_win + 1, " ", "x"),
				kimarite,
				opponent = rikishi2_shikona,
				rank = rikishi2_rank
			)
	}, digits = 2, na = "")
	
	# HEADER: second wrestler's name
	output$shikona2 = renderText({
		req(input$banzuke_rows_selected[2], results)
		
		shikona <- banzuke_to_show()$rikishi[input$banzuke_rows_selected[2]]
		
		results() %>% 
			filter(
				basho == max(banzuke()$basho),
				rikishi1_shikona == shikona
			) %>% 
			arrange(day) %>% 
			pull(rikishi1_result) %>% 
			tail(1) %>% 
			paste(shikona, .)
	})
	
	# TABLE: second wrestler's performance
	output$rikishi2 = renderTable({
		req(banzuke, results, odds, input$banzuke_rows_selected[2])
		
		results() %>% 
			filter(
				basho == max(banzuke()$basho),
				rikishi1_shikona == banzuke_to_show()$rikishi[input$banzuke_rows_selected[2]]
			) %>% 
			left_join(
				odds() %>% 
					transmute(
						rikishi1_shikona = rikishi1,
						rikishi2_shikona = rikishi2,
						odds1
					)
			) %>%
			left_join(
				odds() %>% 
					transmute(
						rikishi1_shikona = rikishi2,
						rikishi2_shikona = rikishi1,
						odds2
					)
			) %>% 
			transmute(
				day,
				odds = coalesce(odds1, odds2),
				win = recode(rikishi1_win + 1, " ", "x"),
				kimarite,
				opponent = rikishi2_shikona,
				rank = rikishi2_rank
			)
	}, digits = 2, na = "")
	
	# HEADER: first vs second
	output$vs = renderText({
		req(input$banzuke_rows_selected[2])
		paste(
			banzuke_to_show()$rikishi[input$banzuke_rows_selected[1]],
			"vs",
			banzuke_to_show()$rikishi[input$banzuke_rows_selected[2]]
		)
	})
	
	# TABLE: head-to-head history
	output$head_to_head = renderTable({
		req(results, input$banzuke_rows_selected)
		
		ids <- banzuke() %>% 
			filter(rikishi %in% banzuke_to_show()$rikishi[input$banzuke_rows_selected]) %>% 
			pull(id)
		
		
		if (length(ids) >= 2) results() %>% 
			filter(
				rikishi1_id == ids[1],
				rikishi2_id == ids[2]
			) %>% 
			transmute(
				basho,
				day,
				rikishi1_rank,
				rikishi1_shikona,
				rikishi1_result,
				rikishi1_win = recode(rikishi1_win + 1, " ", "x"),
				kimarite,
				rikishi2_win = recode(rikishi2_win + 1, " ", "x"),
				rikishi2_rank,
				rikishi2_shikona,
				rikishi2_result
			) %>% 
			arrange(-as.numeric(basho), -day) %>% 
			set_names(sub("rikishi[12]_", "", names(.)))
	}, na = "")
	
	# in case something needs to be checked
	output$debug = renderPrint({
		banzuke_to_show()$rikishi[input$banzuke_rows_selected[1]]
	})
}


ui <- fluidPage(
	tags$head(tags$style(HTML("body { font-size: 10px; }"))),
	
	fluidRow(
		column(
			6,
			# verbatimTextOutput("debug"),
			h1(textOutput("basho")),
			DT::dataTableOutput("banzuke")
		),
		column(
			6,
			fluidRow(
				column(
					6,
					h1(textOutput("shikona1")),
					tableOutput("rikishi1")
				),
				column(
					6,
					h1(textOutput("shikona2")),
					tableOutput("rikishi2")
				)
			),
			fluidRow(
				column(
					12,
					h1(textOutput("vs")),
					tableOutput("head_to_head")
				)
			)
		)
	)
)


shinyApp(ui, server)
