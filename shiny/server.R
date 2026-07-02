# server.R
server <- function(input, output, session) {
  init_map_server(input, output, session)       # 在 R/tab_map.R 中定义
  init_table1_server(input, output, session)    # 在 R/tab_table1.R 中定义
  init_table2_server(input, output, session)    # 在 R/tab_table2.R 中定义
  init_composition_server(input, output, session) 
}