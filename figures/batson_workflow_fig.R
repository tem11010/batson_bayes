library(DiagrammeR)

# string as graphviz input

flow <- "

digraph flowchart {
  rankdir = LR
  node [fontname = arial, shape = rectangle]
  tab1 [label = '@@1', color = red]
  tab2 [label = '@@2', shape = ellipse]
  tab3 [label = '@@3', color = red]

  tab1 -> tab2 -> tab3;
  
}
  
  [1]: 'prima facie case'
  [2]: 'proffered reasons'    
  [3]: 'prove bias'   
  "

# export graph

save_path <- here::here("figures","batson_workflow_fig.png")

library(DiagrammeRsvg)
library(rsvg)
library(htmltools)

grViz(flow) %>% export_svg %>% 
  charToRaw %>% # convert SVG string into raw data
  rsvg_png(file = save_path)
