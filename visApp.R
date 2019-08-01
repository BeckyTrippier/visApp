#' This function to run an app for visualising and editing networks using VisNetwork. This updates the data and makes this downloadable for the user by clicking update nodes and edges, then after viewing these changes the user can download the csvs. data can also be edited in the tables in the viewer.
#' Becky Trippier 01/02/2019
#'
#'@param nodes nodes (id first col)
#'@param edges edges (from and to first 2 cols)
#'@return shiny app to develop network diagrams and save changes
#'@examples
#'@export

visApp <- function(nodes, edges){

  if(!"x" %in% names(nodes)){
    movemax <- 30 * nodes$id
    nodes<-tibble::add_column(nodes, x = 0, .after = "id")
    nodes$x <- round(stats::runif(length(nodes$id),-movemax, movemax))
  }else{
    names(nodes)[names(nodes) == 'x'] <- 'old.x'
    nodes<-tibble::add_column(nodes, x = nodes$old.x, .after = "id")
    nodes$old.x <- NULL
  }

  if(!"y" %in% names(nodes)){
    movemax <- 30 * nodes$id
    nodes <- tibble::add_column(nodes, y = 0, .after = "x")
    nodes$y <- round(stats::runif(length(nodes$id),-movemax, movemax))
  }else{
    names(nodes)[names(nodes) == 'y'] <- 'old.y'
    nodes<-tibble::add_column(nodes, y = nodes$old.y, .after = "x")
    nodes$old.y <- NULL
  }
  nodes<- nodes[,c("id", "x", "y", "label", "color", "group", "shape")]
  edges<- edges[,c("from", "to", "arrows", "color")]

  # define ui (the layout/appearance of the shiny app)
  ui <- shiny::fluidPage(
    # App title ----
    shiny::sidebarLayout(
      shinyjqui::jqui_resizabled(shiny::sidebarPanel (
        shiny::actionButton("getdata", "Get data"),
        shiny::actionButton("update_nodes", "Update nodes"),
        shiny::actionButton("update_edges", "Update edges"),
        shiny::downloadButton("downloadNode", "Download Nodes"),
        shiny::downloadButton("downloadEdge", "Download Edges"),
        shiny::actionButton("close", "close"),
        shiny::tabsetPanel(
          shiny::tabPanel("nodes",
                          rhandsontable::rHandsontableOutput("nodes_data_from_shiny")),
          shiny::tabPanel("edges",
                          rhandsontable::rHandsontableOutput("edges_data_from_shiny")),
          shiny::tabPanel("updated nodes",
                          DT::dataTableOutput("new_nodes")),
          shiny::tabPanel("updated edges",
                          DT::dataTableOutput("new_edges"))
        )
      )),
      shiny::mainPanel(
        visNetwork::visNetworkOutput("network_proxy", width = "100%",height = "1000px")
      ),
      fluid = TRUE, position = "left"
    )
  )
  # define server - (the instructions to build the app)

  server <- function(input, output, session) {

    output$network_proxy <- visNetwork::renderVisNetwork({
      visNetwork::visNetwork(nodes, edges) %>%
        visNetwork::visNodes(physics=FALSE) %>%
        visNetwork::visOptions(manipulation = TRUE)
    })

    new_node_data <- shiny::reactiveValues()
    new_edge_data <- shiny::reactiveValues()

    node_data <-  shiny::reactive ({
      if(!is.null(input$network_proxy_nodes)){
        newnode <- list()
        rightnode <- list()
        extranode <- list()


        for (i in 1:length(input$network_proxy_nodes)){

          if(length(input$network_proxy_nodes[[i]]) == ncol(nodes)){
            rightnode <- append(rightnode, input$network_proxy_nodes[[i]])
          } else if (length(input$network_proxy_nodes[[i]]) > ncol(nodes)){
            extra <- input$network_proxy_nodes[[i]]
            cols<- names(unlist(input$network_proxy_nodes[[i]], use.names = T))
            extra <- data.frame(matrix(unlist(extra), ncol=length(unlist(extra)), byrow=T, dimnames = list(1,names(unlist(extra)))),stringsAsFactors = FALSE)
            extranode <- append(extranode, list(extra))
          } else{
            newnode <- append(newnode, input$network_proxy_nodes[[i]])
          }
        }


        cat(paste(length(rightnode),"."))
        cat(paste(length(extranode), "."))
        cat(paste(length(newnode), "."))

        if (length(rightnode) != 0){
          info <- data.frame(matrix(unlist(rightnode), ncol = ncol(nodes)[1], byrow=T),stringsAsFactors = FALSE)
          colnames(info) <- colnames(nodes)
        }

        if (length(extranode) != 0){
          extrainfo <- plyr::rbind.fill(extranode)

          extrainfo$hiddenColor <- as.character(extrainfo$hiddenColor)
          extrainfo$color <- as.character(extrainfo$color)
          extrainfo$label <- as.character(extrainfo$label)
          extrainfo$hiddenLabel <- as.character(extrainfo$hiddenLabel)

          for (i in 1:nrow(extrainfo)){
            if (is.na(extrainfo$label[i])==TRUE){
              extrainfo$label[i] <- extrainfo$hiddenLabel[i]
            }else{
              extrainfo$label[i] <- extrainfo$label[i]
            }
            if (grepl("rgba", extrainfo$color[i])==TRUE){
              extrainfo$color[i] <- extrainfo$hiddenColor[i]
            } else {
              extrainfo$color[i] <- extrainfo$color[i]
            }
          }
          extrainfo <- extrainfo[,which(names(extrainfo) %in% names(nodes))]
          cat(names(extrainfo))
          cat(nrow(extrainfo))
        }


        if (length(newnode) != 0){
          newinfo <- data.frame(matrix(unlist(newnode),ncol = 4,byrow=T),stringsAsFactors=FALSE)
          colnames(newinfo) <- c("id", "x","y","label")
          start <- max(nodes$id)+1
          end <- start+(nrow(newinfo)-1)
          newinfo$id <- start:end
          newinfo$id <- as.character(newinfo$id)
          cat(names(newinfo))
          cat(nrow(newinfo))
        }


        if (length(extranode)==0 & length(newnode)==0 & length(rightnode)!=0){ #just right node lengths
          return(info)
        } else if (length(rightnode)==0 & length(newnode) ==0 & length(extranode)!= 0){ # just extra long node lengths
          return(extrainfo)
        } else if (length(rightnode) != 0 & length(extranode) != 0 & length(newnode) == 0) { #both, no new nodes
          combonodes <- rbind(info, extrainfo)
          return(combonodes)
        } else if (length(rightnode) != 0 & length(extranode) == 0 & length(newnode) != 0){ #norm length + new nodes
          combo <- merge(info, newinfo, by=c("id", "x","y","label"), all = TRUE)
          return(combo)
        } else if (length(rightnode == 0) & length(extranode) != 0 & length(newnode) != 0){ #long + new nodes
          combo <- merge(extrainfo, newinfo, by=c("id", "x","y","label"), all = TRUE)
          return(combo)
        } else{ #all present
          combonodes <- rbind(info, extrainfo)
          combo <- merge(combonodes, newinfo, by=c("id", "x","y","label"), all = TRUE)
          return(combo)
        }
      } else {
        return(nodes)
      }
    })


    edge_data <- shiny::reactive({
      if(!is.null(input$network_proxy_edges)){
        oldedges <- list()
        newedges <- list()

        for (i in 1:length(input$network_proxy_edges)){
          if(length(input$network_proxy_edges[[i]]) == ncol(edges)+1){
            oldedges <- append(oldedges, input$network_proxy_edges[[i]])
          } else{
            newedges <- append(newedges, input$network_proxy_edges[[i]])
          }
        }


        if (length(newedges)==0){
          edgo <- data.frame(matrix(unlist(oldedges), ncol = ncol(edges)[1]+1,byrow=T),stringsAsFactors=FALSE)
          edgename <- c(colnames(edges), "pos")
          colnames(edgo) <- edgename
          return(edgo)
        } else{
          edgo <- data.frame(matrix(unlist(oldedges), ncol = ncol(edges)[1]+1,byrow=T),stringsAsFactors=FALSE)
          edgename <- c(colnames(edges), "pos")
          colnames(edgo) <- edgename
          newlink <- data.frame(matrix(unlist(newedges),ncol = 3,byrow=T),stringsAsFactors=FALSE)
          colnames(newlink) <- c("from", "to","pos")
          combo <- merge(edgo, newlink, by=c("from", "to","pos"), all = TRUE)
          return(combo)
        }
        } else{
        return(edges)
      }
    })

    output$nodes_data_from_shiny <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(node_data())
    })

    output$edges_data_from_shiny <- rhandsontable::renderRHandsontable({
      rhandsontable::rhandsontable(edge_data())
    })
    output$new_nodes <- DT::renderDataTable({
      DT::datatable(new_node_data$data)
    })
    output$new_edges <- DT::renderDataTable({
      DT::datatable(new_edge_data$data)
    })

    observeEvent(input$update_nodes,{
      new_node_data$data <-  rhandsontable::hot_to_r(input$nodes_data_from_shiny)
      # visNetwork::visNetworkProxy("network_proxy") %>%
      # visNetwork::visUpdateNodes()
    })

    observeEvent(input$update_edges,{
      new_edge_data$data <-  rhandsontable::hot_to_r(input$edges_data_from_shiny)

    })


    observeEvent(input$getdata, {
      visNetwork::visNetworkProxy("network_proxy") %>%
        visNetwork::visGetEdges() %>%
        visNetwork::visGetNodes(addCoordinates = TRUE) %>%
        visNetwork::visNodes(physics = FALSE)
    })

    observeEvent(input$close, {
      if(ncol("new_node_data")==0){
        list2env(nodes=nodes,edges=edges)
        stopApp()
      }else{
        out <- list(nodes=new_node_data$data,edges=new_edge_data$data)
        list2env(out, envir = .GlobalEnv)
        stopApp()
      }
    })

    output$downloadNode <- shiny::downloadHandler(
      filename= function() {
        paste("nodes.csv", sep = "")
      },
      content = function(file) {
        write.csv(new_node_data$data, file, row.names = FALSE)
      }
    )

    output$downloadEdge <- shiny::downloadHandler(
      filename= function() {
        paste("edges.csv", sep = "")
      },
      content = function(file) {
        write.csv(new_edge_data$data, file, row.names = FALSE)

      }
    )

  }

  shiny::shinyApp(ui = ui, server = server)
}
