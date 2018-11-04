server <- function(input, output, session){
  curTree <- reactiveVal(NULL)

  code <- reactiveVal("library(sdcTable)")

  allNodeNames <- function() {
    curTree()$Get('name')
  }

  # moegliche referenzknoten
  poss_ref_nodes <- function() {
    levs <- allNodeNames()
    res <- sapply(levs, function(x) {
      !is.null(FindNode(curTree(), x)$children)
    })
    return(levs[res])
  }

  allChildren <- reactive({
    names(FindNode(curTree(), input$refnode_del)$children)
  })


  ## creation of the hierarchy
  observe({
    if (is.null(input$root_name)  || input$root_name=="") {
      shinyjs::hide("id_btn_create_root")
    } else {
      shinyjs::show("id_btn_create_root")
    }
  })

  ## root-node
  observeEvent(input$btn_create_root, {
    nn <- create_node(total_lab=input$root_name)
    curTree(nn)

    # update code
    cc <- paste0("dim <- create_node(total_lab=",shQuote(input$root_name),")")
    code(c(code(), cc))

    shinyjs::hide("create_root")
    shinyjs::show("select_action")

    updateSelectInput(session, inputId="refnode_add", choices=allNodeNames(), selected = "")
    updateSelectInput(session, inputId="refnode_del", choices=poss_ref_nodes(), selected = "")

    updateRadioButtons(session, inputId="action", selected=character(0))
    output$print_dim <- renderPrint({
      nn <- curTree()
      if (is.null(nn)) {
        return(NULL)
      }
      print(nn)
    })
  })

  observeEvent(input$action, {
    if (!is.null(curTree())) {
      if (input$action=="add") {
        shinyjs::hide("delete_nodes")
        shinyjs::hide("modify_nodes")
        shinyjs::show("add_nodes")
      }
      if (input$action=="delete") {
        shinyjs::hide("add_nodes")
        shinyjs::hide("modify_nodes")
        shinyjs::show("delete_nodes")
      }
      if (input$action=="modify") {
        shinyjs::hide("add_nodes")
        shinyjs::hide("delete_nodes")
        shinyjs::show("modify_nodes")
      }
    }
  })

  ## adding nodes
  observe({
    if (is.null(input$refnode_add) || input$refnode_add=="") {
      shinyjs::hide("row_childs")
      shinyjs::hide("id_btn_add_nodes")
      shinyjs::hide("row_nrchoices")
    } else {
      shinyjs::show("row_nrchoices")
      shinyjs::show("row_childs")
    }
  })

  # can button be shown?
  observe({
    req(input$nrchilds)
    res <- list(); length(res) <- input$nrchilds
    for (i in 1:input$nrchilds) {
      r <- input[[paste0("child_",i)]]
      if (is.null(r)) {
        res[[i]] <- ""
      } else {
        res[[i]] <- r
      }
    }

    res <- sapply(res, function(x) {
      x==""
    })

    if (sum(res)==0) {
      shinyjs::show("id_btn_add_nodes")
    } else {
      shinyjs::hide("id_btn_add_nodes")
    }
  })
  observeEvent(input$refnode_add, {
    for (i in 1:input$nrchilds) {
      updateTextInput(session, inputId=paste0("child_",i), value="")
    }
  })
  output$childs <- renderUI({
    req(input$nrchilds)
    lapply(1:input$nrchilds, function(x) {
      textInput(paste0("child_",x), paste("child",x), value="")
    })
  })
  observeEvent(input$btn_add_nodes, {
    for (i in 1:input$nrchilds) {
      curTree(add_nodes(curTree(), reference_node = input$refnode_add,
        node_labs = input[[paste0("child_",i)]]))

      cc <- paste0("dim <- add_nodes(dim, reference_node=",shQuote(input$refnode_add),", node_labs=",shQuote(input[[paste0("child_",i)]]),")")
      code(c(code(), cc))
    }
    updateSelectInput(session, inputId="refnode_add", choices=allNodeNames(), selected="")
    updateSelectInput(session, inputId="refnode_del", choices=poss_ref_nodes(), selected = "")

    output$print_dim <- renderPrint({
      nn <- curTree()
      if (is.null(nn)) {
        return(NULL)
      }
      print(nn)
    })

    #output$xx <- renderGrViz({
    #  grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(curTree())),engine = "dot")
    #})
  })

  ## deleting nodes
  # show/hide select input to select delete nodes
  observeEvent(input$refnode_del, {
    if(input$refnode_del!="") {
      shinyjs::show("row_node_to_del")
      updateSelectInput(session, inputId="nodes_to_del", choices=allChildren())
    } else {
      shinyjs::hide("row_node_to_del")
    }
  })

  # show/hide button
  observeEvent(input$nodes_to_del, {
    if (input$nodes_to_del != "") {
      shinyjs::show("id_btn_del_nodes")
    } else {
      shinyjs::hide("id_btn_del_nodes")
    }
  })

  observeEvent(input$btn_del_nodes, {
    curTree(delete_nodes(curTree(), reference_node = input$refnode_del,
      node_labs = input$nodes_to_del))
    cc <- paste0("dim <- delete_nodes(dim, reference_node=",shQuote(input$refnode_del),", node_labs=",shQuote(input$nodes_to_del),")")
    code(c(code(), cc))
    updateSelectInput(session, inputId="refnode_add", choices=allNodeNames(), selected="")
    updateSelectInput(session, inputId="refnode_del", choices=poss_ref_nodes(), selected = "")

    output$print_dim <- renderPrint({
      nn <- curTree()
      if (is.null(nn)) {
        return(NULL)
      }
      print(nn)
    })
  })



  ## code
  output$eval_code <- renderUI({
    #HTML(paste("<pre>",code(),"</pre>"))
    HTML(paste0("<pre>",paste(code(), collapse="<br />"),"</pre>"))
    #cat(code(), collapse=";\n")
  })




#   observeEvent({
#     list(input$Parent_name_remove,
#          input$add_child ,
#          input$remove_child)},{
#            if(!is.null(input[["Parent_name_remove"]])){
#              node_=FindNode(node=vv$org,name = input$Parent_name_remove)
#              children_names=node_$Get('name')
#              updateSelectInput(session,inputId ="Name_to_remove",choices =  children_names[children_names!=input$Parent_name_remove] )
#
#            }
#          })
  # observeEvent(input$remove_child,{
  #   if(input$Name_to_remove!=""){
  #     FindNode(node=vv$org,name = input$Parent_name_remove)$RemoveChild(input$Name_to_remove)
  #     vv$names=vv$org$Get('name')# get names of new tree
  #     #re-generate chart
  #     output$xx=renderGrViz({
  #       grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
  #     })
  #   }
  # })


  # observeEvent(input$add_child,{
  #   FindNode(node=vv$org,name = input$Parent_name)$AddChildNode(Node$new(input$new_node_name)) # add child
  #   vv$names=vv$org$Get('name')# get names of new tree
  #
  #   #re-generate chart
  #   output$xx=renderGrViz({
  #     grViz(DiagrammeR::generate_dot(ToDiagrammeRGraph(vv$org)),engine = "dot")
  #   })
  # })
}
