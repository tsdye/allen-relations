allen.determine.relation <- function(mcmc.file, positions,
                                       mcmc.file.2=NULL, positions.2=NULL,
                                       out.file="", file.type="pdf",
                                       palette="grey", main="", sub="",
                                       six.value=FALSE, threshold=0.95)
{
  allen.load.libraries <- function()
  {
    if(!require(Xmisc))
    {
      install.packages("XMisc")
    }
    if(!require(igraph))
    {
      install.packages("igraph")
    }
    if(!require(ArchaeoPhases))
    {
      install.packages("ArchaeoPhases")
    }
    library(Xmisc)
    if(!is.package.loaded(igraph))
    {
      library(igraph)
    }
    if(!is.package.loaded(ArchaeoPhases))
    {
      library(ArchaeoPhases)
    }
  }
   if (six.value)
    {
      allen.six.value.graph <- function(shape="rectangle", size=52)
      {
        allen.load.libraries <- function()
        {
          if(!require(Xmisc))
          {
            install.packages("XMisc")
          }
          if(!require(igraph))
          {
            install.packages("igraph")
          }
          if(!require(ArchaeoPhases))
          {
            install.packages("ArchaeoPhases")
          }
          library(Xmisc)
          if(!is.package.loaded(igraph))
          {
            library(igraph)
          }
          if(!is.package.loaded(ArchaeoPhases))
          {
            library(ArchaeoPhases)
          }
        }
        allen.load.libraries()
        nodes <- c('precedes', 'overlaps',
                   'contains', 'during',
                   'overlapped-by', 'preceded-by')
        from <- c('precedes', 'overlaps', 'overlaps', 'contains',
                  'during', 'overlapped-by')
        to <- c('overlaps', 'contains', 'during', 'overlapped-by',
                'overlapped-by', 'preceded-by')
        x <- c(0, 0, 1, -1, 0, 0)
        y <- c(4, 3, 2, 2, 1, 0)
        node.df <- data.frame(nodes, x, y)
        edge.df <- data.frame(from, to)
        graph <- graph_from_data_frame(vertices=node.df, d=edge.df, directed=FALSE)
        V(graph)$shape <- shape
        V(graph)$size <- size
        graph
      }
      
      allen.coerce.six <- function(result.vector)
      {
        allen.six.value.set <- function()
        {
          ret <- c("p", "o", "D", "d", "O", "P");
          ret
        }
        allen.complement.set <- function(allen.set)
        {
          allen.basic.relation.set <- function()
          {
            ret <- c("p", "m", "o", "F", "s", "D", "e", "d", "S", "f", "O", "M", "P")
            ret
          }
          allen.full.set <- allen.basic.relation.set()
          switch(mode(allen.set),
                 "NULL" = result.set <- "",
                 "character" = result.set <- allen.set,
                 "numeric" = result.set <- names(allen.set[allen.set != 0]),
                 stop("unrecognized Allen set"))
          complement.set <- setdiff(allen.full.set, result.set)
          complement.set
        }
        allen.update.result <- function(result.vector, relation)
        {
          result.vector[relation] <- result.vector[relation] + 1
          result.vector
        }
        ret <- result.vector
        allen.six <- allen.six.value.set()
        allen.other <- allen.complement.set(allen.six)
        for (relation in allen.other)
        {
          neighbors <- switch(relation,
                              "m" = c("p","o"),
                              "F" = c("o", "D"),
                              "s" = c("o", "d"),
                              "e" = c("o", "O", "d", "D"),
                              "S" = c("D", "O"),
                              "f" = c("d", "O"),
                              "M" = c("O", "P"),
                              stop("unrecognized relation"))
          for (foo in seq_len(ret[relation]))
          {
            ret <- allen.update.result(ret, sample(neighbors, 1, replace=TRUE))
            ret[relation] <- ret[relation] - 1
          }
        }
        if (sum(ret[allen.other]) != 0)
          stop("coercion failed")
        else
          ret <- ret[allen.six]
        ret
      }
    }
  else
  {
    allen.basic.relations.graph <- function(shape="rectangle", size=52)
    {
      allen.load.libraries <- function()
      {
        if(!require(Xmisc))
        {
          install.packages("XMisc")
        }
        if(!require(igraph))
        {
          install.packages("igraph")
        }
        if(!require(ArchaeoPhases))
        {
          install.packages("ArchaeoPhases")
        }
        library(Xmisc)
        if(!is.package.loaded(igraph))
        {
          library(igraph)
        }
        if(!is.package.loaded(ArchaeoPhases))
        {
          library(ArchaeoPhases)
        }
      }
      nodes <- c('precedes', 'meets', 'overlaps', 'finished-by', 'starts',
                 'contains', 'equals', 'during', 'started-by', 'finishes',
                 'overlapped-by', 'met-by', 'preceded-by')
      from <- c('precedes', 'meets', 'overlaps', 'overlaps', 'finished-by',
                'finished-by', 'starts', 'starts', 'contains', 'equals', 'equals',
                'during', 'started-by', 'finishes', 'overlapped-by', 'met-by')
      to <- c('meets', 'overlaps', 'finished-by', 'starts', 'contains', 'equals',
              'equals', 'during', 'started-by', 'started-by', 'finishes',
              'finishes', 'overlapped-by', 'overlapped-by', 'met-by', 'preceded-by')
      x <- c(0, 0, 0, -1, 1, -2, 0, 2, -1, 1, 0, 0, 0)
      y <- c(8, 7, 6, 5, 5, 4, 4, 4, 3, 3, 2, 1, 0)
      node.df <- data.frame(nodes, x, y)
      edge.df <- data.frame(from, to)
      graph <- graph_from_data_frame(vertices=node.df, d=edge.df, directed=FALSE)
      V(graph)$shape <- shape
      V(graph)$size <- size
      graph
    }
    
  }
  allen.color.graph.nodes <- function(graph, results.vector, palette="grey")
  {
    allen.load.libraries <- function()
    {
      if(!require(Xmisc))
      {
        install.packages("XMisc")
      }
      if(!require(igraph))
      {
        install.packages("igraph")
      }
      if(!require(ArchaeoPhases))
      {
        install.packages("ArchaeoPhases")
      }
      library(Xmisc)
      if(!is.package.loaded(igraph))
      {
        library(igraph)
      }
      if(!is.package.loaded(ArchaeoPhases))
      {
        library(ArchaeoPhases)
      }
    }
    allen.proportion.results <- function(result.vector)
    {
      res <- result.vector / sum(result.vector)
      res
    }
    colors <- switch(palette,
                     "blue" = rev(c("#084594", "#2171B5", "#4292C6", "#6BAED6",
                                    "#9ECAE1", "#C6DBEF", "#DEEBF7", "#F7FBFF")),
                     "red" = rev(c("#99000D", "#CB181D", "#EF3B2C", "#FB6A4A",
                                   "#FC9272", "#FCBBA1", "#FEE0D2", "#FFF5F0")),
                     "green" = rev(c("#005A32", "#238B45", "#41AB5D", "#74C476",
                                     "#A1D99B", "#C7E9C0", "#E5F5E0", "#F7FCF5")),
                     rev(c("#000000","#252525", "#525252", "#737373",
                           "#969696", "#BDBDBD", "#D9D9D9", "#F0F0F0")))
    proportioned.results <- allen.proportion.results(results.vector^(1/3))
    color.cut <- colors[cut(proportioned.results, breaks = 5, include.lowest = TRUE)]
    label.cut <- ifelse(color.cut %in% colors[6],"white","black")
    V(graph)$color <- color.cut
    V(graph)$label.color <- label.cut
    V(graph)$frame.color <- NA
    graph
  }
  allen.save.graph <- function(graph, file.name, file.type="pdf",
                               main="", sub="")
  {
    allen.load.libraries <- function()
    {
      if(!require(Xmisc))
      {
        install.packages("XMisc")
      }
      if(!require(igraph))
      {
        install.packages("igraph")
      }
      if(!require(ArchaeoPhases))
      {
        install.packages("ArchaeoPhases")
      }
      library(Xmisc)
      if(!is.package.loaded(igraph))
      {
        library(igraph)
      }
      if(!is.package.loaded(ArchaeoPhases))
      {
        library(ArchaeoPhases)
      }
    }
    plot(graph, main=main, sub=sub)
    if(file_test("-f", file.name))
        stop("file exists, specify a different file name")
    else
    {
      switch(file.type,
             "pdf" = pdf(file=file.name),
             "png" = png(file=file.name),
             "jpg" =, "jpeg" = jpeg(file=file.name),
             "postscript" =, "ps" =, "eps" = postscript(file=file.name),
             "svg" = svg(file=file.name),
             "tiff" =, "tif" = tiff(file=file.name),
             "bmp" = bmp(file=file.name),
             "pictex" = pictex(file=file.name),
             "xfig" = xfig(file=file.name),
             "bitmap" =, "bmp" = bitmap(file=file.name),
             stop("unsupported file type"))
      plot(graph, main=main, sub=sub)
      dev.off()
    }
  }
  
  allen.read.mcmc.chains <- function(mcmc.file, positions,
                                     mcmc.file.2=NULL, positions.2=NULL,
                                     iteration.column = 1)
  {
    if(missing(mcmc.file) || missing(positions))
      stop("please set required parameters")
    if(is.null(mcmc.file.2) && !is.null(positions.2))
      stop("missing input file")
    if(is.null(mcmc.file.2) && length(positions) != 4)
      stop("four interval boundaries are required")
    if(!is.null(mcmc.file.2) && is.null(positions.2))
      stop("interval boundaries are required")
    allen.load.libraries <- function()
    {
      if(!require(Xmisc))
      {
        install.packages("XMisc")
      }
      if(!require(igraph))
      {
        install.packages("igraph")
      }
      if(!require(ArchaeoPhases))
      {
        install.packages("ArchaeoPhases")
      }
      library(Xmisc)
      if(!is.package.loaded(igraph))
      {
        library(igraph)
      }
      if(!is.package.loaded(ArchaeoPhases))
      {
        library(ArchaeoPhases)
      }
    }
    mcmc.1 <- ImportCSV(mcmc.file, iterationColumn = iteration.column)
    start.1 <- as.vector(mcmc.1[,positions[[1]]])
    end.1 <- as.vector(mcmc.1[,positions[[2]]])
    if(is.null(mcmc.file.2))
    {
      start.2 <- as.vector(mcmc.1[,positions[[3]]])
      end.2 <- as.vector(mcmc.1[,positions[[4]]])
    }
    else
      {
        mcmc.2 <- ImportCSV(mcmc.file.2, iterationColumn = iteration.column)
        start.2 <- as.vector(mcmc.2[,positions.2[[1]]])
        end.2 <- as.vector(mcmc.2[,positions.2[[2]]])
      }
    result <- list(start.1, end.1, start.2, end.2)
    names(result) <- c("start.1", "end.1", "start.2", "end.2")
    result
  }
  allen.create.result.vector <- function(initial.value=0)
  {
    allen.basic.relation.set <- function()
    {
      ret <- c("p", "m", "o", "F", "s", "D", "e", "d", "S", "f", "O", "M", "P")
      ret
    }
    result.vector <- rep(initial.value, times=13)
    names(result.vector) <- allen.basic.relation.set()
    result.vector
  }
  allen.relations.set <- function(allen.set)
  {
    switch(mode(allen.set),
           "character" = result.set <- allen.set,
           "numeric" = result.set <- names(allen.set[allen.set != 0]),
           stop("unrecognized Allen set"))
    temp <- c()
    for(x in result.set)
      temp <- paste(temp, x, sep="")
    result.set <- paste("(",temp,")", sep="")
    result.set
  }
  allen.relations.concur <- function(allen.set)
  {
    allen.concurrent.relation.set <- function()
    {
      ret <- c("o", "F", "D", "s", "e", "S", "d", "f", "O")
      ret
    }
    allen.relations.set <- function(allen.set)
    {
      switch(mode(allen.set),
             "character" = result.set <- allen.set,
             "numeric" = result.set <- names(allen.set[allen.set != 0]),
             stop("unrecognized Allen set"))
      temp <- c()
      for(x in result.set)
        temp <- paste(temp, x, sep="")
      result.set <- paste("(",temp,")", sep="")
      result.set
    }
    concurrence.set = allen.concurrent.relation.set()
    switch(mode(allen.set),
           "character" = result.set <- allen.set,
           "numeric" = result.set <- names(allen.set[allen.set != 0]),
           stop("unrecognized Allen set"))
    if(setequal(union(result.set, concurrence.set), concurrence.set))
      ret <- paste("Concurrent:",allen.relations.set(result.set),
                   "is contained in", allen.relations.set(concurrence.set))
    else
      ret <- paste("Not concurrent:",allen.relations.set(result.set),
                   "is not contained in", allen.relations.set(concurrence.set))
    ret
  }
  allen.concurrent.relation.set <- function()
  {
    ret <- c("o", "F", "D", "s", "e", "S", "d", "f", "O")
    ret
  }
  allen.proportion.results <- function(result.vector)
  {
    res <- result.vector / sum(result.vector)
    res
  }
  allen.relation <- function(start.1, end.1, start.2, end.2)
  {
    if((end.1 < start.1) || (end.2 < start.2))
      stop("beta is older than alpha")
    else
    {
      result <- if(start.1 < start.2)
                {
                  if(end.1 < start.2) "p"
                  else
                    if(end.1 == start.2) "m"
                  else
                    if(end.1 < end.2) "o"
                  else
                    if(end.1 == end.2) "F"
                  else
                    "D"
                }
                else
                  if(start.1 == start.2)
                  {
                    if(end.1 > end.2) "s"
                    else
                      if(end.1 == end.2) "e"
                    else
                      "S"
                  }
                else
                  if(start.1 > start.2)
                  {
                    if(start.1 < end.2)
                    {
                      if(end.1 < end.2) "d"
                      else
                        if(end.1 == end.2) "f"
                      else
                        "O"
                    }
                    else
                      if(start.1 == end.2) "M"
                    else
                      "P"
                  }
      result
    }
  }
  allen.calculate.relations <- function(result.vector, mcmc.chain.list)
  {
    allen.relation <- function(start.1, end.1, start.2, end.2)
    {
      if((end.1 < start.1) || (end.2 < start.2))
        stop("beta is older than alpha")
      else
      {
        result <- if(start.1 < start.2)
                  {
                    if(end.1 < start.2) "p"
                    else
                      if(end.1 == start.2) "m"
                    else
                      if(end.1 < end.2) "o"
                    else
                      if(end.1 == end.2) "F"
                    else
                      "D"
                  }
                  else
                    if(start.1 == start.2)
                    {
                      if(end.1 > end.2) "s"
                      else
                        if(end.1 == end.2) "e"
                      else
                        "S"
                    }
                  else
                    if(start.1 > start.2)
                    {
                      if(start.1 < end.2)
                      {
                        if(end.1 < end.2) "d"
                        else
                          if(end.1 == end.2) "f"
                        else
                          "O"
                      }
                      else
                        if(start.1 == end.2) "M"
                      else
                        "P"
                    }
        result
      }
    }
    allen.update.result <- function(result.vector, relation)
    {
      result.vector[relation] <- result.vector[relation] + 1
      result.vector
    }
    min.length <- (min(length(mcmc.chain.list[["start.1"]]),
    (length(mcmc.chain.list[["start.2"]]))))
    for(x in seq_len(min.length))
    {
    relation <- allen.relation(mcmc.chain.list[["start.1"]][x],
                               mcmc.chain.list[["end.1"]][x],
                               mcmc.chain.list[["start.2"]][x],
                               mcmc.chain.list[["end.2"]][x])
    result.vector <- allen.update.result(result.vector, relation)
    }
    result.vector
  }
  
  allen.composition <- function(allen.set.1, allen.set.2)
  {
    if(is.null(allen.set.1) || is.null(allen.set.2))
      ret <- NULL
    else
      {
        allen.relations.union <- function(allen.set.1, allen.set.2)
        {
          switch(mode(allen.set.1),
                 NULL = result.set.1 <- NULL,
                 "character" = result.set.1 <- allen.set.1,
                 "numeric" = result.set.1 <- names(allen.set.1[allen.set.1 != 0]),
                 stop("unrecognized Allen set"))
          switch(mode(allen.set.2),
                 NULL = result.set.2 <- NULL,
                 "character" = result.set.2 <- allen.set.2,
                 "numeric" = result.set.2 <- names(allen.set.2[allen.set.2 != 0]),
                 stop("unrecognized Allen set"))
          ret <- union(result.set.1, result.set.2)
          ret
        }
        
      composition.lookup.table <- function()
      {
        full <- "pmoFDseSdfOMP"
        concur <- "oFDseSdfO"
        names.vector <- c("p", "m", "o", "F", "D", "s", "e", "S", "d", "f", "O", "M", "P")
      
        temp.data <- c("p", "p", "p", "p", "p", "p", "p", "p", "pmosd", "pmosd", "pmosd", "pmosd", full,
                       "p", "p", "p", "p", "p", "m", "m", "m", "osd", "osd", "osd", "Fef", "DSOMP",
                       "p", "p", "pmo", "pmo", "pmoFD", "o", "o", "oFD", "osd", "osd", concur, "DSO", "DSOMP",
                       "p", "m", "o", "F", "D", "o", "F", "D", "osd", "Fef", "DSO", "DSO", "DSOMP",
                       "pmoFD", "oFD", "oFD", "D", "D", "oFD", "D", "D", concur, "DSO", "DSO", "DSO", "DSOMP",
                       "p", "p", "pmo", "pmo", "pmoFD", "s", "s", "seS", "d", "d", "dfO", "M", "P",
                       "p", "m", "o", "F", "D", "s", "e", "S", "d", "f", "O", "M", "P",
                       "pmoFD", "oFD", "oFD", "D", "D", "seS", "S", "S", "dfO", "O", "O", "M", "P",
                       "p", "p", "pmosd", "pmosd", full, "d", "d", "dfOMP", "d", "d", "dfOMP", "P", "P",
                       "p", "m", "osd", "Fef", "DSOMP", "d", "f", "OMP", "d", "f", "OMP", "P", "P",
                       "pmoFD", "oFD", concur, "DSO", "DSOMP", "dfO", "O", "OMP", "dfO", "O", "OMP", "P", "P",
                       "pmoFD", "seS", "dfO", "M", "P", "dfO", "M", "P", "dfO", "M", "P", "P", "P",
                       full, "dfOMP", "dfOMP", "P", "P", "dfOMP", "P", "P", "dfOMP", "P", "P", "P", "P")
      
        lookup.table <- matrix(temp.data, 13, 13, byrow=TRUE)
        colnames(lookup.table) <- names.vector
        rownames(lookup.table) <- names.vector
        lookup.table
      }
      allen.set.vector <- function(allen.set.string)
      {
        ret <- unlist(strsplit(allen.set.string, ""))
        ret
      }
      lookup.table <- composition.lookup.table()
        relation.pairs <- expand.grid(allen.set.1, allen.set.2,
                                      KEEP.OUT.ATTRS=FALSE,
                                      stringsAsFactors=FALSE)
        ret <- c()
        for(pair in seq_len(dim(relation.pairs)[1]))
        {
          relation <- lookup.table[relation.pairs[pair,1], relation.pairs[pair,2]]
          ret <- allen.relations.union(ret, allen.set.vector(relation))
        }
      }
    ret
  }
  allen.set.vector.from.result <- function(result.vector)
  {
    ret <- names(result.vector[result.vector != 0])
    ret
  }
  allen.relations.intersection <- function(allen.set.1, allen.set.2)
  {
    switch(mode(allen.set.1),
           NULL = result.set.1 <- NULL,
           "character" = result.set.1 <- allen.set.1,
           "numeric" = result.set.1 <- names(allen.set.1[allen.set.1 != 0]),
           stop("unrecognized Allen set"))
    switch(mode(allen.set.2),
           NULL = result.set.2 <- NULL,
           "character" = result.set.2 <- allen.set.2,
           "numeric" = result.set.2 <- names(allen.set.2[allen.set.2 != 0]),
           stop("unrecognized Allen set"))
    ret <- intersect(result.set.1, result.set.2)
    ret
  }
  allen.uncertain.inference <- function(result.vector, threshold=0.95)
  {
  
    temp <- c()
    proportion.vector <- allen.proportion.results(result.vector[result.vector != 0])
    proportion.vector <- sort(proportion.vector, decreasing=TRUE)
    for(relation in seq_along(proportion.vector))
    {
      if(sum(temp) < threshold)
      {
        temp <- c(temp, proportion.vector[relation])
      }
    }
    ret <- names(temp)
    ret
  }
  allen.load.libraries()
  result.vector <- allen.create.result.vector()
  mcmc.chains <- allen.read.mcmc.chains(mcmc.file, positions,
                                        mcmc.file.2, positions.2)
  result.vector <- allen.calculate.relations(result.vector, mcmc.chains)
  if (six.value)
  {
    graph <- allen.six.value.graph()
    result.vector <- allen.coerce.six(result.vector)
    allen.six.value <- c("p", "o", "d", "D", "O", "P")
  }
  else
    graph <- allen.basic.relations.graph()
  graph <- allen.color.graph.nodes(graph=graph,
                                   results.vector=result.vector,
                                   palette=color)
  allen.save.graph(graph=graph, file.name=out.file,
                   file.type=file.type, main=main, sub=sub)
  allen.set <- allen.relations.set(result.vector)
  allen.concurs <- allen.relations.concur(result.vector)
  allen.result <- result.vector
  allen.set <- allen.set.vector.from.result(result.vector)
  allen.proportion <- allen.proportion.results(result.vector[result.vector != 0])
  allen.proportion <- sort(allen.proportion, decreasing=TRUE)
  concurrence.intersection <- allen.relations.intersection(allen.proportion, allen.concurrent.relation.set())
  allen.proportion.concurs <- sum(allen.proportion[concurrence.intersection])
  allen.uncertain.set <- allen.uncertain.inference(result.vector, threshold=threshold)
  allen.infer.1.to.precedes.2 <- allen.composition(allen.set, c("P"))
  allen.infer.1.to.meets.2 <- allen.composition(allen.set, c("M"))
  allen.infer.1.to.preceded.by.2 <- allen.composition(allen.set, c("p"))
  allen.infer.1.to.met.by.2 <- allen.composition(allen.set, c("m"))
  allen.infer.1.to.precedes.2.uncertain <- allen.composition(allen.uncertain.set, c("P"))
  allen.infer.1.to.meets.2.uncertain <- allen.composition(allen.uncertain.set, c("M"))
  allen.infer.1.to.preceded.by.2.uncertain <- allen.composition(allen.uncertain.set, c("p"))
  allen.infer.1.to.met.by.2.uncertain <- allen.composition(allen.uncertain.set, c("m"))
  ret <- list(allen.result = allen.result,
              allen.set = allen.set,
              allen.uncertain.set = allen.uncertain.set,
              allen.concurrence = allen.concurs,
              allen.proportion = round(allen.proportion, digits=4),
              allen.proportion.concurs = round(allen.proportion.concurs, digits=3),
              allen.1.to.precedes.2 = if(six.value)
                                      {
                                        allen.relations.intersection(
                                          allen.infer.1.to.precedes.2,
                                          allen.six.value)
                                      }
                                      else
                                      {allen.infer.1.to.precedes.2},
              allen.1.to.meets.2 = if(six.value)
                                   {
                                     allen.relations.intersection(
                                       allen.infer.1.to.meets.2,
                                       allen.six.value)
                                   }
                                   else
                                   {allen.infer.1.to.meets.2},
              allen.1.to.preceded.by.2 = if(six.value)
                                         {
                                           allen.relations.intersection(
                                             allen.infer.1.to.preceded.by.2,
                                             allen.six.value)
                                         }
                                         else
                                         {allen.infer.1.to.preceded.by.2},
              allen.1.to.met.by.2 = if(six.value)
                                    {
                                      allen.relations.intersection(
                                        allen.infer.1.to.met.by.2,
                                        allen.six.value)
                                    }
                                    else
                                    {allen.infer.1.to.met.by.2},
              allen.1.to.precedes.2.uncertain = if(six.value)
                                                {
                                                  allen.relations.intersection(
                                                    allen.infer.1.to.precedes.2.uncertain,
                                                    allen.six.value)
                                                }
                                                else
                                                {allen.infer.1.to.precedes.2.uncertain},
              allen.1.to.meets.2.uncertain = if(six.value)
                                   {
                                     allen.relations.intersection(
                                       allen.infer.1.to.meets.2.uncertain,
                                       allen.six.value)
                                   }
                                   else
                                   {allen.infer.1.to.meets.2.uncertain},
              allen.1.to.preceded.by.2.uncertain = if(six.value)
                                         {
                                           allen.relations.intersection(
                                             allen.infer.1.to.preceded.by.2.uncertain,
                                             allen.six.value)
                                         }
                                         else
                                         {allen.infer.1.to.preceded.by.2.uncertain},
              allen.1.to.met.by.2.uncertain = if(six.value)
                                    {
                                      allen.relations.intersection(
                                        allen.infer.1.to.met.by.2.uncertain,
                                        allen.six.value)
                                    }
                                    else
                                    {allen.infer.1.to.met.by.2.uncertain})
  ret
}
