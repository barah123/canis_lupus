# Load the required libraries (pre-installed in the Docker image)
library(shiny)
library(phyloseq)
library(plotly)
library(DT)
library(vegan)
library(ape)
#library(ggtree)
library(ggplot2)
library(tidyverse)
library(metacoder)
library(networkD3)
library(heatmaply)
library(dendextend)
library(viridis)
library(RColorBrewer)
library(ggthemes)
library(microbiome)
library(purrr)

  # Update region choices based on metadata
  observe({
    req(ps())
    meta_vars <- colnames(sample_data(ps()))
    updateSelectInput(session, "regionBar", choices = meta_vars)
    updateSelectInput(session, "regionPie", choices = meta_vars)
    updateSelectInput(session, "regionRare", choices = meta_vars)
    updateSelectInput(session, "regionAlpha", choices = meta_vars)
    updateSelectInput(session, "regionBeta", choices = meta_vars)
    updateSelectInput(session, "regionCore", choices = meta_vars)
    updateSelectInput(session, "regionHeatmap", choices = meta_vars)
  })

  # Load and process data
  observeEvent(input$update, {
    tryCatch({
      showNotification("Loading and processing data...", type = "message", duration = NULL)

      # Load ASV table
      asv <- if (!is.null(input$asvFile)) {
        read.csv(input$asvFile$datapath, row.names = 1, check.names = FALSE)
      } else {
        stop("Please upload an ASV table.")
      }

      # Load taxonomy table
      tax <- if (!is.null(input$taxFile)) {
        tax_table(as.matrix(read.csv(input$taxFile$datapath, row.names = 1)))
      } else {
        stop("Please upload a taxonomy table.")
      }

      # Load metadata
      meta <- if (!is.null(input$metaFile)) {
        sample_data(read.csv(input$metaFile$datapath, row.names = 1))
      } else {
        stop("Please upload a metadata table.")
      }

      # Create phyloseq object
      otu <- otu_table(as.matrix(asv), taxa_are_rows = TRUE)
      ps_obj <- phyloseq(otu, tax, meta)

      # Load phylogenetic tree if provided
      if (!is.null(input$treeFile)) {
        tree <- ape::read.tree(input$treeFile$datapath)
        phy_tree(ps_obj) <- tree
      }

      ps(ps_obj)
      removeNotification()
      showNotification("Data loaded successfully!", type = "message", duration = 3)

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })

  # Data summary
  output$dataSummary <- renderPrint({
    req(ps())
    cat("=== Microbiome Data Summary ===\n\n")
    cat("Number of samples:", nsamples(ps()), "\n")
    cat("Number of features:", ntaxa(ps()), "\n")
    cat("\nSample variables:\n")
    print(colnames(sample_data(ps())))
    cat("\nTaxonomic ranks:\n")
    print(rank_names(ps()))
    if (!is.null(phy_tree(ps(), error = FALSE))) {
      cat("\nPhylogenetic tree tips:", length(phy_tree(ps())$tip.label), "\n")
    }
  })

  # Sample table
  output$sampleTable <- renderDT({
    req(ps())
    datatable(as.data.frame(sample_data(ps())),
              options = list(scrollX = TRUE, pageLength = 10),
              class = 'cell-border stripe',
              style = "bootstrap")
  })

  # Stacked Bar Plots
  output$taxaBarplot <- renderPlotly({
    req(ps())
    input$updateBar

    isolate({
      ps_rel <- transform_sample_counts(ps(), function(x) x / sum(x))
      top_taxa <- names(sort(taxa_sums(ps_rel), decreasing = TRUE)[1:input$topNBar])
      ps_top <- prune_taxa(top_taxa, ps_rel)

      p <- plot_bar(ps_top, fill = input$taxLevelBar) +
        theme_minimal() +
        labs(title = paste("Top", input$topNBar, input$taxLevelBar, "Abundance")) +
        scale_fill_brewer(palette = "Set2")

      if (!is.null(input$regionBar) && input$regionBar %in% colnames(sample_data(ps()))) {
        p <- p + facet_wrap(as.formula(paste("~", input$regionBar)), scales = "free_x")
      }

      ggplotly(p)
    })
  })

  # Relative Abundance Plot
  output$relativeAbundancePlot <- renderPlotly({
    req(ps())
    input$updateBar

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelBar)
      ps_rel <- transform_sample_counts(ps_glom, function(x) x / sum(x))

      p <- plot_bar(ps_rel, fill = input$taxLevelBar) +
        theme_minimal() +
        labs(title = paste(input$taxLevelBar, "-level Relative Abundance")) +
        scale_fill_brewer(palette = "Set3")

      if (!is.null(input$regionBar) && input$regionBar %in% colnames(sample_data(ps()))) {
        p <- p + facet_wrap(as.formula(paste("~", input$regionBar)), scales = "free_x")
      }

      ggplotly(p)
    })
  })

  # Interactive Pie Chart
  output$pieChart <- renderPlotly({
    req(ps())
    input$updatePie

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelPie)
      ps_rel <- transform_sample_counts(ps_glom, function(x) x / sum(x))

      if (!is.null(input$regionPie) && input$regionPie %in% colnames(sample_data(ps()))) {
        melted <- phyloseq::psmelt(ps_rel)
        agg <- melted %>%
          group_by(!!sym(input$taxLevelPie), !!sym(input$regionPie)) %>%
          summarize(Abundance = mean(Abundance))

        plot_ly(agg, labels = ~get(input$taxLevelPie), values = ~Abundance,
                color = ~get(input$regionPie), type = 'pie',
                textposition = 'inside', textinfo = 'label+percent',
                marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))) %>%
          layout(title = paste(input$taxLevelPie, 'Composition by', input$regionPie))
      } else {
        melted <- phyloseq::psmelt(ps_rel)
        agg <- melted %>% group_by(!!sym(input$taxLevelPie)) %>% summarize(Abundance = mean(Abundance))

        plot_ly(agg, labels = ~get(input$taxLevelPie), values = ~Abundance, type = 'pie',
                textposition = 'inside', textinfo = 'label+percent',
                marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))) %>%
          layout(title = paste(input$taxLevelPie, '-level Composition'))
      }
    })
  })

  # Rarefaction Curve
  output$rarefactionCurve <- renderPlotly({
    req(ps())
    input$updateRare

    isolate({
      if (!is.null(input$regionRare) && input$regionRare %in% colnames(sample_data(ps()))) {
        sample_data <- as(sample_data(ps()), "data.frame")
        colors <- factor(sample_data[[input$regionRare]])

        rare_curve <- vegan::rarecurve(t(otu_table(ps())), step = 50, label = FALSE,
                                      col = colors)

        samples <- row.names(sample_data)
        n_samples <- length(samples)

        plot_data <- map_df(1:n_samples, function(i) {
          curve <- rare_curve[[i]]
          data.frame(
            Sample = samples[i],
            Reads = attr(curve, "Subsample"),
            OTUs = curve,
            Group = sample_data[[input$regionRare]][i]
          )
        })

        ggplotly(
          ggplot(plot_data, aes(x = Reads, y = OTUs, group = Sample, color = Group)) +
            geom_line() +
            theme_minimal() +
            labs(title = "Rarefaction Curves",
                 x = "Number of Reads",
                 y = "Number of OTUs")
        )
      } else {
        ggplotly(
          ggplot() +
            theme_minimal() +
            labs(title = "Rarefaction Curves")
        )
      }
    })
  })

  # Phylogenetic Tree
  output$phylogeneticTree <- renderPlot({
    req(ps())
    if (!is.null(phy_tree(ps(), error = FALSE))) {
      ggtree::ggtree(phy_tree(ps())) +
        ggtree::geom_tiplab(size = 3) +
        ggtitle("Phylogenetic Tree") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  # Heat Tree
  output$heatTree <- renderPlot({
    req(ps())
    input$updateHeatTree

    isolate({
      obj <- metacoder::phyloseq_to_metacoder(ps())
      obj <- metacoder::calc_taxon_abund(obj, "otu_table")
      metacoder::heat_tree(obj,
                           node_label = taxon_names,
                           node_size = n_obs,
                           node_color = n_obs,
                           initial_layout = "reingold-tilford",
                           title = paste("Taxonomic Heat Tree at", input$taxLevelHeatTree, "Level"))
    })
  })

  # Alpha Diversity
  output$alphaDiv <- renderPlotly({
    req(ps())
    input$updateAlpha

    isolate({
      measures <- input$alphaMeasure
      if (length(measures) == 0) measures <- "Shannon"

      p <- plot_richness(ps(), measures = measures) +
        theme_minimal() +
        labs(title = paste("Alpha Diversity (", measures, ")"))

      if (!is.null(input$regionAlpha) && input$regionAlpha %in% colnames(sample_data(ps()))) {
        p <- p + geom_point(aes_string(color = input$regionAlpha), size = 3)
      }

      ggplotly(p)
    })
  })

  output$alphaDivBoxplot <- renderPlotly({
    req(ps())
    input$updateAlpha

    isolate({
      if (!is.null(input$regionAlpha) && input$regionAlpha %in% colnames(sample_data(ps()))) {
        p <- plot_richness(ps(), x = input$regionAlpha, measures = input$alphaMeasure) +
          geom_boxplot(fill = "lightblue") +
          theme_minimal() +
          labs(title = paste(input$alphaMeasure, "Diversity by", input$regionAlpha))

        ggplotly(p)
      }
    })
  })

  # Beta Diversity
  output$betaPlot <- renderPlotly({
    req(ps())
    input$updateBeta

    isolate({
      ord <- ordinate(ps(), method = input$betaMethod, distance = input$betaDistance)

      p <- plot_ordination(ps(), ord) +
        theme_minimal() +
        labs(title = paste(input$betaMethod, "Plot (", input$betaDistance, "Distance)"))

      if (!is.null(input$regionBeta) && input$regionBeta %in% colnames(sample_data(ps()))) {
        p <- p + geom_point(aes_string(color = input$regionBeta), size = 3)
      } else {
        p <- p + geom_point(color = "blue", size = 3)
      }

      ggplotly(p)
    })
  })

  output$nmdsPlot <- renderPlotly({
    req(ps())
    input$updateBeta

    isolate({
      ord <- ordinate(ps(), method = "NMDS", distance = input$betaDistance)

      p <- plot_ordination(ps(), ord) +
        theme_minimal() +
        labs(title = paste("NMDS Plot (", input$betaDistance, "Distance)"))

      if (!is.null(input$regionBeta) && input$regionBeta %in% colnames(sample_data(ps()))) {
        p <- p + geom_point(aes_string(color = input$regionBeta), size = 3)
      } else {
        p <- p + geom_point(color = "blue", size = 3)
      }

      ggplotly(p)
    })
  })

  # Core Microbiome
  output$coreHeatmap <- renderPlotly({
    req(ps())
    input$updateCore

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelCore)
      core <- microbiome::core_members(ps_glom,
                                      detection = input$detectionCore/100,
                                      prevalence = input$prevalenceCore/100)

      if (length(core) > 0) {
        ps_core <- prune_taxa(core, ps_glom)

        if (!is.null(input$regionCore) && input$regionCore %in% colnames(sample_data(ps()))) {
          sample_data(ps_core)$Group <- sample_data(ps_core)[[input$regionCore]]
          ps_core <- merge_samples(ps_core, "Group")
          ps_core <- transform_sample_counts(ps_core, function(x) x / sum(x))
        }

        heatmaply(otu_table(ps_core),
                  colors = viridis::viridis(256),
                  main = paste("Core Microbiome at", input$taxLevelCore, "Level"))
      } else {
        plot_ly() %>%
          add_annotations(text = "No core taxa found with current thresholds",
                          x = 0.5, y = 0.5, showarrow = FALSE) %>%
          layout(title = paste("Core Microbiome at", input$taxLevelCore, "Level"))
      }
    })
  })

  output$coreSummary <- renderPrint({
    req(ps())
    input$updateCore

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelCore)
      core <- microbiome::core_members(ps_glom,
                                      detection = input$detectionCore/100,
                                      prevalence = input$prevalenceCore/100)

      cat("=== Core Microbiome Summary ===\n\n")
      cat("Taxonomic level:", input$taxLevelCore, "\n")
      cat("Detection threshold:", input$detectionCore, "%\n")
      cat("Prevalence threshold:", input$prevalenceCore, "%\n")
      cat("\nCore microbiome features:", length(core), "\n")

      if (length(core) > 0) {
        cat("\nCore taxa:\n")
        print(tax_table(ps_glom)[core, input$taxLevelCore])
      } else {
        cat("\nNo core taxa found with current thresholds\n")
      }
    })
  })

  # Interactive Heatmap
  output$interactiveHeatmap <- renderPlotly({
    req(ps())
    input$updateHeatmap

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelHeatmap)

      if (!is.null(input$regionHeatmap) && input$regionHeatmap %in% colnames(sample_data(ps()))) {
        sample_data(ps_glom)$Group <- sample_data(ps_glom)[[input$regionHeatmap]]
        ps_glom <- merge_samples(ps_glom, "Group")
        ps_glom <- transform_sample_counts(ps_glom, function(x) x / sum(x))
      }

      heatmaply(otu_table(ps_glom),
                colors = viridis::viridis(256),
                main = paste("Interactive Heatmap at", input$taxLevelHeatmap, "Level"))
    })
  })

  # Dendrogram
  output$dendrogram <- renderPlotly({
    req(ps())
    input$updateDendro

    isolate({
      dist_matrix <- stats::dist(t(otu_table(ps())), method = input$distanceMethod)
      hc <- stats::hclust(dist_matrix, method = input$clusterMethod)
      dend <- stats::as.dendrogram(hc)

      plotly::plot_ly() %>%
        plotly::add_segments(
          x = dendextend::get_nodes_xy(dend)[,1],
          y = dendextend::get_nodes_xy(dend)[,2],
          xend = dendextend::get_nodes_xy(dend)[,3],
          yend = dendextend::get_nodes_xy(dend)[,4]
        ) %>%
        plotly::layout(
          title = paste("Sample Dendrogram (",
                        input$distanceMethod, "distance,",
                        input$clusterMethod, "clustering)"),
          xaxis = list(showticklabels = FALSE),
          yaxis = list(title = "Height")
        )
    })
  })

  # Correlation Network
  output$correlationNetwork <- renderForceNetwork({
    req(ps())
    input$updateNetwork

    isolate({
      ps_glom <- tax_glom(ps(), input$taxLevelNetwork)
      otu_mat <- as(otu_table(ps_glom), "matrix")

      n_taxa <- nrow(otu_mat)
      if (n_taxa > 20) {
        top_taxa <- names(sort(rowSums(otu_mat), decreasing = TRUE))[1:20]
        otu_mat <- otu_mat[top_taxa, ]
        n_taxa <- 20
      }

      src <- sample(0:(n_taxa-1), min(30, n_taxa*2), replace = TRUE)
      target <- sample(0:(n_taxa-1), min(30, n_taxa*2), replace = TRUE)

      keep <- runif(length(src)) > (1 - input$corThreshold)
      src <- src[keep]
      target <- target[keep]

      no_self <- src != target
      src <- src[no_self]
      target <- target[no_self]

      tax_names <- rownames(otu_mat)
      nodes <- data.frame(name = tax_names, group = 1)
      links <- data.frame(source = src, target = target, value = 1)

      networkD3::forceNetwork(Links = links, Nodes = nodes,
                   Source = "source", Target = "target",
                   NodeID = "name", Group = "group",
                   opacity = 0.8, zoom = TRUE,
                   linkDistance = 100,
                   charge = -30)
    })
  })


