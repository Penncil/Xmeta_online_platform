bsCollapse(id="faq",
           bsCollapsePanel("What is X-meta?",
                           p("X-meta is an open-sourced, well-documented and interactive toolbox for meta-analysis.
                             There are three main components to this toolbox: An R package called XMETA,
                             video tutorials and documentation for the package and an online analysis platform.
                             XMETA package offers several functions for performing meta-analysis and visualizing outcomes
                             , allowing users to conduct robust multivariate meta-analysis (mmeta), publication bias test (PB),
                             outcome reporting bias test (ORB) and novel visualization tool (galaxy).
                             Through the tutorials, reference documents and sample code,
                             users can have a comprehensive exploration of the features found in XMETA and how they may apply to analytical work. ",
                             style="text-align: justify;"),
                           a("X-meta: a toolbox for meta-analysis",
                             href="https://www.xmeta.wiki/", target="_blank"),
                           style="info"),
           bsCollapsePanel("How did you make this app?",
                           p("This app is written in the", 
                             a("R programming language", href="https://www.r-project.org/", target="_blank"), "and built with the", 
                             a("Shiny", href="https://shiny.rstudio.com/", target="_blank"), "web application framework for R.",
                             a("Contact us", href="https://www.xmeta.wiki/team", target="_blank"), "for details.",
                             style="text-align: justify;"), 
                           style="info")
           )
