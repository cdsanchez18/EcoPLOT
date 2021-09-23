


EcoPLOT_FAQ <- tabPanel("FAQ",
                        tabsetPanel(id = "FAQui",
                                    tabPanel("Troubleshooting",
                                             includeMarkdown("Modules/FAQ/Guides/faq.rmd")),
                                    tabPanel("Developer Guide",
                                             includeMarkdown("Modules/FAQ/Guides/contributing.rmd")),
                                    tabPanel("Code of Conduct",
                                             includeMarkdown("Modules/FAQ/Guides/code_of_conduct.rmd")),
                                    tabPanel("Citations",
                                    includeMarkdown("Modules/FAQ/Guides/citations.rmd")),
                                    tabPanel("License",
                                             includeMarkdown("Modules/FAQ/Guides/license.rmd")))
)
