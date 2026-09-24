# The static causal diagram shown in README.md. GitHub cannot run the
# widget, so the README shows this image and links to the live article.
#
# Run with: source("data-raw/readme_dag.R")

library(ggextreme)

dag <- ggcausal(
  cleft_dag$edges, cleft_dag$nodes,
  legend_title = "Role",
  title = "Maternal smoking and orofacial clefts"
)
graph_save(dag, "man/figures/README-dag.png", res = 200)
