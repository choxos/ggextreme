# An illustrative causal diagram for maternal smoking and orofacial clefts

A small directed acyclic graph for a study of maternal smoking and
orofacial clefts in the child, with a rationale for every node and arrow
and published references for several of them. It shows each kind of role
[`ggcausal()`](https://choxos.github.io/ggextreme/reference/ggcausal.md)
colors: an exposure, an outcome, a confounder, an unobserved cause of
the outcome, a collider created by counting only live births, and a free
text role. The structure is a teaching example rather than the result of
a formal review.

## Usage

``` r
cleft_dag
```

## Format

A list of two data frames.

- nodes:

  Six nodes with columns `name`, `label`, `role`, `rationale`,
  `references` and `timing`. `timing` is not a reserved column, so it
  appears as a field in the hover card and panel.

- edges:

  Eight arrows with columns `from`, `to`, `rationale` and `references`.

## Source

Rationales written for this package. References were checked against
PubMed and are listed in full in the `references` columns.

## Examples

``` r
cleft_dag$nodes[c("name", "role")]
#>      name              role
#> 1     ses        confounder
#> 2 smoking          exposure
#> 3  folate protective factor
#> 4   genes        unobserved
#> 5   cleft           outcome
#> 6   birth          collider
ggcausal(cleft_dag$edges, cleft_dag$nodes)
```
