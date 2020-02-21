library(igraph)
library(tidygraph)
library(ggraph)

## Small example graph, leading eigenvector by default
kite <- make_graph("Krackhardt_kite")
spectrum(kite)[c("values", "vectors")]

## Double check
eigen(as_adj(kite, sparse=FALSE))$vectors[,1]
eigen(laplacian_matrix(kite, sparse=FALSE))


g <- make_graph(c("b.spence", "l.tweedie", "b.spence", "h.dawkes", "b.spence", "hua su",
                  "l.tweedie", "h.dawkes", "l.tweedie", "hua su",
                  "h.dawkes", "hua su",
                  "hua su", "huw dawkes", "hua su", "bob spence", "hua su", "lisa tweedie",
                  "bob spence", "huw dawkes", "bob spence", "lisa tweedie",
                  "huw dawkes", "lisa tweedie", "huw dawkes", "hus su", "huw dawkes", "robert spence",
                  "lisa tweedie", "robert spence", "lisa tweedie", "hus su",
                  "hus su", "robert spence"), directed = F) 

plot(g, layout = layout_in_circle)

round(eigen(laplacian_matrix(g, sparse=FALSE))$values,2)

E(g)[1:17]$color = "black"

g <- g %>% add_edges(c("bob spence", "robert spence", "b.spence", "robert spence"), color = "red")

plot(g, layout = layout_in_circle)
round(eigen(laplacian_matrix(g, sparse=FALSE))$values,2)

g <- delete_edges(g, c("robert spence|b.spence"))

V(g)[c(1,6)]$color = "red"

E(g)$weight = 5*runif(length(E(g)),0,1)
plot(g, layout = layout_in_circle, edge.width=E(g)$weight)


# spectral graph wavelets
res = eigen(laplacian_matrix(g, sparse=FALSE))
eigenValues = res$values
eigenVectors = res$vectors
gs = function(lambda, s) exp(-lambda*s)
s = 0.2
phi = eigenVectors %*% diag(gs(eigenValues, s)) %*% t(eigenVectors)
dimnames(phi) = list(names(V(g)), names(V(g)))
a = 1
V(g)[a]
phi[,a]


b.spence     l.tweedie      h.dawkes        hua su    huw dawkes 
8.598384e+00 -4.545200e+00 -3.717397e+00 -3.357871e-01 -2.744718e-16 
bob spence  lisa tweedie        hus su robert spence 
5.581955e-16  3.916620e-16  3.083953e-17  8.912624e-16


