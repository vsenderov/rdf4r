# RDF4R: R Library for Working with RDF

## Introduction

RDF4R is an R package for working with [Resource Description Framework (RDF)](https://www.w3.org/RDF/) data. It was developed as part of the [OpenBiodiv](http://openbiodiv.net) project but is completely free of any OpenBiodiv-specific code and can be used for generic purposes requiring tools to work with RDF data in the [R programming environment](https://www.r-project.org/).

## Installation

RDF4R depends on the following packages (list may change in future releases):

- [gsubfn](https://cran.r-project.org/web/packages/gsubfn/index.html)
- [httr](https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html)
- [xml2](https://cran.r-project.org/web/packages/xml2/index.html)
- [R6](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html)
- [devtools](https://cran.r-project.org/web/packages/devtools/index.html): needed for the GitHub install

Please, first install these packages with `install.packages.` Pay attention to error messages during their installation as additional OS-level components may need to be installed.

Then, install RDF4R from GitHub with the following command:

```
devtools::install_github("vsenderov/rdf4r")
```

We are currently in the process of submitting the package to a repository and making it available through the standard installation facilities of R. Our intention is to publish it through [CRAN](https://cran.r-project.org/) and/or [rOpenSci](https://github.com/ropensci/onboarding). In addition to that this README is being prepared as a software description/ R package paper in the [RIO Journal](http://riojournal.com).

## Specification

RDF4R has the features listed in the following subsections.

### Connection to a triple-store

Triple-stores, also known as quad-stores, graph databases, or semantic databases, are databases that store RDF data and allow the quering of RDF data via the [SPARQL query language](https://www.w3.org/TR/rdf-sparql-query/). RDF4R can connect to triple-stores that support the [RDF4J server REST API](http://docs.rdf4j.org/rest-api/) such as [GraphDB](http://graphdb.ontotext.com/). It is possible to establish both basic connections (requiring no password or requiring basic HTTP user-pass authentication) or connection secured with an API access token.

### Work with repositories on a triple-store

Once a connection to a triple-store has been established, it is possible to inspect the talk protocol version, view the list of repositories on the database, execute SPARQL Read (SELECT keyword and related) and SPARQL Update (INSERT and related) queries on the database, as well as submit serialized RDF data directly to the database.

### Function factories to convert SPARQL queries to R functions

An important feature of RDF4R are its facilities for converting SPARQL queries and the like to R functions. This conversion is realized by a family of functions that return functions (function factories). In a nutshell, given a parameterized SPARQL query (parametrization syntax is explained in the ["Using RDF4R" vignette](vignettes/using-rdf4r.Rmd)), the `query_factory` function returns a function whose arguments are the parameters of the query. Upon called this function submits the query a SPARQL endpoint and returns the results.

### Work with literals and identifiers

The building blocks of RDF are literals (e.g. strings, numbers, dates, etc.) and resource identifiers. RDF4R provides classes for literals and resource identifiers that are tightly integrated with the other facilities of the package.

### Prefix management

Prefixes are managed automatically during serialization by being extracted from the resource identifiers.

### Creation and serialization of RDF

RDF4R uses an amortized vector data structure to store RDF triples as mutable [R6](https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html) objects (third element). Blank nodes are partially supported: a triple may contain an anonymous RDF object (a list of triples with the same subject) as its object. In this case, the parent RDF is serialed as [Turtle by using the bracket syntax](https://www.w3.org/TR/turtle/#BNodes). Current serialization only supports Turtle (and its variant Trig) and only supports adding new triples.

### A basic vocabulary of semantic elements

RDF4R has some basic resource identifiers for widely used classes and predicates predifined (e.g. for `rdf:type`, `rdfs:label`, etc.).

## Usage

The usage of the package is discussed in detail in the vignette ["Using RDF4R"](vignettes/using-rdf4r.Rmd). After installing the package, you can open the vignette from R with:

```
vignette("using-rdf4r")
```

If you would like an overview of all package facilities grouped by category, please consult the package documentation available via `?rdf4r.`

## Discussion

We would like now to compare RDF4R to other programs designed for a similar purpose and critically discuss some of its features.

### Related Packages

Perhaps, the closest match to RDF4R is the [rdflib R package](https://github.com/ropensci/rdflib.git) by [Carl Boettiger](https://github.com/cboettig). `rdflib`'s' first official release was on [Dec 10, 2017](https://github.com/ropensci/rdflib/releases/tag/0.0.1), whereas work on the codebase that is now known as RDF4R began at Pensoft around mid-2015 at the onset of the OpenBiodiv project. This explains why two closely related R packages for working with RDF exist. In our opinion, the packages have different design philosophies and are thus complementary.

`rdflib` is a high-level wrapper to [`redland`](http://librdf.org/), a powerful C library that provides support for RDF. `redland` provides an in-memory storage model for RDF beyond what is available in RDF4R and also persistent storage working with a number of databases. It enables the user to query RDF objects with SPARQL. Thus, `redland` can be considered a complete graph database implementation in C. There is also a [`redland` R package](https://cran.r-project.org/web/packages/redland/index.html), which is a low-level wrapper to the C `redland`, essentially mapping function calls one-to-one.

In our opinion `redland` is more complex than needed for OpenBiodiv. At the onset of the OpenBiodiv project, we decided not to use it as we were going to rely on GraphDB for our storage and querying. RDF4R's main purpose was (and is) to provide a convenient R interface for users of GraphDB and similar RDF4J compatible graph databases.

A feature that differentiates `redland`/`rdflib` from RDF4R is the design philosophy. While `rdflib` concentrates on JSON-LD support (and many others), RDF4R was designed primariy with the [Turtle](https://www.w3.org/TR/turtle/) and [TriG](https://www.w3.org/TR/trig/) serializations in mind. This means that RDF4R can work with named graphs, where their usage is discouraged or perhaps [impossible with `rdflib`](https://github.com/ropensci/rdflib/issues/23).

It is hard to ignore the superior in-memory model of `redland`/`rdflib`. Therefore, [the maintainer of RDF4R](@https://github.com/vsenderov/), has contributed several compatibility patches to `rdflib`. Thus makes it possible to extend RDF4R to use either one of the in-memory models - RDF4R's own amortized vector, or `rdflib`/`redland`. Thus, it will be possible for the user of RDF4R to retain its syntax and high-level features - constructor factories, functors, etc, and the ability to use named graphs - but benefit from performance increases, stability, and scalability with the `redland`/`rdflib` backend.

This will enable the users of the R programming environment to use whichever syntax they prefer and benefit from an efficient storage engine.

### Elements of functional style

When choosing a programming environment for the project, a choice was made to use R as it is one of two most widely used programming languages for data science (the other being Python). Having settled on R, we decided to incorporate elements of the [functional programming style](https://en.wikipedia.org/wiki/Functional_programming) that R supports in order to allow the users of the package to write simple code to solve complex tasks.

RDF4R is not written as in a pure functional programming style: some of its functions have side-effects. However, we make use of functions as first-class citizens and [closures](https://en.wikipedia.org/wiki/Closure_(computer_programming)). By "functions as first-class citizens" we mean that RDF4R has both functions that return functions and functions that take functions as arguments.

The simplest example of such a function is the [`query_factory`](R/db-access-high-level.R
) function which converts a SPARQL query to an R function. We believe that this is a very useful feature because the working ontologist often has quite a few SPARQL queries that they want to execute and then parse the results of. Had we just provided the generic `submit_sparql` function, every time the user would have needed to modify the SPARQL query somehow (e.g. by for example changing a label that is matched), execute the `submit_sparql` while not forgetting to specify the correct access point, and then parse the results. `query_factory` packages all of this functionality in one place and hides the complexity by allowing the user to simply write

```
genus_lookup("\"Drosophila\"")
```

This example is taken from the vignette. Here, `genus_lookup` is a function that has been dynamically generated by `query_factory` and encloses the functionality for parameterizing the query, executing it, and then parsing the results. This is possible thanks to the implementation of functions in R as _closures_.







For example, `identifier_factory` is a functor that takes in as argument a list of lookup functions (created for example with the facilities for converting SPARQL to an R function) and returns an identifier constructor function that can be used to construct identifier objects based on executing the lookup functions from the argument list supplied to the functor. The reasoning behing this functor is to enable the working ontologist to generate code that first looks up a resource identifier in several different places before coining a new one.

### Pros and Cons

### Future Directions