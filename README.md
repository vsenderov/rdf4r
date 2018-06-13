# RDF4R: R Library for Working with RDF

## Introduction

RDF4R is an R package for working with [Resource Description Framework (RDF)](https://www.w3.org/RDF/) data. It was developed as part of the [OpenBiodiv](http://openbiodiv.net) project but is completely free of any OpenBiodiv-specific code and can be used for generic purposes requiring tools to work with RDF data in the [R programming environment](https://www.r-project.org/).

### Features

RDF4R supports the following operations:

*1. Connection to a triple store.*

Tripe stores, also known as quad stores, graph databases or semantic databases, are databases that store RDF data and allow the quering of RDF data via the [SPARQL query language](https://www.w3.org/TR/rdf-sparql-query/). RDF4R can connect to triple stores that support the [RDF4J API](http://docs.rdf4j.org/rest-api/) such as [GraphDB](http://graphdb.ontotext.com/). It is possible to establish both basic connections (no password or basic HTTP authentication), and connection secured with an API access token.

*2. Work with repositories on a triple store.*

Once you have established connection to a triple store, it is possible to inspect the protocol version, view the list of repositories in the database, execute SPARQL read (SELECT keyword and related) and SPARQL update (INSERT and related) queries, as well as submit serialized RDF data directly.
