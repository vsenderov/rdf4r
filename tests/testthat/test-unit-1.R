context("test-unit-1.R")

test_that("Connection to the provided triple-store works.", {
  graphdb = rdf4r::basic_triplestore_access(
    server_url = "http://graph.openbiodiv.net:7777",
    user = "dbuser",
    password = "public-access",
    repository = "obkms_i6"
  )
  expect_match(class(graphdb), "triplestore_access_options", all = FALSE)
  expect_equal(get_protocol_version(graphdb), 8)
  expect_match(class(list_repositories(graphdb)), "data.frame")
})

test_that("Query conversions work.", {
  graphdb = rdf4r::basic_triplestore_access(
    server_url = "http://graph.openbiodiv.net:7777",
    user = "dbuser",
    password = "public-access",
    repository = "obkms_i6"
  )
  p_query =
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    PREFIX dwc: <http://rs.tdwg.org/dwc/terms/>
    PREFIX openbiodiv: <http://openbiodiv.net/>
    PREFIX dwciri: <http://rs.tdwg.org/dwc/iri/>
    PREFIX pkm: <http://proton.semanticweb.org/protonkm#>
    PREFIX fabio: <http://purl.org/spar/fabio/>
    PREFIX po: <http://www.essepuntato.it/2008/12/pattern#>
    PREFIX dc: <http://purl.org/dc/elements/1.1/>

    SELECT (SAMPLE(?name) AS ?name) (SAMPLE(?genus) AS ?genus) (SAMPLE(?title) AS ?title)

    WHERE
    {
      ?name rdfs:label %label ;
        rdf:type openbiodiv:LatinName ;
  		dwc:genus ?genus.

      ?article rdf:type fabio:JournalArticle ;
  		   po:contains/pkm:mentions ?name ;
         dc:title ?title .

    } GROUP BY ?article"

  genus_lookup = rdf4r::query_factory(p_query = p_query, access_options = graphdb)

  expect_match(class(genus_lookup), "function")
  expect_true(nrow(genus_lookup("\"Drosophila\"")) >= 2)
})
