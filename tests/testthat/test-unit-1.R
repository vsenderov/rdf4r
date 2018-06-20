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

test_that("Literal constructors work.", {
  lking_lear      = literal(text_value = "King Lear",        lang = "en")
  expect_match(represent(lking_lear), "\"King Lear\"@en")
  lshakespeare         = literal(text_value = "Shakespeare")
  expect_match(represent(lshakespeare), "\"Shakespeare\"")
  l1599 = literal(text_value = "1599", xsd_type = xsd_integer)
  # TODO expect_match(squote(l1599), "\"1599\""^^xsd:integer') w00t?
  expect_match(l1599$text_value, "1599")
  expect_match(l1599$xsd_type$uri, "<http://www.w3.org/2001/XMLSchema#integer>")
})


test_that("Identifier constructors work", {
  graphdb = rdf4r::basic_triplestore_access(
    server_url = "http://graph.openbiodiv.net:7777",
    user = "dbuser",
    password = "public-access",
    repository = "obkms_i6"
  )
  prefixes = c(
    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    example = "http://rdflib-rdf4r.net/",
    art = "http://art-ontology.net/"
  )
  art = prefixes[4]
  eg = prefixes[3]
  artist = identifier(id = "Artist", prefix = art)

  expect_match(class(artist), "identifier")
  expect_match(artist$uri, "<http://art-ontology.net/Artist>")

  lking_lear      = literal(text_value = "King Lear",        lang = "en")

  p_query = "SELECT DISTINCT ?id WHERE {
    ?id rdfs:label %label
  }"

  simple_lookup = query_factory(p_query, access_options = graphdb)
  lookup_or_mint_id = identifier_factory(fun = list(simple_lookup),
                                         prefixes = prefixes,
                                         def_prefix = eg)

  expect_match(class(lookup_or_mint_id), "function")

  idking_lear = lookup_or_mint_id(list(lking_lear))

  expect_match(class(idking_lear), "identifier")
})


test_that("RDF generation works", {
  graphdb = rdf4r::basic_triplestore_access(
    server_url = "http://graph.openbiodiv.net:7777",
    user = "dbuser",
    password = "public-access",
    repository = "obkms_i6"
  )
  prefixes = c(
    rdfs = "http://www.w3.org/2000/01/rdf-schema#",
    rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
    example = "http://rdflib-rdf4r.net/",
    art = "http://art-ontology.net/"
  )
  art = prefixes[4]
  eg = prefixes[3]
  wrote = identifier(id = "wrote", prefix = art)
  p_query = "SELECT DISTINCT ?id WHERE {
    ?id rdfs:label %label
  }"

  simple_lookup = query_factory(p_query, access_options = graphdb)
  lookup_or_mint_id = identifier_factory(fun = list(simple_lookup),
                                         prefixes = prefixes,
                                         def_prefix = eg)

  lshakespeare         = literal(text_value = "Shakespeare")
  idshakespeare = lookup_or_mint_id(list(lshakespeare))

  lking_lear      = literal(text_value = "King Lear",        lang = "en")
  idking_lear = lookup_or_mint_id(list(lking_lear))

  classics_rdf = ResourceDescriptionFramework$new()

  expect_match(class(classics_rdf), "ResourceDescriptionFramework", all = FALSE)
  expect_true(classics_rdf$add_triple(subject = idshakespeare,    predicate = wrote,      object = idking_lear))

  classics_rdf$set_context(idshakespeare)
  expect_match(class(classics_rdf$serialize()), "character")

})
