#' Namespace prefixes used when serialising the APD to RDF
#'
#' Passed to `rdflib::rdf_serialize()` so that Turtle output carries short
#' prefixes (`APD:trait_0000012`) instead of full URIs.
#'
#' Three namespace maps exist in this repo and they have diverged. This one is
#' the only one the build reads. The others are
#' `data/APD_namespace_declaration.csv` -- which Wenk et al. 2024 (p.8) describes
#' as "the namespace declaration when compiling the RDF representation", but
#' which no code reads -- and seven base URIs hardcoded in
#' `convert_to_triples.R`. Only 23 of the 38 URIs are shared. Stage 3 of
#' `plans/build-workflow-overhaul.md` collapses all three onto the CSV, which is
#' what makes the paper's claim true; until then, treat this vector as
#' authoritative and edit it here.
APD_NAMESPACES <- c(
  APD = "https://w3id.org/APD/traits/",
  APD_glossary = "https://w3id.org/APD/glossary/",
  dc = "http://purl.org/dc/elements/1.1/",
  skos = "http://www.w3.org/2004/02/skos/core#",
  dwc = "http://rs.tdwg.org/dwc/terms/attributes/",
  dcam = "http://purl.org/dc/dcam/",
  dcterms = "http://purl.org/dc/terms/",
  ets = "http://terminologies.gfbio.org/terms/ETS/",
  obo = "http://purl.obolibrary.org/obo/",
  oboecore = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
  ont = "https://w3id.org/iadopt/ont/",
  owl = "http://www.w3.org/2002/07/owl#",
  rdfs = "http://www.w3.org/2000/01/rdf-schema#",
  uom = "https://w3id.org/uom/",
  datacite = "http://purl.org/datacite/v4.4/",
  xsd = "http://www.w3.org/2001/XMLSchema#",
  Cerrado = "http://cerrado.linkeddata.es/ecology/",
  CorVeg = "http://linked.data.gov.au/def/corveg-cv/",
  CO = "https://cropontology.org/rdf/",
  DCM = "http://dicom.nema.org/resources/ontology/DCM/",
  EDAM = "http://edamontology.org/",
  EFO = "http://www.ebi.ac.uk/efo/",
  EnvThes = "http://vocabs.lter-europe.net/EnvThes/",
  hupson = "http://scai.fraunhofer.de/HuPSON#",
  IOBC = "http://purl.jp/bio/4/id/",
  MESH = "http://purl.bioontology.org/ontology/MESH/",
  odo = "http://purl.dataone.org/odo/",
  ORCID = "https://orcid.org/",
  SIO = "http://semanticscience.org/resource/",
  SWEET_phenSolid = "http://sweetontology.net/phenSolid/",
  SWEET_phenSystem = "http://sweetontology.net/phenSystem/",
  SWEET_procWave = "http://sweetontology.net/procWave/",
  SWEET_prop = "http://sweetontology.net/prop/",
  SWEET_propConductivity = "http://sweetontology.net/propConductivity",
  SWEET_propPressure = "http://sweetontology.net/propPressure/",
  SWEET_propTime = "http://sweetontology.net/propTime/",
  SWEET_realmSoil = "http://sweetontology.net/realmSoil/",
  SWEET_reprSciComponent = "http://sweetontology.net/reprSciComponent/",
  SWEET_reprTimeDay = "http://sweetontology.net/reprTimeDay/"
)
