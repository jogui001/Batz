#' List bat species found in given US states/territories, or the states where given bat species are found
#'
#' Given a vector or data frame containing any mix of bat species identifiers
#' (latin name, common name, 4-letter code, 6-letter code) and/or US
#' state/territory identifiers (official name, short name, 2-letter code),
#' returns a table crossing the requested species against the requested
#' states, laid out to match Josh's own \code{"batlist model1.csv"}
#' (\code{table.type = "state"}) or \code{"batlist model2.csv"}
#' (\code{table.type = "matrix"}) template files. If only state identifiers
#' are supplied, every species is checked against those states. If only
#' species identifiers are supplied, every state/territory is checked for
#' that species. If both are supplied, only the specific species x specific
#' states combination is returned.
#'
#' @param data A vector, or a data frame, of bat species identifiers and/or
#'   US state/territory identifiers, in any mix. A data frame is flattened
#'   to one combined vector before matching - this function's output is a
#'   summary table, not an element-wise recode, so (unlike
#'   \code{batz.batusa_recode.names}) there's no reason to preserve
#'   \code{data}'s original shape.
#' @param grammar.dash Logical, default \code{TRUE}. Hyphens are ignored
#'   (treated the same as a space) when MATCHING an input value regardless of
#'   this flag. This flag only controls OUTPUT labels: \code{TRUE} (default)
#'   keeps hyphens as written in the reference tables; \code{FALSE} replaces
#'   every hyphen in an output species/state label with a space instead.
#' @param statename.format Character, default \code{"code2"}. How states are
#'   labeled in the output: \code{"code2"} (2-letter code), \code{"official.name"},
#'   or \code{"short.name"}.
#' @param batname.format Character, default \code{"full"}. How species are
#'   labeled in the output's \code{"Bat Species"} field:
#'   \code{"full"} - \code{"<common name> (<latin name>)<hibernation.strat>"},
#'   all concatenated with NO separator before \code{hibernation.strat}
#'   (matches Josh's own model files exactly, cramped join included);
#'   \code{"both"} - \code{"<common name> (<latin name>)"}, with no
#'   hibernation.strat suffix; \code{"common"} - common name only;
#'   \code{"latin"} - latin name only.
#' @param bat.code Character, default \code{"code4"}. Which code populates
#'   the separate \code{"Species Code"} column: \code{"code4"},
#'   \code{"code6"}, or \code{"none"} (omits the column entirely).
#' @param table.type Character, default \code{"state"}. The output table's
#'   shape:
#'   \code{"state"} (matches \code{"batlist model1.csv"}) - one row per
#'   (state, species) PRESENT pair (a species absent from a given state
#'   simply has no row for that state). Columns, in order: \code{"State"},
#'   \code{"Bat Species"}, \code{"Species Code"} (if \code{bat.code !=
#'   "none"}), \code{"Federal"}, \code{"State"} (again - see Details),
#'   \code{"Phonic Group"} (if \code{phonic.group = TRUE}).
#'   \code{"matrix"} (matches \code{"batlist model2.csv"}) - one row per
#'   requested/implied species, columns \code{"Bat Species"}, \code{"Species
#'   Code"}, \code{"Phonic Group"}, \code{"Federal"}, then one column per
#'   requested/implied state (named per \code{statename.format}), each cell
#'   per \code{presence.absence} (see Details).
#' @param presence.absence Character vector of length 2, default
#'   \code{c("*", "-")}. Used for a species that is present in a state but
#'   not listed there (symbol 1) or absent from a state entirely (symbol
#'   2). A LISTED species' cell/column shows its listing status instead of
#'   symbol 1 - see Details.
#' @param phonic.group Logical, default \code{TRUE}. Adds a
#'   \code{"Phonic Group"} column (one row per species, in both
#'   \code{table.type} layouts).
#' @param species.extirpated Logical, default \code{TRUE}. When \code{TRUE},
#'   would append \code{"#"} to a status value for any state a species has
#'   been extirpated from - but the reference database does not yet have an
#'   extirpation column (per Josh's own note that "this information may not
#'   be available yet"). If the reference table has no column literally
#'   named \code{states.extirpated}, this flag has no effect beyond printing
#'   a one-time notice (not an error).
#'
#' @return A data frame - one row per (state, species) present pair
#'   (\code{table.type = "state"}) or one row per species
#'   (\code{table.type = "matrix"}), with column headers matching Josh's own
#'   model files literally (not this project's usual \code{$collum.name}
#'   dot-convention - a deliberate, spec-driven exception).
#'
#' @details
#' \strong{Rebuilt 2026-08-25 against the real \code{batlist model1.csv} /
#' \code{batlist model2.csv} template files}, once Josh granted access to
#' the \code{"4 Current  test data"} folder (an earlier same-day version of
#' this function had guessed a different, incorrect layout from the prose
#' spec alone, before those files were reachable). Confirmed against the
#' real templates: \code{table.type = "state"} is one row per
#' (state, species) PRESENT pair, not an aggregated per-state list;
#' \code{table.type = "matrix"} is one row per species with one column per
#' state; \code{batname.format = "full"} bakes \code{hibernation.strat}
#' directly into the \code{"Bat Species"} text; \code{"Species Code"} is
#' its own column, not appended onto the species label; \code{"Federal"} is
#' always included, abbreviated from \code{$fedstatus} (\code{"Not
#' Listed"} -> \code{""}, \code{"Under Review"} -> \code{"UR"},
#' \code{"Endangered"} -> \code{"E"}, \code{"Proposed Endangered"} ->
#' \code{"PE"}).
#'
#' \strong{One real gap remains, even with the model files in hand:} both
#' models clearly want a per-(species,state) LISTING SEVERITY value (e.g.
#' \code{"E"}, \code{"T"}, \code{"SC"}) wherever a species is listed, not
#' just a generic symbol - but \code{NAbat.names.csv} only has a flat
#' \code{$states.listed} (listed/not, no severity level) and
#' \code{$state.soc} (species-of-concern states). This function uses the
#' best available proxy: a state in \code{$state.soc} shows \code{"SC"}; a
#' state in \code{$states.listed} (and not \code{$state.soc}) shows
#' \code{"L"} - a generic placeholder, NOT a real abbreviation from Josh's
#' models, standing in for "listed, severity unspecified" until real
#' per-state Endangered/Threatened/Special-Concern data is available. A
#' state that's neither shows \code{presence.absence[1]} (matrix) or blank
#' (state table.type, where a row already existing implies presence).
#' Absent states show \code{presence.absence[2]} (matrix) or get no row at
#' all (state table.type). \strong{If Josh has, or builds, real per-state
#' severity data, replace the \code{"L"} placeholder with it.}
#'
#' Every element of \code{data} is checked against BOTH reference tables
#' (bat: \code{latin}/\code{common}/\code{code4}/\code{code6}; state:
#' \code{official.name}/\code{short.name}/\code{code2}), ignoring case,
#' underscores/dashes (treated as spaces), and leading/trailing/extra
#' whitespace. If any element matches the bat reference, ONLY those matched
#' species are used (not all 54); otherwise all 54 are used. Symmetrically
#' for states (matched subset, or all 56 states/territories). An element
#' matching neither reference is reported via \code{"WARNING: X inputs did
#' not match either reference table: ..."} (not a literal spec requirement -
#' added for consistency with other \code{batz} functions' unmatched-input
#' handling). If NOTHING in \code{data} matches either reference table at
#' all, that's an error.
#'
#' The two reference tables (\code{NAbat.names.csv}, 54 species x 15
#' columns; \code{NAstates.names.csv}, 56 states/territories x 8 columns -
#' confirmed, via a real copy Josh saved into his own
#' \code{"reference database files"} folder as \code{"USAstates.names.csv"},
#' to be exactly what his spec calls \code{"USAstate.names.database"}) are
#' embedded directly in this function - there is no reference-file-path
#' argument. To update either list later, replace the corresponding data
#' frame inside this function with a fresh \code{dput()} of an updated CSV
#' (never hand-retype a wide reference table into R code - generate the
#' literal programmatically and splice it in).
#'
#' @examples
#' \dontrun{
#' # which states have Big brown bat and Little brown bat? (table.type = "state")
#' batz.batusa_list.species(c("epfu", "mylu"))
#'
#' # same species, full species x state grid
#' batz.batusa_list.species(c("epfu", "mylu"), table.type = "matrix")
#'
#' # which species are found in Ohio and Vermont?
#' batz.batusa_list.species(c("OH", "VT"))
#'
#' # a specific species x specific states cross (matches batlist model1/model2's
#' # own MA/ME/PA x 9-species test case in structure)
#' batz.batusa_list.species(c("epfu", "mylu", "OH", "VT"), table.type = "matrix")
#' }
#'
#' @export
batz.batusa_list.species <- function(data,
                                      grammar.dash = TRUE,
                                      statename.format = "code2",
                                      batname.format = "full",
                                      bat.code = "code4",
                                      table.type = "state",
                                      presence.absence = c("*", "-"),
                                      phonic.group = TRUE,
                                      species.extirpated = TRUE) {

  # ---------------------------------------------------------------------------
  # Reference database 1: bat species (Josh's NAbat.names.csv, embedded as
  # currently supplied - 54 species x 15 columns). See @details above for
  # how to update this.
  # ---------------------------------------------------------------------------
  nabat <- structure(list(latin = c("Antrozous pallidus", "Artibeus jamaicensis", 
"Brachyphylla cavernarum", "Choeronycteris mexicana", "Corynorhinus rafinesquii", 
"Corynorhinus townsendii", "Corynorhinus townsendii ingens", 
"Corynorhinus townsendii virginianus", "Diphylla ecaudata", "Eptesicus fuscus", 
"Euderma maculatum", "Eumops floridanus", "Eumops perotis", "Eumops underwoodi", 
"Idionycteris phyllotis", "Lasionycteris noctivagans", "Lasiurus borealis", 
"Lasiurus cinereus", "Lasiurus cinereus semotus", "Lasiurus ega", 
"Lasiurus frantzii", "Lasiurus intermedius", "Lasiurus minor", 
"Lasiurus seminolus", "Lasiurus xanthinus", "Leptonycteris nivalis", 
"Leptonycteris yerbabuenae", "Macrotus californicus", "Molossus molossus", 
"Mormoops megalophylla", "Myotis auriculus", "Myotis austroriparius", 
"Myotis californicus", "Myotis ciliolabrum", "Myotis evotis", 
"Myotis grisescens", "Myotis keenii", "Myotis leibii", "Myotis lucifugus", 
"Myotis occultus", "Myotis septentrionalis", "Myotis sodalis", 
"Myotis thysanodes", "Myotis velifer", "Myotis volans", "Myotis yumanensis", 
"Noctilio leporinus", "Nycticeius humeralis", "Nyctinomops femorosaccus", 
"Nyctinomops macrotis", "Parastrellus hesperus", "Perimyotis subflavus", 
"Stenoderma rufum", "Tadarida brasiliensis"), common = c("Pallid bat", 
"Jamaican fruit-eating bat", "Antillean fruit-eating bat", "Mexican long-tongued bat", 
"Rafinesque's big-eared bat", "Townsend's big-eared bat", "Ozark big-eared bat", 
"Virginia big-eared bat", "Hairy-legged vampire bat", "Big brown bat", 
"Spotted bat", "Florida bonneted bat", "Greater bonneted bat", 
"Underwood's bonneted bat", "Allen's big-eared bat", "Silver-haired bat", 
"Eastern red bat", "Hoary bat", "Hawaiian hoary bat", "Southern yellow bat", 
"Desert Red Bat", "Northern yellow bat", "Minor red bat", "Seminole bat", 
"Western yellow bat", "Mexican long-nosed bat", "Lesser long-nosed bat", 
"California leaf-nosed bat", "Pallas' mastiff bat", "Peter's ghost-faced bat", 
"Southwestern myotis", "Southeastern myotis", "California myotis", 
"Western small-footed myotis", "Long-eared myotis", "Gray bat", 
"Keen's myotis", "Eastern small-footed myotis", "Little brown bat", 
"Arizona myotis", "Northern long-eared bat", "Indiana bat", "Fringed myotis", 
"Cave bat myotis", "Long-legged myotis", "Yuma myotis", "Greater bulldog bat", 
"Evening bat", "Pocketed free-tailed bat", "Big free-tailed bat", 
"Canyon bat", "Tri-colored bat", "Red fruit bat", "Brazilian free-tailed bat"
), code4 = c("anpa", "arja", "brca", "chme", "cora", "coto", 
"coti", "cotv", "diec", "epfu", "euma", "eufl", "eupe", "euun", 
"idph", "lano", "labo", "laci", "lacs", "laeg", "lafr", "lain", 
"lami", "lase", "laxa", "leni", "leye", "maca", "momo", "mome", 
"myar", "myau", "myca", "myci", "myev", "mygr", "myke", "myle", 
"mylu", "myoc", "myse", "myso", "myth", "myve", "myvo", "myyu", 
"nole", "nyhu", "nyfe", "nyma", "pahe", "pesu", "stru", "tabr"
), code6 = c("antpal", "artjam", "bracav", "chomex", "corraf", 
"cortow", "cotoin", "cotovi", "dipeca", "eptfus", "eudmac", "eumflo", 
"eumper", "eumund", "idiphy", "lasnoc", "lasbor", "lascin", "lacise", 
"lasega", "lasfra", "lasint", "lasmin", "lassem", "lasxan", "lepniv", 
"lepyer", "maccal", "molmol", "mormeg", "myoaur", "myoaus", "myocal", 
"myocil", "myoevo", "myogri", "myokee", "myolei", "myoluc", "myoocc", 
"myosep", "myosod", "myothy", "myovel", "myovol", "myoyum", "noclep", 
"nychum", "nycfem", "nycmac", "parhes", "persub", "steruf", "tadbra"
), fedstatus = c("Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Endangered", "Endangered", "Not Listed", 
"Not Listed", "Not Listed", "Endangered", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Endangered", "Endangered", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Endangered", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Endangered", "Not Listed", "Not Listed", "Under Review", 
"Not Listed", "Endangered", "Endangered", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Proposed Endangered", "Not Listed", 
"Not Listed"), iucnstatus = c("Least Concern", "Least Concern", 
"Least Concern", "Near Threatened", "Least Concern", "Least Concern", 
"", "", "Least Concern", "Least Concern", "Least Concern", "Vulnerable", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"", "Least Concern", "Vulnerable", "Least Concern", "Least Concern", 
"Endangered", "Vulnerable", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Vulnerable", "Least Concern", 
"Endangered", "Endangered", "Least Concern", "Near Threatened", 
"Near Threatened", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Vulnerable", "Near Threatened", 
"Least Concern"), states.listed = c("", "", "", "AZ,CA", "", 
"", "", "", "", "", "", "FL", "", "", "", "", "", "", "", "", 
"", "", "", "OK", "", "NM,TX", "", "", "", "", "", "", "", "", 
"", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NH,NJ,NY,OH,OK,PA,TN,VA,VT,WV", 
"CT,MA,ME,MI,NH,NJ,OH,PA,TN,VA,VT,WI", "", "", "", "", "", "", 
"", "", "IN,KY,MI,OH", "", "", "", "", "", ""), states.present = c("AZ,CA,CO,ID,KS,MT,NM,NV,OK,OR,TX,UT,WA", 
"PR", "PR,VI", "AZ,CA,NM,TX", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,SC,TN,VA,WV", 
"AR,AZ,CA,CO,ID,KS,KY,MO,MT,NC,ND,NE,NM,NV,OK,OR,SD,TX,UT,VA,WA,WV,WY", 
"AR,MO,OK", "KY,NC,VA,WV", "TX", "AK,AL,AR,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,CO,MT,NM,NV,OR,UT,WA,WY", "FL", "AZ,CA,NM,TX", "AZ", "AZ,CA,CO,NM,NV,UT", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AL,AR,CO,CT,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,HI,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"HI", "AZ,CA,NM,TX", "AZ,CA,NM,TX", "AL,FL,GA,LA,MS,NC,PA,SC,TX,VA", 
"PR", "AL,AR,FL,GA,KY,LA,MO,MS,NC,OK,SC,TN,TX,VA", "AZ,CA,NM", 
"AZ,NM,TX", "AZ,CA,NM", "AZ,CA,NV", "FL", "AZ,TX", "AZ,NM", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,OK,SC,TN,TX", 
"AZ,CA,CO,ID,MT,NM,NV,OR,TX,UT,WA,WY", "AZ,CA,CO,ID,KS,MT,ND,NE,NM,NV,OK,OR,SD,TX,UT,WA,WY", 
"AZ,CA,CO,ID,MT,ND,NM,NV,OR,SD,UT,WA,WY", "AL,AR,GA,IL,IN,KS,KY,MO,MS,NC,OK,TN,VA,WV", 
"AK,WA", "AL,AR,CT,GA,KY,MA,MD,ME,MI,MO,NC,NH,NJ,NY,OH,OK,PA,RI,TN,VA,VT,WV", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,NM", "AL,AR,CT,DE,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NY,OH,OK,PA,RI,SC,SD,TN,VA,VT,WI,WV,WY", 
"AL,AR,CT,IA,IL,IN,KY,MD,MI,MO,NC,NJ,NY,OH,OK,PA,TN,VA,VT,WV", 
"AZ,CA,CO,NM,NV,OR,SD,TX,UT,WA,WY", "AZ,CA,KS,NM,OK,TX", "AK,CA,CO,ID,MT,ND,NE,NM,OR,SD,TX,WY", 
"CA,CO,ID,MT,NV,OR,TX,UT,WA", "", "AL,AR,FL,GA,IA,IL,IN,KS,KY,LA,MD,MI,MN,MO,MS,NC,NE,OH,OK,PA,SC,TN,TX,VA,WI,WV", 
"AZ,CA,NM,TX", "CA,NV,TX,UT", "AZ,CA,CO,NM,NV,OK,TX,UT,WA", "AL,AR,CO,CT,DC,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,NC,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"PR,VI", "AZ,CA,CO,FL,KS,NM,NV,OK,TX,UT"), states.end = c("", 
"", "", "", "", "", "", "", "", "", "", "FL", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "NM,TX", "", "", "", "", 
"", "", "", "", "", "", "", "NH", "CT,MA,ME,NH,NJ,PA,VA,VT", 
"", "", "", "", "", "", "", "", "IN", "", "", "", "", "", ""), 
    states.the = c("", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "OK", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "PA,VT", 
    "TN,WI", "", "", "", "", "", "", "", "", "KY,MI", "", "", 
    "", "", "", ""), state.soc = c("", "", "", "AZ,CA", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "AK,WA", "CT,GA,MA,MD,MO,NC,NJ,NY,OH,OK,TN,VA,WV", "MI,OH", 
    "", "", "", "", "", "", "", "", "OH", "", "", "", "", "", 
    ""), fed.proposed = c("", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
    "", "", "", "", "", "", "", "", "", "", "", "", "", "", "Under Review, start year unconfirmed", 
    "", "", "", "", "", "", "", "", "", "", "", "", "Proposed Endangered, 2022", 
    "", ""), hibernation.strat = c("resident", "resident", "resident", 
    "migratory", "hibernating", "hibernating", "hibernating", 
    "hibernating", "unknown", "hibernating", "mixed", "resident", 
    "resident", "resident", "unknown", "migratory", "migratory", 
    "migratory", "resident", "resident", "migratory", "resident", 
    "resident", "mixed", "resident", "migratory", "migratory", 
    "resident", "resident", "unknown", "hibernating", "mixed", 
    "hibernating", "hibernating", "hibernating", "hibernating", 
    "hibernating", "hibernating", "hibernating", "hibernating", 
    "hibernating", "hibernating", "hibernating", "mixed", "hibernating", 
    "mixed", "unknown", "migratory", "migratory", "migratory", 
    "resident", "hibernating", "resident", "mixed"), phonic.group = c("Lof", 
    "None", "None", "Hif", "Lof", "Lof", "Lof", "Lof", "None", 
    "Lof", "Lof", "Lof", "Lof", "Lof", "Lof", "Lof", "Hif", "Lof", 
    "Lof", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
    "None", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
    "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
    "Hif", "Hif", "Hif", "Hif", "Lof", "Lof", "Hif", "Hif", "None", 
    "Lof"), notes = c("", "", "", "", "", "", "same as C. townsendii (subspecies)", 
    "same as C. townsendii (subspecies)", "only marginal/historical US records - winter behavior in US range not documented; $phonic.group reflects general vampire-bat biology (faint, short-range echolocation), not US-specific data", 
    "", "some individuals migrate to warmer areas in winter, others do not - genuinely mixed at species level per general accounts, LOWER CONFIDENCE on the exact split", 
    "", "", "very limited US (AZ) records", "poorly studied - winter/hibernation-site behavior not well documented for this species", 
    "", "", "", "same call/hibernation biology as mainland L. cinereus, but the Hawaiian population does not undertake the mainland's continental migration", 
    "LOWER CONFIDENCE call-frequency estimate (yellow bat group, less-studied)", 
    "recently split from L. blossevillii - LOWER CONFIDENCE, based on close congeners", 
    "LOWER CONFIDENCE call-frequency estimate (yellow bat group)", 
    "Caribbean population - LOWER CONFIDENCE, based on close congeners (L. borealis-type)", 
    "documented partial migrant - some individuals overwinter via torpor in the Deep South rather than migrating", 
    "LOWER CONFIDENCE call-frequency estimate (yellow bat group)", 
    "", "", "the ONLY North American bat documented to stay fully active year-round with no hibernation or migration, even in the desert", 
    "", "LOWER CONFIDENCE hibernation call: only a marginal edge-of-range US (TX/AZ) population, poorly documented", 
    "LOWER CONFIDENCE (desert Myotis, less-studied than eastern species)", 
    "documented species-level variability - some populations hibernate in caves, Florida populations largely remain active year-round", 
    "LOWER CONFIDENCE (mild-climate coastal populations may be less strict hibernators than assumed here)", 
    "", "", "", "", "", "", "LOWER CONFIDENCE (desert Myotis, less-studied)", 
    "", "", "", "documented species-level variability - northern populations hibernate, southern/border populations may remain active in mild winters", 
    "", "documented species-level variability - similar pattern to M. velifer/austroriparius", 
    "no confirmed current PR/US-territory population per $states.present (blank) - hibernation.strat reflects lack of a documented US-range population, not species biology generally; $phonic.group instead reflects general species/family biology (a loud, high-frequency fishing bat) since call type is a fixed physical trait independent of range presence", 
    "some populations migrate, southern populations may be more resident - classified migratory per general accounts, LOWER CONFIDENCE on the split", 
    "", "", "", "", "", "very well-documented species-level mix: most populations (e.g. the famous Bracken Cave, TX colony) migrate to Mexico for winter, but Florida/Gulf coast populations are non-migratory and active year-round"
    )), row.names = c(NA, -54L), class = "data.frame")

  # ---------------------------------------------------------------------------
  # Reference database 2: US states/territories (confirmed to be Josh's
  # "USAstate.names.database" - see @details - embedded from
  # NAstates.names.csv, 56 rows x 8 columns).
  # ---------------------------------------------------------------------------
  states <- structure(list(official.name = c("State of Alabama", "State of Alaska", 
"State of Arizona", "State of Arkansas", "State of California", 
"State of Colorado", "State of Connecticut", "State of Delaware", 
"State of Florida", "State of Georgia", "State of Hawaii", "State of Idaho", 
"State of Illinois", "State of Indiana", "State of Iowa", "State of Kansas", 
"Commonwealth of Kentucky", "State of Louisiana", "State of Maine", 
"State of Maryland", "Commonwealth of Massachusetts", "State of Michigan", 
"State of Minnesota", "State of Mississippi", "State of Missouri", 
"State of Montana", "State of Nebraska", "State of Nevada", "State of New Hampshire", 
"State of New Jersey", "State of New Mexico", "State of New York", 
"State of North Carolina", "State of North Dakota", "State of Ohio", 
"State of Oklahoma", "State of Oregon", "Commonwealth of Pennsylvania", 
"State of Rhode Island", "State of South Carolina", "State of South Dakota", 
"State of Tennessee", "State of Texas", "State of Utah", "State of Vermont", 
"Commonwealth of Virginia", "State of Washington", "State of West Virginia", 
"State of Wisconsin", "State of Wyoming", "District of Columbia", 
"Commonwealth of Puerto Rico", "Virgin Islands of the United States", 
"Territory of Guam", "Territory of American Samoa", "Commonwealth of the Northern Mariana Islands"
), short.name = c("Alabama", "Alaska", "Arizona", "Arkansas", 
"California", "Colorado", "Connecticut", "Delaware", "Florida", 
"Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", 
"Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
"Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
"Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", 
"New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", 
"Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
"Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
"West Virginia", "Wisconsin", "Wyoming", "Washington, D.C.", 
"Puerto Rico", "U.S. Virgin Islands", "Guam", "American Samoa", 
"Northern Mariana Islands"), code2 = c("AL", "AK", "AZ", "AR", 
"CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", 
"KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", 
"NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", 
"PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", 
"WI", "WY", "DC", "PR", "VI", "GU", "AS", "MP"), entity.type = c("State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"State", "State", "State", "State", "State", "State", "State", 
"Federal District", "Territory", "Territory", "Territory", "Territory", 
"Territory"), USFWS.Region = c("4", "7", "2", "4", "8", "6", 
"5", "5", "4", "4", "1", "1", "3", "3", "3", "6", "4", "4", "5", 
"5", "5", "3", "3", "4", "3", "6", "6", "8", "5", "5", "2", "5", 
"4", "6", "3", "2", "1", "5", "5", "4", "6", "4", "2", "6", "5", 
"5", "1", "5", "3", "6", "5", "4", "4", "1", "1", "1"), time.zone = c("6", 
"9", "7", "6", "8", "7", "5", "5", "5", "5", "10", "7", "6", 
"5", "6", "6", "5", "6", "5", "5", "5", "5", "6", "6", "6", "7", 
"6", "8", "5", "5", "7", "5", "5", "6", "5", "6", "8", "5", "5", 
"5", "6", "6", "6", "7", "5", "5", "8", "5", "6", "7", "5", "4", 
"4", "-10", "11", "-10"), dst.observed = c("TRUE", "TRUE", "FALSE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "FALSE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", 
"TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", "TRUE", 
"FALSE", "FALSE", "FALSE", "FALSE", "FALSE"), notes = c("", "", 
"Does not observe DST (Mountain Standard Time year-round; the Navajo Nation portion of AZ does observe DST, not broken out here)", 
"", "", "", "", "", "Panhandle (west of Apalachicola River, e.g. Pensacola) is Central; majority Eastern used here", 
"", "Does not observe DST", "Northern panhandle (~10 counties, e.g. Coeur d'Alene, Lewiston) is Pacific; majority Mountain used here", 
"", "NW corner (Chicago area) and SW corner (Evansville area) are Central; majority Eastern used here", 
"", "Far western counties are Mountain; majority Central used here", 
"", "", "", "", "", "Western Upper Peninsula (a few counties) is Central; majority Eastern used here", 
"", "", "", "", "Panhandle (Scottsbluff area) is Mountain; majority Central used here", 
"Nevada is Pacific by state law statewide; a few easternmost points are geographically nearer Mountain but not officially observed", 
"", "", "", "", "", "", "West is Mountain (~13 westernmost counties); majority Central used here", 
"", "Malheur County (far east) is Mountain; majority Pacific used here", 
"", "", "", "West is Mountain (~10 westernmost counties); majority Central used here", 
"East third (Knoxville/Chattanooga area) is Eastern; majority Central used here", 
"El Paso area (far west) is Mountain; majority Central used here", 
"", "", "", "", "", "", "", "", "Does not observe DST (Atlantic Standard Time year-round)", 
"Does not observe DST (Atlantic Standard Time year-round)", "Does not observe DST (Chamorro Standard Time, UTC+10 year-round)", 
"Does not observe DST (Samoa Standard Time, UTC-11 year-round)", 
"Does not observe DST (Chamorro Standard Time, UTC+10 year-round)"
)), row.names = c(NA, -56L), class = "data.frame")

  nabat[]  <- lapply(nabat,  function(col) trimws(as.character(col)))
  states[] <- lapply(states, function(col) trimws(as.character(col)))

  normalize <- function(x) {
    x <- as.character(x)
    x <- gsub("[-_]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }

  split.codes <- function(cell) {
    cell <- trimws(as.character(cell))
    if (is.na(cell) || cell == "") return(character(0))
    trimws(strsplit(cell, ",")[[1]])
  }

  federal.abbrev <- function(fedstatus) {
    switch(fedstatus,
      "Not Listed"          = "",
      "Under Review"        = "UR",
      "Endangered"          = "E",
      "Proposed Endangered" = "PE",
      fedstatus)
  }

  status.value <- function(bat.row, state.code2, presence.absence) {
    present <- state.code2 %in% split.codes(bat.row[["states.present"]])
    soc     <- state.code2 %in% split.codes(bat.row[["state.soc"]])
    listed  <- state.code2 %in% split.codes(bat.row[["states.listed"]])
    if (!present) return(presence.absence[2])
    if (soc) return("SC")
    if (listed) return("L")
    presence.absence[1]
  }

  bat.cols   <- c("latin", "common", "code4", "code6")
  state.cols <- c("official.name", "short.name", "code2")

  x <- if (is.data.frame(data)) unlist(lapply(data, as.character), use.names = FALSE) else as.character(data)
  x.norm <- normalize(x)

  bat.lookup   <- unlist(lapply(bat.cols,   function(cn) normalize(nabat[[cn]])),  use.names = FALSE)
  bat.rowidx   <- rep(seq_len(nrow(nabat)),  times = length(bat.cols))
  state.lookup <- unlist(lapply(state.cols, function(cn) normalize(states[[cn]])), use.names = FALSE)
  state.rowidx <- rep(seq_len(nrow(states)), times = length(state.cols))

  bat.match   <- bat.rowidx[match(x.norm, bat.lookup)]
  state.match <- state.rowidx[match(x.norm, state.lookup)]

  both <- !is.na(bat.match) & !is.na(state.match)
  if (any(both)) {
    cat(sprintf("WARNING: %d input(s) matched BOTH the bat and state reference tables (%s) - treated as bat matches (documented tie-break, not expected on real data).\n",
                sum(both), paste(unique(x[both]), collapse = ", ")))
  }

  unmatched <- x[is.na(bat.match) & is.na(state.match)]

  # preserve FIRST-SEEN order (not sorted) - matters for matrix column order
  bat.seen   <- bat.match[!is.na(bat.match)]
  state.seen <- state.match[!is.na(state.match)]
  bat.rows   <- bat.seen[!duplicated(bat.seen)]
  state.rows <- state.seen[!duplicated(state.seen)]

  if (length(bat.rows) == 0 && length(state.rows) == 0) {
    stop("No recognizable bat species or US state/territory identifiers were found anywhere in `data`.")
  }

  species.idx <- if (length(bat.rows) > 0) bat.rows else seq_len(nrow(nabat))
  state.idx   <- if (length(state.rows) > 0) state.rows else seq_len(nrow(states))

  if (length(unmatched) > 0) {
    shown <- head(unique(unmatched), 25)
    omitted.note <- if (length(unique(unmatched)) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(unique(unmatched)))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match either reference table: %s%s\n",
                 length(unmatched), paste(shown, collapse = ", "), omitted.note))
  }

  has.extirpated.col <- "states.extirpated" %in% names(nabat)
  if (species.extirpated && !has.extirpated.col) {
    cat("NOTE: species.extirpated = TRUE was requested, but the reference database has no 'states.extirpated' column yet - no '#' extirpation markers were applied.\n")
  }
  is.extirpated <- function(bat.row, state.code2) {
    if (!has.extirpated.col) return(FALSE)
    state.code2 %in% split.codes(bat.row[["states.extirpated"]])
  }

  if (!(table.type %in% c("state", "matrix"))) {
    stop(sprintf("table.type must be one of \"state\", \"matrix\" (got '%s')", table.type))
  }

  format.bat.label <- function(row) {
    name <- switch(batname.format,
      "full"   = sprintf("%s (%s)%s", row[["common"]], row[["latin"]], row[["hibernation.strat"]]),
      "both"   = sprintf("%s (%s)", row[["common"]], row[["latin"]]),
      "common" = row[["common"]],
      "latin"  = row[["latin"]],
      stop(sprintf("batname.format must be one of \"full\", \"both\", \"common\", \"latin\" (got '%s')", batname.format))
    )
    if (!grammar.dash) name <- gsub("-", " ", name)
    name
  }

  format.state.label <- function(row) {
    label <- switch(statename.format,
      "code2"         = row[["code2"]],
      "official.name" = row[["official.name"]],
      "short.name"    = row[["short.name"]],
      stop(sprintf("statename.format must be one of \"code2\", \"official.name\", \"short.name\" (got '%s')", statename.format))
    )
    if (!grammar.dash) label <- gsub("-", " ", label)
    label
  }

  species.labels <- vapply(species.idx, function(i) format.bat.label(nabat[i, ]), character(1))
  state.labels   <- vapply(state.idx,   function(j) format.state.label(states[j, ]), character(1))
  code.col       <- if (bat.code %in% c("code4", "code6")) bat.code else if (bat.code == "none") NULL else
                       stop(sprintf("bat.code must be one of \"code4\", \"code6\", \"none\" (got '%s')", bat.code))
  federal.codes  <- vapply(nabat[species.idx, "fedstatus"], federal.abbrev, character(1))

  if (table.type == "matrix") {

    out <- data.frame("Bat Species" = species.labels, check.names = FALSE, stringsAsFactors = FALSE)
    if (!is.null(code.col)) out[["Species Code"]] <- nabat[species.idx, code.col]
    if (phonic.group) out[["Phonic Group"]] <- nabat[species.idx, "phonic.group"]
    out[["Federal"]] <- federal.codes

    for (k in seq_along(state.idx)) {
      j <- state.idx[k]
      state.code2 <- states[j, "code2"]
      col <- vapply(species.idx, function(i) {
        val <- status.value(nabat[i, ], state.code2, presence.absence)
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        val
      }, character(1))
      out[[state.labels[k]]] <- col
    }

    out

  } else {
    # table.type == "state": one row per (state, species) PRESENT pair

    rows <- list()
    for (k in seq_along(state.idx)) {
      j <- state.idx[k]
      state.code2 <- states[j, "code2"]
      for (m in seq_along(species.idx)) {
        i <- species.idx[m]
        present <- state.code2 %in% split.codes(nabat[i, "states.present"])
        if (!present) next
        val <- status.value(nabat[i, ], state.code2, presence.absence)
        if (val == presence.absence[1]) val <- ""   # a row existing already implies presence
        if (is.extirpated(nabat[i, ], state.code2)) val <- paste0(val, "#")
        # Built POSITIONALLY, not as a named list: the model reuses "State"
        # as the header for BOTH the row's state and the listing-status
        # column, and a second `row[["State"]] <- val` would silently
        # overwrite the first entry instead of adding a column. Names are
        # applied once, after rbind, below.
        row <- list(state.labels[k], species.labels[m])
        if (!is.null(code.col)) row <- c(row, list(nabat[i, code.col]))
        row <- c(row, list(federal.codes[m]), list(val))
        if (phonic.group) row <- c(row, list(nabat[i, "phonic.group"]))
        rows[[length(rows) + 1]] <- row
      }
    }

    if (length(rows) == 0) {
      stop("No requested species are present in any of the requested states - nothing to build a 'state' table.type table from.")
    }

    out <- do.call(rbind.data.frame, c(rows, stringsAsFactors = FALSE))
    col.names <- c("State", "Bat Species")
    if (!is.null(code.col)) col.names <- c(col.names, "Species Code")
    col.names <- c(col.names, "Federal", "State")
    if (phonic.group) col.names <- c(col.names, "Phonic Group")
    names(out) <- col.names   # set directly so the duplicate "State" header survives literally
    out
  }
}
