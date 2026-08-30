#' Recode US bat species identifiers between common name, latin name, species codes, and status fields
#'
#' Given a vector or data frame containing any mix of common name, latin
#' (scientific) name, 4-letter species code, or 6-letter species code for
#' North American bat species, looks each value up in an internal reference
#' table and returns it re-expressed in a single chosen format
#' (\code{batname.format.out}). Matching ignores case, underscores, dashes, and
#' leading/trailing/extra whitespace, so formatting differences between the
#' input and the reference table (or between different inputs) don't cause a
#' false mismatch.
#'
#' @param data A vector, or a data frame, of bat species identifiers to
#'   recode. If a data frame is supplied, every column is recorded the same
#'   way (there is no column-selection argument) and comes back as a data
#'   frame of the same dimensions, with columns returned as character
#'   vectors.
#' @param batname.format.out Character, default \code{"common"}. The desired
#'   output format - must be one of the reference table's own headers:
#'   \code{"latin"}, \code{"common"}, \code{"code4"}, \code{"code6"},
#'   \code{"fedstatus"}, \code{"iucnstatus"}, \code{"states.listed"},
#'   \code{"states.present"}, \code{"states.end"}, \code{"states.the"},
#'   \code{"state.soc"}, \code{"fed.proposed"}, \code{"hibernation.strat"},
#'   \code{"phonic.group"}, or \code{"notes"}. Matching an input element
#'   to a reference row always uses \code{latin}/\code{common}/\code{code4}/
#'   \code{code6} only, regardless of \code{batname.format.out} - so, for
#'   example, \code{batname.format.out = "fedstatus"} looks a species up by any
#'   of its four names/codes and returns its federal listing status instead
#'   of another name/code. An unrecognized value is an error.
#'
#'   \code{"hibernation.strat"} is one of \code{"migratory"} (tree bats that
#'   head south for winter), \code{"hibernating"} (cave bats that go into
#'   torpor over winter), \code{"resident"} (active year-round, no
#'   hibernation), \code{"mixed"} (species with both migratory and
#'   non-migratory populations), or \code{"unknown"}. \code{"phonic.group"}
#'   is one of \code{"Lof"} (echolocation calls below 35 kHz), \code{"Hif"}
#'   (above 35 kHz), \code{"None"} (does not echolocate), or
#'   \code{"Unknown"}. Both are populated from general bat natural-history/
#'   acoustics literature, not a source file Josh supplied for these two
#'   columns specifically - see \code{NAbat.names.csv}'s own \code{$notes}
#'   column (also selectable via \code{batname.format.out = "notes"}) for the
#'   species where a call is genuinely split or lower-confidence.
#'
#'   Eight non-species detection/category labels are also recognized as
#'   ordinary rows in the same reference table (added 2026-08-27, per
#'   Josh): \code{"All detections"}, \code{"40KHzMyo"}, \code{"HiF"},
#'   \code{"LoF"}, \code{"HiFrag"}, \code{"LoFrag"}, \code{"Multiple"},
#'   \code{"Social"}. They match the same case-insensitive way as species
#'   names (e.g. \code{"hif"}, \code{"HIF"}, \code{"Hif"} all match), and
#'   \code{batname.format.out} values of \code{"latin"}/\code{"common"}/
#'   \code{"code4"}/\code{"code6"} all return the exact literal casing shown
#'   above. Every other \code{batname.format.out} (\code{"fedstatus"},
#'   \code{"states.present"}, \code{"phonic.group"}, etc.) returns
#'   \code{""} for these eight, since those columns don't apply to a
#'   non-species label.
#' @param grammar.dash Logical, default \code{TRUE}. Hyphens are ignored
#'   (treated the same as a space) when MATCHING an input value regardless of
#'   this flag. This flag only controls the OUTPUT: \code{TRUE} (default)
#'   returns matched values exactly as written in the reference table
#'   (hyphens kept, e.g. \code{"Silver-haired bat"}); \code{FALSE} replaces
#'   every hyphen in a matched value with a space instead (e.g.
#'   \code{"Silver haired bat"}).
#'
#' @return A vector (if \code{data} is a vector) or data frame (if
#'   \code{data} is a data frame) of the same length/dimensions as
#'   \code{data}, with every element re-expressed in \code{batname.format.out}.
#'   An input element with no match anywhere in the reference table is
#'   returned unchanged (not \code{NA}, no error).
#'
#' @details
#' If one or more input elements don't match anything in the reference
#' table, a warning is printed (not raised via \code{warning()} - a plain
#' \code{cat()} message, matching how similar diagnostics are reported
#' elsewhere in the \code{batz} package): \code{"WARNING: X inputs did not
#' match: ..."}, where X counts every unmatched INSTANCE (not just distinct
#' values), followed by the first 25 unique unmatched values (a note is
#' appended if more than 25 unique values were omitted from the printed
#' list).
#'
#' The reference table (54 North American bat species, as supplied in
#' Josh's \code{NAbat.names.csv}, now including \code{$hibernation.strat}/
#' \code{$phonic.group}/\code{$notes} added 2026-08-25, plus 8 non-species
#' detection/category label rows added 2026-08-27 - see \code{batname.format.out}
#' above) is embedded directly in this function - there is no
#' reference-file-path argument, since the spec's inputs are just
#' \code{data}/\code{batname.format.out}/\code{grammar.dash}. To update the
#' species list later, replace the \code{nabat.names} data frame inside
#' this function with a newer export of the same 15-column format.
#'
#' @examples
#' \dontrun{
#' batz.batusa_recode.names(c("epfu", "myotis_lucifugus", "Hoary bat"))
#' # -> "Big brown bat"    "Little brown bat"    "Hoary bat"
#'
#' batz.batusa_recode.names("epfu", batname.format.out = "latin")
#' # -> "Eptesicus fuscus"
#'
#' batz.batusa_recode.names("lano", batname.format.out = "common", grammar.dash = FALSE)
#' # -> "Silver haired bat"   (hyphen replaced with a space)
#'
#' batz.batusa_recode.names("myse", batname.format.out = "fedstatus")
#' # -> "Endangered"
#'
#' batz.batusa_recode.names("tabr", batname.format.out = "hibernation.strat")
#' # -> "mixed"   (most populations migrate to Mexico; Florida's is resident)
#'
#' batz.batusa_recode.names("mylu", batname.format.out = "phonic.group")
#' # -> "Hif"
#'
#' batz.batusa_recode.names(c("hif", "LOFRAG", "40khzmyo"))
#' # -> "HiF"      "LoFrag"   "40KHzMyo"
#' }
#'
#' @export
batz.batusa_recode.names <- function(data, batname.format.out = "common", grammar.dash = TRUE) {

  # ---------------------------------------------------------------------------
  # Reference database (Josh's real NAbat.names.csv, embedded as supplied -
  # 54 species x 15 columns, including $hibernation.strat/$phonic.group/
  # $notes added 2026-08-25, plus 8 non-species detection/category label
  # rows - All detections/40KHzMyo/HiF/LoF/HiFrag/LoFrag/Multiple/Social -
  # added 2026-08-27, per Josh). See @details above for how to update this.
  # ---------------------------------------------------------------------------
  nabat.names <- structure(list(latin = c("Antrozous pallidus", "Artibeus jamaicensis",
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
"Stenoderma rufum", "Tadarida brasiliensis", "All detections",
"40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", "Multiple", "Social"
), common = c("Pallid bat", "Jamaican fruit-eating bat", "Antillean fruit-eating bat",
"Mexican long-tongued bat", "Rafinesque's big-eared bat", "Townsend's big-eared bat",
"Ozark big-eared bat", "Virginia big-eared bat", "Hairy-legged vampire bat",
"Big brown bat", "Spotted bat", "Florida bonneted bat", "Greater bonneted bat",
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
"Canyon bat", "Tri-colored bat", "Red fruit bat", "Brazilian free-tailed bat",
"All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag",
"Multiple", "Social"), code4 = c("anpa", "arja", "brca", "chme",
"cora", "coto", "coti", "cotv", "diec", "epfu", "euma", "eufl",
"eupe", "euun", "idph", "lano", "labo", "laci", "lacs", "laeg",
"lafr", "lain", "lami", "lase", "laxa", "leni", "leye", "maca",
"momo", "mome", "myar", "myau", "myca", "myci", "myev", "mygr",
"myke", "myle", "mylu", "myoc", "myse", "myso", "myth", "myve",
"myvo", "myyu", "nole", "nyhu", "nyfe", "nyma", "pahe", "pesu",
"stru", "tabr", "All detections", "40KHzMyo", "HiF", "LoF", "HiFrag",
"LoFrag", "Multiple", "Social"), code6 = c("antpal", "artjam",
"bracav", "chomex", "corraf", "cortow", "cotoin", "cotovi", "dipeca",
"eptfus", "eudmac", "eumflo", "eumper", "eumund", "idiphy", "lasnoc",
"lasbor", "lascin", "lacise", "lasega", "lasfra", "lasint", "lasmin",
"lassem", "lasxan", "lepniv", "lepyer", "maccal", "molmol", "mormeg",
"myoaur", "myoaus", "myocal", "myocil", "myoevo", "myogri", "myokee",
"myolei", "myoluc", "myoocc", "myosep", "myosod", "myothy", "myovel",
"myovol", "myoyum", "noclep", "nychum", "nycfem", "nycmac", "parhes",
"persub", "steruf", "tadbra", "All detections", "40KHzMyo", "HiF",
"LoF", "HiFrag", "LoFrag", "Multiple", "Social"), fedstatus = c("Not Listed",
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed",
"Endangered", "Endangered", "Not Listed", "Not Listed", "Not Listed",
"Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed",
"Not Listed", "Endangered", "Endangered", "Not Listed", "Not Listed",
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered",
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed",
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered",
"Not Listed", "Not Listed", "Under Review", "Not Listed", "Endangered",
"Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed",
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed",
"Proposed Endangered", "Not Listed", "Not Listed", "", "", "",
"", "", "", "", ""), iucnstatus = c("Least Concern", "Least Concern",
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
"Least Concern", "", "", "", "", "", "", "", ""), states.listed = c("",
"", "", "AZ,CA", "", "", "", "", "", "", "", "FL", "", "", "",
"", "", "", "", "", "", "", "", "OK", "", "NM,TX", "", "", "",
"", "", "", "", "", "", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NH,NJ,NY,OH,OK,PA,TN,VA,VT,WV",
"CT,MA,ME,MI,NH,NJ,OH,PA,TN,VA,VT,WI", "", "", "", "", "", "",
"", "", "IN,KY,MI,OH", "", "", "", "", "", "", "", "", "", "",
"", "", "", ""), states.present = c("AZ,CA,CO,ID,KS,MT,NM,NV,OK,OR,TX,UT,WA",
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
"PR,VI", "AZ,CA,CO,FL,KS,NM,NV,OK,TX,UT", "", "", "", "", "",
"", "", ""), states.end = c("", "", "", "", "", "", "", "", "",
"", "", "FL", "", "", "", "", "", "", "", "", "", "", "", "",
"", "NM,TX", "", "", "", "", "", "", "", "", "", "", "", "NH",
"CT,MA,ME,NH,NJ,PA,VA,VT", "", "", "", "", "", "", "", "", "IN",
"", "", "", "", "", "", "", "", "", "", "", "", "", ""), states.the = c("",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "OK", "", "", "", "", "", "", "", "",
"", "", "", "", "", "PA,VT", "TN,WI", "", "", "", "", "", "",
"", "", "KY,MI", "", "", "", "", "", "", "", "", "", "", "",
"", "", ""), state.soc = c("", "", "", "AZ,CA", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NJ,NY,OH,OK,TN,VA,WV",
"MI,OH", "", "", "", "", "", "", "", "", "OH", "", "", "", "",
"", "", "", "", "", "", "", "", "", ""), fed.proposed = c("",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "",
"", "", "", "", "", "Under Review, start year unconfirmed", "",
"", "", "", "", "", "", "", "", "", "", "", "Proposed Endangered, 2022",
"", "", "", "", "", "", "", "", "", ""), hibernation.strat = c("resident",
"resident", "resident", "migratory", "hibernating", "hibernating",
"hibernating", "hibernating", "unknown", "hibernating", "mixed",
"resident", "resident", "resident", "unknown", "migratory", "migratory",
"migratory", "resident", "resident", "migratory", "resident",
"resident", "mixed", "resident", "migratory", "migratory", "resident",
"resident", "unknown", "hibernating", "mixed", "hibernating",
"hibernating", "hibernating", "hibernating", "hibernating", "hibernating",
"hibernating", "hibernating", "hibernating", "hibernating", "hibernating",
"mixed", "hibernating", "mixed", "unknown", "migratory", "migratory",
"migratory", "resident", "hibernating", "resident", "mixed",
"", "", "", "", "", "", "", ""), phonic.group = c("Lof", "None",
"None", "Hif", "Lof", "Lof", "Lof", "Lof", "None", "Lof", "Lof",
"Lof", "Lof", "Lof", "Lof", "Lof", "Hif", "Lof", "Lof", "Hif",
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "None", "Hif",
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif",
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif",
"Hif", "Lof", "Lof", "Hif", "Hif", "None", "Lof", "", "", "",
"", "", "", "", ""), notes = c("", "", "", "", "", "", "same as C. townsendii (subspecies)",
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
"", "", "", "", "", "very well-documented species-level mix: most populations (e.g. the famous Bracken Cave, TX colony) migrate to Mexico for winter, but Florida/Gulf coast populations are non-migratory and active year-round",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\".",
"Category/detection-type label (not a species), added 2026-08-27 per Josh's request. latin/common/code4/code6 all hold this same literal string, so it matches case-insensitively (same normalize() rule as species rows) and any of those four batname.format.out values return the exact given casing. Species-only batname.format.out columns return \"\"."
)), row.names = c(NA, -62L), class = "data.frame")

  match.cols <- c("latin", "common", "code4", "code6")

  if (!(batname.format.out %in% names(nabat.names))) {
    stop(sprintf("batname.format.out must be one of the reference database's headers: %s (got '%s')",
                  paste(names(nabat.names), collapse = ", "), batname.format.out))
  }

  reference <- nabat.names
  reference[] <- lapply(reference, function(col) trimws(as.character(col)))

  # matching-only normalization: fold case, treat underscores/dashes as
  # spaces, collapse/trim whitespace. Never affects the VALUE returned.
  normalize <- function(x) {
    x <- as.character(x)
    x <- gsub("[-_]+", " ", x)
    x <- gsub("\\s+", " ", x)
    x <- trimws(x)
    tolower(x)
  }

  recode.vec <- function(x) {
    lookup.values <- unlist(lapply(match.cols, function(cn) normalize(reference[[cn]])),
                             use.names = FALSE)
    lookup.rowidx <- rep(seq_len(nrow(reference)), times = length(match.cols))

    x.chr  <- as.character(x)
    x.norm <- normalize(x.chr)

    match.idx <- match(x.norm, lookup.values)
    row.idx   <- lookup.rowidx[match.idx]   # NA where match.idx is NA
    found     <- !is.na(row.idx)

    out <- x.chr
    out[found] <- as.character(reference[[batname.format.out]][row.idx[found]])

    if (!grammar.dash) {
      out[found] <- gsub("-", " ", out[found])
    }

    list(values = out, unmatched = x.chr[!found])
  }

  if (is.data.frame(data)) {
    results <- lapply(data, recode.vec)
    out <- as.data.frame(lapply(results, function(r) r$values), stringsAsFactors = FALSE)
    names(out) <- names(data)
    unmatched.all <- unlist(lapply(results, function(r) r$unmatched), use.names = FALSE)
  } else {
    result <- recode.vec(data)
    out <- result$values
    unmatched.all <- result$unmatched
  }

  if (length(unmatched.all) > 0) {
    warning.vector <- unique(unmatched.all)
    shown <- head(warning.vector, 25)
    omitted.note <- if (length(warning.vector) > 25) {
      sprintf(" (showing first 25 of %d unique unmatched values)", length(warning.vector))
    } else ""
    cat(sprintf("WARNING: %d inputs did not match: %s%s\n",
                 length(unmatched.all), paste(shown, collapse = ", "), omitted.note))
  }

  out
}
