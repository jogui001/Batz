#' Bat reference tables (internal)
#'
#' Added 2026-10-23, per Josh. One shared copy of the bat reference tables,
#' used by every batz function that works with bat names
#' (\code{\link{batz.batusa_recode.names}}, \code{\link{batz.batusa_list.species}},
#' \code{\link{batz.merge_vetted.acoustics}}), taken from Josh's
#' \code{Batz reference tables_20261023_v2.xlsx} (MAMM-Batz Package/8reference
#' tables - the master copy; update these tables from it):
#' \describe{
#'   \item{\code{bat.ref.names()}}{tab \code{bat.names} - 54 species + 8
#'     category labels, plus the new \code{$family} column (blank for the
#'     category labels).}
#'   \item{\code{bat.ref.groups()}}{tab \code{BAT Groupings} - species pairs
#'     and genus-level groups (e.g. \code{LUSO} = little brown bat / Indiana
#'     bat).}
#'   \item{\code{bat.ref.freqclasses()}}{tab \code{NABATFrequency Classes} -
#'     frequency classes and other call labels (\code{HiF}, \code{40kMyo},
#'     \code{NoID}, ...). \code{$Label} holds the spellings found in vetting
#'     files.}
#'   \item{\code{bat.ref.filter.regions(df, regions)}}{keeps the rows whose
#'     \code{$Region} matches any of \code{regions} (rows marked \code{All}
#'     are always kept). A \code{$Region} cell may list several regions
#'     separated by \code{;} or \code{,}; the row is kept if any of them
#'     matches. Case and quotes are ignored.}
#'   \item{\code{aru.ref.types()}}{tab \code{aru.types} (added 2026-10-23):
#'     \code{$header} = input-file headers that may name the recorder model,
#'     \code{$recorder.type} = recognized recorder models. Used by
#'     \code{\link{batz.merge_vetted.acoustics}} for \code{$aru.model}.}
#' }
#' Non-breaking spaces in the spreadsheet cells were changed to normal spaces
#' and trimmed; nothing else was changed. After any edit run
#' \code{stopifnot(all(sapply(tbl, length) == nrow(tbl)))} on each table.
#' @keywords internal
#' @noRd
bat.ref.names <- function() {
structure(list(Region = c("NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "All", "All", "All", 
"All", "All", "All", "All", "All"), scientific_name = c("Antrozous pallidus", "Artibeus jamaicensis", 
"Brachyphylla cavernarum", "Choeronycteris mexicana", "Corynorhinus rafinesquii", "Corynorhinus townsendii", 
"Corynorhinus townsendii ingens", "Corynorhinus townsendii virginianus", "Diphylla ecaudata", "Eptesicus fuscus", 
"Euderma maculatum", "Eumops floridanus", "Eumops perotis", "Eumops underwoodi", "Idionycteris phyllotis", 
"Lasionycteris noctivagans", "Lasiurus borealis", "Lasiurus cinereus", "Lasiurus cinereus semotus", "Lasiurus ega", 
"Lasiurus frantzii", "Lasiurus intermedius", "Lasiurus minor", "Lasiurus seminolus", "Lasiurus xanthinus", 
"Leptonycteris nivalis", "Leptonycteris yerbabuenae", "Macrotus californicus", "Molossus molossus", "Mormoops megalophylla", 
"Myotis auriculus", "Myotis austroriparius", "Myotis californicus", "Myotis ciliolabrum", "Myotis evotis", 
"Myotis grisescens", "Myotis keenii", "Myotis leibii", "Myotis lucifugus", "Myotis occultus", "Myotis septentrionalis", 
"Myotis sodalis", "Myotis thysanodes", "Myotis velifer", "Myotis volans", "Myotis yumanensis", "Noctilio leporinus", 
"Nycticeius humeralis", "Nyctinomops femorosaccus", "Nyctinomops macrotis", "Parastrellus hesperus", 
"Perimyotis subflavus", "Stenoderma rufum", "Tadarida brasiliensis", "All detections", "40KHzMyo", "HiF", 
"LoF", "HiFrag", "LoFrag", "Multiple Bats", "Social"), common_name = c("Pallid bat", "Jamaican fruit-eating bat", 
"Antillean fruit-eating bat", "Mexican long-tongued bat", "Rafinesque's big-eared bat", "Townsend's big-eared bat", 
"Ozark big-eared bat", "Virginia big-eared bat", "Hairy-legged vampire bat", "Big brown bat", "Spotted bat", 
"Florida bonneted bat", "Greater bonneted bat", "Underwood's bonneted bat", "Allen's big-eared bat", 
"Silver-haired bat", "Eastern red bat", "Hoary bat", "Hawaiian hoary bat", "Southern yellow bat", "Desert Red Bat", 
"Northern yellow bat", "Minor red bat", "Seminole bat", "Western yellow bat", "Mexican long-nosed bat", 
"Lesser long-nosed bat", "California leaf-nosed bat", "Pallas' mastiff bat", "Peter's ghost-faced bat", 
"Southwestern myotis", "Southeastern myotis", "California myotis", "Western small-footed myotis", "Long-eared myotis", 
"Gray bat", "Keen's myotis", "Eastern small-footed myotis", "Little brown bat", "Arizona myotis", "Northern long-eared bat", 
"Indiana bat", "Fringed myotis", "Cave bat myotis", "Long-legged myotis", "Yuma myotis", "Greater bulldog bat", 
"Evening bat", "Pocketed free-tailed bat", "Big free-tailed bat", "Canyon bat", "Tri-colored bat", "Red fruit bat", 
"Brazilian free-tailed bat", "All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", "Multiple Bats", 
"Social"), code4 = c("anpa", "arja", "brca", "chme", "cora", "coto", "coti", "cotv", "diec", "epfu", 
"euma", "eufl", "eupe", "euun", "idph", "lano", "labo", "laci", "lacs", "laeg", "lafr", "lain", "lami", 
"lase", "laxa", "leni", "leye", "maca", "momo", "mome", "myar", "myau", "myca", "myci", "myev", "mygr", 
"myke", "myle", "mylu", "myoc", "myse", "myso", "myth", "myve", "myvo", "myyu", "nole", "nyhu", "nyfe", 
"nyma", "pahe", "pesu", "stru", "tabr", "All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", 
"Multiple", "Social"), code6 = c("antpal", "artjam", "bracav", "chomex", "corraf", "cortow", "cotoin", 
"cotovi", "dipeca", "eptfus", "eudmac", "eumflo", "eumper", "eumund", "idiphy", "lasnoc", "lasbor", "lascin", 
"lacise", "lasega", "lasfra", "lasint", "lasmin", "lassem", "lasxan", "lepniv", "lepyer", "maccal", "molmol", 
"mormeg", "myoaur", "myoaus", "myocal", "myocil", "myoevo", "myogri", "myokee", "myolei", "myoluc", "myoocc", 
"myosep", "myosod", "myothy", "myovel", "myovol", "myoyum", "noclep", "nychum", "nycfem", "nycmac", "parhes", 
"persub", "steruf", "tadbra", "All detections", "40KHzMyo", "HiF", "LoF", "HiFrag", "LoFrag", "Multiple", 
"Social"), family = c("Vespertilionidae", "Phyllostomidae", "Phyllostomidae", "Phyllostomidae", "Vespertilionidae", 
"Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Phyllostomidae", "Vespertilionidae", "Vespertilionidae", 
"Molossidae", "Molossidae", "Molossidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", 
"Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", 
"Vespertilionidae", "Vespertilionidae", "Phyllostomidae", "Phyllostomidae", "Phyllostomidae", "Molossidae", 
"Mormoopidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", 
"Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", 
"Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Vespertilionidae", "Noctilionidae", 
"Vespertilionidae", "Molossidae", "Molossidae", "Vespertilionidae", "Vespertilionidae", "Phyllostomidae", 
"Molossidae", "", "", "", "", "", "", "", ""), listing.status_federal = c("Not Listed", "Not Listed", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered", "Endangered", "Not Listed", "Not Listed", 
"Not Listed", "Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered", 
"Endangered", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Endangered", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Endangered", "Not Listed", "Not Listed", "Under Review", "Not Listed", "Endangered", "Endangered", 
"Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", "Not Listed", 
"Not Listed", "Proposed Endangered", "Not Listed", "Not Listed", "", "", "", "", "", "", "", ""), listing.status_iucn = c("Least Concern", 
"Least Concern", "Least Concern", "Near Threatened", "Least Concern", "Least Concern", "", "", "Least Concern", 
"Least Concern", "Least Concern", "Vulnerable", "Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", "", "Least Concern", "Vulnerable", 
"Least Concern", "Least Concern", "Endangered", "Vulnerable", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", "Least Concern", "Vulnerable", "Least Concern", 
"Endangered", "Endangered", "Least Concern", "Near Threatened", "Near Threatened", "Least Concern", "Least Concern", 
"Least Concern", "Least Concern", "Least Concern", "Least Concern", "Least Concern", "Least Concern", 
"Least Concern", "Vulnerable", "Near Threatened", "Least Concern", "", "", "", "", "", "", "", ""), states.listed = c("", 
"", "", "AZ,CA", "", "", "", "", "", "", "", "FL", "", "", "", "", "", "", "", "", "", "", "", "OK", 
"", "NM,TX", "", "", "", "", "", "", "", "", "", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NH,NJ,NY,OH,OK,PA,TN,VA,VT,WV", 
"CT,MA,ME,MI,NH,NJ,OH,PA,TN,VA,VT,WI", "", "", "", "", "", "", "", "", "IN,KY,MI,OH", "", "", "", "", 
"", "", "", "", "", "", "", "", "", ""), states.present = c("AZ,CA,CO,ID,KS,MT,NM,NV,OK,OR,TX,UT,WA", 
"PR", "PR,VI", "AZ,CA,NM,TX", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,SC,TN,VA,WV", "AR,AZ,CA,CO,ID,KS,KY,MO,MT,NC,ND,NE,NM,NV,OK,OR,SD,TX,UT,VA,WA,WV,WY", 
"AR,MO,OK", "KY,NC,VA,WV", "TX", "AK,AL,AR,AZ,CA,CO,CT,DC,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,CO,MT,NM,NV,OR,UT,WA,WY", "FL", "AZ,CA,NM,TX", "AZ", "AZ,CA,CO,NM,NV,UT", "AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"AL,AR,CO,CT,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,HI,IA,ID,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,TX,UT,VA,VT,WA,WI,WV,WY", 
"HI", "AZ,CA,NM,TX", "AZ,CA,NM,TX", "AL,FL,GA,LA,MS,NC,PA,SC,TX,VA", "PR", "AL,AR,FL,GA,KY,LA,MO,MS,NC,OK,SC,TN,TX,VA", 
"AZ,CA,NM", "AZ,NM,TX", "AZ,CA,NM", "AZ,CA,NV", "FL", "AZ,TX", "AZ,NM", "AL,AR,FL,GA,IL,IN,KY,LA,MS,NC,OK,SC,TN,TX", 
"AZ,CA,CO,ID,MT,NM,NV,OR,TX,UT,WA,WY", "AZ,CA,CO,ID,KS,MT,ND,NE,NM,NV,OK,OR,SD,TX,UT,WA,WY", "AZ,CA,CO,ID,MT,ND,NM,NV,OR,SD,UT,WA,WY", 
"AL,AR,GA,IL,IN,KS,KY,MO,MS,NC,OK,TN,VA,WV", "AK,WA", "AL,AR,CT,GA,KY,MA,MD,ME,MI,MO,NC,NH,NJ,NY,OH,OK,PA,RI,TN,VA,VT,WV", 
"AK,AL,AR,AZ,CA,CO,CT,DE,FL,GA,IA,ID,IL,IN,KS,KY,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NM,NV,NY,OH,OK,OR,PA,RI,SC,SD,TN,UT,VA,VT,WA,WI,WV,WY", 
"AZ,CA,NM", "AL,AR,CT,DE,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,MT,NC,ND,NE,NH,NJ,NY,OH,OK,PA,RI,SC,SD,TN,VA,VT,WI,WV,WY", 
"AL,AR,CT,IA,IL,IN,KY,MD,MI,MO,NC,NJ,NY,OH,OK,PA,TN,VA,VT,WV", "AZ,CA,CO,NM,NV,OR,SD,TX,UT,WA,WY", "AZ,CA,KS,NM,OK,TX", 
"AK,CA,CO,ID,MT,ND,NE,NM,OR,SD,TX,WY", "CA,CO,ID,MT,NV,OR,TX,UT,WA", "", "AL,AR,FL,GA,IA,IL,IN,KS,KY,LA,MD,MI,MN,MO,MS,NC,NE,OH,OK,PA,SC,TN,TX,VA,WI,WV", 
"AZ,CA,NM,TX", "CA,NV,TX,UT", "AZ,CA,CO,NM,NV,OK,TX,UT,WA", "AL,AR,CO,CT,DC,DE,FL,GA,IA,IL,IN,KS,KY,LA,MA,MD,ME,MI,MN,MO,MS,NC,NE,NH,NJ,NM,NY,OH,OK,PA,RI,SC,SD,TN,TX,VA,VT,WI,WV,WY", 
"PR,VI", "AZ,CA,CO,FL,KS,NM,NV,OK,TX,UT", "", "", "", "", "", "", "", ""), states.endangered = c("", 
"", "", "", "", "", "", "", "", "", "", "FL", "", "", "", "", "", "", "", "", "", "", "", "", "", "NM,TX", 
"", "", "", "", "", "", "", "", "", "", "", "NH", "CT,MA,ME,NH,NJ,PA,VA,VT", "", "", "", "", "", "", 
"", "", "IN", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), states.threatened = c("", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "PA,VT", "TN,WI", "", "", "", "", "", "", "", "", "KY,MI", "", "", 
"", "", "", "", "", "", "", "", "", "", "", ""), state.sppofconcern = c("", "", "", "AZ,CA", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "AK,WA", "CT,GA,MA,MD,MO,NC,NJ,NY,OH,OK,TN,VA,WV", "MI,OH", "", "", "", "", "", "", "", 
"", "OH", "", "", "", "", "", "", "", "", "", "", "", "", "", ""), listing.status_federal.proposed = c("", 
"", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
"", "", "", "", "", "", "", "", "", "", "", "Under Review, start year unconfirmed", "", "", "", "", "", 
"", "", "", "", "", "", "", "Proposed Endangered, 2022", "", "", "", "", "", "", "", "", "", ""), hibernation.strategy = c("resident", 
"resident", "resident", "migratory", "hibernating", "hibernating", "hibernating", "hibernating", "unknown", 
"hibernating", "mixed", "resident", "resident", "resident", "unknown", "migratory", "migratory", "migratory", 
"resident", "resident", "migratory", "resident", "resident", "mixed", "resident", "migratory", "migratory", 
"resident", "resident", "unknown", "hibernating", "mixed", "hibernating", "hibernating", "hibernating", 
"hibernating", "hibernating", "hibernating", "hibernating", "hibernating", "hibernating", "hibernating", 
"hibernating", "mixed", "hibernating", "mixed", "unknown", "migratory", "migratory", "migratory", "resident", 
"hibernating", "resident", "mixed", "", "", "", "", "", "", "", ""), phonic.group = c("Lof", "None", 
"None", "Hif", "Lof", "Lof", "Lof", "Lof", "None", "Lof", "Lof", "Lof", "Lof", "Lof", "Lof", "Lof", "Hif", 
"Lof", "Lof", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "None", "Hif", "Hif", "Hif", "Hif", 
"Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", "Hif", 
"Hif", "Lof", "Lof", "Hif", "Hif", "None", "Lof", "", "", "", "", "", "", "", ""), notes = c("", "", 
"", "", "", "", "same as C. townsendii (subspecies)", "same as C. townsendii (subspecies)", "only marginal/historical US records - winter behavior in US range not documented; $phonic.group reflects general vampire-bat biology (faint, short-range echolocation), not US-specific data", 
"", "some individuals migrate to warmer areas in winter, others do not - genuinely mixed at species level per general accounts, LOWER CONFIDENCE on the exact split", 
"", "", "very limited US (AZ) records", "poorly studied - winter/hibernation-site behavior not well documented for this species", 
"", "", "", "same call/hibernation biology as mainland L. cinereus, but the Hawaiian population does not undertake the mainland's continental migration", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group, less-studied)", "recently split from L. blossevillii - LOWER CONFIDENCE, based on close congeners", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group)", "Caribbean population - LOWER CONFIDENCE, based on close congeners (L. borealis-type)", 
"documented partial migrant - some individuals overwinter via torpor in the Deep South rather than migrating", 
"LOWER CONFIDENCE call-frequency estimate (yellow bat group)", "", "", "the ONLY North American bat documented to stay fully active year-round with no hibernation or migration, even in the desert", 
"", "LOWER CONFIDENCE hibernation call: only a marginal edge-of-range US (TX/AZ) population, poorly documented", 
"LOWER CONFIDENCE (desert Myotis, less-studied than eastern species)", "documented species-level variability - some populations hibernate in caves, Florida populations largely remain active year-round", 
"LOWER CONFIDENCE (mild-climate coastal populations may be less strict hibernators than assumed here)", 
"", "", "", "", "", "", "LOWER CONFIDENCE (desert Myotis, less-studied)", "", "", "", "documented species-level variability - northern populations hibernate, southern/border populations may remain active in mild winters", 
"", "documented species-level variability - similar pattern to M. velifer/austroriparius", "no confirmed current PR/US-territory population per $states.present (blank) - hibernation.strat reflects lack of a documented US-range population, not species biology generally; $phonic.group instead reflects general species/family biology (a loud, high-frequency fishing bat) since call type is a fixed physical trait independent of range presence", 
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
}

#' @keywords internal
#' @noRd
bat.ref.groups <- function() {
structure(list(Region = c("NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT"), common_name = c("Pallid bat, big brown bat", "Rafinesque's big-eared bat, Ozark big-eared bat", 
"Rafinesque's big-eared bat, Townsend's big-eared bat", "Rafinesque's big-eared bat, Virginia big-eared bat", 
"Big brown bat, eastern red bat", "Big brown bat, Hoary bat", "Big brown bat, silver-haired bat", "Big brown bat, Western yellow bat", 
"Big brown bat, little brown bat", "Big brown bat, Evening bat", "Big brown bat, Brazilian free-tailed bat", 
"Spotted bat, Greater bonneted bat", "Spotted bat, Allen's big-eared bat", "Greater bonneted bat, Big free-tailed bat", 
"Eastern red bat, Desert Red Bat", "Eastern red bat, Northern yellow bat", "Eastern red bat, Seminole bat", 
"Eastern red bat, little brown bat", "Eastern red bat, evening bat", "Eastern red bat, tricolored bat", 
"Hoary bat, silver-haired bat", "Hoary bat, Pocketed free-tailed bat", "Hoary bat, Mexican free-tailed bat", 
"Desert red bat, Western yellow bat", "Western red bat, canyon bat", "Silver-haired bat, Brazilian free-tailed bat", 
"Unknown species in the Leptonycteris genus", "Southwestern myotis, Long-eared myotis", "Southeastern myotis, Eastern small-footed myotis", 
"Southeastern myotis, Indiana bat", "California myotis, western small-footed myotis", "California myotis, Yuma myotis", 
"Western small-footed myotis, long-legged myotis", "Long-eared myotis, Northern long-eared bat", "Long-eared myotis , fringed myotis", 
"Little brown bat, western small-footed myotis", "Little brown bat, Arizona myotis", "Little brown bat, northern long-eared bat", 
"Little brown bat, Indiana bat", "Little brown bat, Cave bat myotis", "Little brown bat, long-legged myotis", 
"Little brown bat, Yuma myotis", "Northern long-eared bat, Indiana bat", "Unknown species in the Myotis genus", 
"Pocketed free-tailed bat, Big free-tailed bat", "Pocketed free-tailed bat, Brazilian free-tailed bat", 
"Unknown species in the Nyctinomops genus", "Canyon bat, Tri-colored bat"), scientific_name = c("Antrozous pallidus, Eptesicus fuscus", 
"Corynorhinus rafinesquii, Corynorhinus townsendii ingens", "Corynorhinus rafinesquii, Corynorhinus townsendii", 
"Corynorhinus rafinesquii, Corynorhinus townsendii virginianus", "Eptesicus fuscus, Lasiurus borealis", 
"Eptesicus fuscus, Lasiurus cinereus", "Eptesicus fuscus, Lasionycteris noctivagans", "Eptesicus fuscus, Lasiurus xanthinus", 
"Eptesicus fuscus, Myotis lucifugus", "Eptesicus fuscus, Nycticeius humeralis", "Eptesicus fuscus, Tadarida brasiliensis", 
"Euderma maculatum, Eumops perotis", "Euderma maculatum, Idionycteris phyllotis", "Eumops perotis, Nyctinomops macrotis", 
"Lasiurus borealis, Lasiurus frantzii", "Lasiurus borealis, Lasiurus intermedius", "Lasiurus borealis, Lasiurus seminolus", 
"Lasiurus borealis, Myotis lucifugus", "Lasiurus borealis, Nycticeus humeralis", "Lasiurus borealis, Perimyotis subflavus", 
"Lasiurus cinereus, Lasionycteris noctivagans", "Lasiurus cinereus, Nyctinomops femorosaccus", "Lasiurus cinereus, Tadarida brasiliensis", 
"Lasiurus frantzii, Lasiurus xanthinus", "Lasiurus frantzii, Parastrellus hesperus", "Lasiurus noctivagans, Tadarida brasiliensis", 
"Leptonycteris sp.", "Myotis auriculus, Myotis evotis", "Myotis austroriparius, Myotis leibii", "Myotis austroriparius, Myotis sodalis", 
"Myotis californicus, Myotis ciliolabrum", "Myotis californicus, Myotis yumanensis", "Myotis ciliolabrum, Myotis volans", 
"Myotis evotis, Myotis septentrionalis", "Myotis evotis, Myotis thysanodes", "Myotis lucifugus, Myotis ciliolabrum", 
"Myotis lucifugus, Myotis occultus", "Myotis lucifugus, Myotis septentrionalis", "Myotis lucifugus, Myotis sodalis", 
"Myotis lucifugus, Myotis velifer", "Myotis lucifugus, Myotis volans", "Myotis lucifugus, Myotis yumanensis", 
"Myotis septentrionalis, Myotis sodalis", "Myotis sp.", "Nyctinomops femorosaccus, Nyctinomops macrotis", 
"Nyctinomops femorosaccus, Tadarida brasiliensis", "Nyctinomops sp.", "Parastrellus hesperus, Perimyotis subflavus"
), code4 = c("ANPAEPFU", "CORACOTI", "CORACOTO", "CORACOTV", "EPFULABO", "EPFULACI", "EPFULANO", "EPFULAXA", 
"EPFUMYLU", "EPFUNYHU", "EPFUTABR", "EUMAEUPE", "EUMAIDPH", "EUPENYMA", "LABOLAFR", "LABOLAIN", "LABOLASE", 
"LABOMYLU", "LABONYHU", "LABOPESU", "LACILANO", "LACINYFE", "LACITABR", "LAFRLAXA", "LAFRPAHE", "LANOTABR", 
"LESP", "MYARMYEV", "MYAUMYLE", "MYAUMYSO", "MYCAMYCI", "MYCAMYYU", "MYCIMYVO", "MYEVMYSE", "MYEVMYTH", 
"MYLUMYCI", "MYLUMYOC", "MYLUMYSE", "LUSO", "MYLUMYVE", "MYLUMYVO", "MYLUMYYU", "MYSEMYSO", "MYSP", "NYFENYMA", 
"NYFETABR", "NYSP", "PAHEPESU"), code6 = c("ANPAEPFU", "CORACOTI", "CORACOTO", "CORACOTV", "EPFULABO", 
"EPFULACI", "EPFULANO", "EPFULAXA", "EPFUMYLU", "EPFUNYHU", "EPFUTABR", "EUMAEUPE", "EUMAIDPH", "EUPENYMA", 
"LABOLAFR", "LABOLAIN", "LABOLASE", "LABOMYLU", "LABONYHU", "LABOPESU", "LACILANO", "LACINYFE", "LACITABR", 
"LAFRLAXA", "LAFRPAHE", "LANOTABR", "LESP", "MYARMYEV", "MYAUMYLE", "MYAUMYSO", "MYCAMYCI", "MYCAMYYU", 
"MYCIMYVO", "MYEVMYSE", "MYEVMYTH", "MYLUMYCI", "MYLUMYOC", "MYLUMYSE", "MYLUMYSO", "MYLUMYVE", "MYLUMYVO", 
"MYLUMYYU", "MYSEMYSO", "MYSP", "NYFENYMA", "NYFETABR", "NYSP", "PAHEPESU")), row.names = c(NA, -48L), class = "data.frame")
}

#' @keywords internal
#' @noRd
bat.ref.freqclasses <- function() {
structure(list(Region = c("NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", 
"NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "NABAT", "All"
), Definition = c("Various species with pulses that have a minim um frequency of approximately 15-25 kHz", 
"Various species with pulses that have a minimum frequency of approximately 35-45 kHz", "Various species of Myotis with pulses that have a minimum frequency of approximately 35-45 kHz", 
"Myotis volans or other species of Myotis with pulses that have a minimum frequency of approximately 35-45 kHz.", 
"Various species with pulses that have a minimum frequency of approximately 50 kHz.", "Various species with pulses having a minimum frequency higher than ~30 kHz", 
"Various species with pulses having a minimum frequency higher than ~30 kHz", "Various species with pulses having a minimum frequency higher than ~30 kHz", 
"Two or more bats from distinct frequency classes vocalizing simultaneously within a recording.", "Various species with pulses having a minimum frequency lower than ~30 kHz", 
"Various species with pulses having a minimum frequency lower than ~30 kHz", "Various species with pulses having a minimum frequency lower than ~30 kHz", 
"Noise", "Not a bat", "Bat, but no grouping or user-defined category applies", "Various species with pulses having a minimum frequency lower than ~20 kHz.", 
"Had no species-specific characteristics as it was social in nature", "More than one bat in a recording", 
"The call has fewer than five good-quality pulses above 35KHz", "The call has fewer than five good-quality pulses below 35KHz", 
"All ultrasonic calls that are bats including fragments and unidentified species"), Label = c("25k", 
"40k", "40kMyo", "40kmyomyvo", "50k", "HighF/HiF", "HighF", "HiF", "Hilo", "LowF/LoF", "LoF", "LowF", 
"NOISE", "NOTBAT", "NoID", "lt20k", "Social", "2BAT", "HiFrag", "LoFrag", "All detections"), scientific_name = c("Unidentifiable Species 35-45 kHz", 
"Unidentifiable Species 35-45 kHz", "Unidentifiable Myotis Species", "Myotis volans or other Myotis spp.", 
"Unidentifiable Species > 50 kHz", "Unidentifiable Species > 30 kHz", "Unidentifiable Species > 30 kHz", 
"Unidentifiable Species > 30 kHz", "Multiple bats High & Low Frequencies", "Unidentifiable Species < 30 kHz", 
"Unidentifiable Species < 30 kHz", "Unidentifiable Species < 30 kHz", "Noise", "Not a bat", "Unknown bat species", 
"lt20k", "Social Calls", "Multiple bats", "HiFrag", "LoFrag", "All detections"), common_name = c("Unidentifiable Species 15-25 kHz", 
"Unidentifiable Species 35-45 kHz", "Unidentifiable Myotis Species", "Myotis volans or other Myotis spp.", 
"Unidentifiable Species > 50 kHz", "Unidentifiable Species > 30 kHz", "Unidentifiable Species > 30 kHz", 
"Unidentifiable Species > 30 kHz", "Multiple bats High & Low Frequencies", "Unidentifiable Species < 30 kHz", 
"Unidentifiable Species < 30 kHz", "Unidentifiable Species < 30 kHz", "Noise", "Not a bat", "Unknown bat species", 
"lt20k", "Social Calls", "Multiple bats", "HiFrag", "LoFrag", "All detections"), code4 = c("25k", "40k", 
"40KHzMyo", "40kmyomyvo", "50k", "HiF", "HiF", "HiF", "Hilo", "LowF/LoF", "LoF", "LoF", "LoF", "NOTBAT", 
"NoID", "lt20k", "Social", "Multiple", "HiFrag", "LoFrag", "All detections"), code6 = c("25k", "40k", 
"40KHzMyo", "40kmyomyvo", "50k", "HiF", "HiF", "HiF", "Hilo", "LowF/LoF", "LoF", "LoF", "LoF", "NOTBAT", 
"NoID", "lt20k", "Social", "Multiple", "HiFrag", "LoFrag", "All detections")), row.names = c(NA, -21L
), class = "data.frame")
}

#' @keywords internal
#' @noRd
aru.ref.types <- function() {
list(header = c("model", "MODEL", "aru.model", "aru.type"), recorder.type = c("AudioMoth", "Song Meter Micro 2", 
"Song Meter Mini 2", "Song Meter SM5", "Song Meter Mini Bat 2", "Frontier Labs BAR-LT", "Titley Chorus", 
"SM1", "SM2", "SM3", "SM4", "SM5", "SMART", "Chorus", "Titley Ranger", "Ranger", "Peterson DX500", "DX500"
))
}

#' @keywords internal
#' @noRd
bat.ref.split.regions <- function(x) {
  x <- unlist(strsplit(as.character(x), "[;,]"))
  x <- tolower(trimws(gsub("[\"'\u201c\u201d]", "", x)))
  x[!is.na(x) & nzchar(x)]
}

#' @keywords internal
#' @noRd
bat.ref.filter.regions <- function(df, regions = c("ALL", "NABAT")) {
  want <- unique(c(bat.ref.split.regions(regions), "all"))
  keep <- vapply(df$Region, function(r) any(bat.ref.split.regions(r) %in% want), logical(1))
  df[keep, , drop = FALSE]
}

