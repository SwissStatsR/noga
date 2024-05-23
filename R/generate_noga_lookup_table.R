#' Function to generate Noga Lookup Table for this package
#' @name generate_noga_lookup_table
#' @noRd
#'
generate_noga_lookup_table <- function(){
  de <- utils::read.table(file="data/HCL_NOGA_levels_1-5_de.csv",header=TRUE,sep=",",encoding = "UTF-8") |>
    subset(select = c("Code", "Name_de"))
  en <- utils::read.table(file="data/HCL_NOGA_levels_1-5_en.csv",header=TRUE,sep=",",encoding = "UTF-8")
  fr <- utils::read.table(file="data/HCL_NOGA_levels_1-5_fr.csv",header=TRUE,sep=",",encoding = "UTF-8") |>
    subset(select = c("Code", "Name_fr"))
  it <- utils::read.table(file="data/HCL_NOGA_levels_1-5_it.csv",header=TRUE,sep=",",encoding = "UTF-8") |>
    subset(select = c("Code", "Name_it"))
  noga_names <- merge(en,de,by="Code",sort=FALSE) |>
    merge(x=_,y=fr,by="Code",sort=FALSE) |>
    merge(x=_,y=it,by="Code",sort=FALSE)
    noga_names$Division <- ifelse(nchar(as.character(noga_names$Division))==1,substr(noga_names$Code,1,2),as.character(noga_names$Division))
    noga_names$Division <- ifelse(is.na(noga_names$Division)&!is.na(as.numeric(noga_names$Code)),substr(noga_names$Code,1,2),as.character(noga_names$Division))
    noga_names$Group <- ifelse(nchar(as.character(noga_names$Group))==2,substr(noga_names$Code,1,3),as.character(noga_names$Group))
    noga_names$Group <- ifelse(is.na(noga_names$Group)&(!is.na(as.numeric(noga_names$Code))&nchar(noga_names$Code)>2),substr(noga_names$Code,1,3),as.character(noga_names$Group))
    noga_names$Class <- ifelse(nchar(as.character(noga_names$Class))==3,substr(noga_names$Code,1,4),as.character(noga_names$Class))
    noga_names$Class <- ifelse(is.na(noga_names$Class)&(!is.na(as.numeric(noga_names$Code))&nchar(noga_names$Code)>3),substr(noga_names$Code,1,4),as.character(noga_names$Class))
    noga_names$Type <- ifelse(nchar(as.character(noga_names$Type))==5,substr(noga_names$Code,1,6),as.character(noga_names$Type))
    for(i in 2:nrow(noga_names)){
      if(noga_names$Section[i]==""){
        noga_names$Section[i] <- noga_names$Section[i-1]
      }
    }
    lookup <- noga_names
    names(lookup) <- tolower(names(lookup))
    lookup$name_de <- stringi::stri_escape_unicode(lookup$name_de)
    lookup$name_en <- stringi::stri_escape_unicode(lookup$name_en)
    lookup$name_fr <- stringi::stri_escape_unicode(lookup$name_fr)
    lookup$name_it <- stringi::stri_escape_unicode(lookup$name_it)
    usethis::use_data(lookup)

}
