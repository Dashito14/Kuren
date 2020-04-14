library("RODBC")
library("rjson")
library("jsonlite")

setwd("C:/Users/dipdn/Desktop/MIN")

education<-fromJSON(file = "C:/Users/dipdn/Desktop/MIN/NivelEducacion.json")
str(education)
data.class(education)

#Dimensiones
gender() <- list()
provinces() <- list()
achieved() <- list() #Nivel de formacion conseguido


gender <- NULL
provinces <- NULL
achieved <- NULL


for(i in 1:480){
  gender[i] <- education[[i]][[6]][[2]][[3]]
  provinces[i] <- education[[i]][[6]][[3]][[3]]
  achieved[i] <- education[[i]][[6]][[4]][[3]]
}

provinces <- unlist(provinces, use.names = FALSE)
gender <- unlist(gender, use.names = FALSE)
achieved <- unlist(achieved, use.names = FALSE)

values <- data.frame(
  "gender" = gender,
  "provinces" = provinces,
  "achieved" = achieved
)

periodos <- achieved[[1]][[7]]

for(i in 1:480){
  for(j in 1:24){
    year <- paste(periodos[[j]][[3]], periodos[[j]][[4]], sep = "")
    if(i == 1){
      if(!year %in% colnames(values)){
        values[year] <- 1:480
      }
    }
    values[i, year] <- education[[i]][[7]][[j]][[5]]
  }
}

#conexion con la base de datos
conn <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-NANJ7V5;database=mineria;trusted_connection=true')

#cargamos las dimensiones
#Provincias
dim_prov <- sqlQuery(conn, "SELECT s_name FROM dbo.dim_provinces WHERE n_province = 'Whole'")

n_s <- unique(provinces)

for(level in n_s){
  
  if((level %in% dim_prov$s_name) == FALSE){
    
    insert_query <- paste("INSERT INTO dbo.dim_provinces (n_province, s_name)
             VALUES ('Whole','", level, "')", sep="")
    
    sqlQuery(conn, insert_query)
    
  }
  
}

#Generos
dim_gender <- sqlQuery(conn, "SELECT n_gender FROM dbo.dim_gender")

n_g <- unique(gender)

for(level in n_g){
  if((level %in% dim_gender$n_gender) == FALSE){
    
    insert_query <- paste("INSERT INTO dbo.dim_gender (n_gender) VALUES ('", level, "')")
    
    sqlQuery(conn, insert_query)
  }
}

#Niveles de educacion
dim_edu <- sqlQuery(conn, "SELECT nm_edu FROM dbo.dim_education")

n_e <- unique(achieved)

for(level in n_e){
  if((level %in% dim_edu$nm_edu)){
    insert_query <- paste("INSERT INTO dbo.dim_education (nm_edu) VALUES ('", level, "')")
    
    sqlQuery(conn, insert_query)
  }
}


years_levels <- 2014:2019

level %in% dim_years$n_year

for(level in years_levels){
  
  if((level %in% dim_years$n_year) == FALSE || length(dim_years)==0){
    
    for(d in 1:4){
      p <- paste("T",d)
      
      insert_query <- paste("INSERT INTO dbo.dim_year (n_year, y_period)
              VALUES ('", level, "','", p, "')", sep="")
    
      sqlQuery(conn, insert_query)
    }
    
  }
  
}

dim_edu <- sqlQuery(conn, "SELECT id_edu, nm_edu FROM dbo.dim_risk")
dim_prov <- sqlQuery(conn, "SELECT id_province, s_name FROM dbo.dim_provinces WHERE n_province = 'Whole'")

for(i in 1:nrow(values)){
  row <- values[i,]
  
  #Provinces
  prov <- row[2]
  
  p_index <- match(prov$provinces,dim_prov$s_name)   
  
  id_prov <- dim_prov$id_province[p_index]
  
  #Education
  e <- row[3]
  
  e_index <- match(e$achieved, dim_edu$nm_edu)
  
  id_e <- dim_edu$id_edu[e_index]
  
  #Gender
  g <- row[1]
  
  g_index <- match(g$gender, dim_gender$n_gender)
  
  id_g <- dim_gender$id_gender[g_index]
  
  for(j in 2014:2019){
    year <- j
    
    for(d in 1:4){
      p <- paste("T",d)
      id_year <- sqlQuery(conn, "SELECT id_year FROM dbo.dim_year WHERE y_period = ' ", p, " ' AND n_year = '", year, "'")
    }
  }
  
  
}