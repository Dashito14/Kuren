library("RODBC")
library(rjson)
library(plyr)

library(jsonlite)
detach("package:jsonlite", unload=TRUE)

#establecemos el directorio de trabajo
setwd("C:/Users/CGil/Documents/MIN")


#lectura del json
data <- fromJSON(file= "10011.json")
data
str(data)
data.class(data)



#creamos tres listas para las dimensiones
provinces <- list()
ages <- list()
risks <- list()

provinces <-NULL
ages <-NULL
risks <- NULL

provinces[80] <- "aaa"
ages[80] <- "aaa"
risks[80] <- "aaa"

typeof(ages)


#rellenamos las dimensiones
for(i in 1:80){
  provinces[i] <- data[[i]][[5]][[1]][3]
  ages[i] <- data[[i]][[5]][[2]][3]
  risks[i] <- data[[i]][[5]][[3]][3]
  
}

#convertimos las listas a arrays para rellenar el df
provinces <- unlist(provinces, use.names=FALSE)
ages <- unlist(ages, use.names=FALSE)
risks <- unlist(risks, use.names=FALSE)

#creamos el dataframe
values <- data.frame(
  "province" = provinces,
  "ages"= ages,
  "risk"=risks,
  "2018"=1:80,
  "2017"=1:80,
  "2016"=1:80,
  "2015"=1:80,
  "2014"=1:80,
  "2013"=1:80,
  "2012"=1:80,
  "2011"=1:80,
  "2010"=1:80,
  "2009"=1:80,
  "2008"=1:80
)

values

#rellenamos los valores para los porcentajes
for(i in 1:80){
  
  for(j in 1:11){
    
    values[i, j+3]<-data[[i]][[6]][[j]][5]
    
  }
  
}

values

colnames(values)

(fltr_values<-values[values$province == "Aragón",  !(names(values) %in% c("risk","ages"))])

plot(t(fltr_values[3,]), type = "h", xlab="Años 2018-2008", ylab="porcentaje", main="Familias con carencia material severa")

fltr_values[3,]

#conexion con la base de datos
conn <- odbcDriverConnect('driver={SQL Server};server=DESKTOP-BM96OLK;database=MIN;trusted_connection=true')

#cargamos las dimensiones
#PROVINCIAS
dim_prov <- sqlQuery(conn, "SELECT s_name FROM dbo.dim_provinces WHERE n_province = 'Whole'")
dim_prov

prov_levels <- unique(provinces)
prov_levels

for(level in prov_levels){
  
  if((level %in% dim_prov$s_name) == FALSE || length(dim_prov)<20){
    
    insert_query <- paste("INSERT INTO dbo.dim_provinces (n_province, s_name)
             VALUES ('Whole','", level, "')", sep="")
    
    sqlQuery(conn, insert_query)
  
  }

}

#NIVELES DE POBREZA
dim_risks <- sqlQuery(conn, "SELECT explanation_risk FROM dbo.dim_risk")
dim_risks

risk_levels <- unique(risks)
risk_levels

for(level in risk_levels){
  
  if((level %in% dim_risks$explanation_risk) == FALSE || nrow(dim_risks)<4){
    
    insert_query <- paste("INSERT INTO dbo.dim_risk (explanation_risk)
             VALUES ('", level, "')", sep="")
    
    sqlQuery(conn, insert_query)
    
  }
  
}

dim_risks[1,]

r <- risk_levels[2]
r %in% dim_risks$explanation_risk
nrow(dim_risks) < 4

#AÑOS

dim_years <- sqlQuery(conn, "SELECT n_year FROM dbo.dim_year WHERE y_period = 'F'")
dim_years

years_levels <- 2008:2018
years_levels

level %in% dim_years$n_year

for(level in years_levels){
  
  if((level %in% dim_years$n_year) == FALSE || length(dim_years)==0){
    
    insert_query <- paste("INSERT INTO dbo.dim_year (n_year, y_period)
             VALUES ('", level, "','F')", sep="")
    
    sqlQuery(conn, insert_query)
    
  }
  
}

#recorremos el dataframe y cargamos los datos en la base de datos

dim_risks <- sqlQuery(conn, "SELECT id_risk, explanation_risk FROM dbo.dim_risk")
dim_prov <- sqlQuery(conn, "SELECT id_province, s_name FROM dbo.dim_provinces WHERE n_province = 'Whole'")
dim_years <- sqlQuery(conn, "SELECT id_year, n_year FROM dbo.dim_year WHERE y_period = 'F'" )


dim_prov
dim_risks
dim_years



for (i in 1:nrow(values)){
  row <- values[i,]
  
  prov <- row[1]
  
  p_index <- match(prov$province,dim_prov$s_name)   
  
  id_prov <- dim_prov$id_province[p_index]
  
  
  r <- row[3]
  
  r_index <- match(r$risk, dim_risks$explanation_risk)
  
  id_r <- dim_risks$id_risk[r_index]
  
  for(j in 2008:2018){
    year <- j
    
    y_index <- match(year, dim_years$n_year)
    
    id_y <- dim_years$id_year[y_index]
    
    select_query <- paste("SELECT * FROM dbo.facts_poverty 
                          WHERE id_province = ", id_prov, 
                          " AND id_risk = ", id_r,
                          " AND id_year = ", id_y,
                          "", sep="")
    
    exist <- sqlQuery(conn, select_query)
    
    if(nrow(exist) == 0){
      
      perc_index <- paste("X", year, sep="")
      
      perc <- row[perc_index]
      
      insert_query <- paste("INSERT INTO dbo.facts_poverty (id_year, id_province, id_risk, perc)
             VALUES (", id_y, ",",id_prov, ",", id_r ,",",perc[perc_index],")", sep="")
      
      sqlQuery(conn, insert_query)
      
    }
    
    
    
    
  }
  
  
}

j <- 2008

row


