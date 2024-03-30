
s_simple = function(x, decimales = 1, clipboard = FALSE){
  if (!is.numeric(x)){
    cat("Debes ingresar un vector numérico.")
  } else {
    omitidos = length(x[is.na(x)])
    nulos = length(x[is.null(x)])
    infinitos = length(x[x==Inf])
    if (omitidos + nulos ==0){
      nulos ="-"
    }
    if (infinitos ==0){
      infinitos ="-"
    }
    x = x[(!is.na(x))&(x!=Inf)]
    if (length(x)>1){
      tipo = ifelse(
      class(x) == "integer",
      "V.a. cuantitativa discreta",
      "V.a. cuantitativa continua"
    )
    mm = mean(x)|>setNames("Media")
    mmt5 = mean(x, trim = 0.05)|>setNames("Media recortada al 5%")
    mmt10 = mean(x, trim = 0.1)|>setNames("Media recortada al 10%")
    min = min(x)|>setNames("Min.")
    max = max(x)|>setNames("Max.")
    cuantiles = quantile(x,
                         probs = c(.1, .2, .25, .4, .5, .6, .75, .8, .9),
                         type = 2)
    varianza = var(x)|>setNames("Varianza muestral")
    s = sd(x)|>setNames("Desviación típica muestral")
    r = diff(range(x))|>setNames("Rango")
    ri = diff(cuantiles[c("25%","75%")])|>setNames("RI")
    cv = sd(x) / mean(x) * 100|>setNames("CV")
    n = length(x)|>setNames("n")
    ee = sd(x) / sqrt(n)|>setNames("ee")
    atipicos = boxplot.stats(x, coef = 1.5)$out
    extremos = boxplot.stats(x, coef = 3)$out
    if (length(atipicos)==0){
      atipicos="-"
    }
    if (length(extremos)==0){
      extremos="-"
    }
    sn = sqrt(varianza * (n-1) / n)
    momento3 = mean((x - mm)^3)
    momento4 = mean((x - mm)^4)
    sesgo = momento3 / (sn^3)|>setNames("Asimetría (momentos)")
    curtosis = momento4 / (sn^4)|>setNames("Curtosis (momentos)")
    cat("\n--------------------------------------------")
    cat("\n===")
    cat(" Medidas de resumen - Series Simples ")
    cat("===")
    cat("\n--------------------------------------------")
    cat("\n", tipo)
    cat("\n Tamaño muestral: ", n)
    cat("\n Valores faltantes: ", nulos)
    cat("\n Valores Inf: ", infinitos)
    resumen = c(mm,
                  mmt5,
                  mmt10,
                  min,
                  max,
                  cuantiles,
                  varianza,
                  s,
                  cv,
                  ee,
                  r,
                  ri,
                  sesgo,
                  curtosis)|>round(decimales)
    sapply(1:length(resumen),function(i){
      cat("\n",names(resumen[i]),": ",resumen[i])
    })
    cat("\n Valor/es atípico/s (1,5 x RI): ", atipicos)
    cat("\n Valor/es extremo/s (3 x RI): ", extremos )
    if ((!is.integer(x))&(length(x)>=10)){
      ic80 = (mm + c(1,-1) * qt(0.1,n-1) * ee)|>round(decimales)
      ic90 = (mm + c(1,-1) * qt(0.05,n-1) * ee)|>round(decimales)
      ic95 = (mm + c(1,-1) * qt(0.025,n-1) * ee)|>round(decimales)
      ic99 = (mm + c(1,-1) * qt(0.005,n-1) * ee)|>round(decimales)
      cat("\n IC 80%: [",ic80|>paste0(collapse = ", "),"]")
      cat("\n IC 90%: [",ic90|>paste0(collapse = ", "),"]")
      cat("\n IC 95%: [",ic95|>paste0(collapse = ", "),"]")
      cat("\n IC 99%: [",ic99|>paste0(collapse = ", "),"]")
    }

    cat("\n--------------------------------------------")

  if (clipboard == TRUE){
    data.frame(
      Medida = names(resumen),
      x = resumen
    )|>
      write.table("clipboard", row.names = FALSE, dec = ",")
  }
    } else {
    cat("Ingresa un vector numérico con más de un elemento")
  }
 }
}

s_agrupada = function(x,
                      decimales = 1,
                      li = NULL,
                      ls = NULL,
                      a = NULL,
                      derecha = TRUE,
                      clipboard = FALSE){
  tff = function(x, limites){
    mc = ((limites[-length(limites)]+limites[-1]) / 2)|>round(decimales)
    tf= x |>
          cut(breaks = limites,
              include.lowest = TRUE,
              dig.lab = decimales,
              right = derecha) |>
          table(dnn = list("Clase")) |>
          as.data.frame(responseName = "fi")|>
          transform(
            fri = (fi / sum(fi))|>round(decimales),
            Fi = cumsum(fi),
            Fri = (cumsum(fi/sum(fi)))|>round(decimales)
          )|>cbind(mc)
    tf[,c(1,6,2:5)]
  }

  if (!is.numeric(x)){
    cat("Debes ingresar un vector numérico")
  } else {
    omitidos = length(x[is.na(x)])
    nulos = length(x[is.null(x)])
    infinitos = length(x[x==Inf])
    if (omitidos + nulos ==0){
      nulos ="-"
    }
    if (infinitos ==0){
      infinitos ="-"
    }
    n = length(x)
    x = x[(!is.na(x))&(x!=Inf)]
    tipo = ifelse(
      class(x) == "integer",
      "V.a. cuantitativa discreta",
      "V.a. cuantitativa continua"
    )
    cat("\n-----------------------------------------------")
    cat("\n===")
    cat(" Tabla de frecuencias - Series Agrupadas ")
    cat("===")
    cat("\n-----------------------------------------------")
    cat("\n", tipo)
    cat("\n Tamaño muestral: ", n)
    cat("\n Valores faltantes: ", nulos)
    cat("\n Valores Inf.: ", infinitos)
    cat("\n")
    cat("\n")
    if (is.integer(x)){
      (tf = x|>
        factor(levels = (min(x):max(x))|>as.character())|>
        table(dnn = list("mc"))|>
        as.data.frame(responseName = "fi")|>
        transform(
          fri = (fi / sum(fi))|>round(decimales),
          Fi = cumsum(fi),
          Fri = (cumsum(fi/sum(fi)))|>round(decimales)
        ))|>print()
    } else {
      if ((length(x)<2)|(length(unique(x))<=1)){
        cat("Ingrese un vector numérico que permita la agrupación en intervalos de clase.")
      }else{
        if ((!is.null(li)) & (!is.null(ls)) & (!is.null(a))){
          limites = seq(li, ls, a)
          (tf = tff(x, limites))|>print()
        } else {
          k = ceiling(1 + 3.3 * log10(n))
          li = min(x)
          ls = max(x)
          a = (max(x) - min(x)) / k
          limites = seq(li, ls, a)
          (tf = tff(x, limites))|>print()
        }
      }
    }
    if (clipboard == TRUE){
      tf|>
        write.table("clipboard", row.names = FALSE, dec = ",")
    }
  }
}


