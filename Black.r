ranking <- function(V)
# Calcul de l'estimateur de fraction selon le modèle
# rk(i)=(i-0.3)/(n+0.4) pour un vecteur V.
{
    rk <- (rank(V)-0.3)/(length(V)+0.4)
}



Weibit <- function(P)
# Calcul du Weibit selon le modèle
# W= ln(-ln(1-P)) pour un vecteur P.
{
    W=log(-log(1-P))
}

lognorm.p <- function(P)
# Calcul de la proba en échelle normal selon le modèle
# W= qnorm(P) pour un vecteur P. Nécessaire pour un graph
# gausso arythmétique. Droite de Henri.
{
    W=qnorm(P)
}
