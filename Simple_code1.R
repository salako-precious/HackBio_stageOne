GeneA  <- "GCATTTAT"
GeneB <- "gcaTTTAT"
length(GeneA)
length(GeneB)
GeneA
GeneB
#Captalize gcaTTTAT. 
Capitalize_GeneB <- toupper(GeneB)
print(Capitalize_GeneB)
#split into characters 
New_GeneA <- strsplit(x= GeneA, split = "")[[1]]
New_GeneA
New_GeneB <- strsplit(x= Capitalize_GeneB, split = "")[[1]]
New_GeneB
#Gc Calculation
GC_counter <- 0 
for (nuc in New_GeneA) {
  print(nuc)
  
  if(nuc == "G"| nuc == "C"){
    GC_counter<- GC_counter+1
  }
}
(GC_counter/ length(New_GeneA))*100
#GeneB
GC_counter <- 0 
for (nuc in New_GeneB) {
  print(nuc)
  
  if(nuc == "G"| nuc == "C"){
    GC_counter<- GC_counter+1
  }
}
(GC_counter/ length(New_GeneB))*100

aa_weights <- c(A = 89.09,  R = 174.20, N = 132.12, D = 133.10, C = 121.15,
                E = 147.13, Q = 146.15, G = 75.07,  H = 155.16, I = 131.18,
                L = 131.18, K = 146.19, M = 149.21, F = 165.19, P = 115.13,
                S = 105.09, T = 119.12, W = 204.23, Y = 181.19, V = 117.15)
print(aa_weights)
length(aa_weights)


#calculate the weight
Calculate_protein_weight= function(PRECIOUS){
  aa_weights <- c(A = 89.09,  R = 174.20, N = 132.12, D = 133.10, C = 121.15,
                  E = 147.13, Q = 146.15, G = 75.07,  H = 155.16, I = 131.18,
                  L = 131.18, K = 146.19, M = 149.21, F = 165.19, P = 115.13,
                  S = 105.09, T = 119.12, W = 204.23, Y = 181.19, V = 117.15)
  Character_cases <- strsplit(toupper(PRECIOUS), "")[[1]]
  individual_weights <- aa_weights[Character_cases]
  individual_weights[is.na(individual_weights)] <- 0
  total_da <- sum(individual_weights)
  total_kda <- total_da / 1000
  return(total_kda)
}
aa_weights <- c(A = 89.09,  R = 174.20, N = 132.12, D = 133.10, C = 121.15,
                E = 147.13, Q = 146.15, G = 75.07,  H = 155.16, I = 131.18,
                L = 131.18, K = 146.19, M = 149.21, F = 165.19, P = 115.13,
                S = 105.09, T = 119.12, W = 204.23, Y = 181.19, V = 117.15)
Calculate_protein_weight(PRECIOUS = aa_weights )
