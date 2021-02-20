Modelo1<-rda(Basidios[,c("Hyphal_length",
                                "Hyphal_number",
                                "Hyphal_tip_width",
                                "Hyphal_main_width",
                                "Hyphal_angle",                                                                              "Mycelial_area",
                                "Hyphal_density")])

Modelo2<-rda(Basidios[,c("Hyphal_length",
                         "Hyphal_number",
                         "Hyphal_tip_width",
                         "Hyphal_main_width",
                         "Hyphal_angle",                                                                              "Mycelial_area",
                         "Hyphal_density")],scale = T)
plot(
#Modelo1$Ybar[,1],
Modelo2$Ybar[,1],

scale(as.matrix(Basidios[,c("Hyphal_length",
                            "Hyphal_number",
                            "Hyphal_tip_width",
                            "Hyphal_main_width",
                            "Hyphal_angle",                                                                              "Mycelial_area",
                            "Hyphal_density")]),center = T,scale = T)[,1] 
)


(scale(as.matrix(Basidios[,c("Hyphal_length",
                            "Hyphal_number",
                            "Hyphal_tip_width",
                            "Hyphal_main_width",
                            "Hyphal_angle",                                                                              "Mycelial_area",
                            "Hyphal_density")]),center = T,scale = T))/Modelo1$Ybar

(scale(as.matrix(Basidios[,c("Hyphal_length",
                             "Hyphal_number",
                             "Hyphal_tip_width",
                             "Hyphal_main_width",
                             "Hyphal_angle",                                                                              "Mycelial_area",
                             "Hyphal_density")]),center = T,scale = T))/Modelo2$Ybar

var(as.matrix(scale(as.matrix(Basidios[,c("Hyphal_length",
                            "Hyphal_number",
                            "Hyphal_tip_width",
                            "Hyphal_main_width",
                            "Hyphal_angle",                                                                              "Mycelial_area",
                            "Hyphal_density")]),center = T,scale = T)))
