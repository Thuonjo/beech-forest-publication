 
 
 # colour option for publication one
  scale_shape_manual(name = "Valley",
                     labels = c("E", "H"),
                     values = c(25,21)) +

  scale_colour_manual(name = "Stoat control",
                      labels = c("Eglinton", "Hollyford", "Hollyford"),
                      values = c("darkgoldenrod","black", "black")) +
  scale_fill_manual(name = "Stoat control",
                    labels = c("Yes", "No", "Yes"),
                    values = c("darkgoldenrod","black", "darkgoldenrod")) +

 # Black and white option for publication one.

 scale_colour_manual(name = "Stoat control",
                    labels = c("E-", "H+", "H-"),
                    values = c("black","black", "black")) +
scale_shape_manual(name = "Valley",
                  labels = c("E", "H"),
                 values = c(25,21)) +
scale_fill_manual(name = "Stoat control",
                 labels = c("E-", "H+", "H-"),
                values = c("white","black", "white")) +