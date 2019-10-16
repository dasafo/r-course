family.salary = c(40000, 60000, 50000, 80000, 60000, 70000, 60000)
family.size = c(4, 3, 2, 2, 3, 4, 3)
family.car = c("Lujo", "Compacto", "Utilitario", "Lujo", 
               "Compacto", "Compacto", "Compacto")

family <- data.frame(family.salary, family.size, family.car)

family.unique <- unique(family) #evita duplicaciones

duplicated(family) #Para que nos diga con bolleano donde se reptiren datos

family[duplicated(family),] #para que nos muestre los datos repetios
