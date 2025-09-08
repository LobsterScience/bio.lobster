rm.from.list2=function (list1)  {
             a <- dim_list(list1)
             if (any(a[, 1] == 0)) {
                     list1 <- list1[-which(a[, 1] == 0)]
                 }
         return(list1)
         }