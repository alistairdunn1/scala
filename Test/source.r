a <-list.files("C:/Users/alist/OneDrive/Projects/Software/scala/scala/R/", pattern = "\\.[rR]$", full.names = T)
for(i in 1:length(a)) {
  source(a[i])
}
