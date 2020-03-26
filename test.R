library (rJava)
library (ggplot2)

## Calling TrapGrid from within R using rJava
dir_to_java <- "/home/hil32c/Dropbox/trapgrid/trapgrid/test/"

##  Initialise the Java Virtual Machine
.jinit('.')

## add the .jar to the classpath
.jaddClassPath(paste0(dir_to_java,'TrapGrid.jar'))

## create a new instance (this points to Driver as this is the main class defined in MANIFEST.MF)
trapgrid <- .jnew("com.reallymany.trapgrid.Driver")

## Requesting vectors of methods, constructors and fields by class
.jmethods(trapgrid) ## method is called "main"

## lowlevel call of the function
## make sure the defaul grid file has complete path
.jcall(obj=trapgrid, method="main", c("-tg",paste0(dir_to_java, "foogrid")), returnSig = "V")

## function call can also be written as
trapgrid$main(c("-tg",paste0(dir_to_java, "foogrid")))
