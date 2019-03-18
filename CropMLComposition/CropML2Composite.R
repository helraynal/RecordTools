# name: CropML2Composite 
# author: H.Raynal
#Project AMEI
# CropML to RECORD
#script --> version V1

setwd("C:/Users/hraynal/EspaceTravailBadet/Projets/Projet_MFPC/AMEI/CropML/CropMLComposition")

# Load the package required to read XML files.
library("XML")
library("methods")

inputFile <- "EnergyBalance.xml"

# Give the input file name to the function.
result <- xmlTreeParse(file = inputFile)

# Print the result.
#print(result)



#========================================
# Get the nodes and their attributes  
#========================================
doc <-xmlParse(inputFile)

#Parsing name of the "composite model"/"coupled model"
ModelComposition<- getNodeSet(doc, "//ModelComposition")
ModelCompositionName<- sapply(ModelComposition, function(el) xmlGetAttr(el, "name"))
  
#Parsing "Composition" node in order to get the list of "model unit" / "Atomic Model" 
CompositionList <- getNodeSet(doc, "//Composition")
SubmodelList <- getNodeSet(doc,"//Model")
SubmodelName <- sapply(ModelList, function(el) xmlGetAttr(el, "name"))
SubmodelDynamic <- sapply(ModelList, function(el) xmlGetAttr(el, "id"))
SubmodelFilename <- sapply(ModelList, function(el) xmlGetAttr(el, "Filename"))

#Parsing "Links" node in order to get the list of links/connections between Model units 
Links <- getNodeSet(doc, "//Links")
InputLink <- getNodeSet(doc, "//InputLink")
InputLinkSourceList <- sapply(InputLink, function(el) xmlGetAttr(el, "source"))
InputLinkSourceTarget <- sapply(InputLink, function(el) xmlGetAttr(el, "target"))

OutputLink <- getNodeSet(doc, "//OutputLink")
OutputLinkSourceList <- sapply(OutputLink, function(el) xmlGetAttr(el, "source"))
OutputLinkSourceTarget <- sapply(OutputLink, function(el) xmlGetAttr(el, "target"))

InternalLink <- getNodeSet(doc, "//InternalLink")
InternalLinkSourceList <- sapply(InternalLink, function(el) xmlGetAttr(el, "source"))
InternalLinkTargetList <- sapply(InternalLink, function(el) xmlGetAttr(el, "target"))


# ====================================================================
# Generate "vpz file" associated to Coupled Model (compliant to RECORD)
# ====================================================================


# To read empty.vpz file
emptyFile <- "empty2.vpz"

# To give the input file name to the function.
emptyResult <- xmlParse(file = emptyFile)
NodeStructure <- getNodeSet(emptyResult, "//structures")
NodeModel <- getNodeSet(emptyResult,"//model")
NodeDynamics <- getNodeSet(emptyResult, "//dynamics")
NodeExperiment <- getNodeSet(emptyResult, "//experiment")
NodeConditions<- getNodeSet(emptyResult, "//conditions")
NodeSubModels <- getNodeSet(emptyResult, "//submodels")
ModelName<- sapply(ModelList, function(el) xmlGetAttr(el, "name"))


#To create the node "submodels" associated to the atomic model
#===========================================================
Model <- newXMLNode("model")
addAttributes(Model, y="109", x="53", observables="", 
              name=ModelCompositionName, height="50", 
              conditions="", width="50", type="coupled")
cat(saveXML(Model), "\n")

#List all the Submodels
Submodels <- newXMLNode("submodels")

#All submodels
Submodel <- newXMLNode("Model")
SubmodelNode <- sapply( SubmodelName, function(Out) newXMLNode("Submodel", attrs=c(name=Out )))

Submodels <- addChildren(Submodels, SubmodelNode)
Submodel <- addChildren( Submodel, Submodels)
Model <- addChildren( Model, Submodel)
cat(saveXML(Model), "\n")


#To create the node "connections" associated each model unit / atomic model
#===========================================================
Connections <- newXMLNode("Connections")

#All internal connections
cmpt <- length(InternalLinkTargetList)
for(i in 1:cmpt) {
  Connection <- newXMLNode("connection")
  addAttributes(Connection, type="internal")
  origin <- newXMLNode("origin")
  addAttributes(origin , 
                model=(strsplit(InternalLinkSourceList[cmpt],".",fixed=TRUE))[[1]][1] , 
                port=(strsplit(InternalLinkSourceList[cmpt],".",fixed=TRUE))[[1]][2] )
  destination <-  newXMLNode("destination")
  addAttributes(destination, 
                model=(strsplit(InternalLinkTargetList[cmpt],".",fixed=TRUE))[[1]][1], 
                port=(strsplit(InternalLinkTargetList[cmpt],".",fixed=TRUE))[[1]][2])
  Connection <- addChildren( Connection, origin)
  Connection <- addChildren( Connection, destination)
  Connections <- addChildren(Connections,Connection)
}
Model <- addChildren( Model, Connections)
cat(saveXML(Model), "\n")

#All input Links /  connections
#to do

#All output Links / connections
#to do

saveXML(Model, file="CropMLcomposition.vpz", prefix="<?xml version=2.0 encoding=UTF-8?>")
