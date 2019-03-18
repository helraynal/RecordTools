#CropML2Atomic - RECORD
# H.Raynal
#Project AMEI

setwd("C:/Users/hraynal/EspaceTravailBadet/Projets/Projet_MFPC/AMEI/CropML")

# Load the package required to read XML files.
library("XML")
library("methods")

inputFile <- "Example_Sirius_CanopyTemperature.xml"

# Give the input file name to the function.
result <- xmlTreeParse(file = inputFile)

# Print the result.
#print(result)



#========================================
# Get the nodes and their attributes  
#========================================
doc <-xmlParse(inputFile)

#Parsing name of the modele
ModelList <- getNodeSet(doc, "//ModelUnit")
ModelName<- sapply(ModelList, function(el) xmlGetAttr(el, "name"))
  
#Parsing Inputs 
InputList <- getNodeSet(doc, "//Input")
InputName <- sapply(InputList, function(el) xmlGetAttr(el, "name"))
InputDescription <- sapply(InputList, function(el) xmlGetAttr(el, "description"))
InputDatatype <- sapply(InputList, function(el) xmlGetAttr(el, "datatype"))
InputMin <- sapply(InputList, function(el) xmlGetAttr(el, "min"))
InputMax <- sapply(InputList, function(el) xmlGetAttr(el, "max"))
InputDefaultValue <- sapply(InputList, function(el) xmlGetAttr(el, "default"))
InputUnit <- sapply(InputList, function(el) xmlGetAttr(el, "unit"))
InputUri <- sapply(InputList, function(el) xmlGetAttr(el, "uri"))
InputType <- sapply(InputList, function(el) xmlGetAttr(el, "inputtype"))

#Parsing Outputs 
OutputList <- getNodeSet(doc, "//Output")
OutputName <- sapply(OutputList, function(el) xmlGetAttr(el, "name"))
OutputDescription <- sapply(OutputList, function(el) xmlGetAttr(el, "description"))
OutputDatatype <- sapply(OutputList, function(el) xmlGetAttr(el, "datatype"))
OutputMin <- sapply(OutputList, function(el) xmlGetAttr(el, "min"))
OutputMax <- sapply(OutputList, function(el) xmlGetAttr(el, "max"))
OutputDefaultValue <- sapply(OutputList, function(el) xmlGetAttr(el, "default"))
OutputUnit <- sapply(OutputList, function(el) xmlGetAttr(el, "unit"))
OutputUri <- sapply(OutputList, function(el) xmlGetAttr(el, "uri"))
OutputType <- sapply(OutputList, function(el) xmlGetAttr(el, "Outputtype"))

# Parsing algorithm node
#=======================
Algo <- getNodeSet(doc, "//Algorithm")
AlgoC <- xmlValue(Algo[[1]])




# ====================================================================
# Generate "vpz file" associated to Atomic Model (compliant to RECORD)
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
AtomicModel <- newXMLNode("model")
addAttributes(AtomicModel, y="109", x="53", observables="", 
              dynamics= paste("dyn",ModelName, sep=""), name=ModelName, height="50", 
              conditions="", width="50", type="atomic")
cat(saveXML(AtomicModel), "\n")

#In and out ports (!!! only Inputs with attribute InputType=variable)
In <- newXMLNode("In")
x <- cbind(InputName,InputType)
x2 <- x[x[,2] =="variable",] 
InPort <- lapply( x2[,1], function(x) newXMLNode("port", attrs=c(name=x)))
In <- addChildren(In, InPort)

Out <- newXMLNode("Out")
OutPort <- sapply( OutputName, function(Out) newXMLNode("port", attrs=c(name=Out )))
Out <- addChildren(Out, OutPort)

AtomicModel <- addChildren( AtomicModel, In)
AtomicModel <- addChildren( AtomicModel, Out)
cat(saveXML(AtomicModel), "\n")


#To create the node "Experiment" (conditions and views)
#===========================================================

#To create Conditions
condition <- newXMLNode("condition", attrs=c(name=paste("cond",ModelName, sep="")))
#parameters. They are declared as Inputs with attributes "parameter"
par <- cbind(InputName,InputType,InputDatatype,InputDefaultValue)
par2 <- par[par[,2] =="parameter",] 
#ParPort <- lapply( par2[,1], function(x) newXMLNode("port", attrs=c(name=x)))
for (i in 1:length(par2[,1]))
{
  Father <- newXMLNode("port", attrs=(name=par2[i,1]) )
  Son <-newXMLNode(par2[i,3])
  temp2 <- addChildren(Son, par2[i,4])
  test <- addChildren(Father,temp2)
  condition <- addChildren(condition, Father) 
}


#To create Views
NodeViews <- getNodeSet(emptyResult, "//views")



Node1A <- newXMLNode("vle_project", attrs=c(version="2.0", author="RECORD", date=""))
Node2A <- newXMLNode("structure")
Node3A <- newXMLNode("model", attrs=c(y="0", x="0", name="top", height="300", width="300", type="coupled"))
Node4A <- newXMLNode("submodel")
Node4A <- addChildren(Node4A,AtomicModel)
Node3A <- addChildren(Node3A,Node4A)
Node3B <- addChildren(newXMLNode("connections"))
Node2A <- addChildren(Node2A,Node3A)
Node2A <- addChildren(Node2A,Node3B)
Node1A <- addChildren(Node1A,Node2A)

Node2B <- newXMLNode("dynamics")
Node1A <- addChildren(Node1A,Node2B)

Node2C <- newXMLNode("experiment", attrs=c(name="test", seed="123456789"))
Node3C <- addChildren( NodeConditions[[1]],condition)
Node2C <- addChildren(Node2C,Node3C)

Node3D <-NodeViews[[1]]
Node2C <- addChildren(Node2C,Node3D)

Node1A <- addChildren(Node1A, Node2C)

cat(saveXML(Node1A), "\n")
saveXML(Node1A, file="CropML.vpz", prefix="<?xml version=2.0 encoding=UTF-8?>")


#=========================================
#To generate the dynamic (C++ code)
#=========================================
dynFile <- file("myFile.cpp", open = "w")
cat("/*", "AMEI project \n", file = dynFile)
cat("@file \n", "@....\n", "*/", " \n", file = dynFile, sep = " ")

cat("#include <vle/DiscreteTime.hpp>", " \n", file = dynFile)
cat("namespace vd = vle::devs;", " \n", file = dynFile)
cat("namespace vv = vle::value;", " \n", file = dynFile)
cat("namespace vle {", "\n" ,file = dynFile)
cat("namespace discrete_time {", " \n",file = dynFile)
cat("namespace AMEI {", " \n", file = dynFile)

cat("class ",file = dynFile)
cat(ModelName,file = dynFile)
cat(" : public DiscreteTimeDyn \n " , file = dynFile)
cat(" { \n ", file = dynFile)
cat(" public: \n ", file = dynFile)
cat(ModelName, "( \n ", file = dynFile )
cat("const vd::DynamicsInit& init,","( \n ", file = dynFile)
cat("const vd::InitEventList& evts)" ,"( \n ", file = dynFile )
cat("   : DiscreteTimeDyn(init, evts)","( \n ", file = dynFile )
cat(" { \n ", file = dynFile)
# init avec les entrees et cond exp ""
#for (i in 1:length(InputName)) {
#  cat(InputName[i], ".init(this, " , "\"",InputName[i], " \", evts); \n" ,sep="", file = dynFile)
#}
#for (i in 1:length(OutputName)) {
#  cat(OutputName[i], ".init(this, " , "\"",OutputName[i], " \", evts); \n" ,sep="", file = dynFile)
#}
cat(" } \n ", file = dynFile)

cat("virtual ",file = dynFile)
cat("~",ModelName, "()",file = dynFile)
cat("{}"," \n ", " \n ",file = dynFile)

cat("void compute(const vle::devs::Time& t) ",file = dynFile)
cat(" { \n ", file = dynFile)
# Declaration 
cat("TOTO \n", file = dynFile)

#cat(AlgoC," \n ", file = dynFile)
cat(" { \n ", file = dynFile)
#Declaration of Var

#for (i in 1:length(InputName)) {
#  cat("Var " ,InputName[i], "; \n" ,sep="", file = dynFile)
#}
#for (i in 1:length(OutputName)) {
#  cat("Var", OutputName[i], " \n" ,sep="", file = dynFile)
#}
cat("TOTO \n", file = dynFile)

cat("}; \n ",file = dynFile)
cat("} \n ",file = dynFile)
cat("} \n ",file = dynFile)
cat("} \n ",file = dynFile)
cat("DECLARE_DYNAMICS(vle::discrete_time::AMEI::",ModelName,file = dynFile)
close(dynFile)
warnings()

