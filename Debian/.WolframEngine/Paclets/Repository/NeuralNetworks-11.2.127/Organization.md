# Organization of NeuralNetworks paclet

This paclet is split into several sub-directories that reflect major implementation concerns:

* Apply: applying layers/nets, NetEncoders, and NetDecoders to inputs
* Containers: compound nets (NetGraph and NetChain)
* Create: run-time creation of layers, NetEncoders, and NetDecoders
* Define: load-time definition of types of layers, NetEncoders, and NetDecoders
* Encoders/Decoders/Layers: definition files for layers, NetEncoders, and NetDecoders
* ImportExport: importing and exporting nets to file-based formats
* Kernel: load script that loads pieces in the right order
* MXNet: code that interfaces with MXNetLink
* Symbolic: construct and represent symbolic computation graphs for (Binary)ElementwiseLayer
* TopLevel: definitions of system functions like NetInitialize and NetTrain
* Types: the type and inference system
* Utils: all other code

Each sub-directory will eventually contain an .md file that explains the architecture of that module.
