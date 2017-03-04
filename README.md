# ACNC
A G-code parser and CNC controller (in Ada)

For more info: http://blog.adacore.com/make-with-ada-arm-cortex-m-cnc-controller

##  Build instructions

### Gcode Viewer

 - Download and install the [GNAT and GtkAda packages](http://libre.adacore.com/download/configurations) in the same directory
 - Start GPS (GNAT Programing Studio)
 - Open the project file gcode_viewer/gcode_viewer.gpr
 - Use the "Build all" button to compile
 - use the "Run Main: main_gtkada" button to start the application

### CNC controller on the STM32F4 Discovery

- Make sure to get all the Git submodules "$ git submodules update"
- Download and install the [GNAT for ARM package](http://libre.adacore.com/download/configurations)
- Start GPS (GNAT Programing Studio)
- Open the project file stm32f4-disco_controller/stm32f4_disco_CNC_controller.gpr
- Use the "Build all" button to compile
- use the "Flash to board" button to program the STM32F4 Discovery board

#### 
