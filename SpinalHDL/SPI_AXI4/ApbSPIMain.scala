package workshop.SPI_AXI4

import spinal.core._

//Run this main to generate the RTL
object ApbSPIMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
          gen = Axi4Wrapper_SPI(
              addressWidth = 8,
              dataWidth = 32,
              idWidth = 1,
          )
    )
  }


}