package workshop.quadDecoder_AXI4

import spinal.core._

//Run this main to generate the RTL
object ApbQuadDecoderMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
          gen = Axi4Wrapper_QuadDecoder(
              addressWidth = 8,
              dataWidth = 32,
              idWidth = 1,
          )
    )
  }
}