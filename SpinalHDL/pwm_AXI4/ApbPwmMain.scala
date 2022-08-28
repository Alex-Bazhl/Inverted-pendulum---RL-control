package workshop.pwm_AXI4

import spinal.core._

//Run this main to generate the RTL
object ApbPwmMain{
  def main(args: Array[String]) {
    SpinalConfig(targetDirectory = "rtl").generateVerilog(
          gen = Axi4Wrapper_Pwm(
              addressWidth = 8,
              dataWidth = 32,
              idWidth = 1,
          )
    )
  }
}