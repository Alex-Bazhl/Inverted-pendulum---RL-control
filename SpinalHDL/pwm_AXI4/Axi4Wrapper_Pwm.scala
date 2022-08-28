package workshop.pwm_AXI4

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SharedToApb3Bridge}

object Apb3Pwm{

  def getApb3Config() = Apb3Config(addressWidth = 8,dataWidth = 32,selWidth = 1,useSlaveError = false)
}

case class ApbPwm(timerWidth : Int) extends Component{


  val io = new Bundle{
    val apb  = slave(Apb3(Apb3Pwm.getApb3Config()))
    val pwm = out Bool ()
  }



  val logic = new Area {
    val enable    = Reg(Bool) init(False)
    val timer     = Reg(UInt(timerWidth bits)) init(0)
    val dutyCycle = Reg(UInt(timerWidth bits))
    val output    = Reg(Bool) init(False)
    val pulseEnable    = Reg(Bool) init(False)
    val pulseCycle     = Reg(UInt(timerWidth bits)) init(0)



    when(enable) {
      timer := timer + 1
    }


    when(pulseEnable)
    {
      when(timer === pulseCycle){
        output := True
      }
    }otherwise
    {
      when(timer === 0){
        output := True
      }
    }

    when(timer === dutyCycle){
      output := False
    }
    io.pwm := output
  }



  val control = Apb3SlaveFactory(io.apb)
  control.driveAndRead(logic.enable, 0)
  control.driveAndRead(logic.pulseEnable, 4)
  control.driveAndRead(logic.dutyCycle, 8)
  control.driveAndRead(logic.pulseCycle, 12)



//  val control = new Area{
//    val doWrite = io.apb.PSEL(0) && io.apb.PENABLE && io.apb.PWRITE
//    io.apb.PRDATA := 0
//    io.apb.PREADY := True
//    switch(io.apb.PADDR){
//      is(16){
//        io.apb.PRDATA(0) := logic.enable
//        when(doWrite){
//          logic.enable := io.apb.PWDATA(0)
//        }
//      }
//      is(4){
//        io.apb.PRDATA := logic.dutyCycle.asBits.resized
//        when(doWrite){
//          logic.dutyCycle := io.apb.PWDATA.asUInt.resized
//        }
//      }
//      is(8){
//        io.apb.PRDATA(0) := logic.pulseEnable
//        when(doWrite){
//          logic.pulseEnable := io.apb.PWDATA(0)
//        }
//      }
//      is(12){
//        io.apb.PRDATA := logic.pulseCycle.asBits.resized
//        when(doWrite){
//          logic.pulseCycle := io.apb.PWDATA.asUInt.resized
//        }
//      }
//    }
//  }
}

case class Axi4Wrapper_Pwm(addressWidth: Int, dataWidth: Int, idWidth: Int) extends Component{

  val axi4Config = Axi4Config(
    addressWidth = addressWidth,
    dataWidth    = dataWidth,
    idWidth      = idWidth,
    useLock      = false,
    useRegion    = false,
    useCache     = false,
    useProt      = false,
    useQos       = false
  )

  val io = new Bundle{
    val axi =  slave(Axi4(axi4Config))
    val pwmout = out Bool ()   //  输出口的datawidth
    val axiclk = in Bool()
    val axireset = in Bool()
  }
  noIoPrefix()

  val axiClockDomain = ClockDomain(
    clock = io.axiclk,
    reset = io.axireset,
    config = ClockDomainConfig(resetActiveLevel = LOW),
    frequency = FixedFrequency(100 MHz)           //  时钟频率
  )

  val axi = new ClockingArea(axiClockDomain){
    val apbBridge = Axi4SharedToApb3Bridge(
      addressWidth = addressWidth,
      dataWidth    = dataWidth,
      idWidth      = idWidth
    )
    apbBridge.io.axi <> io.axi.toShared()

    //  pwm的时间width
    val PwmCtrl = ApbPwm(timerWidth = 16)

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        PwmCtrl.io.apb  -> (0x0000, 20 Bytes),
      )
    )
    PwmCtrl.io.pwm <> io.pwmout
  }




  // Function used to rename all signals of the blackbox
  private def renameIO(): Unit = {
    io.flatten.foreach(bt => {
      if(bt.getName().contains("axi_"))
      {
        bt.setName( bt.getName().replace( "axi_", "*_")  )
        bt.setName( bt.getName().replace( "_payload_", "")  )
        bt.setName( bt.getName().replace( "_", "")  )
        bt.setName( bt.getName().replace( "*", "axi_")  )
        bt.setName("s00_" + bt.getName() )
      }
      if(bt.getName().contains("axiclk"))
      {
        bt.setName("s00_axi_aclk" )
      }
      if(bt.getName().contains("axireset"))
      {
        bt.setName("s00_axi_aresetn" )
      }
    })
  }
  // Execute the function renameIO after the creation of the component
  addPrePopTask(() => renameIO())

}

