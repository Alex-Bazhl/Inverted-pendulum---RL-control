package workshop.quadDecoder_AXI4

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SharedToApb3Bridge}

object Apb3QuadDecoder{

  def getApb3Config() = Apb3Config(addressWidth = 8,dataWidth = 32,selWidth = 1,useSlaveError = false)
}


object QuadDecoderState extends SpinalEnum {
  val IDLE, S00, S01, S10, S11 = newElement()
}


case class ApbQuadDecoder(countWidth : Int) extends Component{


  val io = new Bundle{

    val A_input = in Bool()
    val B_input = in Bool()

    val apb   = slave(Apb3(Apb3QuadDecoder.getApb3Config()))
  }



  val logic = new Area {
    val enable      = Reg(Bool) init(False)
    val countReset  = Reg(Bool) init(False)
    val count       = Reg(SInt(countWidth bits)) init(0)
    val AB          = BufferCC( io.A_input ## io.B_input )
    import QuadDecoderState._
    val state = RegInit(IDLE)

    when(countReset)
    {
      count := 0
    }

    when(!enable)
    {
      state := IDLE
    }otherwise
    {
        switch(state) {
          is(IDLE) {
              state := S00
          }
          is(S00) {
            when( AB ===  B"2'01") {
              state := S01
              count := count-1;
            }otherwise( AB ===  B"2'10")
            {
              state := S10
              count := count+1;
            }
          }
          is(S01) {
            when( AB ===  B"2'00") {
              state := S00
              count := count+1;
            }otherwise( AB ===  B"2'11")
            {
              state := S11
              count := count-1;
            }
          }
          is(S10) {
            when( AB ===  B"2'00") {
              state := S00
              count := count-1;
            }otherwise( AB ===  B"2'11")
            {
              state := S11
              count := count+1;
            }
          }
          is(S11) {
            when( AB ===  B"2'01") {
              state := S01
              count := count+1;
            }otherwise( AB ===  B"2'10")
            {
              state := S10
              count := count-1;
            }
          }
        }
    }

  }

  val control = Apb3SlaveFactory(io.apb)
  control.driveAndRead(logic.enable, 0)
  control.driveAndRead(logic.countReset, 4)
  control.read(logic.count, 8)
}

case class Axi4Wrapper_QuadDecoder(addressWidth: Int, dataWidth: Int, idWidth: Int) extends Component{

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
    val axiclk = in Bool()
    val axireset = in Bool()

    val A_input = in Bool()
    val B_input = in Bool()
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

    //  countwidth
    val QuadDecoderCtrl = ApbQuadDecoder(countWidth = 32)

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        QuadDecoderCtrl.io.apb  -> (0x0000, 20 Bytes),
      )
    )
    QuadDecoderCtrl.io.A_input <> io.A_input
    QuadDecoderCtrl.io.B_input <> io.B_input
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

