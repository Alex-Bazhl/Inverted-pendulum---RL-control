package workshop.SPI_AXI4

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba3.apb.{Apb3, Apb3Config, Apb3Decoder, Apb3SlaveFactory}
import spinal.lib.bus.amba4.axi.{Axi4, Axi4Config, Axi4SharedToApb3Bridge}
import spinal.lib.com.spi.{SpiMaster, SpiMasterCtrl, SpiMasterCtrlGenerics, SpiMasterCtrlMemoryMappedConfig}

object Apb3SPI{

  def getApb3Config() = Apb3Config(addressWidth = 8,dataWidth = 32,selWidth = 1,useSlaveError = false)
}


object SPIConfig{
  def getSPIConfig() = SpiMasterCtrlMemoryMappedConfig( ctrlGenerics = SpiMasterCtrlGenerics( ssWidth =1, timerWidth = 8, dataWidth = 16),
                                                        cmdFifoDepth  = 32,
                                                        rspFifoDepth  = 32)
}

case class ApbSPI(generics : SpiMasterCtrlMemoryMappedConfig) extends Component{


  val io = new Bundle{

    val apb   = slave(Apb3(Apb3SPI.getApb3Config()))
    val spi = master(SpiMaster(ssWidth = generics.ctrlGenerics.ssWidth))
    val interrupt = out Bool()

  }

  val spiCtrl = new SpiMasterCtrl(generics.ctrlGenerics)
  io.spi <> spiCtrl.io.spi

  val busCtrl = Apb3SlaveFactory(io.apb)
  val bridge = spiCtrl.io.driveFrom(busCtrl)(generics)
  io.interrupt := bridge.interruptCtrl.interrupt
}

case class Axi4Wrapper_SPI(addressWidth: Int, dataWidth: Int, idWidth: Int) extends Component{

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

    val spi = master( SpiMaster(ssWidth = SPIConfig.getSPIConfig().ctrlGenerics.ssWidth) )

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
    val SPICtrl = ApbSPI( SPIConfig.getSPIConfig() )

    val apbDecoder = Apb3Decoder(
      master = apbBridge.io.apb,
      slaves = List(
        SPICtrl.io.apb  -> (0x0000, 30 Bytes),
      )
    )
    SPICtrl.io.spi <> io.spi

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

