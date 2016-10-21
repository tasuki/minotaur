package minotaur.convnet

import org.deeplearning4j.nn.conf.{MultiLayerConfiguration, NeuralNetConfiguration, Updater}
import org.slf4j.{Logger, LoggerFactory}

object Train {
  lazy val log: Logger = LoggerFactory.getLogger("ConvNet")

  def main(args: Array[String]): Unit = {
    log.info("Here we go...")
    log.info("And another one")
    val builder = new NeuralNetConfiguration.Builder()
  }
}
