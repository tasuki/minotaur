import org.deeplearning4j.nn.modelimport.keras.KerasModelImport

object Test {
  def main(args: Array[String]): Unit = {
    val model = KerasModelImport.importKerasModelAndWeights("src/main/resources/neuralnet.h5")
  }
}
