package puzzle2

object IntCodeProgramFactory {
  def createIntCodeProgram(file : String) : IntCodeProgram = {
    val intstructions = file.replaceAll(" ", "").split(",").map(Integer.parseInt)
    new IntCodeProgram(intstructions)
  }

}
