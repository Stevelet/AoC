package puzzle2

object IntCodeProgramFactory {
  def createIntCodeProgram(file : String) : IntCodeProgram =
    new IntCodeProgram(file.replaceAll(" ", "").split(",").map(Integer.parseInt))

}
