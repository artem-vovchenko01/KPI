package streams

class IndividualSuite extends munit.FunSuite {
  val task = new IndividualTask()
  test("individual task test") {
    assertEquals(task.toList(2 to 4, 500), List[BigInt](500,500,500))
    task.testFunctions
  }
}
