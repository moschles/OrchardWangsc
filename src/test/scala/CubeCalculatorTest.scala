class CubeCalculatorTest extends org.scalatest.FunSuite {

  test("CubeCalculator.cube on positive and zero") {
    assert(CubeCalculator.cube(3) === 27)
	    assert(CubeCalculator.cube(0) === 0)

  }
  test("CubeCalculator.cube on negative values") {
    assert(CubeCalculator.cube(-7) === -343)
	assert(CubeCalculator.cube(-1) === -1)
  }
}
