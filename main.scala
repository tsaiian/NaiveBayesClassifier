import scala.math
import scala.collection.mutable._

case class MyDouble(a:Double, i:Integer = 0)
{
	def mul(that:MyDouble):MyDouble = {
		new MyDouble(this.a * that.a, i)
	}
	def max(that:MyDouble):MyDouble = {
		if(this.a > that.a)
			this
		else
			new MyDouble(that.a, that.i)
	}
}

object Main extends App
{
	val trnData = new ListBuffer[ListBuffer[Double]]
	val testdata = new ListBuffer[ListBuffer[Double]]

	val typeCount = 2
	val featureCount = 3

	trnData += (ListBuffer(1,(6),(180),(12)))
	trnData += (ListBuffer(1,(5.92),(190),(11)))
	trnData += (ListBuffer(1,(5.58),(170),(12)))
	trnData += (ListBuffer(1,(5.92),(165),(10)))
	trnData += (ListBuffer(0,(5),(100),(6)))
	trnData += (ListBuffer(0,(5.5),(150),(8)))
	trnData += (ListBuffer(0,(5.42),(130),(7)))
	trnData += (ListBuffer(0,(5.75),(150),(9)))

	val meanList = ListBuffer[Double]()
	val varianceList = ListBuffer[Double]()

	for((category,array) <- trnData.groupBy(_(0)))
	{
		var fIndex = 0
		for( fIndex <- 1 to featureCount)
		{
			var fArray = array.flatMap(f => extractIndex(f,fIndex))
			var mean = fArray.sum / fArray.length
			var variance = fArray.flatMap(f=>div(f,mean)).sum/(fArray.length-1)

			meanList += mean
			varianceList += variance
		}
	}

	/////////////test/////////////////////
	
	val inputList = List(6, 130, 8)

	def loop_getTypePro(n:Integer, t:Integer,  i:MyDouble):MyDouble = 
	{
		val index = n + t * featureCount
		val m = MyDouble(getP(meanList(index), varianceList(index), inputList(n)), typeCount - t - 1)

		if(n >= 1)
			loop_getTypePro(n - 1, t, i.mul(m))
		else
			i.mul(m)


	}

	def loop_findMax(n:Integer, i:MyDouble):MyDouble = 
	{
		val m = loop_getTypePro(featureCount - 1, n, MyDouble(1.0, typeCount - n - 1))

		println("~~~" + m.i)
		println("!!!!!" + (m.a / 2))

		if(n >= 1)
			loop_findMax(n - 1, i.max(m))
		else
			i.max(m)

	}

	def getP(m:Double, v:Double, x:Double): Double =
	{
		val a = 1 / math.sqrt(2.0 * math.Pi * v)
		val b = math.exp((-1) * (x - m) * (x - m) / (2 * v))

		a * b
	}
	def extractIndex(v:ListBuffer[Double], i:Int) = List(v(i))
	def div(next:Double,mean:Double) = List( (next-mean)*(next-mean))

	val ttt = loop_findMax(typeCount - 1, MyDouble(Double.MinValue))
	println( ttt.a + "   " +  ttt.i )

}