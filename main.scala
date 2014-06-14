import scala.math
import scala.collection.mutable._
import scala.io.Source

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
	println("Continue or Discrete?(C/D)")
	import sys.process._
	val c = Console.in.read

	if(c == 99 || c == 67)
		ContinueNB
	else
		DiscreteNB
}

object DiscreteNB
{

	val str = Source.fromFile("TRAIN321.txt")("UTF-8").getLines.toList.map(x => x.split('\t').toList.distinct)
	val vocabulary =str.map(f=>f.drop(1)).flatMap(f=>f).distinct
	val trndata=(0 to str.length-1).map(f=>Pair[String,List[Boolean]](str(f)(0),(0 to vocabulary.length-1).toList.map(g=>str(f).drop(1).contains(vocabulary(g)))))

	def extractIndex(v:List[Boolean],i:Int) = if (i<v.length) List( v(i)) else List(false)
	val model = trndata.groupBy(_._1).map(f=>(0 to vocabulary.length-1).map(g=>f._2.flatMap(h=>extractIndex(h._2,g)).count(p=>p==true)/f._2.length.toDouble)).toList
	val priors = trndata.groupBy(_._1).map(f=>f._2.length/trndata.length.toDouble).toList
	val labels = trndata.groupBy(_._1).map(f=>f._1).toList
	  
	Test("TEST321.txt")
	  
	def posteriorprob(prob:Double,i:Int,category:Int,testdata:List[Boolean]):Double = {
	if(i<vocabulary.length){
		val nextprob=if(testdata(i)==true)	model(category)(i) else (1-model(category)(i))
		posteriorprob(prob*nextprob,i+1,category,testdata)
	}
	else
		prob
	}

	def CategoryofMaxProb(category:Int,index:Int,max:Double,testdata:List[Boolean]):Int=
	{
		if(index<priors.length)
		{
	  		val current=priors(index)*posteriorprob(1.0,0,index,testdata)
	  		if (current> max)
				CategoryofMaxProb(index,index+1,current,testdata)
	  		else
	    		CategoryofMaxProb(category,index+1,current,testdata)
		}
		else
			category
	}

	def Test(name:String)=
	{
		import java.io._
		var writer = new PrintWriter(new File("out2.txt"))
		val str = Source.fromFile(name)("UTF-8").getLines.toList.map(x => x.split("\t").toList.distinct)
		for( j <-0 to str.length-1)
		{
			writer.write((0 to str.length-1).toList.map(f=>labels(CategoryofMaxProb(0,0,0.0,(0 to vocabulary.length-1).toList.map(g=>str(f).drop(1).contains(vocabulary(g)))))).toList(j)+"\t")
			str(j).drop(1).foreach(f=>writer.write(f+"\t"))
			writer.write('\n')
		}

		writer.close()
	}
}


object ContinueNB
{
	val trnData = Source.fromFile("training.txt" ).getLines.toList.map(x => x.split("\t").toList)
	val featureCount = trnData(0).length - 1
	println("trnData\n" + trnData)

	val dictionary = trnData.groupBy(_(0))
	
	val typeCount = dictionary.count(p=>true)
	def Avg(l:List[Double]):Double=l.sum/l.length
	def Var(l:List[Double]):Double=l.flatMap(f=>div(f,Avg(l))).sum/(l.length - 1)
	def extractIndex(v:List[Double], i:Int) = List(v(i))
	def div(next:Double,mean:Double) = List( (next-mean)*(next-mean))

	def stringList2DoubleList(il: List[String]): List[Double] = il.map(_.toDouble)
	val groupedData=dictionary.map(g=>(0 to featureCount-1).toList.map(h=>g._2.flatMap(f => extractIndex(stringList2DoubleList(f.tail), h))))
	val meanList=groupedData.map(f=>f.map(g=>Avg(g))).flatMap(f=>f).toList
	val varianceList=groupedData.map(f=>f.map(g=>Var(g))).flatMap(f=>f).toList
	
	println("END")

	/////////////test/////////////////////
	val testdata = Source.fromFile("test.txt" ).getLines.toList.map(x => x.split("\t").toList)

	loop_predictAllData()

	def loop_predictAllData()
	{
		import java.io._
		var writer = new PrintWriter(new File("out.txt"))
		val fIndex = 0
		for( fIndex <- 0 to testdata.length - 1)
		{
			val ttt = loop_findMax(typeCount - 1, MyDouble(Double.MinValue), fIndex)
			
			writer.write(getactualTypeName(ttt))
			val m = 0
			for(m <- 1 to testdata(fIndex).length - 1 )
			{
				writer.write("\t" + testdata(fIndex)(m))
			}
			writer.write("\n")

		}
		writer.close()

		println("Finish")

	}

	def loop_getTypePro(n:Integer, t:Integer,  i:MyDouble, testDataNo:Integer):MyDouble = 
	{
		val index = n + t * featureCount

		val m = MyDouble(getP(meanList(index), varianceList(index), testdata(testDataNo).tail(n).toDouble), t)

		if(n >= 1)
			loop_getTypePro(n - 1, t, i.mul(m), testDataNo)
		else
			i.mul(m)
	}

	def loop_findMax(n:Integer, i:MyDouble, testDataNo:Integer):MyDouble = 
	{
		val m = loop_getTypePro(featureCount - 1, n, MyDouble(1.0, n), testDataNo)

		if(n >= 1)
			loop_findMax(n - 1, i.max(m), testDataNo)
		else
			i.max(m)
	}

	def getactualTypeName(m:MyDouble):String = 
	{
		dictionary.keySet.toList(m.i)
	}
	def getP(m:Double, v:Double, x:Double): Double =
	{
		val a = 1 / math.sqrt(2.0 * math.Pi * v)
		val b = math.exp((-1) * (x - m) * (x - m) / (2 * v))

		if(v != 0.0)
			a * b
		else
			50
	}

}