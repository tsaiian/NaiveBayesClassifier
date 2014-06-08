import scala.math
import scala.collection.mutable._
import scala.io.Source
import java.io._

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
	val isContinue = true

	if(isContinue)
		ContinueNB
	else
		DiscreteNB
}

object DiscreteNB
{
  val trndata=new ListBuffer[Pair[String,ListBuffer[Boolean]]]// can't use this, Stream or something else
  val vocabulary =new ListBuffer[String]
  val model =new ListBuffer[ListBuffer[Double]]
  val priors=new ListBuffer[Double]
  val labels=new ListBuffer[String]
  Train("TRAIN321.txt")
  Test("TEST321.txt")
  
  def parseTrain(name:String)={
    val str = Source.fromFile(name )("UTF-8").getLines.toList.map(x => x.split("\t").toList.distinct)
    
    for(i <-0 to str.length-1){
      
      for(j <- 1 to str(i).length-1){
        if(!vocabulary.contains(str(i)(j)))
          vocabulary+=str(i)(j)
      }
      var tmp=vocabulary.map(f=>false)
      for(j <- 1 to str(i).length-1)
    	  tmp(vocabulary.indexOf(str(i)(j)))=true
      trndata+=new Pair[String,ListBuffer[Boolean]](str(i)(0),tmp)
    }
  }
  def extractIndex(v:ListBuffer[Boolean],i:Int) = if (i<v.length) List( v(i)) else List(false)
  def assignModel(i:Int,array:ListBuffer[Pair[String,ListBuffer[Boolean]]]):Unit={
    if(i<vocabulary.length){
      var boolAry=array.flatMap(f=>extractIndex(f._2,i))
      model.last+=boolAry.count(p=>p==true)/array.length.toDouble
      assignModel(i+1,array)
    }
  }
  
  def Train(name:String)={
    parseTrain(name)
    for((category,array)<-trndata.groupBy(_._1)){
	    model+=ListBuffer[Double]()
	    assignModel(0,array)
	    priors+=array.length/trndata.length.toDouble
	    labels+=category
	}
  }
  
  
  
  
  def posteriorprob(prob:Double,i:Int,category:Int,testdata:ListBuffer[Boolean]):Double = {
    if(i<vocabulary.length){
    	val nextprob=if(testdata(i)==true)	model(category)(i) else (1-model(category)(i))
    	posteriorprob(prob*nextprob,i+1,category,testdata)
    }
    else
    	prob
  }

  def CategoryofMaxProb(category:Int,index:Int,max:Double,testdata:ListBuffer[Boolean]):Int={

    if(index<priors.length){
      val current=priors(index)*posteriorprob(1.0,0,index,testdata)
      if (current> max)
        CategoryofMaxProb(index,index+1,current,testdata)
      else
        CategoryofMaxProb(category,index+1,current,testdata)
    }
    else
      category
  }

	def Test(name:String)={val str = Source.fromFile(name)("UTF-8").getLines.toList.map(x => x.split("\t").toList.distinct)

	var writer = new PrintWriter(new File("out2.txt"))
	val trnData = Source.fromFile(name)("UTF-8").getLines.toList.map(x => x.split("\t").toList)
    for(i <-0 to str.length-1)
    {
		var testdata=vocabulary.map(f=>false)
		for(j <- 1 to str(i).length-1)
			if (vocabulary.indexOf(str(i)(j)) != -1)
				testdata(vocabulary.indexOf(str(i)(j)))=true
		for(category <- 0 to priors.length-1){
			println(priors(category)*posteriorprob(1.0,0,category,testdata))
		}

		writer.write(labels(CategoryofMaxProb(0,0,0.0,testdata)))

		var m = 0
		for(m <- 1 to trnData(i).length - 1)
		{
			writer.write("\t" + trnData(i)(m))
		}
		writer.write("\n")
		

    }
    writer.close()
  }
}

object ContinueNB
{
	val trnData = Source.fromFile("training.txt" ).getLines.toList.map(x => x.split("\t").toList)
	val featureCount = trnData(0).length - 1

	val meanList = ListBuffer[Double]()
	val varianceList = ListBuffer[Double]()

	val dictionary = trnData.groupBy(_(0))

	val typeCount = dictionary.count(p=>true)
	for((category,array) <- dictionary)
	{
		var fIndex = 0
		for( fIndex <- 0 to featureCount - 1)
		{
			var fArray = array.flatMap(f => extractIndex(stringList2DoubleList(f.tail), fIndex))
			var mean = fArray.sum / fArray.length
			var variance = fArray.flatMap(f=>div(f,mean)).sum/(fArray.length - 1)

			meanList += mean
			varianceList += variance
		}
	}
	println("END")


	def stringList2DoubleList(il: List[String]): List[Double] = il.map(_.toDouble)

	/////////////test/////////////////////
	val testdata = Source.fromFile("test.txt" ).getLines.toList.map(x => x.split("\t").toList)

	loop_predictAllData()

	def loop_predictAllData()
	{
		var writer = new PrintWriter(new File("out.txt"))
		var fIndex = 0
		for( fIndex <- 0 to testdata.length - 1)
		{
			val ttt = loop_findMax(typeCount - 1, MyDouble(Double.MinValue), fIndex)
			
			writer.write(getactualTypeName(ttt))
			var m = 0
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
		var p = 0
		var result = ""
		for((category,array) <- dictionary)
		{
			if(p == m.i)
			{
				//println("" + category)
				//println("P" + (m.a / 2))

				result = category
			}
			p = p + 1
		}
		result
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
	def extractIndex(v:List[Double], i:Int) = List(v(i))
	def div(next:Double,mean:Double) = List( (next-mean)*(next-mean))



}