package mlstudy

/**
  * Created by Administrator on 2017/8/30.
  */
class Model(val F:Array[Array[Double]], val G:Array[Array[Double]], val m:Array[Array[Double]]) {
  def forecast(ts:Array[Double], forecast: Int): Array[Double] = {
    var f = m
    var result = new Array[Double](forecast)
    for(i<-0 until forecast){
      f = Magic.multiply(G,f)
      result(i) = Math.abs(Magic.multiply(F,f)(0)(0))
    }
    result
  }
}
