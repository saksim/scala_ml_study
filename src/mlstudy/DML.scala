package mlstudy

import java.util

/**
  * Created by Administrator on 2017/8/30.
  */
object DML {
  def fitmodel(Y:Array[Double], period: Int):Model = {
//    半周期
    var p = if(period%2 ==0) period/2 else (period-1)/2
//    将f矩阵定义为一个一行N列的二维矩阵
    val f:Array[Array[Double]] = Array.ofDim(1,period)
//    对f矩阵进行赋值操作（按照傅立叶变换所给的公式）
    for(i <- 0 until period){
      if(i == period -1){
        f(0)(i) = 1
      }else if(i%2 == 0){
        f(0)(i) = 1
      }
    }

//    H矩阵
    var hs = new util.ArrayList[Array[Double]]()
    for(i<-1 to p){
      if ( i == p && period%2 == 0){
        hs.add(Array(-1))
      }else{
        hs.add(Array(math.cos(2 * Math.PI * i / period), math.sin(2 * Math.PI * i / period),
          math.sin(-2 * math.Pi * i / period), math.cos( 2 * math.Pi * i /period)))
      }
    }

    var g = Array.ofDim[Double](period,period)
    if (period % 2 == 0){
      for (i <-0 until period){
        if (i == period - 2 ){
        g(i)(i) = hs.get(i/2)(0)
      }else if(i == period - 1){
          g(i)(i) = 1
      }else if (i % 2 == 0){
          g(i)(i) = hs.get(i/2)(0)
          g(i)(i+1) = hs.get(i/2)(1)
          g(i+1)(i) = hs.get(i/2)(2)
          g(i+1)(i+1) = hs.get(i/2)(3)
        }
      }
    }

    var m:Array[Array[Double]] = Array.ofDim(period,1)

    var c = Array.ofDim[Double](period,period)
    for (i <- 0 until period){
      c(i)(i) = Math.pow(10,7)
    }

    var w = Array.ofDim[Double](period,period)

//    __init__ calculate
    var a:Array[Array[Double]] = null
    var R:Array[Array[Double]] = null
    var f = 0.0
    var Q = 0.0
    var e = 0.0
//    calculate
    for (i<-0 until Y.length){
      a = Magic.multiply(g,m)
//      w = Magic.EM(c,)
      
    }

    new Model(f,g`,m)
  }
}
