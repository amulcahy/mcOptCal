package com.dragongate_technologies.mcOptCal

import _root_.android.app.{Activity, Notification, NotificationManager, PendingIntent, Service}
import _root_.android.content
import _root_.android.content.{ComponentName, Context, Intent, ServiceConnection, SharedPreferences}
import _root_.android.graphics.{Bitmap, Canvas, Color, Paint}
import _root_.android.graphics.drawable.Drawable
import _root_.android.os.{Binder, Bundle, Handler, IBinder}
import _root_.android.preference._
import _root_.android.text.method.ScrollingMovementMethod
import _root_.android.util.{AttributeSet, Log}
import _root_.android.view.{SurfaceView, View}
import _root_.android.view.View.OnClickListener
import _root_.android.widget.{Button, TextView}

import java.io.File
import java.io.RandomAccessFile
import java.nio.channels.FileChannel.MapMode._
import java.nio.channels
import java.nio.MappedByteBuffer

import scala.actors.Actor._
import scala.actors._
import scala.annotation.tailrec
import scala.collection.parallel.immutable._
import scala.collection.parallel.mutable._
import scala.concurrent.ops._
import scala.util.parsing.combinator._
import scala.util.Random
import scala.math._

/*
* References
* Wilmott : Paul Wilmott on Quantitative Finance
*
*
*/

object dgmath {

  val rng: Random = new Random

  var spareready: Boolean = false
  var spare: Double = 0.0D

  val sqr = (x: Double) => x * x

  val standardNormalDistribution = (x: Double) => {
    val top: Double = exp(-0.5D*pow(x, 2.0D))
    val bottom: Double = sqrt(2.0D*Math.Pi)
    top/bottom
  }

  val p = 0.2316419D
  val b1 = 0.319381530D
  val b2 = -0.356563782D
  val b3 = 1.781477937D
  val b4 = -1.821255978D
  val b5 = 1.330274429D

  val cumulativeDistribution = (x: Double) => {
    val t = 1.0D/(1.0D+p*abs(x))
    val t1 = b1*pow(t, 1)
    val t2 = b2*pow(t, 2)
    val t3 = b3*pow(t, 3)
    val t4 = b4*pow(t, 4)
    val t5 = b5*pow(t, 5)
    val b = t1+t2+t3+t4+t5
    val cd = 1.0D-standardNormalDistribution(x)*b
    if (x < 0)
      1.0D-cd
    else
      cd
  }

  val bs_d1 = (s: Double, k: Double, r: Double, t: Double, v: Double) => {
    val top: Double = log(s/k)+(r+pow(v, 2.0D)/2.0D)*t
    val bottom: Double = v*sqrt(t)
    top/bottom
  }

  val bs_d2 = (s: Double, k: Double, r: Double, t: Double, v: Double) => {
    bs_d1(s, k, r, t, v)-v*sqrt(t)
  }

  val bsEuropeanCallVal = (s: Double, k: Double, r: Double, t: Double, v: Double) => {
    val cd1: Double = cumulativeDistribution(bs_d1(s, k, r, t, v))
    val cd2: Double = cumulativeDistribution(bs_d2(s, k, r, t, v))
    s * cd1 - k * exp(-r * t) * cd2
  }

  val bsEuropeanPutVal = (s: Double, k: Double, r: Double, t: Double, v: Double) => {
    val cmd1: Double = cumulativeDistribution(-bs_d1(s, k, r, t, v))
    val cmd2: Double = cumulativeDistribution(-bs_d2(s, k, r, t, v))
    k * exp(-r * t) * cmd2 - s * cmd1
  }


  // from Wilmott
  val optionValue3DUS = (vol: Double, intRate: Double, ptype: Boolean, strike: Double, expiration: Double, etype: Boolean, nas: Int) => {

    val s: Array[Double] = new Array[Double](nas+1) // Asset Array
    val payoff: Array[Double] = new Array[Double](nas+1) // Payoff Array
    val dS = 2d*strike/nas // Infinity is twice the strike
    var dt = 0.9d / (vol*vol) / (nas*nas) // for stability
    val nts = (expiration/dt).toInt + 1 // number of time steps
    dt = expiration/nts // to ensure that expiration is an integer number of time steps away

    val v: Array[Array[Double]] = Array.ofDim[Double](nas+1, nts+1)
    var q = 1
    // test for call or put
    if (ptype) {
      q = -1
    }
    for (i <- 0 to nas) {
      s(i) = i*dS // set up s array
      v(i)(0) = max(q*(s(i)-strike), 0) // set up payoff
      payoff(i) = v(i)(0) // store pay off
    }
    // time loop
    for (k <- 1 to nts) {
      // asset loop, end points treated separately
      for (i <- 1 until nas) {
        val delta = (v(i+1)(k-1)-v(i-1)(k-1))/2/dS // central difference
        val gamma = (v(i+1)(k-1)-2*v(i)(k-1)+v(i-1)(k-1))/dS/dS // central difference
        val theta = -0.5*(vol*vol)*(s(i)*s(i))*gamma-intRate*s(i)*delta+intRate*v(i)(k-1) // black scholes
        v(i)(k) = v(i)(k-1)-dt*theta
      }
      v(0)(k) = v(0)(k-1)*(1-intRate*dt) // boundary condition at s=0
      v(nas)(k) = 2*v(nas-1)(k)-v(nas-2)(k) // boundary condition at s=infinity
      // check for early exercise
      if (etype) {
        for (i <- 0 to nas) {
          v(i)(k) = max(v(i)(k), payoff(i))
        }
      }
    }

    // output array
    v
  }

  val bsAmericanCallVal = (s: Double, k: Double, r: Double, t: Double, v: Double) => {
    val dummyVal = 123.0D
    s-s+dummyVal
  }

  val bsBermudanCallVal = (s: Double, k: Double, r: Double, t: Double, v: Double) =>   {
    val dummyVal = 456.0D
    s-s+dummyVal
  }
  
  def getGaussian(): Double = {
    var result: Double = 0.0
    if (spareready) {
      spareready = false
      result = spare
    } else {
      var u: Double = 0
      var v: Double = 0
      var s: Double = 0
      do {
        u = rng.nextDouble*2.0D-1.0D
        v = rng.nextDouble*2.0D-1.0D
        s = u*u + v*v
      } while (s>=1 || s==0.0D)
      spare = v*sqrt(-2.0D*log(s)/s)
      spareready = true
      result = u*sqrt(-2.0D*log(s)/s)
    }
    result
  }

  /*def evaluateBS(s: Double, x: Double, r: Double, sigma: Double, dt: Double): Double = {
    s*exp((r-sigma*sigma/2.0D)*dt+sigma*(rng.nextGaussian)*sqrt(dt))
  }*/

  val optEvaluateBS = (s: Double, d1: Double, d2: Double) => {
    //s*exp((r-sigma*sigma/2.0D)*dt+sigma*(rng.nextGaussian)*sqrt(dt))
    //val d1: Double = (r-sigma*sigma/2.0D)*dt
    //val d2: Double = sigma*sqrt(dt))
    s*exp(d1+rng.nextGaussian*d2)
  }

  /*def evaluateBSCall(s: Double, x: Double, r: Double, sigma: Double, dt: Double): Double = {
    max(0.0D, evaluateBS(s, x, r, sigma, dt)-x)
  }*/

  val optEvaluateBSCall = (s: Double, x: Double, d1: Double, d2: Double) => {
    //val d1: Double = (r-sigma*sigma/2.0D)*dt
    //val d2: Double = sigma*sqrt(dt))
    //max(0.0D, optEvaluateBS(s, d1, d1)-x)
    max(0.0D, (s*exp(d1+rng.nextGaussian*d2))-x)
  }

  /*def evaluateBSPut(s: Double, x: Double, r: Double, sigma: Double, dt: Double) = {
    max(0.0D, x-evaluateBS(s, x, r, sigma, dt))
  }*/

  def scaleY(y: Double): Float = {
    var scaledY: Double = y
    scaledY *= 1000
    scaledY += 50
    scaledY.toFloat
  }

}


/**
 * Matrix is a mutable matrix class.
 *
 * @note An array is used internally for performance reasons.
 *
 * @constructor Create a new m x n Matrix.
 *
 * @param m The number of rows.
 * @param n The number of columns.
 */
class Matrix(m: Int, n: Int, _context: Context) { //todo make this generic for other types than doubles
  val rows = m
  val cols = n
  val context = _context
  val overThreshold = (m*n > Matrix.threshold)

  protected val tempFile = {
    if (overThreshold) {
      //val dataDir = new File("/sdcard/mcTempFiles/") //todo
      val dataDir = context.getExternalFilesDir(null)
      val fileName = this.hashCode.toString+System.nanoTime.toString+".dat"
      assert(!(new File(dataDir, fileName)).isFile())
      new File(dataDir, fileName)
    } else {
      null
    }
  }

  protected val position = (x: Int, y: Int) => (x * cols + y)*8

  protected lazy val mArray = {
    if (overThreshold)
      null //Array.ofDim[Double](1, 1)
    else
      Array.ofDim[Double](rows, cols)
  }

  protected val mMap = {
    if (overThreshold)
      new RandomAccessFile(tempFile, "rw").getChannel().map(READ_WRITE, 0, rows*cols*8)
    else
      null
  }

  /**
   * @constructor Create a new m x 1 Matrix.
   *
   * @param m The number of rows.
   */
  def this(m: Int, context: Context) = this(m, 1, context)

  /**
   * @constructor Create a new Matrix from inArray.
   *
   * @param m The number of rows.
   */
  def this(inArray: Array[Array[Double]], context: Context) = {
    this(inArray.length, inArray(0).length, context)
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < cols) {
        this(i, j) =inArray(i)(j)
        j += 1
      }
      i += 1
    }
  }

  def apply(i: Int, j: Int): Double = {
    if (overThreshold)
      mMap.getDouble(position(i, j))
    else
      mArray(i)(j)
  }

  def update(i: Int, j: Int, elem: Double) {
    if (overThreshold)
      mMap.putDouble(position(i, j), elem)
    else
      mArray(i)(j) = elem
  }

  def update(i: Int, row: Array[Double]) {
    var j = 0
    while (j < cols) {
      this(i, j) = row(j)
      j += 1
    }
  }

  def *(b: Matrix): Matrix = {
    assert (cols == b.rows)
    val p = b.cols
    val ab = new Matrix(rows, p, b.context)
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < p) {
        var acc = 0.0
        var k = 0
        while (k < cols) {
          acc += this(i, k)*b(k, j)
          k += 1
        }
        ab(i, j) = acc
        j += 1
      }
      i += 1
    }
    ab
  }

  /**
   * The method divideRowByC transforms the matrix by dividing a row by a scalar
   * @param row The row that will be operated on.
   * @param c The scalar.
   */
  private def divideRowByC(row: Int, c: Double) = {
    var j = 0
    while (j < cols) {
      this(row, j) = this(row, j)/c + 0.0
      j += 1
    }
  }

  /**
   * The method addRowACtoB transforms the matrix by adding a row multiplied by a scalar to another row.
   * @param a The row that will be added to the target. (unmodified by the method)
   * @param c The scalar.
   * @param b The target row that will be operated on.
   */
  private def addRowACtoB(a: Int, c: Double, b: Int) = {
    var j = 0
    while (j < cols) {
      this(b, j) = (this(b, j) + this(a, j)*c) + 0.0
      j += 1
    }
  }

  /**
   * The method swapRows transforms the matrix by swapping rows a and b.
   */
  private def swapRows(a: Int, b: Int) {
    var j = 0
    while (j < cols) {
      val temp = this(a, j)
      this(a, j) = this(b, j)
      this(b, j) = temp
      j += 1
    }
  }

  override def toString(): String = {
    val strB = new StringBuilder
    strB.append("[")
    var i = 0
    while (i < rows) {
      var j = 0
      while (j < cols) {
        strB.append("% 6.2f".format(this(i, j)))
        strB.append(" ")
        j += 1
      }
      if (i < rows-1)
        strB.append("\n ")
      i += 1
    }
    strB.append("]\n")
    strB.result
  }

}

object Matrix {
  
  var threshold = 50000

  private def identity(m: Int, n: Int, context: Context): Matrix = {
    assert(m == n)
    val idMatrix = new Matrix(m, n, context)
    var idx = 0
    while (idx < m) {
      idMatrix(idx, idx) = 1.0D
      idx += 1
    }
    idMatrix
  }

  /**
   * Generate the inverse matrix using Gaussian-Jordan elimination.
   */
  val gjInverse = (matrix: Matrix) => {
    val gjMatrix = new Matrix(matrix.rows, matrix.cols, matrix.context)
    var i = 0
    while (i < matrix.rows) {
      var j = 0
      while (j < matrix.cols) {
        gjMatrix(i, j) = matrix(i, j)
        j += 1
      }
      i += 1
    }
    val invMatrix = Matrix.identity(matrix.rows, matrix.cols, matrix.context)

    var pivot = 0
    while ((pivot < matrix.rows) && (pivot < matrix.cols)) {
      var i = pivot
      var maxElem = gjMatrix(i, pivot)
      var j = pivot
      while (j < matrix.rows) {
        if (gjMatrix(j, pivot) > maxElem) {
          i = j
          maxElem = gjMatrix(j, pivot)
        }
        j += 1
      }
      if (pivot != i) {
        gjMatrix.swapRows(pivot, i)
        invMatrix.swapRows(pivot, i)
      }
      val c1 = gjMatrix(pivot, pivot)
      gjMatrix.divideRowByC(pivot, c1)
      invMatrix.divideRowByC(pivot, c1)
      var k = 0
      while (k < matrix.rows) {
        if (k != pivot) {
          val c2 = -gjMatrix(k, pivot)
          gjMatrix.addRowACtoB(pivot, c2, k)
          invMatrix.addRowACtoB(pivot, c2, k)
        }
        k += 1
      }
      pivot = pivot+1
    }
    invMatrix
  }

  /**
   * Generate the transpose Matrix.
   */
  val transpose = (matrix: Matrix) => {
    val tMatrix = new Matrix(matrix.cols, matrix.rows, matrix.context)
    var i = 0
    while (i < matrix.rows) {
      var j = 0
      while (j < matrix.cols) {
        tMatrix(j, i) = matrix(i, j)
        j += 1
      }
      i += 1
    }
    tMatrix
  }

}


/*class BigMatrix(m: Int, n: Int) extends Matrix(m, n) { //todo make this generic for other types than doubles

  protected val tempFile = {
    val dataDir = new File("/sdcard/mcTempFiles/") //todo
    val fileName = this.hashCode.toString+System.nanoTime.toString+".dat"
    assert(!(new File(dataDir, fileName)).isFile())
    new File(dataDir, fileName)
  }

  protected val mMap = new RandomAccessFile(tempFile, "rw").getChannel().map(READ_WRITE, 0, rows*cols*8)

  protected val position = (x: Int, y: Int) => (x * cols + y)*8

  override def apply(i: Int, j: Int): Double = {
    //mArray(i)(j)
    mMap.getDouble(position(i, j))
  }

  override def update(i: Int, j: Int, elem: Double) {
    //mArray(i)(j) = elem
    mMap.putDouble(position(i, j), elem)
  }

}*/



/**
  * LsmParams is container for the least squares Monte Carlo algorithm parameters.
  *
  * @param payoffFn The the payoff function.
  * @param numPaths The number of simulation paths.
  * @param expiry The number of years until expiry.
  * @param numSteps The number of exercise steps per year.
  * @param stock The price of the underlying stock.
  * @param strike The exercise price.
  * @param rate The interest rate.
  * @param volatility The volatility of returns.
  * @param numSamples todo
  * @param threshold todo
  * @param uiUpdateInterval todo
  * @param context todo
  * 
  * @note dT = expiry / numSteps
  */
case class LsmParams(
  payoffFn: (Double, LsmParams) => Double,
  isPut: Boolean,
  numPaths: Int,
  expiry: Int,
  numSteps: Int,
  stock: Double,
  strike: Double,
  rate: Double,
  volatility: Double,
  numSamples: Int,
  threshold: Int,
  uiUpdateInterval: Int,
  context: Context
  ) {

  /**
   * dT = expiry / numSteps
   */
  val dT: Double = expiry.toDouble / numSteps.toDouble

  override def toString(): String = {
    val strB = new StringBuilder
    strB.append("[\n")
    strB.append(" payoffFn: "+payoffFn+"\n")
    val formatStr = "% .3f"
    strB.append( {
        if (isPut)
          " Type: Put\n"
        else
          " Type: Call\n"
      })
    strB.append(" numPaths: "+numPaths+"\n")
    strB.append(" numSteps: "+numSteps+"\n")
    strB.append(" dT: "+(formatStr.format(dT))+"\n")
    strB.append(" expiry: "+expiry+"\n")
    strB.append(" stock: "+(formatStr.format(stock))+"\n")
    strB.append(" strike: "+(formatStr.format(strike))+"\n")
    strB.append(" rate: "+(formatStr.format(rate))+"\n")
    strB.append(" volatility: "+(formatStr.format(volatility))+"\n")
    strB.append("]\n")
    strB.result
  }
}

object lsm {
  type PayoffFn = (Double, LsmParams) => Double
  import Matrix._
  private val TAG: String = "lsm"

  private val DEBUG = false

  private val rng: Random = new Random
  private val sqr = (x: Double) => (x * x)

  /**
    * Generate a matrix of simulated stock prices from the lsm parameters.
    *
    * @param params The lsm parameters.
    *
    * @note Half of the paths are antithetic
    */
  val genPriceMatrix = (params: LsmParams) => {
    assert( params.numPaths%2 == 0)
    val n = params.numSteps+1
    val pMatrix = new Matrix(params.numPaths, n, params.context)
    //val pMatrix = new BigMatrix(params.numPaths, n)
    val a = (params.rate -  sqr(params.volatility)*0.5)*params.dT
    val b = params.volatility*sqrt(params.dT)
    var i = 0
    while (i < params.numPaths/2) {
      pMatrix(i*2, 0) = params.stock
      pMatrix(i*2+1, 0) = params.stock
      var s1 = params.stock
      var s2 = params.stock
      var j = 1
      while (j < n) {
        val dZ = rng.nextGaussian
        s1 = s1*exp(a + b*dZ)
        s2 = s2*exp(a - b*dZ) // antithetic path
        pMatrix(i*2, j) = s1
        pMatrix(i*2+1, j) = s2
        j += 1
      }
      i += 1
    }
    pMatrix
  }

  val getPriceMatrixSample = (numSamples: Int, priceMatrix: Matrix) => {
    val rows = min(numSamples, priceMatrix.rows)
    val sampleArray = Array.ofDim[Double](rows + 2, priceMatrix.cols)
    var i = 1
    while (i < rows+1) {
      var j = 0
      while (j < priceMatrix.cols) {
        sampleArray(i)(j) = priceMatrix(i, j)
        j += 1
      }
      i += 1
    }
    sampleArray
  }

  /**
    * Regress y onto the basis function applied to current stock prices.
    *
    * @param fnX The results of applying the basis function to current stock prices.
    * @param y The discounted cash flows received from continuing.
    *
    * @note The current algorithm (calculating the inverse matrix using gaussian elimination) might not be the most suitable one for this task.
    */
  private val regress = (fnX: Matrix, y: Matrix) => {
    val transposeFnX = transpose(fnX)
    val inverse = gjInverse(transposeFnX*fnX)
    val coeff = inverse*transposeFnX*y
    fnX*coeff
  }

  /**
    * Calculate the optimum decision (exercise or continue to hold) for the current step and update the cash flow matrix to reflect this.
    *
    * @param step The current step (instant in time)
    * @param fn The basis function.
    * @param params The lsm parameters.
    * @param priceMatrix The stock price matrix.
    * @param cfMatrix The cash flow matrix.
    *
    * @note The cash flow matrix is modified by this function.
    *
    * @note The current algorithm (calculating the inverse matrix using gaussian elimination) might not be the most suitable one for this task.
    */
  val calcCFAtStep = (
    step: Int, 
    fn: (Double) => Array[Double], 
    params: LsmParams, 
    priceMatrix: Matrix, 
    cfMatrix: Matrix
    ) => {
    //Log.d(TAG, "calcCFAtStep-Start" )
    //val exerciseFn = (idx: Int, step: Int) => (params.strike - priceMatrix(idx, step))
    /*val payoffFn = {
      if (params.isPut)
        EqnParsers.parseEval("strike-mcPrice")
      else 
        EqnParsers.parseEval("mcPrice-strike")
    }*/
    /*val exerciseFn = {
      if (params.isPut)
        (idx: Int, step: Int) => (params.strike - priceMatrix(idx, step))
      else 
        (idx: Int, step: Int) => (priceMatrix(idx, step) - params.strike)
    }*/

    if (step == params.numSteps) {
      var i = 0
      while (i < params.numPaths) {
        //if (exerciseFn(i, step) > 0)
        //  cfMatrix(i, step-1) = exerciseFn(i, step)
        if (params.payoffFn(priceMatrix(i, step), params) > 0)
          cfMatrix(i, step-1) = params.payoffFn(priceMatrix(i, step), params)
        i += 1
      }
      if (DEBUG) {
        println("cfMatrix\n"+cfMatrix)
      }
      cfMatrix
    } else {
      var xySize = 0
      var i = 0
      while (i < params.numPaths) {
        //if (exerciseFn(i, step) > 0)
        if (params.payoffFn(priceMatrix(i, step), params) > 0)
          xySize += 1 
        i += 1
      }

      //val x = new Matrix(xySize, 1)
      val y = new Matrix(xySize, 1, params.context)
      val fnX = new Matrix(xySize, fn(0).length, params.context)
      i = 0
      var k = 0
      while (i < params.numPaths) {
        //if (exerciseFn(i, step) > 0) {
        if (params.payoffFn(priceMatrix(i, step), params) > 0) {
          //x(k, 0) =  priceMatrix(i, step)
          //fnX(k) = fn(x(k, 0))
          fnX(k) = fn(priceMatrix(i, step))
          var maxCF = 0.0
          var j = step // step-1 is current
          while (j < cfMatrix.cols) {
            maxCF = max(maxCF, cfMatrix(i, j)*exp(-params.rate*((j-(step-1))*params.dT)))
            j += 1
          }
          y(k, 0) = maxCF
          k += 1
        }
        i += 1
      }
      val contMatrix = regress(fnX, y)
      //Log.d(TAG, "calcCFAtStep-1" )

      i = 0
      var j = 0
      while (i < params.numPaths) {
        //if (exerciseFn(i, step) > 0) {
        if (params.payoffFn(priceMatrix(i, step), params) > 0) {
          cfMatrix(i, step-1) = {
            //if (exerciseFn(i, step) >= contMatrix(j, 0) ) {
            if (params.payoffFn(priceMatrix(i, step), params) >= contMatrix(j, 0) ) {
              k = step
              while (k < cfMatrix.cols) {
                cfMatrix(i, k) = 0
                k += 1
              }
              //exerciseFn(i, step)
              params.payoffFn(priceMatrix(i, step), params)
            } else {
              0.0 // don't exercise now
            }
          }
          j += 1
        }
        i += 1
      }
      //Log.d(TAG, "calcCFAtStep-End" )
      cfMatrix
    }
  }

  /**
    * Calculate the option value and standard error from the cash flow matrix.
    *
    * @param params The lsm parameters.
    * @param cfMatrix The cash flow matrix.
    *
    */
  val optionValueStdErr = (params: LsmParams, cfMatrix: Matrix) => {
    val m = params.numPaths
    val pvArray = Array.ofDim[Double](m)

    var i = 0
    var n = 0
    var mean = 0.0D
    var m2 = 0.0D
    while (i < m) {
      var j = 0
      while (j < cfMatrix.cols) {
        if (cfMatrix(i, j) > 0)
          pvArray(i) = (cfMatrix(i, j)*exp(-params.rate*((j+1)*params.dT)))
        j += 1
      }

      val x = pvArray(i)
      n += 1
      val delta = x - mean
      mean += delta/n
      m2 += delta*(x - mean)

      i += 1
    }
    val optionValue = pvArray.sum / m
    val variance = m2/(n-1)
    val sampStdDev = sqrt(variance)
    val stdErr = sampStdDev/sqrt(m)
    if (DEBUG) {
      println("pvArray")
      prettyPrint(pvArray)
    }
    (optionValue, stdErr)
  }


  /**
    * Recurse through the cash flow matrix calculating the cash flows at each time step.
    *
    * @param step The current time step.
    * @param params The lsm parameters.
    * @param priceMatrix The stock price matrix.
    * @param cfMatrix The cash flow matrix.
    * @param basisFn The basis function.
    *
    */
  /*@tailrec val recurseCF:(Int, LsmParams, Matrix, Matrix, (Double) => Array[Double], Actor) => Matrix =
  (step: Int, params: LsmParams, priceMatrix: Matrix, cfMatrix: Matrix, basisFn: (Double) => Array[Double], callerService: Actor) => {
    assert(priceMatrix.rows == params.numPaths)
    assert(priceMatrix.cols == params.numSteps+1)
    assert(cfMatrix.rows == params.numPaths)
    assert(cfMatrix.cols == params.numSteps)

    step match {
      case 1 => calcCFAtStep(1, basisFn, params, priceMatrix, cfMatrix)
      case _ => {
        if (callerService != null)
          callerService ! lsmStatusReport(step, params.numSteps)
        else
          Log.d(TAG, "recurseCF step="+step )

        receiveWithin(1) {
          case TIMEOUT => { }
          case CalcStop => {
            Log.d(TAG, "Calc.stopping" )
            exit()
          }
        }
        val newCFMatrix = calcCFAtStep(step, basisFn, params, priceMatrix, cfMatrix)
        recurseCF(step-1, params, priceMatrix, newCFMatrix, basisFn, callerService)
      }
    }
  }*/

  val recurseCF = (initStep: Int, params: LsmParams, priceMatrix: Matrix, cfMatrix: Matrix, basisFn: (Double) => Array[Double], callerService: Actor) => {
    assert(priceMatrix.rows == params.numPaths)
    assert(priceMatrix.cols == params.numSteps+1)
    assert(cfMatrix.rows == params.numPaths)
    assert(cfMatrix.cols == params.numSteps)

    var step = initStep
    var newCFMatrix = cfMatrix
    /*val payoffFn = {
      if (params.isPut)
        EqnParsers.parseEval("strike-mcPrice")
      else 
        EqnParsers.parseEval("mcPrice-strike")
    }*/
    var abort = false // 1.01
    while ((step > 0) && !abort) { // 1.01
      if (step%params.uiUpdateInterval == 0) {
        if (callerService != null)
          callerService ! lsmStatusReport(step, params.numSteps)
        else
          Log.d(TAG, "recurseCF step="+step )
        receiveWithin(1) {
          case TIMEOUT => { }
          case CalcStopLSM => {
            Log.d(TAG, "CalcStopLSM" )
            abort = true // 1.01
          }
        }
      }

      // 1.01
      if (!abort)
        newCFMatrix = calcCFAtStep(step, basisFn, params, priceMatrix, newCFMatrix)
      step -= 1
    }
    // 1.01
    if (abort) {
      callerService ! lsmAbortReport
      exit()
    }
    newCFMatrix
  }

  /**
    * Calculate the value of the option specified by the parameters using the least squares method.
    *
    * @param params The lsm parameters.
    */
  val lsmOptionValue = (params: LsmParams, callerService: Actor) => {
    Matrix.threshold = params.threshold
    val priceMatrix = genPriceMatrix(params)
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps, params.context) 
    //val initCFMatrix = new BigMatrix(params.numPaths, params.numSteps) 

    // normalize parameter x to prevent underflows
    val basisFn = (x: Double) => Array (
      1.0,
      exp(-x/(2.0*params.stock)),
      exp(-x/(2.0*params.stock))*(1.0-x/params.stock),
      exp(-x/(2.0*params.stock))*(1.0-2.0*x/params.stock+sqr(x/params.stock)/2.0)
      )

    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, initCFMatrix, basisFn, callerService)

    //todo create stopping matrix

    if (DEBUG)
      println(cfMatrix)
    val oVsE = optionValueStdErr(params, cfMatrix)
    (oVsE._1, oVsE._2, getPriceMatrixSample(params.numSamples, priceMatrix))
  }

  /**
    * First walk through example in the LSM paper
    *
    */
  /*def lsmOptionValueSimpleExample() {
    val startTime = System.nanoTime
    val params = LsmParams( true, 8, 3, 3, 1.0, 1.10, 0.06, 0.0, 8, 50000, 10 ) 
    println ("params = "+params)

    val priceMatrix = new Matrix(Array(
      Array(1.00, 1.09, 1.08, 1.34),
      Array(1.00, 1.16, 1.26, 1.54),
      Array(1.00, 1.22, 1.07, 1.03),
      Array(1.00, 0.93, 0.97, 0.92),
      Array(1.00, 1.11, 1.56, 1.52),
      Array(1.00, 0.76, 0.77, 0.90),
      Array(1.00, 0.92, 0.84, 1.01),
      Array(1.00, 0.88, 1.22, 1.34)
      ), params.context)
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps, params.context) 
    val basisFn = (x: Double) => Array ( 1.0, x, sqr(x) )
    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, initCFMatrix, basisFn, null)

    if (DEBUG)
      println(cfMatrix)
    val endTime = System.nanoTime

    val lsmOV = optionValueStdErr(params, cfMatrix)
    prettyPrint(lsmOV, endTime-startTime)
  }*/

  private def prettyPrint(a: Array[Double]) {
    var str: String = "["
    var i = 0
    while (i < a.length) {
      str += "% 6.2f".format(a(i))
      if (i < a.length-1)
        str += "\n "
      i += 1
    }
    str += " ]\n"
    println(str)
  }

  def prettyFormat(a: Array[Array[Double]]): String = {
    val strB = new StringBuilder
    strB.append("Array(\n")
    var i = 0
    while (i < a.length) {
      var j = 0
      strB.append("Array(")
      while (j < a(0).length) {
        strB.append("% 1.1f".format(a(i)(j)))
        if (j < a(0).length-1)
          strB.append(", ")
        j += 1
      }
      if (i < a(0).length-1)
        strB.append(" ),\n")
      else
        strB.append(")\n")
      i += 1
    }
    strB.append(")\n")
    println("strB.result")
    println(strB.result)
    strB.result
  }

  private def prettyPrint(lsmOV: Tuple2[Double, Double], time: Double) {
    println(" "+"% 6.4f".format(lsmOV._1)+"  ( "+"%.4f".format(lsmOV._2)+" ) [ "+"%.3f".format(time/1e9)+"sec ]")
  }

  /**
    * Equation Parser
    *
    */
  trait Eqn
  case class Operator(str: String) extends Eqn { override def toString = str }
  case class Function(str: String) extends Eqn { override def toString = str }
  case class Number(value: Double) extends Eqn { override def toString = value.toString }
  case class Symbol(str: String) extends Eqn { override def toString = str }
  case class ErrorText(str: String) extends Eqn { override def toString = str }
  case class FunctionOp(fn: Function, arg: Eqn) extends Eqn { override def toString = "["+fn+"]["+arg+"]" }
  case class ArithmeticOp(arg1: Eqn, op: Operator, arg2: Eqn) extends Eqn { override def toString = "["+arg1+"]["+op+"]["+arg2+"]" }
  case class BracketedOp(arg: Eqn) extends Eqn { override def toString = "["+arg+"]" }

  object EqnParsers extends RegexParsers {

    val functions = new collection.mutable.HashMap[Function, (Double) => Double]
    functions += (
      Function("exp") -> { (x) => exp(x) } )
    val operations = new collection.mutable.HashMap[Operator, (Double, Double) => Double]
    operations += (
      Operator("+") -> { (x, y) => x + y },
      Operator("-") -> { (x, y) => x - y },
      Operator("*") -> { (x, y) => x * y },
      Operator("/") -> { (x, y) => x / y } )

    //def parseEval(input: String): PayoffFn = (mcPrice: Double, params: LsmParams) => evaluate(parse(input), mcPrice, params)
    def parseEval(input: String): PayoffFn = {
      val eqn = parse(input)
      println("parseEval called with input: "+input)
      evaluate(eqn)
    }

    //def evaluate(e: Eqn, mcPrice: Double, params: LsmParams): Double = try {
    def evaluate(e: Eqn): PayoffFn = try {
      //val payoffFn: PayoffFn = (idx: Int, step: Int, priceMatrix: Matrix, params: LsmParams) => (params.strike - priceMatrix(idx, step))
      e match {
        case Number(v) => { (mcPrice: Double, params: LsmParams) => v }
        case Symbol(p) => { (mcPrice: Double, params: LsmParams) => {
          p match { 
            case "STRIKE" => params.strike
            case "MCPRICE" => mcPrice
          }
        } }
        case FunctionOp(fn, arg) => { (mcPrice: Double, params: LsmParams) => functions(fn)(evaluate(arg)(mcPrice, params)) }
        case ArithmeticOp(arg1, op, arg2) => { (mcPrice: Double, params: LsmParams) => operations(op)(evaluate(arg1)(mcPrice, params), evaluate(arg2)(mcPrice, params)) }
        case BracketedOp(arg) => { (mcPrice: Double, params: LsmParams) => (evaluate(arg)(mcPrice, params)) }
      }
    } catch {
      case ex: Exception => {
        println("ex: "+ex)
        (mcPrice: Double, params: LsmParams) => Double.NaN
      }
    }

    def function: Parser[Function] = """exp""".r ^^ (f => Function(f))
    def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ (d => Number(d.toDouble))
    def operator: Parser[Operator] = """[+|\-|*|/]""".r ^^ (s => Operator(s))
    def symbol: Parser[Symbol] = """(STRIKE|MCPRICE)""".r ^^ (s => Symbol(s))

    def operand: Parser[Eqn] = number | symbol | functionop | bracketedop

    def arithmeticop: Parser[ArithmeticOp] = operand~operator~operand ^^ { case arg1~op~arg2 => ArithmeticOp(arg1, op, arg2) }
    def functionop: Parser[FunctionOp] = function~"("~operand~")" ^^ { case fn~"("~arg~")" => FunctionOp(fn, arg) }
    def bracketedop: Parser[Eqn] = "("~(arithmeticop | operand)~")" ^^ { case "("~arg~")" => BracketedOp(arg) }

    //def formula: Parser[Eqn] = functionop | arithmeticop | operator | number | symbol
    def formula: Parser[Eqn] = arithmeticop | bracketedop | functionop

    def parse(input: String): Eqn = parseAll(formula, input) match {
      case Success(e, _) => e
      case f: NoSuccess => ErrorText("NoSuccess: ["+f.msg+"]")
    }
  }

  /**
    * Utility method to time the option price calculation.
    *
    * @param params The lsm parameters.
    *
    */
  /*def simulate(params: LsmParams)  {
    println ("params = "+params)
    val startTime = System.nanoTime
    val lsmOV = lsmOptionValue(params)
    val endTime = System.nanoTime
    prettyPrint(lsmOV, endTime-startTime)
  }

  val rows = 10
  val cols = 10
  //val size = 8L*rows*cols
  val size = 8*rows*cols
  //val position = (x: Int, y: Int) => y.toLong * cols + x
  val position = (x: Int, y: Int) => y * cols + x

  def get(mbb: MappedByteBuffer, x: Int, y: Int) = {
    assert((x >= 0) && (x < rows))
    assert((y >= 0) && (y < cols))
    val p = position(x, y)*8
    mbb.getDouble(p)
  }

  def set(mbb: MappedByteBuffer, x: Int, y: Int, d: Double) = {
    assert((x >= 0) && (x < rows))
    assert((y >= 0) && (y < cols))
    val p = position(x, y)*8
    mbb.putDouble(p, d)
  }

  def main(args: Array[String]): Unit = {
    println("Starting... ")

    lsmOptionValueSimpleExample()

    val test_01 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.20 ) 
    val test_02 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.20 ) 
    val test_03 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.40 ) 
    val test_04 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.40 ) 
    simulate(test_01)
    simulate(test_02)
    simulate(test_03)
    simulate(test_04)

    // examples from the table on p16 of the LSM paper
    val example2_01 = LsmParams( true, 100000, 1, 50, 36.0, 40.0, 0.06, 0.20 ) 
    val example2_02 = LsmParams( true, 100000, 2, 100, 36.0, 40.0, 0.06, 0.20 ) 
    val example2_03 = LsmParams( true, 100000, 1, 50, 36.0, 40.0, 0.06, 0.40 ) 
    val example2_04 = LsmParams( true, 100000, 2, 100, 36.0, 40.0, 0.06, 0.40 ) 

    val example2_05 = LsmParams( true, 100000, 1, 50, 38.0, 40.0, 0.06, 0.20 ) 
    val example2_06 = LsmParams( true, 100000, 2, 100, 38.0, 40.0, 0.06, 0.20 ) 
    val example2_07 = LsmParams( true, 100000, 1, 50, 38.0, 40.0, 0.06, 0.40 ) 
    val example2_08 = LsmParams( true, 100000, 2, 100, 38.0, 40.0, 0.06, 0.40 ) 

    val example2_09 = LsmParams( true, 100000, 1, 50, 40.0, 40.0, 0.06, 0.20 ) 
    val example2_10 = LsmParams( true, 100000, 2, 100, 40.0, 40.0, 0.06, 0.20 ) 
    val example2_11 = LsmParams( true, 100000, 1, 50, 40.0, 40.0, 0.06, 0.40 ) 
    val example2_12 = LsmParams( true, 100000, 2, 100, 40.0, 40.0, 0.06, 0.40 ) 

    val example2_13 = LsmParams( true, 100000, 1, 50, 42.0, 40.0, 0.06, 0.20 ) 
    val example2_14 = LsmParams( true, 100000, 2, 100, 42.0, 40.0, 0.06, 0.20 ) 
    val example2_15 = LsmParams( true, 100000, 1, 50, 42.0, 40.0, 0.06, 0.40 ) 
    val example2_16 = LsmParams( true, 100000, 2, 100, 42.0, 40.0, 0.06, 0.40 ) 

    val example2_17 = LsmParams( true, 100000, 1, 50, 44.0, 40.0, 0.06, 0.20 ) 
    val example2_18 = LsmParams( true, 100000, 2, 100, 44.0, 40.0, 0.06, 0.20 ) 
    val example2_19 = LsmParams( true, 100000, 1, 50, 44.0, 40.0, 0.06, 0.40 ) 
    val example2_20 = LsmParams( true, 100000, 2, 100, 44.0, 40.0, 0.06, 0.40 ) 

    simulate(example2_01)
    simulate(example2_02)
    simulate(example2_03)
    simulate(example2_04)
    simulate(example2_05)
    simulate(example2_06)
    simulate(example2_07)
    simulate(example2_08)
    simulate(example2_09)
    simulate(example2_10)
    simulate(example2_11)
    simulate(example2_12)
    simulate(example2_13)
    simulate(example2_14)
    simulate(example2_15)
    simulate(example2_16)
    simulate(example2_17)
    simulate(example2_18)
    simulate(example2_19)
    simulate(example2_20)

    println("Complete")
  }*/
}

