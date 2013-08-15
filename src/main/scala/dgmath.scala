/*
Copyright (c) 2013 Anthony Mulcahy

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/
// AndroidSpecificCode 
package com.dragongate_technologies.mcOptCal // AndroidSpecificCode  */
/* nonAndroidSpecificCode 
package com.dragongate_technologies.dgmath // nonAndroidSpecificCode  */

// AndroidSpecificCode
import _root_.android.app.{Activity, Notification, NotificationManager, PendingIntent, Service}
import _root_.android.content
import _root_.android.content.{ComponentName, Context, Intent, ServiceConnection, SharedPreferences}
import _root_.android.graphics.{Bitmap, Canvas, Color, Paint}
import _root_.android.graphics.drawable.Drawable
import _root_.android.os.{Binder, Bundle, Debug, Handler, IBinder}
import _root_.android.preference._
import _root_.android.text.method.ScrollingMovementMethod
import _root_.android.util.{AttributeSet, Log}
import _root_.android.view.{SurfaceView, View}
import _root_.android.view.View.OnClickListener
import _root_.android.widget.{Button, TextView} // AndroidSpecificCode END */

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

/**
 * Random Number Generator Class
 *
 */
class dgRandom(_seed: Integer) {

  /**
   * @constructor 
   *
   * @param m The number of rows.
   */
  def this() = this(1)

  private val a: Integer = 16807      // Minimal Standard Random Number Generator
  private val m: Integer = 2147483647 // Minimal Standard Random Number Generator
  private val q: Integer = m/a        // Minimal Standard Random Number Generator
  private val r: Integer = m % a      // Minimal Standard Random Number Generator

  private var seed: Integer = _seed           // Minimal Standard Random Number Generator

  /**
   * Minimal Standard Random Number Generator
   * Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
   */
  def msrng = {
    val hi = seed / q
    val lo = seed % q
    val test = a * lo - r * hi;
    
    if(test > 0)
      seed = test
    else
      seed = test + m
    seed.toDouble/m
  }

  private val bsm_a = Array(
    2.50662823884,
    -18.61500062529,
    41.39119773534,
    -25.44106049637)
  private val bsm_b = Array(
    -8.47351093090,
    23.08336743743,
    -21.06224101826,
    3.13082909833)
  private val bsm_c = Array(
    0.3374754822726147,
    0.9761690190917186,
    0.1607979714918209,
    0.0276438810333863,
    0.0038405729373609,
    0.0003951896511919,
    0.0000321767881768,
    0.0000002888167364,
    0.0000003960315187)

  /**
  * Beasley-Springer-Moro algorithm for approximating the inverse normal
  * Ref: "Monte Carlo Methods in Financial Engineering", Glasserman
  */
  val bsmInvNormal = (u: Double) => {
    var x: Double = Double.NaN
    val y: Double = u - 0.5D
    var r: Double = Double.NaN
    if (abs(y) < 0.42D)
      {
      r = y*y
      x = y*(((bsm_a(3)*r + bsm_a(2))*r + bsm_a(1))*r + bsm_a(0))/ ((((bsm_b(3)*r + bsm_b(2))*r + bsm_b(1))*r + bsm_b(0))*r + 1.0D)
    } else {
      r = u
      if (y > 0.0D)
        r = 1.0D - u
      r = log(-log(r))
      x = bsm_c(0) + r*(bsm_c(1) + r*(bsm_c(2) + r*(bsm_c(3) + r*(bsm_c(4)+ r*(bsm_c(5) + r*(bsm_c(6) + r*(bsm_c(7) + r*bsm_c(8))))))))
      if (y < 0.0D)
        x = -x
    }
    x
  }

  def nextGaussian: Double = bsmInvNormal(msrng)

  def setSeed(s: Integer) { seed = s }

}

/**
 * The Black-Scholes formulas for a European call/put option on a non-dividend-paying stock.
 *
 * @note References : Hull, Options, Futures and Other Derivatives 6th Ed, p295-298
 *
 */
object dgmath {

  val kFn = (x: Double) => 1.0D/(1.0D+p*x)
  val p =  0.2316419D
  val a1 = 0.319381530D
  val a2 = -0.356563782D
  val a3 = 1.781477937D
  val a4 = -1.821255978D
  val a5 = 1.330274429D

  val snd = (x: Double) => { exp(-0.5D*x*x) / sqrt(2.0D*math.Pi) }

  val cnd = (x: Double) => {
    if (x >= 0.0D)
      1.0D - snd(x)*(a1*kFn(x) + a2*pow(kFn(x), 2) + a3*pow(kFn(x), 3) + a4*pow(kFn(x), 4) + a5*pow(kFn(x), 5))
    else
      snd(x)*(a1*kFn(-x) + a2*pow(kFn(-x), 2) + a3*pow(kFn(-x), 3) + a4*pow(kFn(-x), 4) + a5*pow(kFn(-x), 5))
  }

  val d1 = (s0: Double, k: Double, r: Double, t: Double, v: Double) => {
    (log(s0/k)+(r+pow(v, 2.0D)/2.0D)*t) / (v*sqrt(t))
  }

  val d2 = (s0: Double, k: Double, r: Double, t: Double, v: Double) => { d1(s0, k, r, t, v) - v*sqrt(t) }

  val bsEuropeanCallVal = (s0: Double, k: Double, r: Double, t: Double, v: Double) => {
    s0*cnd(d1(s0, k, r, t, v)) - k * exp(-r * t) * cnd(d2(s0, k, r, t, v))
  }

  val bsEuropeanPutVal = (s0: Double, k: Double, r: Double, t: Double, v: Double) => {
    k * exp(-r * t) * cnd(-d2(s0, k, r, t, v)) - s0 * cnd(-d1(s0, k, r, t, v))
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
class Matrix(m: Int, n: Int, _dataDir: File) { //todo make this generic for other types than doubles
  val rows = m
  val cols = n
  val dataDir = _dataDir
  val overThreshold = (m*n > Matrix.threshold)

  protected val tempFile = {
    if (overThreshold) {
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
  def this(m: Int, dataDir: File) = this(m, 1, dataDir)

  /**
   * @constructor Create a new Matrix from inArray.
   *
   * @param m The number of rows.
   */
  def this(inArray: Array[Array[Double]], dataDir: File) = {
    this(inArray.length, inArray(0).length, dataDir)
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
    val ab = new Matrix(rows, p, b.dataDir)
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

  private def identity(m: Int, n: Int, dataDir: File): Matrix = {
    assert(m == n)
    val idMatrix = new Matrix(m, n, dataDir)
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
    val gjMatrix = new Matrix(matrix.rows, matrix.cols, matrix.dataDir)
    var i = 0
    while (i < matrix.rows) {
      var j = 0
      while (j < matrix.cols) {
        gjMatrix(i, j) = matrix(i, j)
        j += 1
      }
      i += 1
    }
    val invMatrix = Matrix.identity(matrix.rows, matrix.cols, matrix.dataDir)

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
    val tMatrix = new Matrix(matrix.cols, matrix.rows, matrix.dataDir)
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


/**
  * LsmParams is container for the least squares Monte Carlo algorithm parameters.
  *
  * @param payoffFn The the payoff function.
  * @param payoffFnStr The the payoff function string representation.
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
  * @param rngSeed todo
  * @param dataDir: File = new File("/home/anthony/scala_work/mcTempFiles/")
  * 
  * @note dT = expiry / numSteps
  */
case class LsmParams(
  payoffFn: (Double, Double, LsmParams) => Double,
  payoffFnStr: String,
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
  rngSeed: Int,
  dataDir: File = new File("/home/anthony/scala_work/mcTempFiles/")
  ) {

  /**
   * dT = expiry / numSteps
   */
  val dT: Double = expiry.toDouble / numSteps.toDouble

  override def toString(): String = {
    val strB = new StringBuilder
    strB.append("[\n")
    strB.append(" payoffFn: "+payoffFnStr+"\n")
    val formatStr = "% .3f"
    strB.append(" numPaths: "+numPaths+"\n")
    strB.append(" numSteps: "+numSteps+"\n")
    strB.append(" dT:\t"+(formatStr.format(dT))+"\n")
    strB.append(" T:\t"++(formatStr.format(expiry.toDouble))+"\n")
    strB.append(" S0:\t"+(formatStr.format(stock))+"\n")
    strB.append(" K:\t"+(formatStr.format(strike))+"\n")
    strB.append(" R:\t"+(formatStr.format(rate))+"\n")
    strB.append(" V:\t"+(formatStr.format(volatility))+"\n")
    strB.append(" rngSeed:\t"+rngSeed+"\n")
    strB.append("]\n")
    strB.result
  }
}

class lsm {

  @native def calcAsianOptionValueJNI(params: LsmParams, rng: Random): Array[Double]

}

object lsm {
  type PayoffFn = (Double, Double, LsmParams) => Double
  import Matrix._
  private val TAG: String = "lsm"

  private val DEBUG = false

  val rng: Random = new Random
  val dgRng: dgRandom = new dgRandom

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
    val pMatrix = new Matrix(params.numPaths, n, params.dataDir)
    val maxMinAvgMatrix = new Matrix(params.numPaths, 3, params.dataDir) // 20130716 addition
    val a = (params.rate -  sqr(params.volatility)*0.5)*params.dT
    val b = params.volatility*sqrt(params.dT)
    var i = 0
    while (i < params.numPaths/2) {
      pMatrix(i*2, 0) = params.stock
      pMatrix(i*2+1, 0) = params.stock
      var s1 = params.stock
      var s2 = params.stock
      var j = 1
      maxMinAvgMatrix(i*2, 0) = params.stock
      maxMinAvgMatrix(i*2+1, 0) = params.stock
      maxMinAvgMatrix(i*2, 1) = params.stock
      maxMinAvgMatrix(i*2+1, 1) = params.stock

      maxMinAvgMatrix(i*2, 2) = 0.0
      maxMinAvgMatrix(i*2+1, 2) = 0.0
      //maxMinAvgMatrix(i*2, 2) = params.stock/(n-1)
      //maxMinAvgMatrix(i*2+1, 2) = params.stock/(n-1)
            //maxCF = max(maxCF, cfMatrix(i, j)*exp(-params.rate*((j-(step-1))*params.dT)))
      while (j < n) {
        val dZ = rng.nextGaussian
        s1 = s1*exp(a + b*dZ)
        s2 = s2*exp(a - b*dZ) // antithetic path
        pMatrix(i*2, j) = s1
        pMatrix(i*2+1, j) = s2

        maxMinAvgMatrix(i*2, 0) = max(maxMinAvgMatrix(i*2, 0), s1)
        maxMinAvgMatrix(i*2+1, 0) = max(maxMinAvgMatrix(i*2+1, 0), s2)
        maxMinAvgMatrix(i*2, 1) = min(maxMinAvgMatrix(i*2, 1), s1)
        maxMinAvgMatrix(i*2+1, 1) = min(maxMinAvgMatrix(i*2+1, 1), s2)

        maxMinAvgMatrix(i*2, 2) = maxMinAvgMatrix(i*2, 2) + s1/(n-1)
        maxMinAvgMatrix(i*2+1, 2) = maxMinAvgMatrix(i*2+1, 2) + s2/(n-1)

        j += 1
      }
      //maxMinAvgMatrix(i*2, 2) = maxMinAvgMatrix(i*2, 2) * exp(-params.rate*params.expiry)
      //maxMinAvgMatrix(i*2+1, 2) = maxMinAvgMatrix(i*2+1, 2) * exp(-params.rate*params.expiry)
      i += 1
    }

    (pMatrix, maxMinAvgMatrix)
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
    maxMinAvgMatrix: Matrix, 
    cfMatrix: Matrix
    ) => {

    if (step == params.numSteps) {
      var i = 0
      while (i < params.numPaths) {
        if (params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params) > 0)
          cfMatrix(i, step-1) = params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params)
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
        if (params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params) > 0)
          xySize += 1 
        i += 1
      }

      //val x = new Matrix(xySize, 1)
      val y = new Matrix(xySize, 1, params.dataDir)
      val fnX = new Matrix(xySize, fn(0).length, params.dataDir)
      i = 0
      var k = 0
      while (i < params.numPaths) {
        if (params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params) > 0) {
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

      i = 0
      var j = 0
      while (i < params.numPaths) {
        if (params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params) > 0) {
          cfMatrix(i, step-1) = {
            if (params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params) >= contMatrix(j, 0) ) {
              k = step
              while (k < cfMatrix.cols) {
                cfMatrix(i, k) = 0
                k += 1
              }
              params.payoffFn(priceMatrix(i, step), maxMinAvgMatrix(i, 2), params)
            } else {
              0.0 // don't exercise now
            }
          }
          j += 1
        }
        i += 1
      }
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

  /*val asianOptionValueStdErr = (params: LsmParams, priceMatrix: Matrix, maxMinAvgMatrix: Matrix) => {
    val m = params.numPaths
    val pvArray = Array.ofDim[Double](m)

    var i = 0
    var n = 0
    var mean = 0.0D
    var m2 = 0.0D
    while (i < m) {
      if (params.payoffFn(priceMatrix(i, params.numSteps), maxMinAvgMatrix(i, 2), params) > 0)
          pvArray(i) = params.payoffFn(priceMatrix(i, params.numSteps), maxMinAvgMatrix(i, 2), params)*exp(-params.rate*params.expiry)

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
  }*/



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

  val recurseCF = (initStep: Int, params: LsmParams, priceMatrix: Matrix, maxMinAvgMatrix: Matrix, cfMatrix: Matrix, basisFn: (Double) => Array[Double], callerService: Actor) => {
    assert(priceMatrix.rows == params.numPaths)
    assert(priceMatrix.cols == params.numSteps+1)
    assert(cfMatrix.rows == params.numPaths)
    assert(cfMatrix.cols == params.numSteps)

    var step = initStep
    var newCFMatrix = cfMatrix
    var abort = false // 1.01
    val msg = "LSM recurseCF step"
    // AndroidSpecificCode
    var startTime = System.currentTimeMillis
    val uiUpdateInterval = params.uiUpdateInterval

    // AndroidSpecificCode END */
    while ((step > 0) && !abort) { // 1.01
      // AndroidSpecificCode
      if (System.currentTimeMillis - startTime > uiUpdateInterval) {
        startTime = System.currentTimeMillis
      //if (step%params.uiUpdateInterval == 0) {
        // AndroidSpecificCode
        if (callerService != null)
          callerService ! lsmStatusReport(step, params.numSteps, msg)
        else
          Log.d(TAG, msg+" ="+step+" of "+params.numSteps)
        println(msg+" ="+step+" of "+params.numSteps)
        receiveWithin(1) {
          case TIMEOUT => { }
          case CalcStopLSM => {
            println("CalcStopLSM" ) // Log.d(TAG, "CalcStopLSM" )
            abort = true // 1.01
            // 1.02
            callerService ! lsmAbortReport
            exit()
          }
        } // AndroidSpecificCode END */
      }

      // 1.01
      if (!abort)
        newCFMatrix = calcCFAtStep(step, basisFn, params, priceMatrix, maxMinAvgMatrix, newCFMatrix)
      step -= 1
    }
    // 1.01
    // AndroidSpecificCode
    if (abort) {
      callerService ! lsmAbortReport
      exit()
    }
    // AndroidSpecificCode END */
    newCFMatrix
  }

  /**
    * Calculate the value of the option specified by the parameters using the least squares method.
    *
    * @param params The lsm parameters.
    */
  val lsmOptionValue = (params: LsmParams, callerService: Actor) => {
    Matrix.threshold = params.threshold
    val genMatrix = genPriceMatrix(params)
    val priceMatrix = genMatrix._1
    val maxMinAvgMatrix = genMatrix._2
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)
    if (DEBUG)
      println("maxMinAvgMatrix:\n"+maxMinAvgMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps, params.dataDir) 

    // normalize parameter x to prevent underflows
    val basisFn = (x: Double) => Array (
      1.0,
      exp(-x/(2.0*params.stock)),
      exp(-x/(2.0*params.stock))*(1.0-x/params.stock),
      exp(-x/(2.0*params.stock))*(1.0-2.0*x/params.stock+sqr(x/params.stock)/2.0)
      )

    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, maxMinAvgMatrix, initCFMatrix, basisFn, callerService)

    //todo create stopping matrix

    if (DEBUG)
      println(cfMatrix)
    val oVsE = optionValueStdErr(params, cfMatrix)
    (oVsE._1, oVsE._2, getPriceMatrixSample(params.numSamples, priceMatrix))
  }

  val calcAsianOptionValue: (LsmParams, Actor) => (Double, Double) = (params: LsmParams, callerService: Actor) => {
    assert( params.numPaths%2 == 0)
    println("calcAsianOptionValue")
    val a = (params.rate -  sqr(params.volatility)*0.5)*params.dT
    val b = params.volatility*sqrt(params.dT)
    var i = 0
    //val m = params.numPaths
    var n = 0
    var mean = 0.0D
    var m2 = 0.0D
    var sum = 0.0D
    var x = 0.0D
    var y = 0.0D
    val pvDiscount = exp(-params.rate*params.expiry)
    var abort = false
    val msg = "calcAsianOptionValue path"
    val payOffFn = (avg: Double) => params.payoffFn(0.0D, avg, params)
    val exp_a = exp(a)
    dgRng.setSeed(params.rngSeed)

    // AndroidSpecificCode
    var startTime = System.currentTimeMillis
    val uiUpdateInterval = params.uiUpdateInterval
    // AndroidSpecificCode END */
    //Debug.startMethodTracing("traceFile")
    while ((i < params.numPaths/2) && !abort) {
      var s1 = params.stock
      var s2 = params.stock
      var avgX = 0.0D
      var avgY = 0.0D

      var j = 1
      while (j < params.numSteps+1) {
        //val dZ = rng.nextGaussian
        val dZ = dgRng.nextGaussian
        val exp_bdZ = exp(b*dZ)
        s1 = s1*exp_a*exp_bdZ
        s2 = s2*exp_a/exp_bdZ // antithetic path

        avgX = avgX + s1/params.numSteps
        avgY = avgY + s2/params.numSteps

        j += 1

        // AndroidSpecificCode
        if (System.currentTimeMillis - startTime > uiUpdateInterval) {
          startTime = System.currentTimeMillis
          if (callerService != null)
            callerService ! lsmStatusReport(params.numPaths - i*2, params.numPaths, msg)
          else
            Log.d(TAG, msg+" ="+(params.numPaths - i*2)+" of "+params.numPaths)
          receiveWithin(1) {
            case TIMEOUT => { }
            case CalcStopLSM => {
              println("CalcStopMC" )
              abort = true
              callerService ! lsmAbortReport
              exit()
            }
          }
        } // AndroidSpecificCode END */

      }

      val payOffX = payOffFn(avgX)
      if (payOffX > 0)
        x = payOffX*pvDiscount
      else
        x = 0.0D

      val payOffY = payOffFn(avgY)
      if (payOffY > 0)
        y = payOffY*pvDiscount
      else
        y = 0.0D

      sum = sum + x
      n += 1
      val deltaX = x - mean
      mean += deltaX/n
      m2 += deltaX*(x - mean)

      sum = sum + y
      n += 1
      val deltaY = y - mean
      mean += deltaY/n
      m2 += deltaY*(y - mean)

      i += 1
    }

    val optionValue = sum / params.numPaths
    val variance = m2/(n-1)
    val sampStdDev = sqrt(variance)
    val stdErr = sampStdDev/sqrt(params.numPaths)
    //Debug.stopMethodTracing()

    (optionValue, stdErr)
  }

  /**
    * First walk through example in the LSM paper
    *
    */
  def lsmOptionValueSimpleExample() {
    val startTime = System.nanoTime
    val payoffFnStr = "K-S"
    val payoffFn = lsm.EqnParsers.parseEval(payoffFnStr)
    val params = LsmParams( payoffFn, payoffFnStr, 8, 3, 3, 1.0, 1.10, 0.06, 0.0, 8, 50000, 10, 1) 
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
      ), params.dataDir)
    if (DEBUG)
      println("priceMatrix:\n"+priceMatrix)
    val maxMinAvgMatrix = new Matrix(Array(
      Array(1.34, 1.00, Double.NaN),
      Array(1.54, 1.00, Double.NaN),
      Array(1.22, 1.00, Double.NaN),
      Array(1.00, 0.92, Double.NaN),
      Array(1.56, 1.00, Double.NaN),
      Array(1.00, 0.76, Double.NaN),
      Array(1.00, 0.92, Double.NaN),
      Array(1.34, 0.88, Double.NaN)
      ), params.dataDir)
    if (DEBUG)
      println("maxMinAvgMatrix:\n"+maxMinAvgMatrix)

    val initCFMatrix = new Matrix(params.numPaths, params.numSteps, params.dataDir) 
    val basisFn = (x: Double) => Array ( 1.0, x, sqr(x) )
    val cfMatrix = recurseCF(params.numSteps, params, priceMatrix, maxMinAvgMatrix, initCFMatrix, basisFn, null)

    if (DEBUG)
      println(cfMatrix)
    val endTime = System.nanoTime

    val lsmOV = optionValueStdErr(params, cfMatrix)
    prettyPrint(lsmOV, endTime-startTime)
  }

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
    * Note: noAndroidSpecificCode
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

    def parseEval(input: String): PayoffFn = {
      val eqn = parse(input)
      println("parseEval called with input: "+input)
      evaluate(eqn)
    }

    def evaluate(e: Eqn): PayoffFn = try {
      e match {
        case Number(v) => { (mcPrice: Double, avgPrice:Double, params: LsmParams) => v }
        case Symbol(p) => {
          (mcPrice: Double, avgPrice:Double, params: LsmParams) => {
            p match {
              case "T" => params.expiry
              case "S0" => params.stock
              case "K" => params.strike
              case "R" => params.rate
              case "V" => params.volatility
              case "S" => mcPrice
              case "SAVG" => avgPrice
              // todo case "SMAX" => 
              // todo case "SMIN" => 
            }
          } 
        }
        case FunctionOp(fn, arg) => {
          (mcPrice: Double, avgPrice:Double, params: LsmParams) => functions(fn)(evaluate(arg)(mcPrice, avgPrice, params))
        }
        case ArithmeticOp(arg1, op, arg2) => { 
          (mcPrice: Double, avgPrice:Double, params: LsmParams) => 
          operations(op)(evaluate(arg1)(mcPrice, avgPrice, params), evaluate(arg2)(mcPrice, avgPrice, params)) 
        }
        case BracketedOp(arg) => { 
          (mcPrice: Double, avgPrice: Double, params: LsmParams) => (evaluate(arg)(mcPrice, avgPrice, params)) 
        }
      }
    } catch {
      case ex: Exception => {
        println("ex: "+ex)
        (mcPrice: Double, avgPrice: Double, params: LsmParams) => Double.NaN
      }
    }

    def function: Parser[Function] = """exp""".r ^^ (f => Function(f))
    def number: Parser[Number] = """-?\d+(\.\d*)?""".r ^^ (d => Number(d.toDouble))
    def operator: Parser[Operator] = """[+|\-|*|/]""".r ^^ (s => Operator(s))
    def symbol: Parser[Symbol] = """(T|S0|SAVG|S|K|R|V)""".r ^^ (s => Symbol(s))

    def operand: Parser[Eqn] = number | symbol | functionop | bracketedop

    def arithmeticop: Parser[ArithmeticOp] = operand~operator~operand ^^ { case arg1~op~arg2 => ArithmeticOp(arg1, op, arg2) }
    def functionop: Parser[FunctionOp] = function~"("~operand~")" ^^ { case fn~"("~arg~")" => FunctionOp(fn, arg) }
    def bracketedop: Parser[Eqn] = "("~(arithmeticop | operand)~")" ^^ { case "("~arg~")" => BracketedOp(arg) }

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
  def simulate(params: LsmParams)  {
    println ("params = "+params)
    val startTime = System.nanoTime
    val lsmOV = lsmOptionValue(params, null)
    val endTime = System.nanoTime
    prettyPrint((lsmOV._1, lsmOV._2), endTime-startTime) // todo
  }

  def asianSimulate(params: LsmParams)  {
    println ("params = "+params)
    val startTime = System.nanoTime
    val asianOV = calcAsianOptionValue(params, null)
    val endTime = System.nanoTime
    //println(asianOV._1, lsmOV._2), endTime-startTime) // todo
    println("asianOptionValue = "+"% 6.4f".format(asianOV._1)+"  ( "+"%.4f".format(asianOV._2)+" ) [ "+"%.3f".format((endTime-startTime)/1e9)+"sec ]")
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

    val payoffFnStr = "K-S"
    val payoffFn = lsm.EqnParsers.parseEval(payoffFnStr)
    val test_01 = LsmParams( payoffFn, payoffFnStr, 10000, 1, 50, 36.0, 40.0, 0.06, 0.20, 50, 50000, 1, 1 ) 
    //simulate(test_01)

    val asianPayoffFnStr = "SAVG-K"
    val asianPayoffFn = lsm.EqnParsers.parseEval(asianPayoffFnStr)
    // hull val asianTest_01 = LsmParams( asianPayoffFn, asianPayoffFnStr, 100000, 1, 100, 50.0, 50.0, 0.10, 0.40, 50, 50000, 1 ) 
    val asianTest_01 = LsmParams( asianPayoffFn, asianPayoffFnStr, 1000000, 1, 100, 2.0, 2.0, 0.02, 0.10, 50, 50000, 1, 1 ) 
    asianSimulate(asianTest_01)
    /*val test_01 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.20 ) 
    val test_02 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.20 ) 
    val test_03 = LsmParams( true, 10000, 1, 50, 36.0, 40.0, 0.06, 0.40 ) 
    val test_04 = LsmParams( true, 10000, 2, 100, 36.0, 40.0, 0.06, 0.40 ) */
    //simulate(test_01)
    /*simulate(test_02)
    simulate(test_03)
    simulate(test_04)*/

    // examples from the table on p16 of the LSM paper
    /*val example2_01 = LsmParams( true, 100000, 1, 50, 36.0, 40.0, 0.06, 0.20 ) 
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
    simulate(example2_20)*/

    println("Complete")
  }
}

