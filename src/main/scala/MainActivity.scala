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
package com.dragongate_technologies.mcOptCal

import _root_.android.app.{Activity, AlertDialog, Dialog, Notification, NotificationManager, PendingIntent, Service, TabActivity}
import _root_.android.content
import _root_.android.content.{ComponentName, Context, DialogInterface, Intent, ServiceConnection, SharedPreferences}
import _root_.android.graphics.{Bitmap, Canvas, Color, Paint, Typeface}
import _root_.android.graphics.drawable.Drawable
import _root_.android.opengl.{GLES20, GLSurfaceView}
import _root_.android.os.{Binder, Bundle, Debug, Handler, IBinder, Message, Messenger}
import _root_.android.preference._
import _root_.android.util.{AttributeSet, Log}
import _root_.android.view.{LayoutInflater, SurfaceView, View}
import _root_.android.view.ViewGroup.LayoutParams
import _root_.android.view.inputmethod.InputMethodManager
import _root_.android.view.View.OnClickListener
import _root_.android.view.ViewTreeObserver.OnGlobalLayoutListener
import _root_.android.widget._


import java.io._

import scala.actors.Actor._
import scala.actors._
import scala.actors.Futures._
//import scala.concurrent.ops._ //todo safe to remove???? 20130725
import scala.io.Source
import scala.util.{Marshal, Random}
import scala.math._

import com.dragongate_technologies.glfuncplot._


class LSMCalcParams(_params: LsmParams, _callerService: Actor) {
  val params = _params
  val callerService = _callerService
}

case class CalcStartLSM(params: LSMCalcParams)
case class CalcStartAsianMC(params: LSMCalcParams, jniFlag: Boolean)
case class mcOptCalServiceAsianResult(lsmOV: Tuple3[Double, Double, Array[Array[Double]]], runTime: Long)
case class mcOptCalServiceLSMResult(lsmOV: Tuple3[Double, Double, Array[Array[Double]]], runTime: Long)
case class lsmStatusReport(step: Int, numSteps: Int, msg: String)
case class lsmAbortReport
case object CalcStopLSM


class Calc extends Actor {
  private val TAG: String = "Calc"

  var callerService: Actor = null

  def jniUpdateUI(msg: String, count: Int, total: Int): Boolean = {
    Log.d("jniUpdateUI", "msg ="+msg)
    var abortFlag = false
    if (callerService != null)
      callerService ! lsmStatusReport(count, total, msg)
    else
      Log.d(TAG, msg+" ="+count+" of "+total)
    receiveWithin(1) {
      case TIMEOUT => { }
      case CalcStopLSM => {
        println("jniAbortMC" )
        //abort = true
        callerService ! lsmAbortReport
        //exit()
        abortFlag = true
      }
    }
    abortFlag
  }

  def act() {
    loop {
      react {
        case CalcStartLSM(lsmCalcParams) => {
          Log.d(TAG, "CalcStartLSM" )
          callerService = lsmCalcParams.callerService
          //Debug.startMethodTracing("traceFile")
          val startTime = System.nanoTime
          val lsmOV = lsm.lsmOptionValue(lsmCalcParams.params, lsmCalcParams.callerService)
          val endTime = System.nanoTime
          //Debug.stopMethodTracing()
          callerService ! mcOptCalServiceLSMResult(lsmOV, ((endTime-startTime)/1e6).toLong)
        }
        case CalcStartAsianMC(lsmCalcParams, jniFlag) => {
          Log.d(TAG, "CalcStartAsianMC" )
          callerService = lsmCalcParams.callerService

          val calc: Calc = this
          if (jniFlag) {
            // JNI version
            val rpn = lsm.EqnRPNParser.rpnParseEval("SAVG-K", lsmCalcParams.params)
            Log.d(TAG, "RPN = "+rpn)
            val rpnAry = rpn.toArray
            val startTime = System.nanoTime
            val lsmJNI = new lsm
            Log.d("ASIAN", "lsmCalcParams.params"+lsmCalcParams.params)
            val asianOVJNI = lsmJNI.calcAsianOptionValueJNI(calc, lsmCalcParams.params, rpnAry)
            val endTime = System.nanoTime
            Log.d("ASIAN", "calcAsianOptionValueJNI = "+asianOVJNI)
            callerService ! mcOptCalServiceAsianResult((asianOVJNI(0), asianOVJNI(1), null), ((endTime-startTime)/1e6).toLong) // */
          } else {
            // Scala version
            val sStartTime = System.nanoTime
            val asianOV = lsm.calcAsianOptionValue(lsmCalcParams.params, lsmCalcParams.callerService)
            val sEndTime = System.nanoTime
            callerService ! mcOptCalServiceAsianResult((asianOV._1, asianOV._2, null), ((sEndTime-sStartTime)/1e6).toLong) // */
          }
        }
      }
    }
  }
}

trait BoundServiceListener {
  def report(str: String)
  def reportComplete(str: String)
}

class mcOptCalService extends Service with Actor {
  private val TAG: String = "mcOptCalService"

  val contentTitle: CharSequence = "mcOptCalService"
  val ns: String = Context.NOTIFICATION_SERVICE

  val icon: Int = R.drawable.logo
  val tickerText: CharSequence = "Monte Carlo Service Started"
  val when: Long = System.currentTimeMillis()
  var notification: Notification = new Notification(icon, tickerText, when)
  var calc: Calc = null
  var working: Boolean = false
  var calcRunning = false
  var calcStr: String = "idle"

  //var mClients = List[Messenger]()

  var statusStr = ""
  var calcComplete = false
  var progressState = false
  var progressVal = 0
  var priceAry: Array[Array[Double]] = null //todo

  class mcOptCalServiceBinder extends Binder {
    var mListener: BoundServiceListener = null

    def getService: mcOptCalService = mcOptCalService.this

    def setListener(listener: BoundServiceListener) { mListener = listener }
  }

  private final val mBinder = new mcOptCalServiceBinder()

  override def onBind(intent: Intent): IBinder = {
    Log.d(TAG, "onBind" )
    mBinder
  }

  override def onCreate() {
    Log.d(TAG, "onCreate" )

    val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]

    //val icon: Int = R.drawable.logo
    //val tickerText: CharSequence = "Monte Carlo Service Started"
    //val when: Long = System.currentTimeMillis()
    //notification = new Notification(icon, tickerText, when)

    val context: Context = getApplicationContext()
    val notificationIntent = new Intent(this, classOf[MainActivity])
    val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

    notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent);
    startForeground(1, notification)
  }

  override def onStart(intent: Intent, StartId: Int) {
    Log.d(TAG, "onStart" )
    val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
    val context: Context = getApplicationContext()
    val notificationIntent = new Intent(this, classOf[MainActivity])
    val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

    notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent);
    mNM.notify(1, notification)
  }

  override def onDestroy() {
    Log.d(TAG, "onDestroy" )
    working = false
    if (calcRunning) {
      calc ! CalcStopLSM
    }
  }

  def act() {
    loop {
      react {
        case mcOptCalServiceLSMResult(lsmOV, runTime) => {
          val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
          val context: Context = getApplicationContext()
          val notificationIntent = new Intent(this, classOf[MainActivity])
          val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

          calcStr = "MC Option Value = "+"% 6.4f".format(lsmOV._1)+"  ( s.e. "+"%.4f".format(lsmOV._2)+" ) [ "+"%.3f".format(runTime.toDouble/1000)+"sec ]\n"
          statusStr = calcStr
          calcComplete = true
          progressState = false
          progressVal = 100
          val priceAryRows = lsmOV._3.length
          val priceAryCols = lsmOV._3(0).length
          priceAry = Array.ofDim[Double](priceAryRows, priceAryCols)
          var i = 0
          while (i < priceAryRows) {
            var j = 0
            while (j < priceAryCols) {
              priceAry(i)(j) = lsmOV._3(i)(j)
              j += 1
            }
            i += 1
          }
          notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent)
          mNM.notify(1, notification)
          Log.d(TAG, "reportComplete("+calcStr )
          mBinder.mListener.reportComplete(calcStr)
          calcRunning = false
        }
        case mcOptCalServiceAsianResult(asianOV, runTime) => {
          val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
          val context: Context = getApplicationContext()
          val notificationIntent = new Intent(this, classOf[MainActivity])
          val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

          calcStr = "Asian Option Value = "+"% 6.4f".format(asianOV._1)+"  ( s.e. "+"%.4f".format(asianOV._2)+" ) [ "+"%.3f".format(runTime.toDouble/1000)+"sec ]\n"
          statusStr = calcStr
          calcComplete = true
          progressState = false
          progressVal = 100
          notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent)
          mNM.notify(1, notification)
          Log.d(TAG, "reportComplete("+calcStr )
          mBinder.mListener.reportComplete(calcStr)
          calcRunning = false
        }
        case lsmStatusReport(step, numSteps, msg) => {
          val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
          val context: Context = getApplicationContext()
          val notificationIntent = new Intent(this, classOf[MainActivity])
          val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

          progressVal = 100 - step*100/numSteps
          notification.setLatestEventInfo(context, contentTitle, msg+" = "+step+" of "+numSteps, contentIntent)
          mNM.notify(1, notification)
          Log.d(TAG, msg+" = "+step+" of "+numSteps)
          mBinder.mListener.report(msg+" = "+step+" of "+numSteps)
        }
        case lsmAbortReport => {
          val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
          val context: Context = getApplicationContext()
          val notificationIntent = new Intent(this, classOf[MainActivity])
          val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

          calcStr = "...Aborted LSM Calculation"
          statusStr = calcStr
          calcComplete = true
          progressState = false
          //notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent)
          //mNM.notify(1, notification)
          // 1.02
          mNM.cancel(1)
          Log.d(TAG, calcStr )
          mBinder.mListener.reportComplete(calcStr)
          calcRunning = false
          // 1.02
          exit()
        }
      }
    }
  }

  def startAsianMC(params: LsmParams, jniFlag: Boolean) { // todo need asianParams?
    Log.d(TAG, "mcOptCalService.startAsianMC" )
    if (!calcRunning) {
      working = true
      calcRunning = true
      calc = new Calc
      this.start //required
      calc.start
      progressState = true

      val contentText: CharSequence = "Running Asian MC calculation"
      val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
      val context: Context = getApplicationContext()
      val notificationIntent = new Intent(this, classOf[MainActivity])
      val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)
      notification.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
      mNM.notify(1, notification)

      val lsmCalcParams = new LSMCalcParams(params, this)
      calc ! CalcStartAsianMC(lsmCalcParams, jniFlag) //todo
    }
  }

  def startLSM(params: LsmParams) {
    Log.d(TAG, "mcOptCalService.startLSM" )
    if (!calcRunning) {
      working = true
      calcRunning = true
      calc = new Calc
      this.start //required
      calc.start
      progressState = true

      val contentText: CharSequence = "Running LSM calculation"
      val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
      val context: Context = getApplicationContext()
      val notificationIntent = new Intent(this, classOf[MainActivity])
      val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)
      notification.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
      mNM.notify(1, notification)

      val lsmCalcParams = new LSMCalcParams(params, this)
      calc ! CalcStartLSM(lsmCalcParams) //todo
    }
  }

  def stopLSM = {
    Log.d(TAG, "mcOptCalService.stopLSM" )
    if (calcRunning) {
      Log.d(TAG, "stopCalc: calcRunning" )
      working = false
      calcRunning = false
      calc ! CalcStopLSM

      this.stopSelf
    }
  }

}


case class StateData(
  payoffFnStr: String,
  numPaths: Int,
  timeToExpiration: Double,
  numSteps: Int,
  stock: Double,
  exercisePrice: Double,
  riskFreeRate: Double,
  volatility: Double,
  numSamples: Int,
  threshold: Int,
  uiUpdateInterval: Int,
  jniFlag: Boolean,
  rngSeed: Integer
  ) {

  def saveToSharedPreferences(context: Context) {
    val prefsEdit = PreferenceManager.getDefaultSharedPreferences(context).edit()
    prefsEdit.putString("payoffFnStr", payoffFnStr).commit()
    prefsEdit.putString("numPaths", numPaths.toString).commit()
    prefsEdit.putString("timeToExpiration", timeToExpiration.toString).commit()
    prefsEdit.putString("numSteps", numSteps.toString).commit()
    prefsEdit.putString("stock", stock.toString).commit()
    prefsEdit.putString("exercisePrice", exercisePrice.toString).commit()
    prefsEdit.putString("riskFreeRate", riskFreeRate.toString).commit()
    prefsEdit.putString("volatility", volatility.toString).commit()
    prefsEdit.putString("numSamples", numSamples.toString).commit()
    prefsEdit.putString("threshold", threshold.toString).commit()
    prefsEdit.putString("uiUpdateInterval", uiUpdateInterval.toString).commit()
    prefsEdit.putString("jniFlag", jniFlag.toString).commit()
    prefsEdit.putString("rngSeed", rngSeed.toString).commit()
  }

}
case object StateData {

  def restoreFromPreferences(context: Context) = StateData( 
    PreferenceManager.getDefaultSharedPreferences(context).getString("payoffFnStr", "K-S"),
    PreferenceManager.getDefaultSharedPreferences(context).getString("numPaths", "10000").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("timeToExpiration", "1.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("numSteps", "50").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("stock", "36.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("exercisePrice", "40.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("riskFreeRate", "0.06").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("volatility", "0.20").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("numSamples", "50").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("threshold", "50000").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("uiUpdateInterval", "200").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("jniFlag", "false").toBoolean,
    PreferenceManager.getDefaultSharedPreferences(context).getString("rngSeed", "1").toInt
  )

}

@serializable class CacheData(a: Array[Array[Double]], s: String) {
  val samplePriceArray = a
  val statusStr = s
}
object CacheData {
  private val TAG: String = "CacheData"
  private val oldFileName: String = "CacheData.obj"
  private val fileName: String = "CacheData_3.obj"

  def dump(c: CacheData, context: Context) {
    val dataDir = context.getExternalFilesDir(null)
    Log.e(TAG, "dump")
    ensureCacheDirExists()
    if (c != null) {
      Log.e(TAG, "dataDir+fileName ="+dataDir.toString+fileName)
      val file = new File (dataDir, fileName)
      val out = new FileOutputStream(file)
      out.write(Marshal.dump(c))
      out.close
    }
  }

  def load(context: Context): CacheData = {
    val dataDir = context.getExternalFilesDir(null)
    Log.e(TAG, "load")
    Log.e(TAG, "dataDir = "+dataDir)
    ensureCacheDirExists()
    if ((new File(dataDir, oldFileName)).isFile()) {
      // clean up previous version after version upgrade
      Log.e(TAG, "deleting oldFileName: "+dataDir.toString+oldFileName)
      (new File(dataDir, oldFileName)).delete()
    }
    if ((new File(dataDir, fileName)).isFile()) {
      Log.e(TAG, "load-1")
      val file = new File (dataDir, fileName)
      val in = new FileInputStream(file)
      Log.e(TAG, "load-2")
      //val bytes = Stream.continually(in.read).takeWhile(-1 !=).map(_.toByte).toArray
      val f = new RandomAccessFile(file, "rw");
      val b = new Array[Byte](f.length().toInt)
      f.read(b)
      Log.e(TAG, "load-3")
      val c: CacheData = Marshal.load[CacheData](b)
      Log.e(TAG, "load-4")
      c
    } else {
      new CacheData(defaultData.samplePriceArray, "Starting mcOptCal ...\n") // todo
    }
  }

  def ensureCacheDirExists() {
    /*val directory = new File(dataDir)
    if (!(directory.isDirectory()))
      directory.mkdirs()*/
    true
  }

}


class MainActivity extends Activity with TypedActivity {

  /** Load the native library where the native method
   *  is stored.
   */
  System.loadLibrary("mcOptCal-jni")

  private val TAG: String = "MainActivity"
  private val ParametersDlg = 1
  private val SettingsDlg = 2
  private val HelpDlg = 3
  private val CopyDlg = 4
  private val LsmParametersDlg = 5
  private val AsianParametersDlg = 6

  var intent: Intent = null
  var calc: Calc = null
  var cacheData: CacheData = _

  private var mService: mcOptCalService = _
  private var mBound: Boolean = false
  private var isAsian: Boolean = false

  var mConnection: mcOptCalServiceConnection = new mcOptCalServiceConnection

  class mcOptCalServiceConnection extends ServiceConnection {

    def onServiceConnected(className: ComponentName, service: IBinder) {
      mService = service.asInstanceOf[mcOptCalService#mcOptCalServiceBinder].getService
      val binder = service.asInstanceOf[mcOptCalService#mcOptCalServiceBinder]
      mBound = true
      binder.setListener(new BoundServiceListener() {

          def report(str: String) {
            MainActivity.this.runOnUiThread(new Runnable() {
                override def run() {
                  Log.d(TAG, "MCReport: "+str )
                  if (mBound) {
                    if (mService.progressState) {
                      progress1.setVisibility(View.VISIBLE)
                      progress1.setProgress(mService.progressVal)
                    } else {
                      progress1.setVisibility(View.INVISIBLE)
                    }
                  }
                }
              })
          }

          def reportComplete(str: String) {
            MainActivity.this.runOnUiThread(new Runnable() {
                override def run() {
                  // 1.01 if (mBound) {
                    if (mService.calcComplete) {
                      Log.e(TAG, "mService.calcComplete")
                      cleanTempFiles //todo
                      if (mService.priceAry != null)
                        cacheData = new CacheData(mService.priceAry, cacheData.statusStr+"\n"+mService.statusStr)
                      else
                        cacheData = new CacheData(cacheData.samplePriceArray, cacheData.statusStr+"\n"+mService.statusStr)
                      CacheData.dump(cacheData, getApplicationContext)
                      mRenderer.init4(cacheData.samplePriceArray)
                      mGLSurfaceViewB.requestRender()
                      updateOutputText(cacheData.statusStr)
                      mService.calcComplete = false
                    }
                    progress1.setVisibility(View.INVISIBLE)
                  }
                })
              // 1.01
              Log.d(TAG, "Stopping mcOptCal Service" )
              if(mBound) {
                Log.d(TAG, "stopping LSM" )
                mService.stopLSM
                Log.d(TAG, "unbinding service" )
                unbindService(mConnection)
                Log.d(TAG, "set mBound to false" )
                mBound = false
              }
              Log.d(TAG, "stopService" )
              stopService(intent)
          }
        })

      // 1.01 start service only when running LSM
      if(mBound) {
        if (!mService.calcRunning) {

          val stateData = StateData.restoreFromPreferences(getApplicationContext)
          val params = LsmParams(
            lsm.EqnParsers.parseEval(stateData.payoffFnStr),
            stateData.payoffFnStr,
            stateData.numPaths,
            stateData.timeToExpiration.toInt,
            stateData.numSteps,
            stateData.stock,
            stateData.exercisePrice,
            stateData.riskFreeRate,
            stateData.volatility,
            stateData.numSamples,
            stateData.threshold,
            stateData.uiUpdateInterval,
            stateData.rngSeed,
            getApplicationContext.getExternalFilesDir(null)
            )

          progress1.setVisibility(View.VISIBLE)
          progress1.setProgress(0)
          val newData = {
            if (isAsian)
              "\nStart Asian MC calculation:\n"+params
            else
              "\nStart LSM MC calculation:\n"+params
          }
          cacheData = new CacheData(cacheData.samplePriceArray, cacheData.statusStr+newData) //todo 
          updateOutputText(cacheData.statusStr)
          if (isAsian)
            mService.startAsianMC(params, stateData.jniFlag)
          mService.startLSM(params)
        }
      }
    }

    def onServiceDisconnected(className: ComponentName) {
      mBound = false
    }
  }


  def btnBS = findView(TR.button1).asInstanceOf[Button]
  def btnASIAN = findView(TR.buttonAsian).asInstanceOf[Button]
  def btnCopy = findView(TR.buttonCopy).asInstanceOf[Button]
  def btnSettings = findView(TR.buttonSettings).asInstanceOf[Button]
  def btnHelp = findView(TR.buttonHelp).asInstanceOf[Button]
  def btnClr = findView(TR.buttonClr).asInstanceOf[Button]
  def btnCancel = findView(TR.buttonCancel).asInstanceOf[Button]
  def btnLSM = findView(TR.button3).asInstanceOf[Button]
  var mRenderer: glFuncPlotRenderer = _
  def mGLSurfaceViewB: GLSurfaceView = findView(TR.glSurfaceViewB).asInstanceOf[GLSurfaceView]
  def progress1 = findView(TR.progress1).asInstanceOf[ProgressBar]
  def scrollv1 = findView(TR.scrollv1).asInstanceOf[ScrollView]
  def textview9mcresult = findView(TR.textview9mcresult).asInstanceOf[TextView]

  override def onCreate(bundle: Bundle) {
    super.onCreate(bundle)
    setContentView(R.layout.main)

    progress1.setVisibility(View.INVISIBLE)

    btnBS.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(ParametersDlg) } })

    btnASIAN.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(AsianParametersDlg) } })

    btnCopy.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(CopyDlg) } })

    btnSettings.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(SettingsDlg) } })

    btnHelp.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(HelpDlg) } })

    btnClr.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          cacheData = new CacheData(cacheData.samplePriceArray, "")
          updateOutputText(cacheData.statusStr)
        }
      })

    btnCancel.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          if(mBound) {
            mService.stopLSM
          }
        }
      })

    btnLSM.setOnClickListener(new View.OnClickListener() { def onClick(v : View) { showDialog(LsmParametersDlg) } })


    PreferenceManager.getDefaultSharedPreferences(this).edit().putString("mBound", "false").commit()
    mBound = false

    mGLSurfaceViewB.setEGLContextClientVersion(2)
    mRenderer = new glFuncPlotRenderer()
    //mGLSurfaceViewB.setEGLConfigChooser(8, 8, 8, 8, 6, 0)
    mGLSurfaceViewB.setRenderer(mRenderer)

    val vt = textview9mcresult.getViewTreeObserver()
    vt.addOnGlobalLayoutListener( new OnGlobalLayoutListener() {
        override def onGlobalLayout() {
          val posn = textview9mcresult.getBottom()
          scrollv1.smoothScrollTo(0, posn)
        }
      })
  }

  override def onPostCreate(bundle: Bundle) {
    super.onPostCreate(bundle)
  }
  
  def datFilter(fn :String) = fn.toLowerCase.endsWith("dat")

  def cleanTempFiles() {
    val dataDir = getApplicationContext.getExternalFilesDir(null)
    val fileList = dataDir.list.filter(datFilter _)
    Log.e(TAG, "cleanTempFiles")
    for (fileName <- fileList) {
      Log.e(TAG, "deleting file: "+dataDir.toString+fileName)
      (new File(dataDir, fileName)).delete()
    }

  }

  def updateOutputText(text: String) {

    Log.d(TAG, "updateOutputText")
    textview9mcresult.setText(text+"\n")
  }

  protected override def onPause() {
    super.onPause
    Log.e(TAG, "onResume")
    CacheData.dump(cacheData, getApplicationContext)
    val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
    prefs.edit().putString("mBound", mBound.toString).commit()
    if(mBound) {
      unbindService(mConnection)
      mBound = false
    }
    mGLSurfaceViewB.onPause
  }


  protected override def onResume() {
    super.onResume
    Log.e(TAG, "onResume")
    val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
    
    mBound = prefs.getString("mBound", "false").toBoolean

    intent = new Intent(MainActivity.this, classOf[mcOptCalService])
    bindService(intent, mConnection, 0) // 1.01

    mGLSurfaceViewB.onResume()
    updateOutputText(cacheData.statusStr)
  }

  protected override def onStart() {
    super.onStart
    Log.e(TAG, "onStart")
    val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
    cacheData = CacheData.load(getApplicationContext)
    Log.e(TAG, "loadedCacheData")
    mRenderer.init4(cacheData.samplePriceArray)
    //updateOutputText(cacheData.statusStr)
  }

  override def onStop() {
    Log.d(TAG, "onStop" )
    CacheData.dump(cacheData, getApplicationContext)
    super.onStop
  }

  override def onDestroy() {
    Log.d(TAG, "onDestroy" )
    if(mBound) {
      unbindService(mConnection)
      mBound = false
    }
    super.onDestroy
  }

  private def negativeBtnClk(dialog: DialogInterface, id: Int, textEntryView: View) {
    val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)
    dialog.cancel()
  }

  private def positiveBtnClk( dialog: DialogInterface, textEntryView: View ) = {
    val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
    imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)

    dialog.dismiss()

    val newStateData = StateData.restoreFromPreferences(getApplicationContext)
    val params = LsmParams(
      lsm.EqnParsers.parseEval(newStateData.payoffFnStr),
      newStateData.payoffFnStr,
      newStateData.numPaths,
      newStateData.timeToExpiration.toInt,
      newStateData.numSteps,
      newStateData.stock,
      newStateData.exercisePrice,
      newStateData.riskFreeRate,
      newStateData.volatility,
      newStateData.numSamples,
      newStateData.threshold,
      newStateData.uiUpdateInterval,
      newStateData.rngSeed,
      getApplicationContext.getExternalFilesDir(null) )

    cleanTempFiles //todo

    params
  }
  
  private def updateSettingsStateData(numSamples: Int, threshold: Int, uiUpdateInterval: Int) {
    val stateData = StateData.restoreFromPreferences(getApplicationContext)
    val newStateData = StateData(
      stateData.payoffFnStr,
      stateData.numPaths,
      stateData.timeToExpiration,
      stateData.numSteps,
      stateData.stock,
      stateData.exercisePrice,
      stateData.riskFreeRate,
      stateData.volatility,
      numSamples,
      threshold,
      uiUpdateInterval, 
      stateData.jniFlag,
      stateData.rngSeed )
    newStateData.saveToSharedPreferences(getApplicationContext)
  }

  private def updateBSParamsStateData( timeToExpiration: Double,
    stock: Double,
    exercisePrice: Double,
    riskFreeRate: Double,
    volatility: Double ) {
    val stateData = StateData.restoreFromPreferences(getApplicationContext)
    val newStateData = StateData(
      stateData.payoffFnStr,
      stateData.numPaths,
      timeToExpiration,
      stateData.numSteps,
      stock,
      exercisePrice,
      riskFreeRate,
      volatility,
      stateData.numSamples,
      stateData.threshold,
      stateData.uiUpdateInterval,
      stateData.jniFlag,
      stateData.rngSeed )
    newStateData.saveToSharedPreferences(getApplicationContext)
  }

  private def updateParamsStateData( payoffFnStr: String,
    numPaths: Int,
    timeToExpiration: Double,
    numSteps: Int,
    stock: Double,
    exercisePrice: Double,
    riskFreeRate: Double,
    volatility: Double,
    rngSeed: Int,
    jniFlag: Boolean ) {
    val stateData = StateData.restoreFromPreferences(getApplicationContext)
    val newStateData = StateData(
      payoffFnStr,
      numPaths,
      timeToExpiration,
      numSteps,
      stock,
      exercisePrice,
      riskFreeRate,
      volatility,
      stateData.numSamples,
      stateData.threshold,
      stateData.uiUpdateInterval,
      jniFlag,
      rngSeed )
    newStateData.saveToSharedPreferences(getApplicationContext)
  }

  override protected def onCreateDialog(id: Int): Dialog = {
    val factory = LayoutInflater.from(this)
    id match {
      case ParametersDlg => {
        val textEntryView = factory.inflate(R.layout.parameters_dialog, null)
        val alrtDialog: AlertDialog = new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.parameters)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
              val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
              val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
              val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
              val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble

              updateBSParamsStateData( timeToExpiration, stock, exercisePrice, riskFreeRate, volatility)
              val params = positiveBtnClk( dialog, textEntryView )

              val strB = new StringBuilder
              //val dWidth = getWindowManager().getDefaultDisplay().getWidth()
              //val dHeight = getWindowManager().getDefaultDisplay().getHeight()
              //strB.append("\nDisplay Width : \n"+dWidth+"dp   "+"Display Height : \n"+dHeight+"dp\n")
              strB.append("\nStart BS calculation:\n")
              strB.append(params)

              val newData = {
                val bsEuroCallVal = dgmath.bsEuropeanCallVal(
                  params.stock,
                  params.strike,
                  params.rate,
                  params.expiry,
                  params.volatility )
                val bsEuroPutVal = dgmath.bsEuropeanPutVal(
                  params.stock,
                  params.strike,
                  params.rate,
                  params.expiry,
                  params.volatility )
                strB.result+"BS Put Option Value  = "+"%1.4f".format(bsEuroCallVal)+"\nBS Call Option Value = "+"%1.4f".format(bsEuroPutVal)+"\n"
              }
              cacheData = new CacheData(cacheData.samplePriceArray, cacheData.statusStr+newData)
              updateOutputText(cacheData.statusStr)

            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { negativeBtnClk(dialog, id, textEntryView) }
          })
        .create()
        alrtDialog
      }
      case AsianParametersDlg => {
        val textEntryView = factory.inflate(R.layout.lsmparameters_dialog, null)
        val asianParamsDialog: AlertDialog = new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.parameters)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val payoffFnStr = textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].getText.toString
              val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
              val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
              val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
              val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
              val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble
              val numPaths = textEntryView.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].getText().toString.toInt
              val numSteps = textEntryView.findViewById(R.id.edittext6).asInstanceOf[EditText].getText().toString.toInt
              val rngSeed = textEntryView.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].getText().toString.toInt
              val jniFlag = {
                val id = textEntryView.findViewById(R.id.radiogroup_sj).asInstanceOf[RadioGroup].getCheckedRadioButtonId
                val scala_jni = ((textEntryView.findViewById(id).asInstanceOf[RadioButton]).getText.toString)
                if (scala_jni == "JNI")
                  true
                else
                  false
              }

              val eqn = lsm.EqnParsers.parse(payoffFnStr)
              eqn match {
                case lsm.ErrorText(e) => {
                  Log.d(TAG, e )
                  textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError("Parse Error!")
                }
                case _ => {

                  textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError(null)

                  updateParamsStateData( payoffFnStr, numPaths, timeToExpiration, numSteps, stock, 
                    exercisePrice, riskFreeRate, volatility, rngSeed, jniFlag)

                  val params = positiveBtnClk( dialog, textEntryView )

                  // 1.01 start service only when running LSM
                  Log.d(TAG, "Starting mcOptCal Service" )
                  intent = new Intent(MainActivity.this, classOf[mcOptCalService])

                  Log.d(TAG, "Starting mcOptCal Service" )
                  startService(intent)
                  bindService(intent, mConnection, Context.BIND_AUTO_CREATE)
                  isAsian = true
                }
              }
            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { negativeBtnClk(dialog, id, textEntryView) }
          })
        .create()
        asianParamsDialog.setOnShowListener(new DialogInterface.OnShowListener() {
            override def onShow(dialog: DialogInterface) {
              val b = asianParamsDialog.getButton(DialogInterface.BUTTON_POSITIVE)
              b.setOnClickListener(new View.OnClickListener() {
                  override def onClick(view: View) {
                    val payoffFnStr = textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].getText.toString
                    val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
                    val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
                    val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
                    val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
                    val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble
                    val numPaths = textEntryView.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].getText().toString.toInt
                    val numSteps = textEntryView.findViewById(R.id.edittext6).asInstanceOf[EditText].getText().toString.toInt
                    val rngSeed = textEntryView.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].getText().toString.toInt
                    val jniFlag = {
                      val id = textEntryView.findViewById(R.id.radiogroup_sj).asInstanceOf[RadioGroup].getCheckedRadioButtonId
                      val scala_jni = ((textEntryView.findViewById(id).asInstanceOf[RadioButton]).getText.toString)
                      if (scala_jni == "JNI")
                        true
                      else
                        false
                    }

                    val eqn = lsm.EqnParsers.parse(payoffFnStr)

                    eqn match {
                      case lsm.ErrorText(e) => {
                        Log.d(TAG, e )
                        textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError("Parse Error!")
                      }
                      case _ => {
                        textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError(null)

                        updateParamsStateData( payoffFnStr, numPaths, timeToExpiration, numSteps, stock, 
                          exercisePrice, riskFreeRate, volatility, rngSeed, jniFlag)

                        val params = positiveBtnClk( asianParamsDialog, textEntryView )

                        // 1.01 start service only when running LSM
                        Log.d(TAG, "Starting mcOptCal Service" )
                        intent = new Intent(MainActivity.this, classOf[mcOptCalService])

                        Log.d(TAG, "Starting mcOptCal Service" )
                        startService(intent)
                        bindService(intent, mConnection, Context.BIND_AUTO_CREATE)
                        isAsian = true
                      }
                    }

                  }
                })
              }
            })
        asianParamsDialog
      }
      case LsmParametersDlg => {
        val textEntryView = factory.inflate(R.layout.lsmparameters_dialog, null)
        val lsmParamsDialog: AlertDialog = new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.parameters)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val payoffFnStr = textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].getText.toString
              val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
              val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
              val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
              val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
              val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble
              val numPaths = textEntryView.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].getText().toString.toInt
              val numSteps = textEntryView.findViewById(R.id.edittext6).asInstanceOf[EditText].getText().toString.toInt
              val rngSeed = textEntryView.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].getText().toString.toInt

              val eqn = lsm.EqnParsers.parse(payoffFnStr)
              eqn match {
                case lsm.ErrorText(e) => {
                  Log.d(TAG, e )
                  textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError("Parse Error!")
                }
                case _ => {
                  textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError(null)

                  val stateData = StateData.restoreFromPreferences(getApplicationContext)
                  updateParamsStateData( payoffFnStr, numPaths, timeToExpiration, numSteps, stock, 
                    exercisePrice, riskFreeRate, volatility, rngSeed, stateData.jniFlag)

                  val params = positiveBtnClk( dialog, textEntryView )

                  // 1.01 start service only when running LSM
                  Log.d(TAG, "Starting mcOptCal Service" )
                  intent = new Intent(MainActivity.this, classOf[mcOptCalService])

                  Log.d(TAG, "Starting mcOptCal Service" )
                  startService(intent)
                  bindService(intent, mConnection, Context.BIND_AUTO_CREATE)
                  isAsian = false
                }
              }
            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { negativeBtnClk(dialog, id, textEntryView) }
          })
        .create()
        lsmParamsDialog.setOnShowListener(new DialogInterface.OnShowListener() {
            override def onShow(dialog: DialogInterface) {
              val b = lsmParamsDialog.getButton(DialogInterface.BUTTON_POSITIVE)
              b.setOnClickListener(new View.OnClickListener() {
                  override def onClick(view: View) {
                    val payoffFnStr = textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].getText.toString
                    val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
                    val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
                    val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
                    val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
                    val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble
                    val numPaths = textEntryView.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].getText().toString.toInt
                    val numSteps = textEntryView.findViewById(R.id.edittext6).asInstanceOf[EditText].getText().toString.toInt
                    val rngSeed = textEntryView.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].getText().toString.toInt

                    val eqn = lsm.EqnParsers.parse(payoffFnStr)
                    eqn match {
                      case lsm.ErrorText(e) => {
                        Log.d(TAG, e )
                        textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError("Parse Error!")
                      }
                      case _ => {
                        textEntryView.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setError(null)

                        val stateData = StateData.restoreFromPreferences(getApplicationContext)
                        updateParamsStateData( payoffFnStr, numPaths, timeToExpiration, numSteps, stock, 
                          exercisePrice, riskFreeRate, volatility, rngSeed, stateData.jniFlag)

                        val params = positiveBtnClk( lsmParamsDialog, textEntryView )

                        // 1.01 start service only when running LSM
                        Log.d(TAG, "Starting mcOptCal Service" )
                        intent = new Intent(MainActivity.this, classOf[mcOptCalService])

                        Log.d(TAG, "Starting mcOptCal Service" )
                        startService(intent)
                        bindService(intent, mConnection, Context.BIND_AUTO_CREATE)
                        isAsian = false
                      }
                    }

                  }
                })
              }
            })
        lsmParamsDialog
      }
      case SettingsDlg => {
        val textEntryView = factory.inflate(R.layout.settings_dialog, null)
        val settingsDialog: AlertDialog = new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.settings)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val numSamples = textEntryView.findViewById(R.id.edittext7).asInstanceOf[EditText].getText().toString.toInt
              val threshold = textEntryView.findViewById(R.id.edittext8).asInstanceOf[EditText].getText().toString.toInt
              val uiUpdateInterval = textEntryView.findViewById(R.id.edittext9).asInstanceOf[EditText].getText().toString.toInt

              updateSettingsStateData(numSamples, threshold, uiUpdateInterval)
              val params = positiveBtnClk( dialog, textEntryView ) // not required?
            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { negativeBtnClk(dialog, id, textEntryView) }
          })
        .create()
        settingsDialog.setOnShowListener(new DialogInterface.OnShowListener() {
            override def onShow(dialog: DialogInterface) {
              val b = settingsDialog.getButton(DialogInterface.BUTTON_POSITIVE)
              b.setOnClickListener(new View.OnClickListener() {
                  override def onClick(view: View) {
                    val numSamples = textEntryView.findViewById(R.id.edittext7).asInstanceOf[EditText].getText().toString.toInt
                    val threshold = textEntryView.findViewById(R.id.edittext8).asInstanceOf[EditText].getText().toString.toInt
                    val uiUpdateInterval = textEntryView.findViewById(R.id.edittext9).asInstanceOf[EditText].getText().toString.toInt
                    updateSettingsStateData(numSamples, threshold, uiUpdateInterval)

                    val params = positiveBtnClk( dialog, textEntryView ) // not required?
                  }
                })
            }
          })
        settingsDialog
      }
      case HelpDlg => {
        val textEntryView = factory.inflate(R.layout.help_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.help)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { dialog.dismiss() }
          })
        .create()
      }
      case CopyDlg => {
        val clipboard = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[android.text.ClipboardManager]
        val text = textview9mcresult.getText()
        clipboard.setText(text)
        val textEntryView = factory.inflate(R.layout.copy_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.copy)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) { dialog.dismiss() }
          })
        .create()
      }
    }
  }

  override protected def onPrepareDialog(id: Int, d: Dialog) {
    id match {
      case ParametersDlg => {
        val stateData = StateData.restoreFromPreferences(getApplicationContext)
        d.findViewById(R.id.edittext1).asInstanceOf[EditText].setText(stateData.stock.toString)
        d.findViewById(R.id.edittext2).asInstanceOf[EditText].setText(stateData.exercisePrice.toString)
        d.findViewById(R.id.edittext3).asInstanceOf[EditText].setText(stateData.riskFreeRate.toString)
        d.findViewById(R.id.edittext4).asInstanceOf[EditText].setText(stateData.volatility.toString)
        d.findViewById(R.id.edittext5).asInstanceOf[EditText].setText(stateData.timeToExpiration.toString)
      }
      case AsianParametersDlg => {
        val stateData = StateData.restoreFromPreferences(getApplicationContext)
        d.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setText(stateData.payoffFnStr)
        d.findViewById(R.id.edittext1).asInstanceOf[EditText].setText(stateData.stock.toString)
        d.findViewById(R.id.edittext2).asInstanceOf[EditText].setText(stateData.exercisePrice.toString)
        d.findViewById(R.id.edittext3).asInstanceOf[EditText].setText(stateData.riskFreeRate.toString)
        d.findViewById(R.id.edittext4).asInstanceOf[EditText].setText(stateData.volatility.toString)
        d.findViewById(R.id.edittext5).asInstanceOf[EditText].setText(stateData.timeToExpiration.toString)
        d.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].setText(stateData.numPaths.toString)
        d.findViewById(R.id.edittext6).asInstanceOf[EditText].setText(stateData.numSteps.toString)
        d.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].setText(stateData.rngSeed.toString)
        d.findViewById(R.id.btnGenRngSeed).asInstanceOf[Button].setOnClickListener(new View.OnClickListener() {
            def onClick(v : View) {
              val seed = lsm.rng.nextInt
              d.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].setText(seed.toString)
            }
          })
        val rBtnGr = d.findViewById(R.id.radiogroup_sj).asInstanceOf[RadioGroup]
        if (stateData.jniFlag)
          rBtnGr.check(R.id.radioBtnJNI)
        else 
          rBtnGr.check(R.id.radioBtnScala)
        d.getWindow().setLayout(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT);
      }
      case LsmParametersDlg => {
        val stateData = StateData.restoreFromPreferences(getApplicationContext)
        d.findViewById(R.id.edittext_payofffn).asInstanceOf[EditText].setText(stateData.payoffFnStr)
        d.findViewById(R.id.edittext1).asInstanceOf[EditText].setText(stateData.stock.toString)
        d.findViewById(R.id.edittext2).asInstanceOf[EditText].setText(stateData.exercisePrice.toString)
        d.findViewById(R.id.edittext3).asInstanceOf[EditText].setText(stateData.riskFreeRate.toString)
        d.findViewById(R.id.edittext4).asInstanceOf[EditText].setText(stateData.volatility.toString)
        d.findViewById(R.id.edittext5).asInstanceOf[EditText].setText(stateData.timeToExpiration.toString)
        d.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].setText(stateData.numPaths.toString)
        d.findViewById(R.id.edittext6).asInstanceOf[EditText].setText(stateData.numSteps.toString)
        d.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].setText(stateData.rngSeed.toString)
        d.findViewById(R.id.btnGenRngSeed).asInstanceOf[Button].setOnClickListener(new View.OnClickListener() {
            def onClick(v : View) {
              val seed = lsm.rng.nextInt
              d.findViewById(R.id.edittextRngSeed).asInstanceOf[EditText].setText(seed.toString)
            }
          })
        val rBtnGr = d.findViewById(R.id.radiogroup_sj).asInstanceOf[RadioGroup]
        val rBtnJNI = d.findViewById(R.id.radioBtnJNI).asInstanceOf[RadioButton]
        val rBtnScala = d.findViewById(R.id.radioBtnScala).asInstanceOf[RadioButton]
        rBtnGr.setEnabled(false)
        rBtnJNI.setEnabled(false)
        rBtnScala.setEnabled(false)
        //if (stateData.jniFlag)
        //  rBtnGr.check(R.id.radioBtnJNI)
        //else 
          rBtnGr.check(R.id.radioBtnScala)
        d.getWindow().setLayout(LayoutParams.FILL_PARENT, LayoutParams.FILL_PARENT);
      }
      case SettingsDlg => {
        val stateData = StateData.restoreFromPreferences(getApplicationContext)
        d.findViewById(R.id.edittext7).asInstanceOf[EditText].setText(stateData.numSamples.toString)
        d.findViewById(R.id.edittext8).asInstanceOf[EditText].setText(stateData.threshold.toString)
        d.findViewById(R.id.edittext9).asInstanceOf[EditText].setText(stateData.uiUpdateInterval.toString)
      }
      case HelpDlg => { }
      case CopyDlg => { }
    }
  }

}

