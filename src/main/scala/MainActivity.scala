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
import _root_.android.view.inputmethod.InputMethodManager
import _root_.android.view.View.OnClickListener
import _root_.android.view.ViewTreeObserver.OnGlobalLayoutListener
import _root_.android.widget._


import java.io._

import scala.actors.Actor._
import scala.actors._
import scala.collection.parallel.immutable._
import scala.collection.parallel.mutable._
import scala.concurrent.ops._
import scala.io.Source
import scala.util.{Marshal, Random}
import scala.math._

import com.dragongate_technologies.glfuncplot._

class LSMCalcParams(_params: LsmParams, _callerService: Actor) {
  val params = _params
  val callerService = _callerService
}

case class CalcStartLSM(params: LSMCalcParams)
case class mcOptCalServiceLSMResult(lsmOV: Tuple3[Double, Double, Array[Array[Double]]], runTime: Long)
case class MCReport(str: String)
case class MCReportComplete(str: String)
case class MCReportAbort(str: String)
case class lsmStatusReport(step: Int, numSteps: Int)
case class lsmAbortReport
case object CalcStopLSM


class Calc extends Actor {
  private val TAG: String = "Calc"

  var callerService: Actor = null

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
          exit()
        }
        /*case CalcStopLSM =>
          Log.d(TAG, "CalcStopLSM" )
          exit()*/
      }
    }
  }
}

class mcOptCalService extends Service with Actor {
  private val TAG: String = "mcOptCalService"

  val contentTitle: CharSequence = "mcOptCalService"
  val ns: String = Context.NOTIFICATION_SERVICE

  var notification: Notification = null
  var calc: Calc = null
  var working: Boolean = false
  var calcRunning = false
  var calcStr: String = "idle"
  var mainActor: Actor = null

  var statusStr = ""
  var calcComplete = false
  var progressState = false
  var progressVal = 0
  var priceAry: Array[Array[Double]] = Array.fill(10, 10)(1.0d) //todo

  class mcOptCalServiceBinder extends Binder {
    def getService: mcOptCalService = mcOptCalService.this
  }

  private final val mBinder = new mcOptCalServiceBinder()

  override def onBind(intent: Intent): IBinder = {
    Log.d(TAG, "onBind" )
    mBinder
  }

  override def onCreate() {
    Log.d(TAG, "onCreate" )

    val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]

    val icon: Int = R.drawable.logo
    val tickerText: CharSequence = "Monte Carlo Service Started"
    val when: Long = System.currentTimeMillis()
    notification = new Notification(icon, tickerText, when)

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

          calcStr = "LSM Option Value = "+"% 6.4f".format(lsmOV._1)+"  ( s.e. "+"%.4f".format(lsmOV._2)+" ) [ "+"%.3f".format(runTime.toDouble/1000)+"sec ]"
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
          mainActor ! MCReportComplete(calcStr)
          calcRunning = false
        }
        case lsmStatusReport(step, numSteps) => {
          val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
          val context: Context = getApplicationContext()
          val notificationIntent = new Intent(this, classOf[MainActivity])
          val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)

          progressVal = 100 - step*100/numSteps
          notification.setLatestEventInfo(context, contentTitle, "recurseCF step="+step, contentIntent)
          mNM.notify(1, notification)
          mainActor ! MCReport("recurseCF step="+step)
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
          progressVal = 100
          /*val priceAryRows = lsmOV._3.length
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
          }*/
          notification.setLatestEventInfo(context, contentTitle, calcStr, contentIntent)
          mNM.notify(1, notification)
          mainActor ! MCReportComplete(calcStr)
          calcRunning = false
        }
      }
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
      Log.d(TAG, "stopCalc: if {" )
      working = false
      calcRunning = false
      calc ! CalcStopLSM

      val contentText: CharSequence = "Stopping LSM calculation"
      val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
      val context: Context = getApplicationContext()
      val notificationIntent = new Intent(this, classOf[MainActivity])
      val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)
      notification.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
      mNM.notify(1, notification)
    }
  }

  /*def stopCalc = {
    Log.d(TAG, "CalcService.stopCalc" )
    if (calcRunning) {
      Log.d(TAG, "stopCalc: if {" )
      working = false
      calcRunning = false
      calc ! CalcStop 

      val contentText: CharSequence = "CalcService.stopCalc"
      calcStr = calcStr+"[ "+contentText.toString+" ]"
      val mNM: NotificationManager  = getSystemService(ns).asInstanceOf[NotificationManager]
      val context: Context = getApplicationContext()
      val notificationIntent = new Intent(this, classOf[MainActivity])
      val contentIntent: PendingIntent = PendingIntent.getActivity(this, 0, notificationIntent, 0)
      notification.setLatestEventInfo(context, contentTitle, contentText, contentIntent);
      mNM.notify(1, notification)
    }
  }*/

}


case class StateData(
  isPut: Boolean,
  numPaths: Int,
  timeToExpiration: Double,
  numSteps: Int,
  stock: Double,
  exercisePrice: Double,
  riskFreeRate: Double,
  volatility: Double,
  numSamples: Int,
  threshold: Int,
  uiUpdateInterval: Int
  ) {

  def saveToSharedPreferences(context: Context) {
    val prefsEdit = PreferenceManager.getDefaultSharedPreferences(context).edit()
    prefsEdit.putString("isPut", isPut.toString).commit()
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
  }

}
case object StateData {

  def restoreFromPreferences(context: Context) = StateData( 
    PreferenceManager.getDefaultSharedPreferences(context).getString("isPut", "true").toBoolean,
    PreferenceManager.getDefaultSharedPreferences(context).getString("numPaths", "10000").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("timeToExpiration", "1.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("numSteps", "50").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("stock", "36.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("exercisePrice", "40.0").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("riskFreeRate", "0.06").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("volatility", "0.20").toDouble,
    PreferenceManager.getDefaultSharedPreferences(context).getString("numSamples", "50").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("threshold", "50000").toInt,
    PreferenceManager.getDefaultSharedPreferences(context).getString("uiUpdateInterval", "1").toInt
  )

}

@serializable class CacheData(a: Array[Array[Double]], s: String) {
  val samplePriceArray = a
  val statusStr = s
}
object CacheData {
  private val TAG: String = "CacheData"
  private val fileName: String = "CacheData.obj"

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
    ensureCacheDirExists()
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


class MainActivity extends Activity with TypedActivity with Actor {
  private val TAG: String = "MainActivity"
  private val ParametersDlg = 1
  private val SettingsDlg = 2
  private val HelpDlg = 3
  private val CopyDlg = 4

  val mainActor = this
  var intent: Intent = null
  var calc: Calc = null
  //var stateData: StateData = _
  //var cacheData: CacheData = new CacheData(Array.fill(2, 2)(0d), "Starting...\n")
  var cacheData: CacheData = _
  var funcAry: Array[Array[Double]] = Array.fill(10, 10)(0d) //todo

  private var mService: mcOptCalService = _
  private var mBound: Boolean = false

  var mConnection: mcOptCalServiceConnection = new mcOptCalServiceConnection

  class mcOptCalServiceConnection extends ServiceConnection {

    def onServiceConnected(className: ComponentName, service: IBinder) {
      mService = service.asInstanceOf[mcOptCalService#mcOptCalServiceBinder].getService
      mBound = true
      mService.mainActor = mainActor
    }

    def onServiceDisconnected(className: ComponentName) {
      mBound = false
    }
  }


  def btn1 = findView(TR.button1).asInstanceOf[Button]
  def btnParameters = findView(TR.buttonParameters).asInstanceOf[Button]
  def btnCopy = findView(TR.buttonCopy).asInstanceOf[Button]
  def btnSettings = findView(TR.buttonSettings).asInstanceOf[Button]
  def btnHelp = findView(TR.buttonHelp).asInstanceOf[Button]
  def btnClr = findView(TR.buttonClr).asInstanceOf[Button]
  def btnCancel = findView(TR.buttonCancel).asInstanceOf[Button]
  def btn3 = findView(TR.button3).asInstanceOf[Button]
  var mRenderer: glFuncPlotRenderer = _
  def mGLSurfaceViewB: GLSurfaceView = findView(TR.glSurfaceViewB).asInstanceOf[GLSurfaceView]
  def progress1 = findView(TR.progress1).asInstanceOf[ProgressBar]
  def scrollv1 = findView(TR.scrollv1).asInstanceOf[ScrollView]
  def textview9mcresult = findView(TR.textview9mcresult).asInstanceOf[TextView]

  override def onCreate(bundle: Bundle) {
    super.onCreate(bundle)
    //CacheData.dump(cacheData)
    setContentView(R.layout.main)

    progress1.setVisibility(View.INVISIBLE)

    btn1.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          val stateData = StateData.restoreFromPreferences(getApplicationContext)
          val params = LsmParams(
            stateData.isPut,
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
            getApplicationContext
            )

          val strB = new StringBuilder
          strB.append("\nStart BS calculation:\n[\n")
          val formatStr = "% .3f"
          strB.append( {
              if (params.isPut)
                " Type: Put\n"
              else
                " Type: Call\n"
            })
          strB.append(" expiry: "+params.expiry+"\n")
          strB.append(" stock: "+(formatStr.format(params.stock))+"\n")
          strB.append(" strike: "+(formatStr.format(params.strike))+"\n")
          strB.append(" rate: "+(formatStr.format(params.rate))+"\n")
          strB.append(" volatility: "+(formatStr.format(params.volatility))+"\n")
          strB.append("]\n")

          val newData = {
            if (stateData.isPut) {

              val bsEuroCallVal = dgmath.bsEuropeanCallVal(
                params.stock,
                params.strike,
                params.rate,
                params.expiry,
                params.volatility )
              strB.result+"BS Option Value = "+"%1.4f".format(bsEuroCallVal)+"\n"
            } else {
              val bsEuroPutVal = dgmath.bsEuropeanPutVal(
                params.stock,
                params.strike,
                params.rate,
                params.expiry,
                params.volatility )
              strB.result+"BS Option Value = "+"%1.4f".format(bsEuroPutVal)+"\n"
            }
          }
          cacheData = new CacheData(cacheData.samplePriceArray, cacheData.statusStr+newData)
          updateOutputText(cacheData.statusStr)
        }
      })

    btnParameters.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          showDialog(ParametersDlg)
        }
      })

    btnCopy.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          val clipboard = getSystemService(Context.CLIPBOARD_SERVICE).asInstanceOf[android.text.ClipboardManager]
          //clipboard.setText("text to clip")
          val text = textview9mcresult.getText()
          clipboard.setText(text)
          showDialog(CopyDlg)
        }
      })

    btnSettings.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          showDialog(SettingsDlg)
        }
      })

    btnHelp.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          showDialog(HelpDlg)
        }
      })

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

    intent = new Intent(MainActivity.this, classOf[mcOptCalService])

    startService(intent)
    bindService(intent, mConnection, Context.BIND_AUTO_CREATE)
    btn3.setOnClickListener(new View.OnClickListener() {
        def onClick(v : View) {
          cleanTempFiles //todo
          if(mBound) {
            if (!mService.calcRunning) {

              val stateData = StateData.restoreFromPreferences(getApplicationContext)
              val params = LsmParams(
                stateData.isPut,
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
                getApplicationContext
                )

              progress1.setVisibility(View.VISIBLE)
              progress1.setProgress(0)
              val newData = "\nStart LSM calculation:\n"+params
              cacheData = new CacheData(cacheData.samplePriceArray, cacheData.statusStr+newData) //todo 
              updateOutputText(cacheData.statusStr)
              mService.startLSM(params)
            }
          }
        }
      })


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
  
  def datFilter(fn :String) = fn.toLowerCase.endsWith("dat")

  def cleanTempFiles() {
    //val dataDir = "/sdcard/mcTempFiles/" // todo
    val dataDir = getApplicationContext.getExternalFilesDir(null)
    //val dir = new File(dataDir)
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
    /*val posn = textview9mcresult.getBottom()
    Log.d(TAG, "posn = "+posn)
    val posn2 = scrollv1.getBottom()
    Log.d(TAG, "posn2 = "+posn2)
    scrollv1.smoothScrollTo(0, posn)*/
  }

  /*protected override def onWindowFocusChanged(hasFocus: Boolean) {
    if (hasFocus) {
      val posn = textview9mcresult.getBottom()
      scrollv1.smoothScrollTo(0, posn)
    }
  }*/

  def act() {
    loop {
      react {
        case MCReportComplete(str) => {
          Log.d(TAG, "MCReportComplete: "+str )
          this.runOnUiThread(new Runnable() {
              override def run() {
                if (mBound) {
                  if (mService.calcComplete) {
                    Log.e(TAG, "mService.calcComplete")
                    cleanTempFiles //todo
                    cacheData = new CacheData(mService.priceAry, cacheData.statusStr+"\n"+mService.statusStr)
                    CacheData.dump(cacheData, getApplicationContext)
                    mRenderer.init4(cacheData.samplePriceArray)
                    mGLSurfaceViewB.requestRender()
                    updateOutputText(cacheData.statusStr)
                    mService.calcComplete = false
                  }
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
        case MCReportAbort(str) => {
          Log.d(TAG, "MCReportAbort: "+str )
          this.runOnUiThread(new Runnable() {
              override def run() {
                if (mBound) {
                  if (mService.calcComplete) {
                    Log.e(TAG, "mService.calcComplete")
                    cleanTempFiles //todo
                    cacheData = new CacheData(mService.priceAry, cacheData.statusStr+"\n"+mService.statusStr)
                    updateOutputText(cacheData.statusStr)
                    CacheData.dump(cacheData, getApplicationContext)
                    mRenderer.init4(cacheData.samplePriceArray)
                    mGLSurfaceViewB.requestRender()
                    mService.calcComplete = false
                  }
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
        case MCReport(str) => {
          Log.d(TAG, "MCReport: "+str )
          this.runOnUiThread(new Runnable() {
              override def run() {
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
        case _ => {}
      }
    }
  }

  protected override def onPause() {
    super.onPause
    CacheData.dump(cacheData, getApplicationContext)
    val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
    prefs.edit().putString("mBound", mBound.toString).commit()
    mGLSurfaceViewB.onPause
  }


  protected override def onResume() {
    super.onResume
    Log.e(TAG, "onResume")
    val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(this)
    
    mBound = prefs.getString("mBound", "false").toBoolean

    intent = new Intent(MainActivity.this, classOf[mcOptCalService])

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
    mainActor.start //required
  }

  override def onStop() {
    Log.d(TAG, "onStop" )
    CacheData.dump(cacheData, getApplicationContext)
    super.onStop
  }

  override def onDestroy() {
    Log.d(TAG, "onDestroy" )
    unbindService(mConnection)
    super.onDestroy
  }

  override protected def onCreateDialog(id: Int): Dialog = {
    val stateData = StateData.restoreFromPreferences(getApplicationContext)
    val factory = LayoutInflater.from(this)
    id match {
      case ParametersDlg => {
        val textEntryView = factory.inflate(R.layout.parameters_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.parameters)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val isPut = {
                val id = textEntryView.findViewById(R.id.radiogroup2).asInstanceOf[RadioGroup].getCheckedRadioButtonId
                ((textEntryView.findViewById(id).asInstanceOf[RadioButton]).getText.toString == "Put")
              }

              val stock = textEntryView.findViewById(R.id.edittext1).asInstanceOf[EditText].getText.toString.toDouble
              val exercisePrice = textEntryView.findViewById(R.id.edittext2).asInstanceOf[EditText].getText().toString.toDouble
              val riskFreeRate = textEntryView.findViewById(R.id.edittext3).asInstanceOf[EditText].getText().toString.toDouble
              val volatility = textEntryView.findViewById(R.id.edittext4).asInstanceOf[EditText].getText().toString.toDouble
              val timeToExpiration = textEntryView.findViewById(R.id.edittext5).asInstanceOf[EditText].getText().toString.toDouble
              val numPaths = textEntryView.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].getText().toString.toInt
              val numSteps = textEntryView.findViewById(R.id.edittext6).asInstanceOf[EditText].getText().toString.toInt

              val newStateData = StateData(
                isPut,
                numPaths,
                timeToExpiration,
                numSteps,
                stock,
                exercisePrice,
                riskFreeRate,
                volatility,
                stateData.numSamples,
                stateData.threshold,
                stateData.uiUpdateInterval )
              newStateData.saveToSharedPreferences(getApplicationContext)

              val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(getApplicationContext)
              prefs.edit().putString("stateDataSaved", "true").commit()

              val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
              imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)

              dialog.dismiss()
            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
              imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)

              dialog.cancel()
            }
          })
        .create()
      }
      case SettingsDlg => {
        val textEntryView = factory.inflate(R.layout.settings_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.settings)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val numSamples = textEntryView.findViewById(R.id.edittext7).asInstanceOf[EditText].getText().toString.toInt
              val threshold = textEntryView.findViewById(R.id.edittext8).asInstanceOf[EditText].getText().toString.toInt
              val uiUpdateInterval = textEntryView.findViewById(R.id.edittext9).asInstanceOf[EditText].getText().toString.toInt

              val newStateData = StateData(
                stateData.isPut,
                stateData.numPaths,
                stateData.timeToExpiration,
                stateData.numSteps,
                stateData.stock,
                stateData.exercisePrice,
                stateData.riskFreeRate,
                stateData.volatility,
                numSamples,
                threshold,
                uiUpdateInterval )
              newStateData.saveToSharedPreferences(getApplicationContext)

              val prefs: SharedPreferences = PreferenceManager.getDefaultSharedPreferences(getApplicationContext)
              prefs.edit().putString("stateDataSaved", "true").commit()

              val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
              imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)

              dialog.dismiss()
            }
          })
        .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              val imm = getSystemService(Context.INPUT_METHOD_SERVICE).asInstanceOf[InputMethodManager]
              imm.hideSoftInputFromWindow(textEntryView.getWindowToken(), 0)

              dialog.cancel()
            }
          })
        .create()
      }
      case HelpDlg => {
        val textEntryView = factory.inflate(R.layout.help_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.help)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              dialog.dismiss()
            }
          })
        .create()
      }
      case CopyDlg => {
        val textEntryView = factory.inflate(R.layout.copy_dialog, null)
        new AlertDialog.Builder(this)
        .setIcon(R.drawable.logo)
        .setTitle(R.string.copy)
        .setView(textEntryView)
        .setPositiveButton(R.string.ok,  new DialogInterface.OnClickListener() {
            override def onClick(dialog: DialogInterface, id: Int) {
              dialog.dismiss()
            }
          })
        .create()
      }
    }
  }

  override protected def onPrepareDialog(id: Int, d: Dialog) {
    val stateData = StateData.restoreFromPreferences(getApplicationContext)
    id match {
      case ParametersDlg => {
        d.findViewById(R.id.radiogroup2).asInstanceOf[RadioGroup].check({
            if (stateData.isPut)
              R.id.radioBtnPut
            else
              R.id.radioBtnCall
          })
        d.findViewById(R.id.edittext1).asInstanceOf[EditText].setText(stateData.stock.toString)
        d.findViewById(R.id.edittext2).asInstanceOf[EditText].setText(stateData.exercisePrice.toString)
        d.findViewById(R.id.edittext3).asInstanceOf[EditText].setText(stateData.riskFreeRate.toString)
        d.findViewById(R.id.edittext4).asInstanceOf[EditText].setText(stateData.volatility.toString)
        d.findViewById(R.id.edittext5).asInstanceOf[EditText].setText(stateData.timeToExpiration.toString)
        d.findViewById(R.id.editTextNumPaths).asInstanceOf[EditText].setText(stateData.numPaths.toString)
        d.findViewById(R.id.edittext6).asInstanceOf[EditText].setText(stateData.numSteps.toString)
      }
      case SettingsDlg => {
        d.findViewById(R.id.edittext7).asInstanceOf[EditText].setText(stateData.numSamples.toString)
        d.findViewById(R.id.edittext8).asInstanceOf[EditText].setText(stateData.threshold.toString)
        d.findViewById(R.id.edittext9).asInstanceOf[EditText].setText(stateData.uiUpdateInterval.toString)
      }
      case HelpDlg => {
      }
      case CopyDlg => {
      }
    }
  }

}

