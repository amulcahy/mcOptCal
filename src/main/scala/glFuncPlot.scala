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
package com.dragongate_technologies.glfuncplot

import _root_.android.app.Activity
import _root_.android.graphics.{Bitmap, BitmapFactory, Color}
import _root_.android.content.Context
import _root_.android.opengl.{GLES20, GLSurfaceView, GLUtils, Matrix}
import _root_.android.os.{Bundle, SystemClock}
import _root_.android.util.{AttributeSet, Log}
import _root_.android.view.MotionEvent
import _root_.android.widget.LinearLayout

import java.io.IOException;
import java.io.InputStream
import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.FloatBuffer
import java.nio.ShortBuffer

import javax.microedition.khronos.egl.EGLConfig
import javax.microedition.khronos.opengles.GL10

import scala.math._


class glFuncPlotRenderer extends GLSurfaceView.Renderer {
  val TAG: String = "glFuncPlotRenderer"

  var funcAry: Array[Array[Double]] = Array.fill(10, 10)(0d) //todo
  var dMax = 0.0

  var plotVertBuffer: FloatBuffer	= _
  var plotColourBuffer: FloatBuffer	= _

  var plotIndexBuffer: ShortBuffer	= _
  var indexLength: Int = _

  var mProgram: Int = _
  var maPositionHandle: Int = _
  var maColourHandle: Int = _
  var muMVPMatrixHandle: Int = _

  val mMVPMatrix: Array[Float] = new Array[Float](16)
  val mMMatrix: Array[Float] = new Array[Float](16)
  val mVMatrix: Array[Float] = new Array[Float](16)
  val mProjMatrix: Array[Float] = new Array[Float](16)

  val vertexShaderCode: String =
  "uniform mat4 uMVPMatrix;	\n" +
  "attribute vec4 vPosition;	\n" +
  "attribute vec4 vColour;	\n" +
  "varying vec4 fColour;		\n" +
  "void main(){				\n" +
  "	fColour = vColour;		\n" +
  "	gl_Position = uMVPMatrix * vPosition;\n" +
  "}							\n";

  val fragmentShaderCode: String =
  "precision mediump float;	\n" +
  "varying vec4 fColour;		\n" +
  "void main(){				\n" +
  "	gl_FragColor = fColour;	\n" +
  //"	gl_FragColor = vec4(1.0, 0.0, 0.0, 1.0); \n" +
  "}							\n";

  private def loadShader(iType: Int, shaderCode: String): Int = {
    val shader: Int = GLES20.glCreateShader(iType);

    GLES20.glShaderSource(shader, shaderCode);
    GLES20.glCompileShader(shader);

    return shader;
  }

  def makeFloatBuffer(aryFloat: Array[Float]): FloatBuffer = {
    val floatBuffer: FloatBuffer	= ByteBuffer.allocateDirect(aryFloat.length * 4).order(ByteOrder.nativeOrder()).asFloatBuffer()
    floatBuffer.put(aryFloat).position(0)
    floatBuffer
  }

  def makeShortBuffer(aryShort: Array[Short]): ShortBuffer = {
    val shortBuffer: ShortBuffer	= ByteBuffer.allocateDirect(aryShort.length * 4).order(ByteOrder.nativeOrder()).asShortBuffer()
    shortBuffer.put(aryShort).position(0)
    shortBuffer
  }

  def normalizeCoord(coord: Int, max: Int, scalingFactor: Float = 2f): Float = {
    (coord.toFloat*2f/max.toFloat-1f)*scalingFactor
  }

  def normalizeMatrix(matrix: Array[Array[Double]]): Array[Array[Float]] = {
    Log.e(TAG, "normalizeMatrix")
    val iSize = matrix.length
    val jSize = matrix(0).length
    var i = 0
    dMax = 0.0d
    while (i < iSize) {
      var j = 0
      while (j < jSize) {
        dMax = max(dMax, matrix(i)(j))
        j += 1
      }
      i += 1
    }
    val normMatrix = Array.ofDim[Float](iSize, jSize)
    i = 0
    while (i < iSize) {
      var j = 0
      while (j < jSize) {
        normMatrix(i)(j) = ((matrix(i)(j))/(dMax)).toFloat*4.0f-2.0f
        j += 1
      }
      i += 1
    }
    normMatrix
  }

  def makeCoordsArray(matrix: Array[Array[Float]]): Array[Float] = {
    Log.e(TAG, "makeCoordsArray")
    val iSize = matrix.length
    val jSize = matrix(0).length
    val plotCoords: Array[Float] = new Array[Float](iSize*jSize*3)
    var i = 0
    while (i < iSize) {
      var j = 0
      while (j < jSize) {
        val index = (i*jSize+j)*3
        //plotCoords(index) = normalizeCoord(i, iSize-1)
        plotCoords(index) = normalizeCoord(0, iSize-1)
        plotCoords(index+1) = matrix(i)(j)
        plotCoords(index+2) = normalizeCoord(j, jSize-1)
        j += 1
      }
      i += 1
    }
    plotCoords
  }

  def makeColorArray(matrix: Array[Array[Float]]): Array[Float] = {
    Log.e(TAG, "makeColorArray")
    val iSize = matrix.length
    val jSize = matrix(0).length
    val plotColours: Array[Float] = new Array[Float](iSize*jSize*4)
    var i = 0
    while (i < iSize) {
      var j = 0
      while (j < jSize) {
        val indexCol = (i*jSize+j)*4
        val color = Color.HSVToColor(Array[Float]((((((matrix(i)(j))+2.0)/4.0)*359.0)%359.0).toFloat, 1.0f, 1.0f))
        val red = Color.red(color).toFloat/255f
        val green = Color.green(color).toFloat/255f
        val blue = Color.blue(color).toFloat/255f
        plotColours(indexCol) = red
        plotColours(indexCol+1) = green
        plotColours(indexCol+2) = blue
        plotColours(indexCol+3) = 1f
        j += 1
      }
      i += 1
    }
    plotColours
  }

  def makeLineIndexArray(iSize: Int, jSize: Int): Array[Short] = {
    Log.e(TAG, "makeLineIndexArray: "+iSize+" , "+jSize)
    val plotIndices: Array[Short] = new Array[Short]((iSize)*(jSize)*2)
    var index2 = 0
    var i = 0
    while (i < iSize) {
      var j = 0
      while (j < jSize-1) {
        val index = (i*(jSize)+j)*2
        plotIndices(index) = index2.toShort
        plotIndices(index+1) = (index2+1).toShort
        index2 += 1
        j += 1
      }
      index2 += 1
      i += 1
    }
    plotIndices
  }

  def init4(_funcAry: Array[Array[Double]]) {
    Log.e(TAG, "init4 Start")
    funcAry = _funcAry
    Matrix.setIdentityM(mMMatrix, 0)
    Matrix.rotateM(mMMatrix, 0, -90, 0, 1, 0)

    val iSize: Int = funcAry.length
    val jSize: Int = funcAry(0).length
    
    val normMatrix: Array[Array[Float]] = normalizeMatrix(funcAry)
    val plotCoords: Array[Float] = makeCoordsArray(normMatrix)
    val plotColours: Array[Float] = makeColorArray(normMatrix)
    val plotIndices: Array[Short] = makeLineIndexArray(iSize, jSize)

    plotVertBuffer = makeFloatBuffer(plotCoords)
    plotColourBuffer = makeFloatBuffer(plotColours)
    plotIndexBuffer = makeShortBuffer(plotIndices)
    indexLength = plotIndices.length

    Log.e(TAG, "init4 End")
  }

  def initView() {
    Log.e(TAG, "initView")
    Matrix.setIdentityM(mMMatrix, 0)
    Matrix.rotateM(mMMatrix, 0, -90, 0, 1, 0)
  }

  override def onSurfaceCreated(glUnused: GL10, config: EGLConfig) {
    Log.e(TAG, "onSurfaceCreated")

    GLES20.glClearColor(0.0f, 0.0f, 0.0f, 1)
    GLES20.glEnable(GLES20.GL_DEPTH_TEST)
    GLES20.glDepthFunc(GLES20.GL_LEQUAL)

    //Log.e(TAG, "initShapes3 Start")
    val iSize: Int = funcAry.length
    val jSize: Int = funcAry(0).length
    
    val normMatrix: Array[Array[Float]] = normalizeMatrix(funcAry)
    val plotCoords: Array[Float] = makeCoordsArray(normMatrix)
    val plotColours: Array[Float] = makeColorArray(normMatrix)
    val plotIndices: Array[Short] = makeLineIndexArray(iSize, jSize)

    plotVertBuffer = makeFloatBuffer(plotCoords)
    plotColourBuffer = makeFloatBuffer(plotColours)
    plotIndexBuffer = makeShortBuffer(plotIndices)
    indexLength = plotIndices.length

    //Log.e(TAG, "initShapes3 End")
    val vertexShader = loadShader(GLES20.GL_VERTEX_SHADER, vertexShaderCode)
    val fragmentShader = loadShader(GLES20.GL_FRAGMENT_SHADER, fragmentShaderCode)

    mProgram = GLES20.glCreateProgram()
    GLES20.glAttachShader(mProgram, vertexShader)
    GLES20.glAttachShader(mProgram, fragmentShader)
    GLES20.glLinkProgram(mProgram)

    maPositionHandle = GLES20.glGetAttribLocation(mProgram, "vPosition")
    maColourHandle = GLES20.glGetAttribLocation(mProgram, "vColour")
    muMVPMatrixHandle = GLES20.glGetUniformLocation(mProgram, "uMVPMatrix")

    Matrix.setIdentityM(mMMatrix, 0)
    Matrix.rotateM(mMMatrix, 0, -45, 1, 0, -1)

    initView
  }

  override def onSurfaceChanged(glUnused: GL10, width: Int, height: Int) {
    Log.e(TAG, "onSurfaceChanged")
    GLES20.glViewport(0, 0, width, height)

    val ratio: Float = width.toFloat / height.toFloat

    //Matrix.frustumM(mProjMatrix, 0, -ratio, ratio, -1, 1, 1, 10)
    Matrix.orthoM(mProjMatrix, 0, -2.1f, 2.1f, -2.1f, 2.1f, 1, 10)
    //Matrix.setLookAtM(mVMatrix, 0, 0.0f, 0.0f, -5.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)
    Matrix.setLookAtM(mVMatrix, 0, 0.0f, 0.0f, -5.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f)
  }

  override def onDrawFrame(gl: GL10) {
    //Log.e(TAG, "onDrawFrame")
    GLES20.glClear(GLES20.GL_COLOR_BUFFER_BIT | GLES20.GL_DEPTH_BUFFER_BIT)
    GLES20.glUseProgram(mProgram)
    GLES20.glVertexAttribPointer(maPositionHandle, 3, GLES20.GL_FLOAT, false, 0, plotVertBuffer)
    GLES20.glEnableVertexAttribArray(maPositionHandle)
    GLES20.glVertexAttribPointer(maColourHandle, 4, GLES20.GL_FLOAT, false, 0, plotColourBuffer)
    GLES20.glEnableVertexAttribArray(maColourHandle)

    //Set up MVP
    Matrix.setIdentityM(mMVPMatrix, 0)
    Matrix.multiplyMM(mMVPMatrix, 0, mMMatrix, 0, mMVPMatrix, 0)
    Matrix.multiplyMM(mMVPMatrix, 0, mVMatrix, 0, mMVPMatrix, 0)
    Matrix.multiplyMM(mMVPMatrix, 0, mProjMatrix, 0, mMVPMatrix, 0)
    GLES20.glUniformMatrix4fv(muMVPMatrixHandle, 1, false, mMVPMatrix, 0)
    GLES20.glDrawElements(GLES20.GL_LINES, indexLength, GLES20.GL_UNSIGNED_SHORT, plotIndexBuffer)
  }

}

