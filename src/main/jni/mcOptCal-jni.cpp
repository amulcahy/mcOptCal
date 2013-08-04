#include <math.h>
#include <stdlib.h>
#include <jni.h>
//#include <android/log.h>

#include "mcOptCal-jni.h"

extern "C" {

  double payOffFn(double savg, double strike)
  {
    return (savg - strike);
  }

  double randGaussianApproximationBySummation()
  {
    double result = 0.0D;

    int i = 0;
    while (i <12)
    {
      result += rand() / static_cast<double>(RAND_MAX);
      i++;
    }
    result -= 6.0D;

    return result;
  }

  /*
   * Class:     com_dragongate_technologies_mcOptCal_lsm
   * Method:    calcAsianOptionValueJNI
   * Signature: (Lcom/dragongate_technologies/mcOptCal/LsmParams;Lscala/util/Random;)[D
   */
  JNIEXPORT jdoubleArray JNICALL Java_com_dragongate_1technologies_mcOptCal_lsm_calcAsianOptionValueJNI
    (JNIEnv *env, jobject callingObject, jobject paramsObject, jobject rngObject)
    {
      jclass paramsClass = env->GetObjectClass(paramsObject);
      jclass rngClass = env->GetObjectClass(rngObject);
      jmethodID rngId = env->GetMethodID(rngClass, "nextGaussian", "()D");

      jfieldID numPathsId = env->GetFieldID(paramsClass, "numPaths", "I");
      jfieldID expiryId = env->GetFieldID(paramsClass, "expiry", "I");
      jfieldID numStepsId = env->GetFieldID(paramsClass, "numSteps", "I");
      jfieldID stockId = env->GetFieldID(paramsClass, "stock", "D");
      jfieldID strikeId = env->GetFieldID(paramsClass, "strike", "D");
      jfieldID rateId = env->GetFieldID(paramsClass, "rate", "D");
      jfieldID volatilityId = env->GetFieldID(paramsClass, "volatility", "D");
      jfieldID uiUpdateIntervalId = env->GetFieldID(paramsClass, "uiUpdateInterval", "I");

      jfieldID dTId = env->GetFieldID(paramsClass, "dT", "D");

      int numPaths = env->GetIntField(paramsObject, numPathsId);
      int expiry = env->GetIntField(paramsObject, expiryId);
      int numSteps = env->GetIntField(paramsObject, numStepsId);
      double stock = env->GetDoubleField(paramsObject, stockId);
      double strike = env->GetDoubleField(paramsObject, strikeId);
      double rate = env->GetDoubleField(paramsObject, rateId);
      double volatility = env->GetDoubleField(paramsObject, volatilityId);
      int uiUpdateInterval = env->GetIntField(paramsObject, uiUpdateIntervalId);

      double dT = env->GetDoubleField(paramsObject, dTId);
      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","dT= %lf \n", dT);

      double a = (rate - volatility*volatility*0.5D)*dT;
      double b = volatility * sqrt(dT);
      int i = 0;
      int n = 0;
      double mean = 0.0D;
      double m2 = 0.0D;
      double sum = 0.0D;
      double x = 0.0D;
      double y = 0.0D;
      double pvDiscount = exp(-rate*expiry);
      double exp_a = exp(a);

      double dZ;
      double exp_bdZ;
      double s1, s2, avgX, avgY, payOffX, payOffY, deltaX, deltaY;
      while (i < numPaths/2)
      {
	s1 = stock;
	s2 = stock;
	avgX = 0.0D;
	avgY = 0.0D;

	int j = 0;
	while (j < numSteps)
	{
	  dZ = env->CallDoubleMethod(rngObject, rngId);
	  //dZ = randGaussianApproximationBySummation();
	  exp_bdZ = exp(b*dZ);
	  s1 = s1*exp_a*exp_bdZ;
	  s2 = s2*exp_a/exp_bdZ; // antithetic path

	  avgX = avgX + s1/static_cast<double>(numSteps);
	  avgY = avgY + s2/static_cast<double>(numSteps);

	  j++;
	}
	payOffX = payOffFn(avgX, strike);
	if (payOffX > 0.0D)
	  x = payOffX*pvDiscount;
	else
	  x = 0.0D;

	payOffY = payOffFn(avgY, strike);
	if (payOffY > 0.0D)
	  y = payOffY*pvDiscount;
	else
	  y = 0.0D;

	sum = sum + x;
	n += 1;
	deltaX = x - mean;
	mean += deltaX/static_cast<double>(n);
	m2 += deltaX*(x - mean);

	sum = sum + y;
	n += 1;
	deltaY = y - mean;
	mean += deltaY/static_cast<double>(n);
	m2 += deltaY*(y - mean);

	i++;
      }

      double optionValue = sum / static_cast<double>(numPaths);
      double variance = m2/static_cast<double>(n-1);
      double sampStdDev = sqrt(variance);
      double stdErr = sampStdDev/sqrt(static_cast<double>(numPaths));

      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","optionValue= %lf \n", optionValue);
      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","stdErr= %lf \n", stdErr);

      jdoubleArray resultArray = env->NewDoubleArray(2);
      jdouble *rAry = env->GetDoubleArrayElements(resultArray, NULL);
      rAry[0] = optionValue;
      rAry[1] = stdErr;

      return resultArray;
    }

}
