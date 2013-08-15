#include <math.h>
#include <stdlib.h>
#include <jni.h>
#include <android/log.h>

#include "mcOptCal-jni.h"


class dgRandom
{
  private:
    long int a;
    long int m;
    long q;
    long r;
    long int seed;
    void init(int _seed);
    static double bsm_a[4];
    static double bsm_b[4];
    static double bsm_c[9];
  public:
    dgRandom(int _seed);
    dgRandom();
    double msrng();
    double bsmInvNormal(double u);
    double msrngInvNormGaussian();
};

dgRandom::dgRandom(int _seed)
{
  init(_seed);
}

dgRandom::dgRandom()
{
  init(rand()); // todo rand()
}

void dgRandom::init(int _seed)
{
  a = 16807;
  m = 2147483647;
  q = (m / a);
  r = (m % a);
  seed = _seed;
}

/**
 * Minimal Standard Random Number Generator
 * Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
 */
double dgRandom::msrng()
{
  long int hi = seed / q;
  long int lo = seed % q;
  long int test = a * lo - r * hi;
  if(test > 0)
    seed = test;
  else
    seed = test + m;
  return static_cast<double>(seed) / m;
}

double dgRandom::bsm_a[] = 
{
  2.50662823884,
  -18.61500062529,
  41.39119773534,
  -25.44106049637
};

double dgRandom::bsm_b[] =
{
  -8.47351093090,
  23.08336743743,
  -21.06224101826,
  3.13082909833
};

double dgRandom::bsm_c[] =
{
  0.3374754822726147,
  0.9761690190917186,
  0.1607979714918209,
  0.0276438810333863,
  0.0038405729373609,
  0.0003951896511919,
  0.0000321767881768,
  0.0000002888167364,
  0.0000003960315187
};

/**
 * Beasley-Springer-Moro algorithm for approximating the inverse normal
 * Ref: "Monte Carlo Methods in Financial Engineering", Glasserman
 */
double dgRandom::bsmInvNormal(double u)
{
  double x, y, r;
  y = u - 0.5D;
  if (fabs(y) < 0.42D)
  {
    r = y*y;
    x = y*(((bsm_a[3]*r + bsm_a[2])*r + bsm_a[1])*r + bsm_a[0])/ ((((bsm_b[3]*r + bsm_b[2])*r + bsm_b[1])*r + bsm_b[0])*r + 1.0D);
  } else {
    r = u;
    if (y > 0.0D)
      r = 1.0D - u;
    r = log(-log(r));
    x = bsm_c[0] + r*(bsm_c[1] + r*(bsm_c[2] + r*(bsm_c[3] + r*(bsm_c[4]+ r*(bsm_c[5] + r*(bsm_c[6] + r*(bsm_c[7] + r*bsm_c[8])))))));
    if (y < 0.0D)
      x = -x;
  }
  return(x);
}

double dgRandom::msrngInvNormGaussian()
{
  return bsmInvNormal(msrng());
}

double payOffFn(double savg, double strike)
{
  return (savg - strike);
}

struct mcResult 
{
  double optionValue;
  double stdErr;
};

mcResult calcAsianOptionValue(
    int numPaths,
    int expiry,
    int numSteps,
    double stock,
    double strike,
    double rate,
    double volatility,
    int uiUpdateInterval,
    double dT)
{
  dgRandom dgrng(1);

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
      //dZ = env->CallDoubleMethod(rngObject, rngId);
      dZ = dgrng.msrngInvNormGaussian();
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

  mcResult result = 
  {
    optionValue,
    stdErr
  };

  return result;
}

extern "C" {

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
      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","seed = %d \n", seed);

      mcResult result = calcAsianOptionValue(
	  numPaths,
	  expiry,
	  numSteps,
	  stock,
	  strike,
	  rate,
	  volatility,
	  uiUpdateInterval,
	  dT);

      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","optionValue= %lf \n", optionValue);
      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","stdErr= %lf \n", stdErr);

      jdoubleArray resultArray = env->NewDoubleArray(2);
      jdouble *rAry = env->GetDoubleArrayElements(resultArray, NULL);
      rAry[0] = result.optionValue;
      rAry[1] = result.stdErr;

      return resultArray;
    }

}
