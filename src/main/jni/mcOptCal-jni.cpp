#include <deque>
#include <stack>
#include <string>
#include <cmath>
#include <stdlib.h>
#include <jni.h>
#include <time.h>
#include <android/log.h>

#include "mcOptCal-jni.h"

/**
 * Minimal Standard Random Number Generator
 * Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
 *
 * Beasley-Springer-Moro algorithm for approximating the inverse normal
 * Ref: "Monte Carlo Methods in Financial Engineering", Glasserman
 */
class dgRandom
{
  private:
    long int a;
    long int m;
    long q;
    long r;
    long int rngSeed;
    void init(int seed);
    static double bsm_a[4];
    static double bsm_b[4];
    static double bsm_c[9];
  public:
    dgRandom(int seed);
    dgRandom();
    double msrng();
    double bsmInvNormal(double u);
    double msrngInvNormGaussian();
    void setSeed(long int seed);
};

class reporter
{
  private:
    JNIEnv *env;
    jobject calcObject;
    jmethodID jniUpdateUIID;
    int uiUpdateInterval;
  public:
    reporter(JNIEnv *env, jobject calcObject, jmethodID jniUpdateUIID, int uiUpdateInterval);
    bool reportAndPoll(jstring msg, int count, int total, timespec *tstart, timespec *tnow);
};

bool isSymbol(std::string token);
double getNumber(std::string token);
bool isAddOp(std::string token);
bool isSubOp(std::string token);
bool isOperator(std::string token);
double rpnPayOffFn(double savg, std::deque<std::string> rpnPostfixDeque);

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
    double dT,
    long int seed,
    bool antithetic,
    reporter r,
    jstring msg,
    std::deque<std::string> rpnPostfixDeque);


/**********************************************************************/
dgRandom::dgRandom(int seed)
{
  init(seed);
}

dgRandom::dgRandom()
{
  init(rand()); // todo rand()
}

void dgRandom::init(int seed)
{
  a = 16807;
  m = 2147483647;
  q = (m / a);
  r = (m % a);
  rngSeed = seed;
}

/**
 * Minimal Standard Random Number Generator
 * Ref: "Random Number Generators: Good Ones Are Hard To Find", Park & Miller
 */
double dgRandom::msrng()
{
  long int hi = rngSeed / q;
  long int lo = rngSeed % q;
  long int test = a * lo - r * hi;
  if(test > 0)
    rngSeed = test;
  else
    rngSeed = test + m;
  return static_cast<double>(rngSeed) / m;
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

void dgRandom::setSeed(long int seed)
{
   rngSeed = seed;
}

reporter::reporter(JNIEnv *e, jobject c, jmethodID j, int u)
{
  env = e;
  calcObject = c;
  jniUpdateUIID = j;
  uiUpdateInterval = u;
}

bool reporter::reportAndPoll(jstring msg, int count, int total, timespec *tstart, timespec *tnow)
{
  bool abort = false;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, tnow);
  if (((tnow->tv_sec != tstart->tv_sec) || ((tnow->tv_nsec - tstart->tv_nsec) > uiUpdateInterval))) {
    abort = env->CallBooleanMethod(calcObject, jniUpdateUIID, msg, count, total);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, tstart);
  }
}


bool isSymbol(std::string token)
{
  return (token.compare("SAVG") == 0) || (token.compare("S") == 0);
}

double getNumber(std::string token)
{
  return atof(token.c_str()); // todo error check
}

bool isAddOp(std::string token)
{
  return (token.compare("+") == 0);
}

bool isSubOp(std::string token)
{
  return (token.compare("-") == 0);
}

bool isOperator(std::string token)
{
  return (token.compare("+") == 0) || (token.compare("-") == 0);
}

// ref: http://en.wikipedia.org/wiki/Reverse_Polish_notation
double rpnPayOffFn(double savg, std::deque<std::string> rpnPostfixDeque)
{
  std::stack<double> valueStack;
  bool hasError = false;
  double payOff = NAN;
  int i = 0;
  while ((i < rpnPostfixDeque.size()) && !hasError) {
    std::string token = rpnPostfixDeque[i];
    if (isSymbol(token)) {
      valueStack.push(savg);
    } else if (isOperator(token)) {
      if (isAddOp(token)) {
	if (valueStack.size() < 2)
	  hasError = true;
	double val_1 = valueStack.top();
	valueStack.pop();
	double val_2 = valueStack.top();
	valueStack.pop();
	valueStack.push(val_1+val_2);
      } else if (isSubOp(token)) {
	if (valueStack.size() < 2)
	  hasError = true;
	double val_1 = valueStack.top();
	valueStack.pop();
	double val_2 = valueStack.top();
	valueStack.pop();
	valueStack.push(val_1-val_2);
      }
    } else { // todo verify that it is a number
      double value = atof(token.c_str());
      valueStack.push(value);
    }
    i++;
  }
  if ((valueStack.size() == 1) && !hasError)
    payOff = valueStack.top();

  return payOff;
}

mcResult calcAsianOptionValue(
    int numPaths,
    int expiry,
    int numSteps,
    double stock,
    double strike,
    double rate,
    double volatility,
    int uiUpdateInterval,
    double dT,
    long int seed,
    bool antithetic,
    reporter r,
    jstring msg,
    std::deque<std::string> rpnPostfixDeque)
{
  dgRandom dgrng(seed);

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

  bool abort = false;
  struct timespec tstart, tnow;
  int total;
  if (antithetic)
    total = static_cast<int>(numPaths/2);
  else
    total = numPaths;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tstart);
  while (i < total)
  {
    s1 = stock;
    avgX = 0.0D;
    if (antithetic) {
      s2 = stock;
      avgY = 0.0D;
    }

    int j = 0;
    abort = r.reportAndPoll(msg, (total-i), total, &tstart, &tnow);
    if (abort) {
      mcResult result = 
      {
	0.0D,
	0.0D
      };
      return result;
    }
    while (j < numSteps)
    {
      dZ = dgrng.msrngInvNormGaussian();
      exp_bdZ = exp(b*dZ);
      s1 = s1*exp_a*exp_bdZ;
      avgX = avgX + s1/static_cast<double>(numSteps);

      if (antithetic) {
	s2 = s2*exp_a/exp_bdZ; // antithetic path
	avgY = avgY + s2/static_cast<double>(numSteps);
      }

      j++;
    }

    payOffX = rpnPayOffFn(avgX, rpnPostfixDeque);
    if (payOffX > 0.0D)
      x = payOffX*pvDiscount;
    else
      x = 0.0D;

    sum = sum + x;
    n += 1;
    deltaX = x - mean;
    mean += deltaX/static_cast<double>(n);
    m2 += deltaX*(x - mean);

    if (antithetic) {
      payOffY = rpnPayOffFn(avgY, rpnPostfixDeque);
      if (payOffY > 0.0D)
	y = payOffY*pvDiscount;
      else
	y = 0.0D;

      sum = sum + y;
      n += 1;
      deltaY = y - mean;
      mean += deltaY/static_cast<double>(n);
      m2 += deltaY*(y - mean);
    }

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
   * Signature: (Lcom/dragongate_technologies/mcOptCal/Calc;Lcom/dragongate_technologies/mcOptCal/LsmParams;Lscala/util/Random;[Ljava/lang/String;)[D
   */
  JNIEXPORT jdoubleArray JNICALL Java_com_dragongate_1technologies_mcOptCal_lsm_calcAsianOptionValueJNI
    (JNIEnv *env, jobject obj, jobject calcObject, jobject paramsObject, jobject rngObject, jobjectArray rpnAry)
    {
      __android_log_print(ANDROID_LOG_INFO,"ASIAN","Java_com_dragongate_1technologies_mcOptCal_lsm_calcAsianOptionValueJNI");

      jsize rpnAryLength = env->GetArrayLength(rpnAry);
      std::deque<std::string> postfixDeque;
      int i=rpnAryLength;
      while (i > 0) {
	i--;

        jstring jniString = (jstring)env->GetObjectArrayElement(rpnAry, i);
	const jsize strLength = env->GetStringUTFLength(jniString);
	const char* strChars = env->GetStringUTFChars(jniString, (jboolean *)0);
	std::string token(strChars, strLength);
	env->ReleaseStringUTFChars(jniString, strChars);

	postfixDeque.push_back(token);
	__android_log_print(ANDROID_LOG_INFO,"ASIAN","rpnTokenString = %s", token.c_str());
      }

      jclass calcClass = env->GetObjectClass(calcObject);
      jclass paramsClass = env->GetObjectClass(paramsObject);
      jclass rngClass = env->GetObjectClass(rngObject);
      //jmethodID rngId = env->GetMethodID(rngClass, "nextGaussian", "()D");
      jmethodID jniUpdateUIID = env->GetMethodID(calcClass, "jniUpdateUI", "(Ljava/lang/String;II)Z");

      jstring msg = env->NewStringUTF("msg from JNI"); //todo

      jfieldID numPathsId = env->GetFieldID(paramsClass, "numPaths", "I");
      jfieldID expiryId = env->GetFieldID(paramsClass, "expiry", "I");
      jfieldID numStepsId = env->GetFieldID(paramsClass, "numSteps", "I");
      jfieldID stockId = env->GetFieldID(paramsClass, "stock", "D");
      jfieldID strikeId = env->GetFieldID(paramsClass, "strike", "D");
      jfieldID rateId = env->GetFieldID(paramsClass, "rate", "D");
      jfieldID volatilityId = env->GetFieldID(paramsClass, "volatility", "D");
      jfieldID uiUpdateIntervalId = env->GetFieldID(paramsClass, "uiUpdateInterval", "I");
      jfieldID dTId = env->GetFieldID(paramsClass, "dT", "D");
      jfieldID seedId = env->GetFieldID(paramsClass, "rngSeed", "I");
      jfieldID antitheticId = env->GetFieldID(paramsClass, "antithetic", "Z");

      int numPaths = env->GetIntField(paramsObject, numPathsId);
      int expiry = env->GetIntField(paramsObject, expiryId);
      int numSteps = env->GetIntField(paramsObject, numStepsId);
      double stock = env->GetDoubleField(paramsObject, stockId);
      double strike = env->GetDoubleField(paramsObject, strikeId);
      double rate = env->GetDoubleField(paramsObject, rateId);
      double volatility = env->GetDoubleField(paramsObject, volatilityId);
      int uiUpdateInterval = env->GetIntField(paramsObject, uiUpdateIntervalId);
      double dT = env->GetDoubleField(paramsObject, dTId);
      long int seed = env->GetIntField(paramsObject, seedId);
      bool antithetic = env->GetBooleanField(paramsObject, antitheticId);

      reporter r(env, calcObject, jniUpdateUIID, uiUpdateInterval*1000000);

      mcResult result = calcAsianOptionValue(
	  numPaths,
	  expiry,
	  numSteps,
	  stock,
	  strike,
	  rate,
	  volatility,
	  uiUpdateInterval,
	  dT,
	  seed,
	  antithetic,
	  r,
	  msg,
	  postfixDeque );

      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","optionValue= %lf \n", optionValue);
      //__android_log_print(ANDROID_LOG_INFO,"ASIAN","stdErr= %lf \n", stdErr);

      jdoubleArray resultArray = env->NewDoubleArray(2);
      jdouble *rAry = env->GetDoubleArrayElements(resultArray, NULL);
      rAry[0] = result.optionValue;
      rAry[1] = result.stdErr;

      return resultArray;
    }

}
