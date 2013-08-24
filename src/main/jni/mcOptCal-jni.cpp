#include <queue>
#include <stack>
#include <string>
#include <math.h>
#include <stdlib.h>
#include <jni.h>
#include <time.h>
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
    bool antithetic;
    bool hasGaussianCache;
    double gaussianCache;
  public:
    dgRandom(int _seed, bool _antithetic);
    dgRandom(int _seed);
    dgRandom();
    double msrng();
    double bsmInvNormal(double u);
    double msrngInvNormGaussian();
    void setSeed(long int _seed);
    void setAntithetic(bool _antithetic);
};

dgRandom::dgRandom(int _seed, bool _antithetic)
{
  init(_seed);
  antithetic = _antithetic;
  hasGaussianCache = false;
}

dgRandom::dgRandom(int _seed)
{
  init(_seed);
  antithetic = false;
  hasGaussianCache = false;
}

dgRandom::dgRandom()
{
  init(rand()); // todo rand()
  antithetic = false;
  hasGaussianCache = false;
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
  /*if (antithetic) {
    if (hasGaussianCache) {
      hasGaussianCache = false;
      return gaussianCache;
    } else {
      double newGaussian = bsmInvNormal(msrng());
      gaussianCache = -newGaussian;
      hasGaussianCache = true;
      return newGaussian;
    }
  } else*/
  return bsmInvNormal(msrng());
}

void dgRandom::setSeed(long int _seed)
{
   seed = _seed;
}

void dgRandom::setAntithetic(bool _antithetic)
{
   antithetic = _antithetic;
}

class reporter
{
  private:
    JNIEnv *env;
    jobject calcObject;
    jmethodID jniUpdateUIID;
  public:
    reporter(JNIEnv *env, jobject calcObject, jmethodID jniUpdateUIID);
    bool reportAndPoll(jstring msg, int count, int total, timespec *tstart, timespec *tnow);
};

reporter::reporter(JNIEnv *e, jobject c, jmethodID j)
{
  env = e;
  calcObject = c;
  jniUpdateUIID = j;
}

bool reporter::reportAndPoll(jstring msg, int count, int total, timespec *tstart, timespec *tnow)
{
  bool abort = false;
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, tnow);
  if (((tnow->tv_sec != tstart->tv_sec) || ((tnow->tv_nsec - tstart->tv_nsec)>200000000))) {
    abort = env->CallBooleanMethod(calcObject, jniUpdateUIID, msg, count, total);
    clock_gettime(CLOCK_THREAD_CPUTIME_ID, tstart);
  }
}


double payOffFnParse(JNIEnv *env, const jstring& exprStr)
{
  using namespace std;

  __android_log_print(ANDROID_LOG_INFO,"ASIAN","payOffFnParse_start");
  queue<jchar> outputQueue;
  stack<jchar> operatorStack;

  int i = 0;
  int length = env->GetStringLength(exprStr);

  const jchar* chars = env->GetStringChars(exprStr, NULL);
  while (i < length) {
    char token = chars[i];
    switch (token)
    {
      case '1':
	outputQueue.push(token);
	break;
      case '+':
	operatorStack.push(token);
	break;
    }
    i++;
  }
  while (!operatorStack.empty()) {
    char op = operatorStack.top();
    operatorStack.pop();
    outputQueue.push(op);
  }

  env->ReleaseStringChars(exprStr, chars);

  stack<int> rpnStack;
  while (!outputQueue.empty()) {
    char token = outputQueue.front();
    outputQueue.pop();
    __android_log_print(ANDROID_LOG_INFO,"ASIAN"," %c", token);
    switch (token)
    {
      case '1':
	rpnStack.push(1);
	break;
      case '+':
	int val_1 = rpnStack.top();
	rpnStack.pop();
	int val_2 = rpnStack.top();
	rpnStack.pop();
	rpnStack.push(val_1+val_2);
	break;
    }
  }
  int result = rpnStack.top();
  rpnStack.pop();
  __android_log_print(ANDROID_LOG_INFO,"ASIAN","result = %d", result);

  __android_log_print(ANDROID_LOG_INFO,"ASIAN","payOffFnParse_end");

  return 0.1D;
}

double payOffFn(double savg, double strike)
{
  return (savg - strike);
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
double rpnPayOffFn(double savg, std::deque<std::string> rpnPostfixDeque )
{
  //std::queue<std::string> postfixQueue = rpnPostfixDeque;
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
    reporter r,
    jstring msg,
    std::deque<std::string> rpnPostfixDeque )
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
  int total = static_cast<int>(numPaths/2);
  clock_gettime(CLOCK_THREAD_CPUTIME_ID, &tstart);
  while (i < total)
  {
    s1 = stock;
    s2 = stock;
    avgX = 0.0D;
    avgY = 0.0D;

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
      s2 = s2*exp_a/exp_bdZ; // antithetic path

      avgX = avgX + s1/static_cast<double>(numSteps);
      avgY = avgY + s2/static_cast<double>(numSteps);

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
	//postfixQueue.push(token);
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

      reporter r(env, calcObject, jniUpdateUIID);
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

      //jstring expr = env->NewStringUTF("1+1+1");
      //payOffFnParse(env, expr);
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
	  dT,
	  seed,
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
