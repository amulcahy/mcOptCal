LOCAL_PATH := $(call my-dir)
include $(CLEAR_VARS)
LOCAL_MODULE := mcOptCal-jni
LOCAL_SRC_FILES := mcOptCal-jni.cpp
#LOCAL_C_INCLUDES += $(SBT_MANAGED_JNI_INCLUDE)
LOCAL_LDLIBS := -L$(SYSROOT)/usr/lib -llog
include $(BUILD_SHARED_LIBRARY)
