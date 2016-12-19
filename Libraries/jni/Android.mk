LOCAL_PATH:= $(call my-dir)/..
include $(CLEAR_VARS)
 
LOCAL_MODULE     := stb-android
LOCAL_C_INCLUDES := ../C
LOCAL_SRC_FILES  := stb.c
 
include $(BUILD_STATIC_LIBRARY)
