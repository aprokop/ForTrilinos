/* ----------------------------------------------------------------------------
 * This file was automatically generated by SWIG (http://www.swig.org).
 * Version 4.0.0
 *
 * This file is not intended to be easily readable and contains a number of
 * coding conventions designed to improve portability and efficiency. Do not make
 * changes to this file unless you know what you are doing--modify the SWIG
 * interface file instead.
 * ----------------------------------------------------------------------------- */

/*
 * Copyright 2017-2018, UT-Battelle, LLC
 *
 * SPDX-License-Identifier: BSD-3-Clause
 * License-Filename: LICENSE
 */


#ifdef __cplusplus
/* SwigValueWrapper is described in swig.swg */
template<typename T> class SwigValueWrapper {
  struct SwigMovePointer {
    T *ptr;
    SwigMovePointer(T *p) : ptr(p) { }
    ~SwigMovePointer() { delete ptr; }
    SwigMovePointer& operator=(SwigMovePointer& rhs) { T* oldptr = ptr; ptr = 0; delete oldptr; ptr = rhs.ptr; rhs.ptr = 0; return *this; }
  } pointer;
  SwigValueWrapper& operator=(const SwigValueWrapper<T>& rhs);
  SwigValueWrapper(const SwigValueWrapper<T>& rhs);
public:
  SwigValueWrapper() : pointer(0) { }
  SwigValueWrapper& operator=(const T& t) { SwigMovePointer tmp(new T(t)); pointer = tmp; return *this; }
  operator T&() const { return *pointer.ptr; }
  T *operator&() { return pointer.ptr; }
};

template <typename T> T SwigValueInit() {
  return T();
}
#endif

/* -----------------------------------------------------------------------------
 *  This section contains generic SWIG labels for method/variable
 *  declarations/attributes, and other compiler dependent labels.
 * ----------------------------------------------------------------------------- */

/* template workaround for compilers that cannot correctly implement the C++ standard */
#ifndef SWIGTEMPLATEDISAMBIGUATOR
# if defined(__SUNPRO_CC) && (__SUNPRO_CC <= 0x560)
#  define SWIGTEMPLATEDISAMBIGUATOR template
# elif defined(__HP_aCC)
/* Needed even with `aCC -AA' when `aCC -V' reports HP ANSI C++ B3910B A.03.55 */
/* If we find a maximum version that requires this, the test would be __HP_aCC <= 35500 for A.03.55 */
#  define SWIGTEMPLATEDISAMBIGUATOR template
# else
#  define SWIGTEMPLATEDISAMBIGUATOR
# endif
#endif

/* inline attribute */
#ifndef SWIGINLINE
# if defined(__cplusplus) || (defined(__GNUC__) && !defined(__STRICT_ANSI__))
#   define SWIGINLINE inline
# else
#   define SWIGINLINE
# endif
#endif

/* attribute recognised by some compilers to avoid 'unused' warnings */
#ifndef SWIGUNUSED
# if defined(__GNUC__)
#   if !(defined(__cplusplus)) || (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4))
#     define SWIGUNUSED __attribute__ ((__unused__))
#   else
#     define SWIGUNUSED
#   endif
# elif defined(__ICC)
#   define SWIGUNUSED __attribute__ ((__unused__))
# else
#   define SWIGUNUSED
# endif
#endif

#ifndef SWIG_MSC_UNSUPPRESS_4505
# if defined(_MSC_VER)
#   pragma warning(disable : 4505) /* unreferenced local function has been removed */
# endif
#endif

#ifndef SWIGUNUSEDPARM
# ifdef __cplusplus
#   define SWIGUNUSEDPARM(p)
# else
#   define SWIGUNUSEDPARM(p) p SWIGUNUSED
# endif
#endif

/* internal SWIG method */
#ifndef SWIGINTERN
# define SWIGINTERN static SWIGUNUSED
#endif

/* internal inline SWIG method */
#ifndef SWIGINTERNINLINE
# define SWIGINTERNINLINE SWIGINTERN SWIGINLINE
#endif

/* qualifier for exported *const* global data variables*/
#ifndef SWIGEXTERN
# ifdef __cplusplus
#   define SWIGEXTERN extern
# else
#   define SWIGEXTERN
# endif
#endif

/* exporting methods */
#if defined(__GNUC__)
#  if (__GNUC__ >= 4) || (__GNUC__ == 3 && __GNUC_MINOR__ >= 4)
#    ifndef GCC_HASCLASSVISIBILITY
#      define GCC_HASCLASSVISIBILITY
#    endif
#  endif
#endif

#ifndef SWIGEXPORT
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   if defined(STATIC_LINKED)
#     define SWIGEXPORT
#   else
#     define SWIGEXPORT __declspec(dllexport)
#   endif
# else
#   if defined(__GNUC__) && defined(GCC_HASCLASSVISIBILITY)
#     define SWIGEXPORT __attribute__ ((visibility("default")))
#   else
#     define SWIGEXPORT
#   endif
# endif
#endif

/* calling conventions for Windows */
#ifndef SWIGSTDCALL
# if defined(_WIN32) || defined(__WIN32__) || defined(__CYGWIN__)
#   define SWIGSTDCALL __stdcall
# else
#   define SWIGSTDCALL
# endif
#endif

/* Deal with Microsoft's attempt at deprecating C standard runtime functions */
#if !defined(SWIG_NO_CRT_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_CRT_SECURE_NO_DEPRECATE)
# define _CRT_SECURE_NO_DEPRECATE
#endif

/* Deal with Microsoft's attempt at deprecating methods in the standard C++ library */
#if !defined(SWIG_NO_SCL_SECURE_NO_DEPRECATE) && defined(_MSC_VER) && !defined(_SCL_SECURE_NO_DEPRECATE)
# define _SCL_SECURE_NO_DEPRECATE
#endif

/* Deal with Apple's deprecated 'AssertMacros.h' from Carbon-framework */
#if defined(__APPLE__) && !defined(__ASSERT_MACROS_DEFINE_VERSIONS_WITHOUT_UNDERSCORES)
# define __ASSERT_MACROS_DEFINE_VERSIONS_WITHOUT_UNDERSCORES 0
#endif

/* Intel's compiler complains if a variable which was never initialised is
 * cast to void, which is a common idiom which we use to indicate that we
 * are aware a variable isn't used.  So we just silence that warning.
 * See: https://github.com/swig/swig/issues/192 for more discussion.
 */
#ifdef __INTEL_COMPILER
# pragma warning disable 592
#endif

/*  Errors in SWIG */
#define  SWIG_UnknownError    	   -1
#define  SWIG_IOError        	   -2
#define  SWIG_RuntimeError   	   -3
#define  SWIG_IndexError     	   -4
#define  SWIG_TypeError      	   -5
#define  SWIG_DivisionByZero 	   -6
#define  SWIG_OverflowError  	   -7
#define  SWIG_SyntaxError    	   -8
#define  SWIG_ValueError     	   -9
#define  SWIG_SystemError    	   -10
#define  SWIG_AttributeError 	   -11
#define  SWIG_MemoryError    	   -12
#define  SWIG_NullReferenceError   -13




#define SWIG_exception_impl(DECL, CODE, MSG, RETURNNULL) \
 { throw std::logic_error("In " DECL ": " MSG); RETURNNULL; }


extern "C" {
void SWIG_check_unhandled_exception_impl(const char* decl);
void SWIG_store_exception(const char* decl, int errcode, const char *msg);
}


#undef SWIG_exception_impl
#define SWIG_exception_impl(DECL, CODE, MSG, RETURNNULL) \
    SWIG_store_exception(DECL, CODE, MSG); RETURNNULL;


#include <stdexcept>


/* Support for the `contract` feature.
 *
 * Note that RETURNNULL is first because it's inserted via a 'Replaceall' in
 * the fortran.cxx file.
 */
#define SWIG_contract_assert(RETURNNULL, EXPR, MSG) \
 if (!(EXPR)) { SWIG_exception_impl("$decl", SWIG_ValueError, MSG, RETURNNULL); } 


#define SWIGVERSION 0x040000 
#define SWIG_VERSION SWIGVERSION


#define SWIG_as_voidptr(a) const_cast< void * >(static_cast< const void * >(a)) 
#define SWIG_as_voidptrptr(a) ((void)SWIG_as_voidptr(*a),reinterpret_cast< void** >(a)) 


#include <string>


#include "BelosTypes.hpp"


#include <stdlib.h>
#ifdef _MSC_VER
# ifndef strtoull
#  define strtoull _strtoui64
# endif
# ifndef strtoll
#  define strtoll _strtoi64
# endif
#endif


struct SwigArrayWrapper {
    void* data;
    size_t size;
};


SWIGINTERN SwigArrayWrapper SwigArrayWrapper_uninitialized() {
  SwigArrayWrapper result;
  result.data = NULL;
  result.size = 0;
  return result;
}


#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif
SWIGEXPORT SwigArrayWrapper _wrap_convertReturnTypeToString(int const *farg1) {
  SwigArrayWrapper fresult ;
  Belos::ReturnType arg1 ;
  std::string result;
  
  arg1 = static_cast< Belos::ReturnType >(*farg1);
  result = Belos::convertReturnTypeToString(arg1);
  fresult.size = (&result)->size();
  if (fresult.size > 0) {
    fresult.data = malloc(fresult.size);
    memcpy(fresult.data, (&result)->c_str(), fresult.size);
  } else {
    fresult.data = NULL;
  }
  return fresult;
}


SWIGEXPORT SWIGEXTERN int const _wrap_BelosPassed = static_cast< int >(Belos::Passed);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosFailed = static_cast< int >(Belos::Failed);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosUndefined = static_cast< int >(Belos::Undefined);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosProblem = static_cast< int >(Belos::Problem);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosRecycleSubspace = static_cast< int >(Belos::RecycleSubspace);

SWIGEXPORT SwigArrayWrapper _wrap_convertStatusTypeToString(int const *farg1) {
  SwigArrayWrapper fresult ;
  Belos::StatusType arg1 ;
  std::string result;
  
  arg1 = static_cast< Belos::StatusType >(*farg1);
  result = Belos::convertStatusTypeToString(arg1);
  fresult.size = (&result)->size();
  if (fresult.size > 0) {
    fresult.data = malloc(fresult.size);
    memcpy(fresult.data, (&result)->c_str(), fresult.size);
  } else {
    fresult.data = NULL;
  }
  return fresult;
}


SWIGEXPORT int _wrap_convertStringToStatusType(SwigArrayWrapper *farg1) {
  int fresult ;
  std::string *arg1 = 0 ;
  std::string tempstr1 ;
  Belos::StatusType result;
  
  tempstr1 = std::string(static_cast<const char *>(farg1->data), farg1->size);
  arg1 = &tempstr1;
  result = (Belos::StatusType)Belos::convertStringToStatusType((std::string const &)*arg1);
  fresult = static_cast< int >(result);
  return fresult;
}


SWIGEXPORT int _wrap_convertStringToScaleType(SwigArrayWrapper *farg1) {
  int fresult ;
  std::string *arg1 = 0 ;
  std::string tempstr1 ;
  Belos::ScaleType result;
  
  tempstr1 = std::string(static_cast<const char *>(farg1->data), farg1->size);
  arg1 = &tempstr1;
  result = (Belos::ScaleType)Belos::convertStringToScaleType((std::string const &)*arg1);
  fresult = static_cast< int >(result);
  return fresult;
}


SWIGEXPORT SwigArrayWrapper _wrap_convertScaleTypeToString(int const *farg1) {
  SwigArrayWrapper fresult ;
  Belos::ScaleType arg1 ;
  std::string result;
  
  arg1 = static_cast< Belos::ScaleType >(*farg1);
  result = Belos::convertScaleTypeToString(arg1);
  fresult.size = (&result)->size();
  if (fresult.size > 0) {
    fresult.data = malloc(fresult.size);
    memcpy(fresult.data, (&result)->c_str(), fresult.size);
  } else {
    fresult.data = NULL;
  }
  return fresult;
}


SWIGEXPORT SWIGEXTERN int const _wrap_BelosErrors = static_cast< int >(Belos::Errors);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosWarnings = static_cast< int >(Belos::Warnings);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosIterationDetails = static_cast< int >(Belos::IterationDetails);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosOrthoDetails = static_cast< int >(Belos::OrthoDetails);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosFinalSummary = static_cast< int >(Belos::FinalSummary);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosTimingDetails = static_cast< int >(Belos::TimingDetails);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosStatusTestDetails = static_cast< int >(Belos::StatusTestDetails);

SWIGEXPORT SWIGEXTERN int const _wrap_BelosDebug = static_cast< int >(Belos::Debug);

SWIGEXPORT SwigArrayWrapper _wrap_convertMsgTypeToString(int const *farg1) {
  SwigArrayWrapper fresult ;
  Belos::MsgType arg1 ;
  std::string result;
  
  arg1 = static_cast< Belos::MsgType >(*farg1);
  result = Belos::convertMsgTypeToString(arg1);
  fresult.size = (&result)->size();
  if (fresult.size > 0) {
    fresult.data = malloc(fresult.size);
    memcpy(fresult.data, (&result)->c_str(), fresult.size);
  } else {
    fresult.data = NULL;
  }
  return fresult;
}


#ifdef __cplusplus
}
#endif

