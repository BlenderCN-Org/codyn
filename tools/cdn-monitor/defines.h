#ifndef __CDN_MONITOR_DEFINES_H__
#define __CDN_MONITOR_DEFINES_H__

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#ifdef PLATFORM_OSX
#define DYLIB_SUFFIX ".dylib"
#else
#ifdef MINGW
#define DYLIB_SUFFIX ".dll"
#else
#define DYLIB_SUFFIX ".so"
#endif
#endif

#endif /* __CDN_MONITOR_DEFINES_H__ */

