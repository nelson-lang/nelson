/* file      : libstudxml/details/expat/expat_config.h
 * license   : MIT; see accompanying LICENSE file
 */

/* Static configuration for the Expat copy bundled with libstudxml. Replaces
 * the expat_config.h that Expat's own build system would generate.
 */

#ifndef EXPAT_CONFIG_H
#define EXPAT_CONFIG_H 1

/* Endianess, reused from libstudxml's own configuration.
 */
#ifdef _MSC_VER
#include <libstudxml/details/config-vc.h>
#else
#include <libstudxml/details/config.h>
#endif

#define BYTEORDER LIBSTUDXML_BYTEORDER

#if BYTEORDER == 4321
#define WORDS_BIGENDIAN 1
#endif

/* Expat feature selection.
 */
#define XML_NS 1
#define XML_DTD 1
#define XML_GE 1
#define XML_CONTEXT_BYTES 1024

/* Entropy source for the hash salt. /dev/urandom is available on every
 * non-Windows platform we support; Windows uses RtlGenRandom.
 */
#ifndef _WIN32
#define XML_DEV_URANDOM 1
#endif

#ifdef _MSC_VER
#define __func__ __FUNCTION__
#endif

#define PACKAGE "expat"
#define PACKAGE_NAME "expat"
#define PACKAGE_TARNAME "expat"
#define PACKAGE_VERSION "2.7.3"
#define PACKAGE_STRING "expat 2.7.3"
#define PACKAGE_BUGREPORT "expat-bugs@libexpat.org"
#define PACKAGE_URL ""

#endif /* EXPAT_CONFIG_H */
