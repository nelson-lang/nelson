/* linse.hpp -- Header-only readline (line-edit) library for C++17
 *
 * -------------------------------------------------------------------------------------
 *
 * Copyright (c) 2018, I
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   1.  Redistributions of source code must retain the above copyright notice,
 *      this list of conditions and the following disclaimer.
 *   2.  Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *   3.  Neither the name of I nor the names of its contributors may be used
 *      to endorse or promote products derived from this software without
 *      specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 *
 * ------------------------------------------------------------------------------------
 *
 * The licenses of original codes are below:
 *
 */

/* linenoise.c -- guerrilla line editing library against the idea that a
 * line editing lib needs to be 20,000 lines of C code.
 *
 * Copyright (c) 2010, Salvatore Sanfilippo <antirez at gmail dot com>
 * Copyright (c) 2010, Pieter Noordhuis <pcnoordhuis at gmail dot com>
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *   * Redistributions of source code must retain the above copyright notice,
 *     this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of Redis nor the names of its contributors may be used
 *     to endorse or promote products derived from this software without
 *     specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

/* Copyright 2001-2004 Unicode, Inc.
 *
 * Disclaimer
 *
 * This source code is provided as is by Unicode, Inc. No claims are
 * made as to fitness for any particular purpose. No warranties of any
 * kind are expressed or implied. The recipient agrees to determine
 * applicability of information provided. If this file has been
 * purchased on magnetic or optical media from Unicode, Inc., the
 * sole remedy for any claim will be exchange of defective media
 * within 90 days of receipt.
 *
 * Limitations on Rights to Redistribute This Code
 *
 * Unicode, Inc. hereby grants the right to freely use the information
 * supplied in this file in the creation of products supporting the
 * Unicode Standard, and to make copies of this file in any form
 * for internal or external distribution as long as this notice
 * remains attached.
 */

/* Markus Kuhn -- 2007-05-26 (Unicode 5.0)
 *
 * Permission to use, copy, modify, and distribute this software
 * for any purpose and without fee is hereby granted. The author
 * disclaims all warranties with regard to this software.
 */

#include <chrono>
#include <thread>

#ifdef _WIN32
#define ssize_t ptrdiff_t
#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <io.h>
#if defined(_MSC_VER) && _MSC_VER < 1900
#define snprintf _snprintf
#endif
#if !(defined __GNUC__)
#define strcasecmp _stricmp
#define STDIN_FILENO (_fileno(stdin))
#define STDOUT_FILENO (_fileno(stdout))
#define STDERR_FILENO (_fileno(stderr))
#endif
#define isatty _isatty
#define write _write

#else /* _WIN32 */

#include <signal.h>
#include <termios.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <cctype>
#include <wctype.h>

#endif /* _WIN32 */

#include <iostream>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <errno.h>
#include <fcntl.h>

#include <cstdint>
#include <tuple>
#include <wchar.h>

#include <array>
#include <string>
#include <vector>
#include <memory>
#include <sstream>
#include <optional>
#include <functional>
#include <cstddef>
#include <numeric>
#include <filesystem>
#include <fstream>

#include "Evaluator.hpp"
#include "NelsonConfiguration.hpp"
#include "ProcessEventsDynamicFunction.hpp"
#include "CallbackQueue.hpp"

static bool stopInputBuffer = false;

struct linse
{
private:
#ifdef _WIN32
#ifdef __GNUC__
    static inline FILE*
    fopen(const wchar_t* f, const wchar_t* type)
    {
        return ::_wfopen(f, type);
    }
#else
    static inline void
    strncpy(char* dest, const char* src, std::size_t count)
    {
        ::strncpy_s(dest, count, src, _TRUNCATE);
    }
    static inline FILE*
    fopen(const wchar_t* f, const wchar_t* type)
    {
        FILE* fp;
        if (::_wfopen_s(&fp, f, type))
            return nullptr;
        return fp;
    }
#endif
#endif

    using UTF32 = char32_t;
    using UTF16 = char16_t;
    using UTF8 = std::uint8_t;

    /* Some fundamental constants */
    static constexpr UTF32 UNI_REPLACEMENT_CHAR = 0x0000FFFD;
    static constexpr UTF32 UNI_MAX_BMP = 0x0000FFFF;
    static constexpr UTF32 UNI_MAX_UTF16 = 0x0010FFFF;
    static constexpr UTF32 UNI_MAX_UTF32 = 0x7FFFFFFF;
    static constexpr UTF32 UNI_MAX_LEGAL_UTF32 = 0x0010FFFF;

    enum class conversion_result : std::uint8_t
    {
        succeeded, /* conversion successful */
        source_exhausted, /* partial character in source, but hit end */
        target_exhausted, /* insuff. room in target for conversion */
        source_illegal /* source sequence is illegal/malformed */
    };

    enum class conversion_flag : bool
    {
        strict = false,
        lenient = true
    };

    static constexpr int halfShift = 10; /* used for shifting by 10 bits */

    static constexpr UTF32 halfBase = 0x0010000UL;
    static constexpr UTF32 halfMask = 0x3FFUL;

    static constexpr UTF32 UNI_SUR_HIGH_START = 0xD800;
    static constexpr UTF32 UNI_SUR_HIGH_END = 0xDBFF;
    static constexpr UTF32 UNI_SUR_LOW_START = 0xDC00;
    static constexpr UTF32 UNI_SUR_LOW_END = 0xDFFF;

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF32*, char16_t*>
    ConvertUTF32toUTF16(const UTF32* sourceStart, const UTF32* sourceEnd, char16_t* targetStart,
        char16_t* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF32* source = sourceStart;
        char16_t* target = targetStart;
        while (source < sourceEnd) {
            if (target >= targetEnd) {
                result = conversion_result::target_exhausted;
                break;
            }
            auto ch = *source++;
            if (ch <= UNI_MAX_BMP) { /* Target is a character <= 0xFFFF */
                /* UTF-16 surrogate values are illegal in UTF-32; 0xffff or 0xfffe are both reserved
                 * values */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    if constexpr (flag == conversion_flag::strict) {
                        --source; /* return to the illegal value itself */
                        result = conversion_result::source_illegal;
                        break;
                    } else
                        *target++ = UNI_REPLACEMENT_CHAR;
                } else
                    *target++ = static_cast<UTF16>(ch); /* normal case */
            } else if (ch > UNI_MAX_LEGAL_UTF32) {
                if constexpr (flag == conversion_flag::strict)
                    result = conversion_result::source_illegal;
                else
                    *target++ = UNI_REPLACEMENT_CHAR;
            } else {
                /* target is a character in range 0xFFFF - 0x10FFFF. */
                if (target + 1 >= targetEnd) {
                    --source; /* Back up source pointer! */
                    result = conversion_result::target_exhausted;
                    break;
                }
                ch -= halfBase;
                *target++ = static_cast<UTF16>((ch >> halfShift) + UNI_SUR_HIGH_START);
                *target++ = static_cast<UTF16>((ch & halfMask) + UNI_SUR_LOW_START);
            }
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF16*, UTF32*>
    ConvertUTF16toUTF32(const UTF16* sourceStart, const UTF16* sourceEnd, UTF32* targetStart,
        UTF32* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF16* source = sourceStart;
        UTF32* target = targetStart;
        UTF32 ch = 0, ch2 = 0;
        while (source < sourceEnd) {
            const auto oldSource
                = source; /*  In case we have to back up because of target overflow. */
            ch = *source++;
            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END) {
                /* If the 16 bits following the high surrogate are in the source buffer... */
                if (source < sourceEnd) {
                    ch2 = *source;
                    /* If it's a low surrogate, convert to UTF32. */
                    if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                        ch = ((ch - UNI_SUR_HIGH_START) << halfShift) + (ch2 - UNI_SUR_LOW_START)
                            + halfBase;
                        ++source;
                    } else if constexpr (flag
                        == conversion_flag::strict) { /* it's an unpaired high surrogate */
                        --source; /* return to the illegal value itself */
                        result = conversion_result::source_illegal;
                        break;
                    }
                } else { /* We don't have the 16 bits following the high surrogate. */
                    --source; /* return to the high surrogate */
                    result = conversion_result::source_exhausted;
                    break;
                }
            } else if constexpr (flag == conversion_flag::strict)
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                    --source; /* return to the illegal value itself */
                    result = conversion_result::source_illegal;
                    break;
                }
            if (target >= targetEnd) {
                source = oldSource; /* Back up source pointer! */
                result = conversion_result::target_exhausted;
                break;
            }
            *target++ = ch;
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    /*
     * Index into the table below with the first byte of a UTF-8 sequence to
     * get the number of trailing bytes that are supposed to follow it.
     * Note that *legal* UTF-8 values can't have 4 or 5-bytes. The table is
     * left as-is for anyone who may want to do such conversion, which was
     * allowed in earlier algorithms.
     */
    static constexpr std::uint8_t trailingBytesForUTF8[256] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2,
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5 };

    /*
     * Magic values subtracted from a buffer value during UTF8 conversion.
     * This table contains as many values as there might be trailing bytes
     * in a UTF-8 sequence.
     */
    static constexpr UTF32 offsetsFromUTF8[6]
        = { 0x00000000UL, 0x00003080UL, 0x000E2080UL, 0x03C82080UL, 0xFA082080UL, 0x82082080UL };

    /*
     * Once the bits are split out into bytes of UTF-8, this is a mask OR-ed
     * into the first byte, depending on how many bytes follow.  There are
     * as many entries in this table as there are UTF-8 sequence types.
     * (I.e., one byte sequence, two byte... etc.). Remember that sequencs
     * for *legal* UTF-8 will be 4 or fewer bytes total.
     */
    static constexpr UTF8 firstByteMark[7] = { 0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC };

    /* --------------------------------------------------------------------- */

    /* The interface converts a whole buffer to avoid function-call overhead.
     * Constants have been gathered. Loops & conditionals have been removed as
     * much as possible for efficiency, in favor of drop-through switches.
     * (See "Note A" at the bottom of the file for equivalent code.)
     * If your compiler supports it, the "isLegalUTF8" call can be turned
     * into an inline function.
     */

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF16*, UTF8*>
    ConvertUTF16toUTF8(const UTF16* sourceStart, const UTF16* sourceEnd, UTF8* targetStart,
        UTF8* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF16* source = sourceStart;
        UTF8* target = targetStart;
        while (source < sourceEnd) {
            UTF32 ch = 0;
            unsigned short bytesToWrite = 0;
            const UTF32 byteMask = 0xBF;
            const UTF32 byteMark = 0x80;
            const UTF16* oldSource
                = source; /* In case we have to back up because of target overflow. */
            ch = *source++;
            /* If we have a surrogate pair, convert to UTF32 first. */
            if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_HIGH_END)
                /* If the 16 bits following the high surrogate are in the source buffer... */
                if (source < sourceEnd) {
                    const UTF32 ch2 = *source;
                    /* If it's a low surrogate, convert to UTF32. */
                    if (ch2 >= UNI_SUR_LOW_START && ch2 <= UNI_SUR_LOW_END) {
                        ch = ((ch - UNI_SUR_HIGH_START) << halfShift) + (ch2 - UNI_SUR_LOW_START)
                            + halfBase;
                        ++source;
                    } else if constexpr (flag
                        == conversion_flag::strict) { /* it's an unpaired high surrogate */
                        --source; /* return to the illegal value itself */
                        result = conversion_result::source_illegal;
                        break;
                    }
                } else { /* We don't have the 16 bits following the high surrogate. */
                    --source; /* return to the high surrogate */
                    result = conversion_result::source_exhausted;
                    break;
                }
            else if constexpr (flag == conversion_flag::strict)
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_LOW_START && ch <= UNI_SUR_LOW_END) {
                    --source; /* return to the illegal value itself */
                    result = conversion_result::source_illegal;
                    break;
                }
            /* Figure out how many bytes the result will require */
            if (ch < static_cast<UTF32>(0x80))
                bytesToWrite = 1;
            else if (ch < static_cast<UTF32>(0x800))
                bytesToWrite = 2;
            else if (ch < static_cast<UTF32>(0x10000))
                bytesToWrite = 3;
            else if (ch < static_cast<UTF32>(0x110000))
                bytesToWrite = 4;
            else {
                bytesToWrite = 3;
                ch = UNI_REPLACEMENT_CHAR;
            }

            target += bytesToWrite;
            if (target > targetEnd) {
                source = oldSource; /* Back up source pointer! */
                target -= bytesToWrite;
                result = conversion_result::target_exhausted;
                break;
            }
            switch (bytesToWrite) { /* note: everything falls through. */
            case 4:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
            case 3:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
            case 2:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
            case 1:
                *--target = (UTF8)(ch | firstByteMark[bytesToWrite]);
            }
            target += bytesToWrite;
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    /*
     * Utility routine to tell whether a sequence of bytes is legal UTF-8.
     * This must be called with the length pre-determined by the first byte.
     * If not calling this from ConvertUTF8to*, then the length can be set by:
     *  length = trailingBytesForUTF8[*source]+1;
     * and the sequence is illegal right away if there aren't that many bytes
     * available.
     * If presented with a length > 4, this returns false.  The Unicode
     * definition of UTF-8 goes up to 4-byte sequences.
     */

    static constexpr bool
    isLegalUTF8(const UTF8* source, int length) noexcept
    {
        UTF8 a = 0;
        const UTF8* srcptr = source + length;
        switch (length) {
        default:
            return false;
            /* Everything else falls through when "true"... */
        case 4:
            if ((a = (*--srcptr)) < 0x80 || a > 0xBF)
                return false;
            [[fallthrough]];
        case 3:
            if ((a = (*--srcptr)) < 0x80 || a > 0xBF)
                return false;
            [[fallthrough]];
        case 2:
            if ((a = (*--srcptr)) > 0xBF)
                return false;

            switch (*source) {
            /* no fall-through in this inner switch */
            case 0xE0:
                if (a < 0xA0)
                    return false;
                break;
            case 0xED:
                if (a > 0x9F)
                    return false;
                break;
            case 0xF0:
                if (a < 0x90)
                    return false;
                break;
            case 0xF4:
                if (a > 0x8F)
                    return false;
                break;
            default:
                if (a < 0x80)
                    return false;
            }
            [[fallthrough]];

        case 1:
            if (*source >= 0x80 && *source < 0xC2)
                return false;
        }
        if (*source > 0xF4)
            return false;
        return true;
    }

    /* --------------------------------------------------------------------- */

    /*
     * Exported function to return whether a UTF-8 sequence is legal or not.
     * This is not used here; it's just exported.
     */
    static constexpr bool
    isLegalUTF8Sequence(const UTF8* source, const UTF8* sourceEnd) noexcept
    {
        int length = trailingBytesForUTF8[*source] + 1;
        if (source + length > sourceEnd)
            return false;
        return isLegalUTF8(source, length);
    }

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF8*, UTF16*>
    ConvertUTF8toUTF16(const UTF8* sourceStart, const UTF8* sourceEnd, UTF16* targetStart,
        UTF16* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF8* source = sourceStart;
        UTF16* target = targetStart;
        while (source < sourceEnd) {
            UTF32 ch = 0;
            unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
            if (source + extraBytesToRead >= sourceEnd) {
                result = conversion_result::source_exhausted;
                break;
            }
            /* Do this check whether lenient or strict */
            if (!isLegalUTF8(source, extraBytesToRead + 1)) {
                result = conversion_result::source_illegal;
                break;
            }
            /*
             * The cases all fall through. See "Note A" below.
             */
            switch (extraBytesToRead) {
            case 5:
                ch += *source++;
                ch <<= 6;
            case 4:
                ch += *source++;
                ch <<= 6;
            case 3:
                ch += *source++;
                ch <<= 6;
            case 2:
                ch += *source++;
                ch <<= 6;
            case 1:
                ch += *source++;
                ch <<= 6;
            case 0:
                ch += *source++;
            }
            ch -= offsetsFromUTF8[extraBytesToRead];

            if (target >= targetEnd) {
                source -= (extraBytesToRead + 1); /* Back up source pointer! */
                result = conversion_result::target_exhausted;
                break;
            }
            if (ch <= UNI_MAX_BMP) /* Target is a character <= 0xFFFF */
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END)
                    if constexpr (flag == conversion_flag::strict) {
                        source -= (extraBytesToRead + 1); /* return to the illegal value itself */
                        result = conversion_result::source_illegal;
                        break;
                    } else
                        *target++ = UNI_REPLACEMENT_CHAR;
                else
                    *target++ = static_cast<UTF16>(ch); /* normal case */
            else if (ch > UNI_MAX_UTF16)
                if constexpr (flag == conversion_flag::strict) {
                    source -= (extraBytesToRead + 1); /* return to the start */
                    result = conversion_result::source_illegal;
                    break; /* Bail out; shouldn't continue */
                } else
                    *target++ = UNI_REPLACEMENT_CHAR;
            else {
                /* target is a character in range 0xFFFF - 0x10FFFF. */
                if (target + 1 >= targetEnd) {
                    source -= (extraBytesToRead + 1); /* Back up source pointer! */
                    result = conversion_result::target_exhausted;
                    break;
                }
                ch -= halfBase;
                *target++ = static_cast<UTF16>((ch >> halfShift) + UNI_SUR_HIGH_START);
                *target++ = static_cast<UTF16>((ch & halfMask) + UNI_SUR_LOW_START);
            }
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF32*, UTF8*>
    ConvertUTF32toUTF8(const UTF32* sourceStart, const UTF32* sourceEnd, UTF8* targetStart,
        UTF8* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF32* source = sourceStart;
        UTF8* target = targetStart;
        while (source < sourceEnd) {
            UTF32 ch = 0;
            unsigned short bytesToWrite = 0;
            constexpr UTF32 byteMask = 0xBF;
            constexpr UTF32 byteMark = 0x80;
            ch = *source++;
            if constexpr (flag == conversion_flag::strict)
                /* UTF-16 surrogate values are illegal in UTF-32 */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END) {
                    --source; /* return to the illegal value itself */
                    result = conversion_result::source_illegal;
                    break;
                }
            /*
             * Figure out how many bytes the result will require. Turn any
             * illegally large UTF32 things (> Plane 17) into replacement chars.
             */
            if (ch < static_cast<UTF32>(0x80))
                bytesToWrite = 1;
            else if (ch < static_cast<UTF32>(0x800))
                bytesToWrite = 2;
            else if (ch < static_cast<UTF32>(0x10000))
                bytesToWrite = 3;
            else if (ch <= UNI_MAX_LEGAL_UTF32)
                bytesToWrite = 4;
            else {
                bytesToWrite = 3;
                ch = UNI_REPLACEMENT_CHAR;
                result = conversion_result::source_illegal;
            }

            target += bytesToWrite;
            if (target > targetEnd) {
                --source; /* Back up source pointer! */
                target -= bytesToWrite;
                result = conversion_result::target_exhausted;
                break;
            }
            switch (bytesToWrite) { /* note: everything falls through. */
            case 4:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
                [[fallthrough]];
            case 3:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
                [[fallthrough]];
            case 2:
                *--target = (UTF8)((ch | byteMark) & byteMask);
                ch >>= 6;
                [[fallthrough]];
            case 1:
                *--target = (UTF8)(ch | firstByteMark[bytesToWrite]);
            }
            target += bytesToWrite;
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    template <conversion_flag flag = conversion_flag::lenient>
    static constexpr std::tuple<conversion_result, const UTF8*, UTF32*>
    ConvertUTF8toUTF32(const UTF8* sourceStart, const UTF8* sourceEnd, UTF32* targetStart,
        UTF32* targetEnd) noexcept
    {
        conversion_result result = conversion_result::succeeded;
        const UTF8* source = sourceStart;
        UTF32* target = targetStart;
        while (source < sourceEnd) {
            UTF32 ch = 0;
            unsigned short extraBytesToRead = trailingBytesForUTF8[*source];
            if (source + extraBytesToRead >= sourceEnd) {
                result = conversion_result::source_exhausted;
                break;
            }
            /* Do this check whether lenient or strict */
            if (!isLegalUTF8(source, extraBytesToRead + 1)) {
                result = conversion_result::source_illegal;
                break;
            }
            /*
             * The cases all fall through. See "Note A" below.
             */
            switch (extraBytesToRead) {
            case 5:
                ch += *source++;
                ch <<= 6;
                [[fallthrough]];
            case 4:
                ch += *source++;
                ch <<= 6;
                [[fallthrough]];
            case 3:
                ch += *source++;
                ch <<= 6;
                [[fallthrough]];
            case 2:
                ch += *source++;
                ch <<= 6;
                [[fallthrough]];
            case 1:
                ch += *source++;
                ch <<= 6;
                [[fallthrough]];
            case 0:
                ch += *source++;
            }
            ch -= offsetsFromUTF8[extraBytesToRead];

            if (target >= targetEnd) {
                source -= (extraBytesToRead + 1); /* Back up the source pointer! */
                result = conversion_result::target_exhausted;
                break;
            }
            if (ch <= UNI_MAX_LEGAL_UTF32)
                /*
                 * UTF-16 surrogate values are illegal in UTF-32, and anything
                 * over Plane 17 (> 0x10FFFF) is illegal.
                 */
                if (ch >= UNI_SUR_HIGH_START && ch <= UNI_SUR_LOW_END)
                    if constexpr (flag == conversion_flag::strict) {
                        source -= (extraBytesToRead + 1); /* return to the illegal value itself */
                        result = conversion_result::source_illegal;
                        break;
                    } else
                        *target++ = UNI_REPLACEMENT_CHAR;
                else
                    *target++ = ch;
            else { /* i.e., ch > UNI_MAX_LEGAL_UTF32 */
                result = conversion_result::source_illegal;
                *target++ = UNI_REPLACEMENT_CHAR;
            }
        }
        return { result, source, target };
    }

    /* --------------------------------------------------------------------- */

    /*
     * This is an implementation of wcwidth() and wcswidth() (defined in
     * IEEE Std 1002.1-2001) for Unicode.
     *
     * http://www.opengroup.org/onlinepubs/007904975/functions/wcwidth.html
     * http://www.opengroup.org/onlinepubs/007904975/functions/wcswidth.html
     *
     * In fixed-width output devices, Latin characters all occupy a single
     * "cell" position of equal width, whereas ideographic CJK characters
     * occupy two such cells. Interoperability between terminal-line
     * applications and (teletype-style) character terminals using the
     * UTF-8 encoding requires agreement on which character should advance
     * the cursor by how many cell positions. No established formal
     * standards exist at present on which Unicode character shall occupy
     * how many cell positions on character terminals. These routines are
     * a first attempt of defining such behavior based on simple rules
     * applied to data provided by the Unicode Consortium.
     *
     * For some graphical characters, the Unicode standard explicitly
     * defines a character-cell width via the definition of the East Asian
     * FullWidth (F), Wide (W), Half-width (H), and Narrow (Na) classes.
     * In all these cases, there is no ambiguity about which width a
     * terminal shall use. For characters in the East Asian Ambiguous (A)
     * class, the width choice depends purely on a preference of backward
     * compatibility with either historic CJK or Western practice.
     * Choosing single-width for these characters is easy to justify as
     * the appropriate long-term solution, as the CJK practice of
     * displaying these characters as double-width comes from historic
     * implementation simplicity (8-bit encoded characters were displayed
     * single-width and 16-bit ones double-width, even for Greek,
     * Cyrillic, etc.) and not any typographic considerations.
     *
     * Much less clear is the choice of width for the Not East Asian
     * (Neutral) class. Existing practice does not dictate a width for any
     * of these characters. It would nevertheless make sense
     * typographically to allocate two character cells to characters such
     * as for instance EM SPACE or VOLUME INTEGRAL, which cannot be
     * represented adequately with a single-width glyph. The following
     * routines at present merely assign a single-cell width to all
     * neutral characters, in the interest of simplicity. This is not
     * entirely satisfactory and should be reconsidered before
     * establishing a formal standard in this area. At the moment, the
     * decision which Not East Asian (Neutral) characters should be
     * represented by double-width glyphs cannot yet be answered by
     * applying a simple rule from the Unicode database content. Setting
     * up a proper standard for the behavior of UTF-8 character terminals
     * will require a careful analysis not only of each Unicode character,
     * but also of each presentation form, something the author of these
     * routines has avoided to do so far.
     *
     * http://www.unicode.org/unicode/reports/tr11/
     */

    struct interval
    {
        char32_t first;
        char32_t last;
    };

    /* auxiliary function for binary search in interval table */
    template <int N>
    static constexpr bool
    bisearch(char32_t ucs, const interval (&table)[N]) noexcept
    {
        if (ucs < table[0].first || ucs > table[N - 1].last)
            return false;
        int min = 0, max = N - 1;
        int mid = 0;
        while (max >= min) {
            mid = (min + max) / 2;
            if (ucs > table[mid].last)
                min = mid + 1;
            else if (ucs < table[mid].first)
                max = mid - 1;
            else
                return true;
        }
        return false;
    }

    /* The following two functions define the column width of an ISO 10646
     * character as follows:
     *
     *    - The null character (U+0000) has a column width of 0.
     *
     *    - Other C0/C1 control characters and DEL will lead to a return
     *      value of -1.
     *
     *    - Non-spacing and enclosing combining characters (general
     *      category code Mn or Me in the Unicode database) have a
     *      column width of 0.
     *
     *    - SOFT HYPHEN (U+00AD) has a column width of 1.
     *
     *    - Other format characters (general category code Cf in the Unicode
     *      database) and ZERO WIDTH SPACE (U+200B) have a column width of 0.
     *
     *    - Hangul Jamo medial vowels and final consonants (U+1160-U+11FF)
     *      have a column width of 0.
     *
     *    - Spacing characters in the East Asian Wide (W) or East Asian
     *      Full-width (F) category as defined in Unicode Technical
     *      Report #11 have a column width of 2.
     *
     *    - All remaining characters (including all printable
     *      ISO 8859-1 and WGL4 characters, Unicode control characters,
     *      etc.) have a column width of 1.
     *
     * This implementation assumes that wchar_t characters are encoded
     * in ISO 10646.
     */

    static constexpr int
    mk_wcwidth(char32_t ucs) noexcept
    {
        /* sorted list of non-overlapping intervals of non-spacing characters */
        /* generated by "uniset +cat=Me +cat=Mn +cat=Cf -00AD +1160-11FF +200B c" */
        constexpr interval combining[] = { { 0x0300, 0x036F }, { 0x0483, 0x0486 },
            { 0x0488, 0x0489 }, { 0x0591, 0x05BD }, { 0x05BF, 0x05BF }, { 0x05C1, 0x05C2 },
            { 0x05C4, 0x05C5 }, { 0x05C7, 0x05C7 }, { 0x0600, 0x0603 }, { 0x0610, 0x0615 },
            { 0x064B, 0x065E }, { 0x0670, 0x0670 }, { 0x06D6, 0x06E4 }, { 0x06E7, 0x06E8 },
            { 0x06EA, 0x06ED }, { 0x070F, 0x070F }, { 0x0711, 0x0711 }, { 0x0730, 0x074A },
            { 0x07A6, 0x07B0 }, { 0x07EB, 0x07F3 }, { 0x0901, 0x0902 }, { 0x093C, 0x093C },
            { 0x0941, 0x0948 }, { 0x094D, 0x094D }, { 0x0951, 0x0954 }, { 0x0962, 0x0963 },
            { 0x0981, 0x0981 }, { 0x09BC, 0x09BC }, { 0x09C1, 0x09C4 }, { 0x09CD, 0x09CD },
            { 0x09E2, 0x09E3 }, { 0x0A01, 0x0A02 }, { 0x0A3C, 0x0A3C }, { 0x0A41, 0x0A42 },
            { 0x0A47, 0x0A48 }, { 0x0A4B, 0x0A4D }, { 0x0A70, 0x0A71 }, { 0x0A81, 0x0A82 },
            { 0x0ABC, 0x0ABC }, { 0x0AC1, 0x0AC5 }, { 0x0AC7, 0x0AC8 }, { 0x0ACD, 0x0ACD },
            { 0x0AE2, 0x0AE3 }, { 0x0B01, 0x0B01 }, { 0x0B3C, 0x0B3C }, { 0x0B3F, 0x0B3F },
            { 0x0B41, 0x0B43 }, { 0x0B4D, 0x0B4D }, { 0x0B56, 0x0B56 }, { 0x0B82, 0x0B82 },
            { 0x0BC0, 0x0BC0 }, { 0x0BCD, 0x0BCD }, { 0x0C3E, 0x0C40 }, { 0x0C46, 0x0C48 },
            { 0x0C4A, 0x0C4D }, { 0x0C55, 0x0C56 }, { 0x0CBC, 0x0CBC }, { 0x0CBF, 0x0CBF },
            { 0x0CC6, 0x0CC6 }, { 0x0CCC, 0x0CCD }, { 0x0CE2, 0x0CE3 }, { 0x0D41, 0x0D43 },
            { 0x0D4D, 0x0D4D }, { 0x0DCA, 0x0DCA }, { 0x0DD2, 0x0DD4 }, { 0x0DD6, 0x0DD6 },
            { 0x0E31, 0x0E31 }, { 0x0E34, 0x0E3A }, { 0x0E47, 0x0E4E }, { 0x0EB1, 0x0EB1 },
            { 0x0EB4, 0x0EB9 }, { 0x0EBB, 0x0EBC }, { 0x0EC8, 0x0ECD }, { 0x0F18, 0x0F19 },
            { 0x0F35, 0x0F35 }, { 0x0F37, 0x0F37 }, { 0x0F39, 0x0F39 }, { 0x0F71, 0x0F7E },
            { 0x0F80, 0x0F84 }, { 0x0F86, 0x0F87 }, { 0x0F90, 0x0F97 }, { 0x0F99, 0x0FBC },
            { 0x0FC6, 0x0FC6 }, { 0x102D, 0x1030 }, { 0x1032, 0x1032 }, { 0x1036, 0x1037 },
            { 0x1039, 0x1039 }, { 0x1058, 0x1059 }, { 0x1160, 0x11FF }, { 0x135F, 0x135F },
            { 0x1712, 0x1714 }, { 0x1732, 0x1734 }, { 0x1752, 0x1753 }, { 0x1772, 0x1773 },
            { 0x17B4, 0x17B5 }, { 0x17B7, 0x17BD }, { 0x17C6, 0x17C6 }, { 0x17C9, 0x17D3 },
            { 0x17DD, 0x17DD }, { 0x180B, 0x180D }, { 0x18A9, 0x18A9 }, { 0x1920, 0x1922 },
            { 0x1927, 0x1928 }, { 0x1932, 0x1932 }, { 0x1939, 0x193B }, { 0x1A17, 0x1A18 },
            { 0x1B00, 0x1B03 }, { 0x1B34, 0x1B34 }, { 0x1B36, 0x1B3A }, { 0x1B3C, 0x1B3C },
            { 0x1B42, 0x1B42 }, { 0x1B6B, 0x1B73 }, { 0x1DC0, 0x1DCA }, { 0x1DFE, 0x1DFF },
            { 0x200B, 0x200F }, { 0x202A, 0x202E }, { 0x2060, 0x2063 }, { 0x206A, 0x206F },
            { 0x20D0, 0x20EF }, { 0x302A, 0x302F }, { 0x3099, 0x309A }, { 0xA806, 0xA806 },
            { 0xA80B, 0xA80B }, { 0xA825, 0xA826 }, { 0xFB1E, 0xFB1E }, { 0xFE00, 0xFE0F },
            { 0xFE20, 0xFE23 }, { 0xFEFF, 0xFEFF }, { 0xFFF9, 0xFFFB }, { 0x10A01, 0x10A03 },
            { 0x10A05, 0x10A06 }, { 0x10A0C, 0x10A0F }, { 0x10A38, 0x10A3A }, { 0x10A3F, 0x10A3F },
            { 0x1D167, 0x1D169 }, { 0x1D173, 0x1D182 }, { 0x1D185, 0x1D18B }, { 0x1D1AA, 0x1D1AD },
            { 0x1D242, 0x1D244 }, { 0xE0001, 0xE0001 }, { 0xE0020, 0xE007F },
            { 0xE0100, 0xE01EF } };

        /* test for 8-bit control characters */
        if (ucs == 0)
            return 0;
        if (ucs < 32 || (ucs >= 0x7f && ucs < 0xa0))
            return -1;

        /* binary search in table of non-spacing characters */
        if (bisearch(ucs, combining))
            return 0;

        /* if we arrive here, ucs is not a combining or C0/C1 control character */

        return 1
            + (ucs >= 0x1100
                && (ucs <= 0x115f || /* Hangul Jamo init. consonants */
                    ucs == 0x2329 || ucs == 0x232a
                    || (ucs >= 0x2e80 && ucs <= 0xa4cf && ucs != 0x303f) || /* CJK ... Yi */
                    (ucs >= 0xac00 && ucs <= 0xd7a3) || /* Hangul Syllables */
                    (ucs >= 0xf900 && ucs <= 0xfaff) || /* CJK Compatibility Ideographs */
                    (ucs >= 0xfe10 && ucs <= 0xfe19) || /* Vertical forms */
                    (ucs >= 0xfe30 && ucs <= 0xfe6f) || /* CJK Compatibility Forms */
                    (ucs >= 0xff00 && ucs <= 0xff60) || /* Fullwidth Forms */
                    (ucs >= 0xffe0 && ucs <= 0xffe6) || (ucs >= 0x20000 && ucs <= 0x2fffd)
                    || (ucs >= 0x30000 && ucs <= 0x3fffd)));
    }

    static constexpr int
    mk_wcswidth(const char32_t* pwcs, size_t n) noexcept
    {
        int w = 0, width = 0;

        for (; *pwcs && n-- > 0; pwcs++)
            if ((w = mk_wcwidth(*pwcs)) < 0)
                return -1;
            else
                width += w;

        return width;
    }

    /*
     * The following functions are the same as mk_wcwidth() and
     * mk_wcswidth(), except that spacing characters in the East Asian
     * Ambiguous (A) category as defined in Unicode Technical Report #11
     * have a column width of 2. This variant might be useful for users of
     * CJK legacy encodings who want to migrate to UCS without changing
     * the traditional terminal character-width behaviour. It is not
     * otherwise recommended for general use.
     */
    static constexpr int
    mk_wcwidth_cjk(wchar_t ucs) noexcept
    {
        /* sorted list of non-overlapping intervals of East Asian Ambiguous
         * characters, generated by "uniset +WIDTH-A -cat=Me -cat=Mn -cat=Cf c" */
        constexpr struct interval ambiguous[] = { { 0x00A1, 0x00A1 }, { 0x00A4, 0x00A4 },
            { 0x00A7, 0x00A8 }, { 0x00AA, 0x00AA }, { 0x00AE, 0x00AE }, { 0x00B0, 0x00B4 },
            { 0x00B6, 0x00BA }, { 0x00BC, 0x00BF }, { 0x00C6, 0x00C6 }, { 0x00D0, 0x00D0 },
            { 0x00D7, 0x00D8 }, { 0x00DE, 0x00E1 }, { 0x00E6, 0x00E6 }, { 0x00E8, 0x00EA },
            { 0x00EC, 0x00ED }, { 0x00F0, 0x00F0 }, { 0x00F2, 0x00F3 }, { 0x00F7, 0x00FA },
            { 0x00FC, 0x00FC }, { 0x00FE, 0x00FE }, { 0x0101, 0x0101 }, { 0x0111, 0x0111 },
            { 0x0113, 0x0113 }, { 0x011B, 0x011B }, { 0x0126, 0x0127 }, { 0x012B, 0x012B },
            { 0x0131, 0x0133 }, { 0x0138, 0x0138 }, { 0x013F, 0x0142 }, { 0x0144, 0x0144 },
            { 0x0148, 0x014B }, { 0x014D, 0x014D }, { 0x0152, 0x0153 }, { 0x0166, 0x0167 },
            { 0x016B, 0x016B }, { 0x01CE, 0x01CE }, { 0x01D0, 0x01D0 }, { 0x01D2, 0x01D2 },
            { 0x01D4, 0x01D4 }, { 0x01D6, 0x01D6 }, { 0x01D8, 0x01D8 }, { 0x01DA, 0x01DA },
            { 0x01DC, 0x01DC }, { 0x0251, 0x0251 }, { 0x0261, 0x0261 }, { 0x02C4, 0x02C4 },
            { 0x02C7, 0x02C7 }, { 0x02C9, 0x02CB }, { 0x02CD, 0x02CD }, { 0x02D0, 0x02D0 },
            { 0x02D8, 0x02DB }, { 0x02DD, 0x02DD }, { 0x02DF, 0x02DF }, { 0x0391, 0x03A1 },
            { 0x03A3, 0x03A9 }, { 0x03B1, 0x03C1 }, { 0x03C3, 0x03C9 }, { 0x0401, 0x0401 },
            { 0x0410, 0x044F }, { 0x0451, 0x0451 }, { 0x2010, 0x2010 }, { 0x2013, 0x2016 },
            { 0x2018, 0x2019 }, { 0x201C, 0x201D }, { 0x2020, 0x2022 }, { 0x2024, 0x2027 },
            { 0x2030, 0x2030 }, { 0x2032, 0x2033 }, { 0x2035, 0x2035 }, { 0x203B, 0x203B },
            { 0x203E, 0x203E }, { 0x2074, 0x2074 }, { 0x207F, 0x207F }, { 0x2081, 0x2084 },
            { 0x20AC, 0x20AC }, { 0x2103, 0x2103 }, { 0x2105, 0x2105 }, { 0x2109, 0x2109 },
            { 0x2113, 0x2113 }, { 0x2116, 0x2116 }, { 0x2121, 0x2122 }, { 0x2126, 0x2126 },
            { 0x212B, 0x212B }, { 0x2153, 0x2154 }, { 0x215B, 0x215E }, { 0x2160, 0x216B },
            { 0x2170, 0x2179 }, { 0x2190, 0x2199 }, { 0x21B8, 0x21B9 }, { 0x21D2, 0x21D2 },
            { 0x21D4, 0x21D4 }, { 0x21E7, 0x21E7 }, { 0x2200, 0x2200 }, { 0x2202, 0x2203 },
            { 0x2207, 0x2208 }, { 0x220B, 0x220B }, { 0x220F, 0x220F }, { 0x2211, 0x2211 },
            { 0x2215, 0x2215 }, { 0x221A, 0x221A }, { 0x221D, 0x2220 }, { 0x2223, 0x2223 },
            { 0x2225, 0x2225 }, { 0x2227, 0x222C }, { 0x222E, 0x222E }, { 0x2234, 0x2237 },
            { 0x223C, 0x223D }, { 0x2248, 0x2248 }, { 0x224C, 0x224C }, { 0x2252, 0x2252 },
            { 0x2260, 0x2261 }, { 0x2264, 0x2267 }, { 0x226A, 0x226B }, { 0x226E, 0x226F },
            { 0x2282, 0x2283 }, { 0x2286, 0x2287 }, { 0x2295, 0x2295 }, { 0x2299, 0x2299 },
            { 0x22A5, 0x22A5 }, { 0x22BF, 0x22BF }, { 0x2312, 0x2312 }, { 0x2460, 0x24E9 },
            { 0x24EB, 0x254B }, { 0x2550, 0x2573 }, { 0x2580, 0x258F }, { 0x2592, 0x2595 },
            { 0x25A0, 0x25A1 }, { 0x25A3, 0x25A9 }, { 0x25B2, 0x25B3 }, { 0x25B6, 0x25B7 },
            { 0x25BC, 0x25BD }, { 0x25C0, 0x25C1 }, { 0x25C6, 0x25C8 }, { 0x25CB, 0x25CB },
            { 0x25CE, 0x25D1 }, { 0x25E2, 0x25E5 }, { 0x25EF, 0x25EF }, { 0x2605, 0x2606 },
            { 0x2609, 0x2609 }, { 0x260E, 0x260F }, { 0x2614, 0x2615 }, { 0x261C, 0x261C },
            { 0x261E, 0x261E }, { 0x2640, 0x2640 }, { 0x2642, 0x2642 }, { 0x2660, 0x2661 },
            { 0x2663, 0x2665 }, { 0x2667, 0x266A }, { 0x266C, 0x266D }, { 0x266F, 0x266F },
            { 0x273D, 0x273D }, { 0x2776, 0x277F }, { 0xE000, 0xF8FF }, { 0xFFFD, 0xFFFD },
            { 0xF0000, 0xFFFFD }, { 0x100000, 0x10FFFD } };

        /* binary search in table of non-spacing characters */
        if (bisearch(ucs, ambiguous))
            return 2;

        return mk_wcwidth(ucs);
    }

    static constexpr int
    mk_wcswidth_cjk(const wchar_t* pwcs, size_t n) noexcept
    {
        int w = 0, width = 0;

        for (; *pwcs && n-- > 0; pwcs++)
            if ((w = mk_wcwidth_cjk(*pwcs)) < 0)
                return -1;
            else
                width += w;

        return width;
    }

    typedef unsigned char char8_t;

    template <typename T>
    static constexpr std::size_t
    strlen(const T* str) noexcept
    {
        const T* ptr = str;

        while (*ptr)
            ++ptr;

        return static_cast<std::size_t>(ptr - str);
    }

    static constexpr std::pair<conversion_result, std::size_t>
    copyString8to32(
        char32_t* dst, std::size_t dstSize, const UTF8* src, std::size_t srcSize) noexcept
    {
        const auto sourceStart = src;
        const auto sourceEnd = sourceStart + srcSize;
        const auto targetStart = dst;
        const auto targetEnd = targetStart + dstSize;

        const auto [res, _, target_end]
            = ConvertUTF8toUTF32(sourceStart, sourceEnd, targetStart, targetEnd);

        if (res != conversion_result::succeeded)
            return std::make_pair(res, std::size_t { 0 });

        return std::make_pair(res, static_cast<std::size_t>(target_end - targetStart));
    }

    static std::pair<conversion_result, std::size_t>
    copyString8to32(
        char32_t* dst, std::size_t dstSize, const char* src, std::size_t srcSize) noexcept
    {
        return copyString8to32(dst, dstSize, reinterpret_cast<const UTF8*>(src), srcSize);
    }

    constexpr static std::pair<conversion_result, std::size_t>
    copyString8to32(char32_t* dst, std::size_t dstSize, const UTF8* src) noexcept
    {
        return copyString8to32(dst, dstSize, src, strlen(src));
    }

    static std::pair<conversion_result, std::size_t>
    copyString8to32(char32_t* dst, std::size_t dstSize, const char* src) noexcept
    {
        return copyString8to32(dst, dstSize, reinterpret_cast<const UTF8*>(src));
    }

    static constexpr std::size_t
    copyString32to8(
        UTF8* dst, std::size_t dstSize, const char32_t* src, std::size_t srcSize) noexcept
    {
        const auto sourceStart = src;
        const auto sourceEnd = sourceStart + srcSize;
        const auto targetStart = dst;
        const auto targetEnd = targetStart + dstSize;

        auto res = ConvertUTF32toUTF8(sourceStart, sourceEnd, targetStart, targetEnd);

        if (std::get<0>(res) == conversion_result::succeeded)
            return std::get<2>(res) - targetStart;
        return 0;
    }

    static constexpr std::size_t
    copyString32to8(UTF8* dst, std::size_t dstLen, const char32_t* src) noexcept
    {
        return copyString32to8(dst, dstLen, src, linse::strlen(src));
    }

    static constexpr void
    copyString32(char32_t* dst, const char32_t* src, std::size_t len) noexcept
    {
        while (len-- > 0 && *src)
            *dst++ = *src++;

        *dst = 0;
    }

    static std::int_least32_t
    strncmp(const char32_t* left, const char32_t* right, std::size_t len) noexcept
    {
        while (len-- > 0 && *left) {
            if (*left != *right)
                return static_cast<std::int_least32_t>(*left - *right);

            ++left;
            ++right;
        }
        return 0;
    }

#ifdef _WIN32

    static constexpr BYTE FOREGROUND_BLACK = 0;
    static constexpr BYTE FOREGROUND_WHITE = FOREGROUND_RED | FOREGROUND_GREEN | FOREGROUND_BLUE;
    static constexpr BYTE BACKGROUND_BLACK = 0;
    static constexpr BYTE BACKGROUND_WHITE = BACKGROUND_RED | BACKGROUND_GREEN | BACKGROUND_BLUE;
    static constexpr BYTE INTENSITY = FOREGROUND_INTENSITY | BACKGROUND_INTENSITY;

    struct WinAttributes
    {
        static constexpr std::size_t
        copyString32to16(
            char16_t* dst, std::size_t dstSize, const char32_t* src, std::size_t srcSize) noexcept
        {
            const auto sourceStart = src;
            const auto sourceEnd = sourceStart + srcSize;
            const auto targetStart = dst;
            const auto targetEnd = targetStart + dstSize;

            auto res = ConvertUTF32toUTF16(sourceStart, sourceEnd, targetStart, targetEnd);

            if (std::get<0>(res) == conversion_result::succeeded)
                return static_cast<std::size_t>(std::get<2>(res) - targetStart);
            return 0;
        }
        static inline std::size_t
        OutputWin(char16_t* text16, const char32_t* text32, std::size_t len32)
        {
            const auto count16 = copyString32to16(text16, len32, text32, len32);
            ::WriteConsoleW(GetStdHandle(STD_OUTPUT_HANDLE), text16, static_cast<DWORD>(count16),
                nullptr, nullptr);

            return count16;
        }
        WinAttributes()
        {
            ::CONSOLE_SCREEN_BUFFER_INFO info;
            ::GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info);
            _defaultAttribute = info.wAttributes & INTENSITY;
            _defaultColor = info.wAttributes & FOREGROUND_WHITE;
            _defaultBackground = info.wAttributes & BACKGROUND_WHITE;

            _consoleAttribute = _defaultAttribute;
            _consoleColor = _defaultColor | _defaultBackground;
        }

        int _defaultAttribute;
        int _defaultColor;
        int _defaultBackground;

        int _consoleAttribute;
        int _consoleColor;

        const char32_t*
        HandleEsc(const char32_t* p, const char32_t* end)
        {
            if (*p++ == '[') {
                int code = 0;

                for (; p < end; ++p) {
                    const auto c = *p;

                    if ('0' <= c && c <= '9')
                        code = code * 10 + (c - '0');
                    else if (c == 'm' || c == ';') {
                        switch (code) {
                        case 0:
                            _consoleAttribute = _defaultAttribute;
                            _consoleColor = _defaultColor | _defaultBackground;
                            break;

                        case 1: // BOLD
                        case 5: // BLINK
                            _consoleAttribute
                                = (_defaultAttribute ^ FOREGROUND_INTENSITY) & INTENSITY;
                            break;

                        case 30:
                            _consoleColor = BACKGROUND_WHITE;
                            break;

                        case 31:
                            _consoleColor = FOREGROUND_RED | _defaultBackground;
                            break;

                        case 32:
                            _consoleColor = FOREGROUND_GREEN | _defaultBackground;
                            break;

                        case 33:
                            _consoleColor = FOREGROUND_RED | FOREGROUND_GREEN | _defaultBackground;
                            break;

                        case 34:
                            _consoleColor = FOREGROUND_BLUE | _defaultBackground;
                            break;

                        case 35:
                            _consoleColor = FOREGROUND_BLUE | FOREGROUND_RED | _defaultBackground;
                            break;

                        case 36:
                            _consoleColor = FOREGROUND_BLUE | FOREGROUND_GREEN | _defaultBackground;
                            break;

                        case 37:
                            _consoleColor = FOREGROUND_GREEN | FOREGROUND_RED | FOREGROUND_BLUE
                                | _defaultBackground;
                            break;
                        }

                        code = 0;
                    }

                    if (c == 'm') {
                        ++p;
                        break;
                    }
                }
            }

            ::SetConsoleTextAttribute(
                ::GetStdHandle(STD_OUTPUT_HANDLE), _consoleAttribute | _consoleColor);

            return p;
        }

        std::size_t
        WinWrite32(char16_t* text16, const char32_t* text32, std::size_t len32)
        {
            auto p = text32;
            auto q = p;
            auto e = text32 + len32;
            std::size_t count16 = 0;

            while (p < e) {
                if (*p == 27) {
                    if (q < p)
                        count16 += OutputWin(text16, q, p - q);

                    q = p = HandleEsc(p + 1, e);
                } else
                    ++p;
            }

            if (q < p)
                count16 += OutputWin(text16, q, p - q);

            return count16;
        }
    } WIN_ATTR;

#endif

    inline int
    write32(int fd, const char32_t* text32, std::size_t len32)
    {
#ifdef _WIN32
        if (isatty(fd)) {
            auto text16 = std::make_unique<char16_t[]>(2 * len32 + 1);
            std::size_t count16 = WIN_ATTR.WinWrite32(text16.get(), text32, len32);

            return static_cast<int>(count16);
        }
#endif
        const std::size_t len8 = 4 * len32 + 1;
        auto text8 = std::make_unique<UTF8[]>(len8);
        std::size_t count8 = copyString32to8(text8.get(), len8, text32, len32);

        return ::write(fd, text8.get(), static_cast<unsigned int>(count8));
    }

    struct Utf32String
    {
        static std::u32string
        convert(const std::string_view& src)
        {
            std::u32string _data(src.size() + 1, 0);
            auto ret = copyString8to32(_data.data(), _data.size(), src.data(), src.size());
            if (ret.first == conversion_result::succeeded)
                _data.resize(ret.second);
            return _data;
        }
        static std::string
        convert(const std::u32string_view& v)
        {
            std::string buf(v.size() * 4 + 1, 0);
            auto s = copyString32to8(
                reinterpret_cast<UTF8*>(buf.data()), buf.size(), v.data(), v.size());
            buf.resize(s);
            return buf;
        }
    };

public:
    inline int
    writeStdout(const std::string& utf8string)
    {
        auto utf32 = Utf32String::convert(utf8string);
        return write32(STDOUT_FILENO, utf32.c_str(), utf32.size());
    }
    inline int
    writeStderr(const std::string& utf8string)
    {
        auto utf32 = Utf32String::convert(utf8string);
        return write32(STDERR_FILENO, utf32.c_str(), utf32.size());
    }
    void
    clearLine()
    {
#ifdef _WIN32
        HANDLE hConsole = GetStdHandle(STD_OUTPUT_HANDLE);
        if (hConsole == INVALID_HANDLE_VALUE) {
            fprintf(stderr, "Error getting console handle.\n");
            return;
        }

        // Get the current cursor position
        CONSOLE_SCREEN_BUFFER_INFO csbi;
        if (!GetConsoleScreenBufferInfo(hConsole, &csbi)) {
            fprintf(stderr, "Error getting console screen buffer info.\n");
            return;
        }

        COORD cursorPosition = csbi.dwCursorPosition;
        DWORD written;

        // Fill the line with spaces
        FillConsoleOutputCharacter(hConsole, ' ', csbi.dwSize.X, cursorPosition, &written);
        FillConsoleOutputAttribute(
            hConsole, csbi.wAttributes, csbi.dwSize.X, cursorPosition, &written);

        // Reset the cursor to the beginning of the line
        cursorPosition.X = 0;
        SetConsoleCursorPosition(hConsole, cursorPosition);
#else
        writeStdout("\33[2K\r");
#endif
    }

    struct completions
    {
        std::vector<std::u32string> completionStrings;
        std::u32string prefix;
        completions&
        add_completion(std::string_view str)
        {
            completionStrings.push_back(Utf32String::convert(str));
            return *this;
        }
        completions&
        set_prefix(std::string_view str)
        {
            prefix = Utf32String::convert(str);
            return *this;
        }
    };

    using completion_callback_type
        = std::function<completions(std::string_view, std::string_view::size_type)>;

    completion_callback_type completion_callback;

private:
    static constexpr std::size_t LINSE_DEFAULT_HISTORY_MAX_LEN = 100;
    static constexpr std::size_t LINSE_DEFAULT_LINE_CAPACITY = 4096;

    // make control-characters more readable
    template <typename T>
    static constexpr T
    ctrlChar(T upperCaseASCII) noexcept
    {
        return upperCaseASCII - 0x40;
    }

    static constexpr void
    recomputeCharacterWidths(const char32_t* text, char* widths, int charCount) noexcept
    {
        for (int i = 0; i < charCount; ++i)
            widths[i] = mk_wcwidth(text[i]);
    }

    /**
     * Calculate a new screen position given a starting position, screen width and
     * character count
     * @param x             initial x position (zero-based)
     * @param y             initial y position (zero-based)
     * @param screenColumns screen column count
     * @param charCount     character positions to advance
     * @param xOut          returned x position (zero-based)
     * @param yOut          returned y position (zero-based)
     */
    static constexpr std::pair<int, int>
    calculateScreenPosition(int x, int y, int screenColumns, int charCount) noexcept
    {
        std::pair<int, int> out = { x, y };
        int charsRemaining = charCount;
        while (charsRemaining > 0) {
            int charsThisRow
                = (x + charsRemaining < screenColumns) ? charsRemaining : screenColumns - x;
            out.first = x + charsThisRow;
            out.second = y;
            charsRemaining -= charsThisRow;
            x = 0;
            ++y;
        }
        if (out.first == screenColumns) { // we have to special-case line wrap
            out.first = 0;
            ++out.second;
        }
        return out;
    }

    static constexpr int
    calculateColumnPosition(const char32_t* buf32, int len) noexcept
    {
        const int width = mk_wcswidth(buf32, len);
        if (width == -1)
            return len;
        else
            return width;
    }

    static constexpr bool
    isControlChar(char32_t testChar) noexcept
    {
        return (testChar < ' ') || // C0 controls
            (testChar >= 0x7F && testChar <= 0x9F); // DEL and C1 controls
    }

    struct PromptBase
    { // a convenience struct for grouping prompt info
        std::u32string promptText; // our copy of the prompt text, edited
        int promptChars; // chars in promptText
        int promptBytes; // bytes in promptText
        int promptExtraLines; // extra lines (beyond 1) occupied by prompt
        int promptIndentation; // column offset to end of prompt
        int promptLastLinePosition; // index into promptText where last line begins
        int promptPreviousInputLen; // promptChars of previous input line, for
                                    // clearing
        int promptCursorRowOffset; // where the cursor is relative to the start of
                                   // the prompt
        int promptScreenColumns; // width of screen in columns
        int promptPreviousLen; // help erasing

        PromptBase() : promptPreviousInputLen(0) { }

        bool
        write(linse& self)
        {
            return self.write32(STDOUT_FILENO, promptText.data(), promptBytes) != -1;
        }
    };

    struct PromptInfo : PromptBase
    {
        PromptInfo(const char* textPtr, int columns)
        {
            promptExtraLines = 0;
            promptLastLinePosition = 0;
            promptPreviousLen = 0;
            promptScreenColumns = columns;
            std::u32string temp_unicode = Utf32String::convert(textPtr);

            // strip control characters from the prompt -- we do allow newline
            char32_t* pIn = temp_unicode.data();
            char32_t* pOut = pIn;

            int len = 0;
            int x = 0;

            const bool strip = (isatty(STDOUT_FILENO) == 0);
            const auto f = [&, strip] {
                if (!strip) {
                    // copy control chars
                    *pOut = *pIn;
                    ++pOut;
                }
                // jump over control chars
                ++pIn;
            };

            while (*pIn) {
                const char32_t c = *pIn;
                if (c == '\n' || !isControlChar(c)) {
                    *pOut = c;
                    ++pOut;
                    ++pIn;
                    ++len;
                    if (c == '\n' || ++x >= promptScreenColumns) {
                        x = 0;
                        ++promptExtraLines;
                        promptLastLinePosition = len;
                    }
                } else if (c == '\x1b') {
                    f();
                    if (*pIn == '[') {
                        f();
                        while (*pIn && ((*pIn == ';') || ((*pIn >= '0' && *pIn <= '9'))))
                            f();
                        if (*pIn == 'm')
                            f();
                    }
                } else
                    ++pIn;
            }
            *pOut = 0;
            promptChars = len;
            promptBytes = static_cast<int>(pOut - temp_unicode.data());
            temp_unicode.resize(promptBytes);
            promptText = std::move(temp_unicode);

            promptIndentation = len - promptLastLinePosition;
            promptCursorRowOffset = promptExtraLines;
        }
    };

    // Used with DynamicPrompt (history search)
    //
    static constexpr const char32_t* forwardSearchBasePrompt = U"(i-search)`";
    static constexpr const char32_t* reverseSearchBasePrompt = U"(reverse-i-search)`";
    static constexpr const char32_t* endSearchBasePrompt = U"': ";
    std::u32string previousSearchText; // remembered across invocations of linenoise()

    // changing prompt for "(reverse-i-search)`text':" etc.
    //
    struct DynamicPrompt : PromptBase
    {
        std::u32string searchText; // text we are searching for
        char* searchCharWidths; // character widths from mk_wcwidth()
        int direction; // current search direction, 1=forward, -1=reverse

        DynamicPrompt(PromptBase& pi, int initialDirection) : direction(initialDirection)
        {
            promptScreenColumns = pi.promptScreenColumns;
            promptCursorRowOffset = 0;
            const std::u32string basePrompt
                = (direction > 0) ? forwardSearchBasePrompt : reverseSearchBasePrompt;
            promptText = basePrompt + endSearchBasePrompt;
            promptBytes = promptChars = static_cast<int>(promptText.size());
            promptLastLinePosition = promptChars; // TODO fix this, we are asssuming
                                                  // that the history prompt won't wrap
                                                  // (!)
            promptPreviousLen = promptChars;
            std::tie(promptIndentation, promptExtraLines)
                = calculateScreenPosition(0, 0, promptScreenColumns, promptChars);
        }

        void
        updateSearchPrompt()
        {
            promptText = (direction > 0 ? forwardSearchBasePrompt : reverseSearchBasePrompt)
                + searchText + endSearchBasePrompt;
            promptBytes = promptChars = static_cast<int>(promptText.size());
        }

        void
        updateSearchText(const char32_t* textPtr)
        {
            searchText = textPtr;
            updateSearchPrompt();
        }
    };

    class rawmode
    {
#ifdef _WIN32
        HANDLE console_in = nullptr, console_out = nullptr;
        DWORD oldMode = 0;
#else
        bool is_rawmode = false;
        struct termios orig_termios = {}; /* in order to restore at exit */
#endif
    public:
        constexpr rawmode() = default;
#ifdef _WIN32
        ::HANDLE
        get_console_in() const
        {
            return console_in;
        }
        ::HANDLE
        get_console_out() const
        {
            return console_out;
        }
#endif
        bool
        enable()
        {
#ifdef _WIN32
            if (console_in == nullptr) {
                console_out = ::GetStdHandle(STD_OUTPUT_HANDLE);
                if (console_out == INVALID_HANDLE_VALUE)
                    goto fatal;
                else {
                    DWORD _;
                    if (!::GetConsoleMode(console_out, &_)) {
                        console_out = nullptr;
                        errno = ENOTTY;
                        return false;
                    }
                }
                console_in = ::GetStdHandle(STD_INPUT_HANDLE);
                if (console_in == INVALID_HANDLE_VALUE) {
                    console_out = nullptr;
                    console_in = nullptr;
                    errno = ENOTTY;
                    return false;
                }

                ::GetConsoleMode(console_in, &oldMode);
                ::SetConsoleMode(console_in,
                    oldMode & ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT));
            }
            return true;
#else
            struct termios raw;

            if (!isatty(STDIN_FILENO))
                goto fatal;
            if (::tcgetattr(0, &orig_termios) == -1)
                goto fatal;

            raw = orig_termios; /* modify the original mode */
            /* input modes: no break, no CR to NL, no parity check, no strip char,
             * no start/stop output control. */
            raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
            /* output modes - disable post processing */
            // this is wrong, we don't want raw output, it turns newlines into straight
            // linefeeds
            // raw.c_oflag &= ~(OPOST);
            /* control modes - set 8 bit chars */
            raw.c_cflag |= (CS8);
            /* local modes - echoing off, canonical off, no extended functions,
             * no signal chars (^Z,^C) */
            raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
            /* control chars - set return condition: min number of bytes and timer.
             * We want read to return every single byte, without timeout. */
            raw.c_cc[VMIN] = 1;
            raw.c_cc[VTIME] = 0; /* 1 byte, no timer */

            /* put terminal in raw mode after flushing */
            if (::tcsetattr(0, TCSADRAIN, &raw) < 0)
                goto fatal;
            is_rawmode = true;
            return true;
#endif
        fatal:
            errno = ENOTTY;
            return false;
        }
        void
        disable()
        {
#ifdef _WIN32
            if (oldMode) {
                ::SetConsoleMode(console_in, oldMode);
                oldMode = 0;
                console_out = nullptr;
                console_in = nullptr;
            }
#else
            if (is_rawmode && ::tcsetattr(0, TCSADRAIN, &orig_termios) != -1)
                is_rawmode = false;
#endif
        }
        bool
        is_enable() const
        {
            return
#ifdef _WIN32
                console_in != nullptr && console_out != nullptr;
#else
                is_rawmode;
#endif
        }
        ~rawmode()
        {
            if (is_enable())
                disable();
        }
    } rm;

    class KillRing
    {
        static constexpr int capacity = 10;
        std::uint8_t size;
        std::uint8_t index;
        std::uint8_t indexToSlot[capacity];
        std::vector<std::u32string> theRing;

    public:
        enum class action
        {
            other,
            kill,
            yank
        };
        action lastAction;
        std::size_t lastYankSize;

        KillRing() : size(0), index(0), lastAction(action::other) { theRing.reserve(capacity); }

        void
        kill(const char32_t* text, std::size_t textLen, bool forward)
        {
            if (textLen == 0)
                return;

            // Utf32String killedText(text, textLen);
            std::u32string killed_text(text, textLen);
            if (lastAction == action::kill && size > 0) {
                const int slot = indexToSlot[0];
                if (forward)
                    theRing[slot] += killed_text;
                else
                    theRing[slot] = killed_text + theRing[slot];
            } else {
                if (size < capacity) {
                    if (size > 0)
                        std::memmove(indexToSlot + 1, indexToSlot, size);
                    indexToSlot[0] = size++;
                    theRing.emplace_back(std::move(killed_text));
                } else {
                    const int slot = indexToSlot[capacity - 1];
                    theRing[slot] = std::move(killed_text);
                    std::memmove(indexToSlot + 1, indexToSlot, capacity - 1);
                    indexToSlot[0] = slot;
                }
                index = 0;
            }
        }

        std::u32string*
        yank()
        {
            return size > 0 ? &theRing[indexToSlot[index]] : nullptr;
        }

        std::u32string*
        yankPop()
        {
            if (size == 0)
                return nullptr;
            if (++index == size)
                index = 0;
            return &theRing[indexToSlot[index]];
        }
    } killRing;

    // Special codes for keyboard input:
    //
    // Between Windows and the various Linux "terminal" programs, there is some
    // pretty diverse behavior in the "scan codes" and escape sequences we are
    // presented with.  So ... we'll translate them all into our own pidgin
    // pseudocode, trying to stay out of the way of UTF-8 and international
    // characters.  Here's the general plan.
    //
    // "User input keystrokes" (key chords, whatever) will be encoded as a single
    // value.
    // The low 21 bits are reserved for Unicode characters.  Popular function-type
    // keys
    // get their own codes in the range 0x10200000 to (if needed) 0x1FE00000,
    // currently
    // just arrow keys, Home, End and Delete.  Keypresses with Ctrl get ORed with
    // 0x20000000, with Alt get ORed with 0x40000000.  So, Ctrl+Alt+Home is encoded
    // as 0x20000000 + 0x40000000 + 0x10A00000 == 0x70A00000.  To keep things
    // complicated,
    // the Alt key is equivalent to prefixing the keystroke with ESC, so ESC
    // followed by
    // D is treated the same as Alt + D ... we'll just use Emacs terminology and
    // call
    // this "Meta".  So, we will encode both ESC followed by D and Alt held down
    // while D
    // is pressed the same, as Meta-D, encoded as 0x40000064.
    //
    // Here are the definitions of our component constants:
    //
    // Maximum unsigned 32-bit value    = 0xFFFFFFFF;   // For reference, max 32-bit
    // value
    // Highest allocated Unicode char   = 0x001FFFFF;   // For reference, max
    // Unicode value
    static constexpr char32_t META = 0x40000000; // Meta key combination
    static constexpr char32_t CTRL = 0x20000000; // Ctrl key combination
    // static constexpr char32_t SPECIAL_KEY = 0x10000000;   // Common bit for all special
    // keys
    static constexpr char32_t UP_ARROW_KEY = 0x10200000; // Special keys
    static constexpr char32_t DOWN_ARROW_KEY = 0x10400000;
    static constexpr char32_t RIGHT_ARROW_KEY = 0x10600000;
    static constexpr char32_t LEFT_ARROW_KEY = 0x10800000;
    static constexpr char32_t HOME_KEY = 0x10A00000;
    static constexpr char32_t END_KEY = 0x10C00000;
    static constexpr char32_t DELETE_KEY = 0x10E00000;
    static constexpr char32_t PAGE_UP_KEY = 0x11000000;
    static constexpr char32_t PAGE_DOWN_KEY = 0x11200000;

    static inline std::string
    get_environment(const char* varname)
    {
#if defined _WIN32 && !(defined __GNUC__)
        char* env_p;
        std::size_t len;
        if (::_dupenv_s(&env_p, &len, varname) != 0 || env_p == nullptr)
            return std::string {};
        std::unique_ptr<char, decltype(&std::free)> liberator(env_p, &std::free);
#else
        char* env_p = ::getenv(varname);
        if (env_p == nullptr)
            return std::string {};
        std::size_t len = std::strlen(env_p);
#endif
        return std::string(env_p, len);
    }

    static bool
    isUnsupportedTerm()
    {
        static const char* unsupported_term[] = { "dumb", "cons25", "emacs", NULL };
        const auto term = get_environment("TERM");
        if (term.empty())
            return false;
        for (int j = 0; unsupported_term[j]; ++j)
            if (!strcasecmp(term.data(), unsupported_term[j]))
                return true;
        return false;
    }

    static void
    beep()
    {
        std::fprintf(stderr, "\x7"); // ctrl-G == bell/beep
        std::fflush(stderr);
    }

    /**
     * convert {CTRL + 'A'}, {CTRL + 'a'} and {CTRL + ctrlChar( 'A' )} into
     * ctrlChar( 'A' )
     * leave META alone
     *
     * @param c character to clean up
     * @return cleaned-up character
     */
    [[nodiscard]] static constexpr char32_t
    cleanupCtrl(char32_t c) noexcept
    {
        if (c & CTRL) {
            const int d = c & 0x1FF;
            if (d >= 'a' && d <= 'z')
                c = (c + ('a' - ctrlChar('A'))) & ~CTRL;
            else if (d >= 'A' && d <= 'Z')
                c = (c + ('A' - ctrlChar('A'))) & ~CTRL;
            else if (d >= ctrlChar('A') && d <= ctrlChar('Z'))
                c &= ~CTRL;
        }
        return c;
    }

    static int
    getScreenColumns()
    {
#ifdef _WIN32
        ::CONSOLE_SCREEN_BUFFER_INFO inf;
        ::GetConsoleScreenBufferInfo(::GetStdHandle(STD_OUTPUT_HANDLE), &inf);
        const int cols = inf.dwSize.X;
#else
        struct winsize ws;
        const int cols = (::ioctl(1, TIOCGWINSZ, &ws) == -1) ? 80 : ws.ws_col;
#endif
        // cols is 0 in certain circumstances like inside debugger, which creates
        // further issues
        return (cols > 0) ? cols : 80;
    }

    static int
    getScreenRows()
    {
#ifdef _WIN32
        ::CONSOLE_SCREEN_BUFFER_INFO inf;
        ::GetConsoleScreenBufferInfo(::GetStdHandle(STD_OUTPUT_HANDLE), &inf);
        const int rows = 1 + inf.srWindow.Bottom - inf.srWindow.Top;
#else
        struct winsize ws;
        const int rows = (::ioctl(1, TIOCGWINSZ, &ws) == -1) ? 24 : ws.ws_row;
#endif
        return (rows > 0) ? rows : 24;
    }

    /**
     * Clear the screen ONLY (no redisplay of anything)
     */
    static void
    ClearScreen()
    {
#ifdef _WIN32
        const ::COORD coord = { 0, 0 };
        ::CONSOLE_SCREEN_BUFFER_INFO inf;
        ::HANDLE screenHandle = ::GetStdHandle(STD_OUTPUT_HANDLE);
        ::GetConsoleScreenBufferInfo(screenHandle, &inf);
        ::SetConsoleCursorPosition(screenHandle, coord);
        ::DWORD count;
        ::FillConsoleOutputCharacterA(
            screenHandle, ' ', inf.dwSize.X * inf.dwSize.Y, coord, &count);
#else
        ssize_t written = ::write(STDOUT_FILENO, "\x1b[H\x1b[2J", 7);
#endif
    }

public:
    int
    get_screen_columns()
    {
        return getScreenColumns();
    }

    int
    get_screen_rows()
    {
        return getScreenRows();
    }

    void
    clear_screen()
    {
        ClearScreen();
    }

    void
    interruptReadLine(bool value = true)
    {
        stopInputBuffer = value;
    }

private:
    class hist : std::vector<std::string>
    {
        friend linse;
        using parent_type = std::vector<std::string>;
        int index = 0;
        // used to emulate Windows command prompt on down-arrow after a recall
        // we use -2 as our "not set" value because we add 1 to the previous index on
        // down-arrow,
        // and zero is a valid index (so -1 is a valid "previous index")
        int previousIndex = -2;
        bool recallMostRecent = false;
        using parent_type::vector;
        auto&
        operator=(parent_type&& other) noexcept(std::is_nothrow_move_assignable<parent_type>::value)
        {
            static_cast<parent_type&>(*this) = std::move(other);
            return *this;
        }

    public:
        using parent_type::clear;
        bool
        add(std::string_view line) noexcept
        try {
            if (empty())
                reserve(LINSE_DEFAULT_HISTORY_MAX_LEN);
            std::string linecopy(line);
            // convert newlines in multi-line code to spaces before storing
            for (auto&& x : linecopy)
                if (x == '\n')
                    x = ' ';
            // prevent duplicate history entries
            if (size() > 0 && !back().empty() && back() == linecopy)
                return false;
            if (size() == capacity()) {
                erase(begin());
                if (--previousIndex < -1)
                    previousIndex = -2;
            }

            emplace_back(std::move(linecopy));
            return true;
        } catch (std::bad_alloc&) {
            return false;
        }

        bool
        set_max_len(std::size_t len) noexcept
        try {
            if (len == 0)
                return false;
            if (size() <= len)
                reserve(len);
            else {
                std::vector<std::string> new_history(len);
                std::move(rbegin(), rbegin() + len, new_history.rbegin());
                *this = std::move(new_history);
            }
            return true;
        } catch (std::bad_alloc&) {
            return false;
        }

        /* Fetch a line of the history by (zero-based) index.  If the requested
         * line does not exist, NULL is returned.  The return value is a heap-allocated
         * copy of the line, and the caller is responsible for de-allocating it. */
        std::optional<std::string>
        operator()(std::size_t index) noexcept
        {
            if (index >= size())
                return std::nullopt;

            return (*this)[index];
        }

        /* Save the history in the specified file. On success true is returned
         * otherwise false is returned. */
        bool
        save(const std::filesystem::path& filename) const
        {
#ifdef _WIN32
            FILE* fp = fopen(filename.c_str(), L"wt");
#else
            int fd = open(filename.c_str(), O_CREAT | O_TRUNC | O_WRONLY, S_IWUSR | S_IRUSR);
            if (fd < 0)
                return false;
            FILE* fp = fdopen(fd, "wt");
#endif
            if (fp == nullptr)
                return false;

            for (const auto& x : *this)
                if (x[0] != '\0')
                    std::fprintf(fp, "%s\n", x.data());
            std::fclose(fp);
            return true;
        }

        /* Load the history from the specified file. If the file does not exist
         * zero is returned and no operation is performed.
         *
         * If the file exists and the operation succeeded true is returned, otherwise
         * on error false is returned. */
        bool
        load(const std::filesystem::path& filename)
        {
#ifndef _WIN32
            using std::fopen;
#endif
            std::ifstream ifs(filename);
            if (!ifs)
                return false;
            std::string buf;
            while (std::getline(ifs, buf))
                add(buf);
            return true;
        }
    };

public:
    hist history;

private:
    static inline bool
    isCharacterAlphanumeric(char32_t testChar)
    {
        return iswalnum(
#ifdef _WIN32
                   static_cast<wint_t>
#endif
                   (testChar))
            != 0;
    }

    class InputBuffer
    {
    private:
        std::vector<char32_t> buf32; // input buffer
        typename decltype(buf32)::size_type pos
            = 0; // character position in buffer ( 0 <= pos <= len )

        void
        clearScreen(PromptBase& pi, linse& ln)
        {
            linse::ClearScreen();
            write_and_refresh(pi, ln);
        }
        // linenoiseReadChar -- read a keystroke or keychord from the keyboard, and
        // translate it
        // into an encoded "keystroke".  When convenient, extended keys are translated
        // into their
        // simpler Emacs keystrokes, so an unmodified "left arrow" becomes Ctrl-B.
        //
        // A return value of zero means "no input available", and a return value of -1
        // means "invalid key".
        //
#ifdef _WIN32
        char32_t
        linenoiseReadChar([[maybe_unused]] rawmode& rm)
        {
            ::INPUT_RECORD rec;
            ::DWORD count;
            int modifierKeys = 0;
            bool escSeen = false;
            void* veval = Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
            Nelson::Evaluator* eval = (Nelson::Evaluator*)veval;
            eval->commandQueue.clear();

            while (true) {
                bool waitKey = true;
                while (waitKey) {
                    try {
                        ProcessEventsDynamicFunctionWithoutWait();
                    } catch (const Nelson::Exception& e) {
                        e.printMe(eval->getInterface());
                        stopInputBuffer = true;
                    }
                    std::this_thread::sleep_for(std::chrono::milliseconds(10));
                    if (stopInputBuffer) {
                        return -1;
                    }
                    if (::GetNumberOfConsoleInputEvents(rm.get_console_in(), &count) && count > 0) {
                        ::ReadConsoleInputW(rm.get_console_in(), &rec, 1, &count);
                        waitKey = false;
                    }

                    if (!eval->commandQueue.isEmpty()
                        || !Nelson::CallbackQueue::getInstance()->isEmpty()) {
                        stopInputBuffer = false;
                        return -1;
                    }
                }
                if (stopInputBuffer) {
                    stopInputBuffer = false;
                    return -1;
                }
                if (rec.EventType != KEY_EVENT)
                    continue;
                // Windows provides for entry of characters that are not on your keyboard by
                // sending the
                // Unicode characters as a "key up" with virtual keycode 0x12 (VK_MENU ==
                // Alt key) ...
                // accept these characters, otherwise only process characters on "key down"
                if (!rec.Event.KeyEvent.bKeyDown && rec.Event.KeyEvent.wVirtualKeyCode != VK_MENU)
                    continue;
                modifierKeys = 0;
                // AltGr is encoded as ( LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED ), so don't
                // treat this
                // combination as either CTRL or META we just turn off those two bits, so it
                // is still
                // possible to combine CTRL and/or META with an AltGr key by using
                // right-Ctrl and/or
                // left-Alt
                if ((rec.Event.KeyEvent.dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED))
                    == (LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED))
                    rec.Event.KeyEvent.dwControlKeyState
                        &= ~(LEFT_CTRL_PRESSED | RIGHT_ALT_PRESSED);
                if (rec.Event.KeyEvent.dwControlKeyState & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED))
                    modifierKeys |= CTRL;
                if (rec.Event.KeyEvent.dwControlKeyState & (RIGHT_ALT_PRESSED | LEFT_ALT_PRESSED))
                    modifierKeys |= META;
                if (escSeen)
                    modifierKeys |= META;
                if (rec.Event.KeyEvent.uChar.UnicodeChar == 0)
                    switch (rec.Event.KeyEvent.wVirtualKeyCode) {
                    case VK_LEFT:
                        return modifierKeys | LEFT_ARROW_KEY;
                    case VK_RIGHT:
                        return modifierKeys | RIGHT_ARROW_KEY;
                    case VK_UP:
                        return modifierKeys | UP_ARROW_KEY;
                    case VK_DOWN:
                        return modifierKeys | DOWN_ARROW_KEY;
                    case VK_DELETE:
                        return modifierKeys | DELETE_KEY;
                    case VK_HOME:
                        return modifierKeys | HOME_KEY;
                    case VK_END:
                        return modifierKeys | END_KEY;
                    case VK_PRIOR:
                        return modifierKeys | PAGE_UP_KEY;
                    case VK_NEXT:
                        return modifierKeys | PAGE_DOWN_KEY;
                    default:
                        continue; // in raw mode, ReadConsoleInput shows shift, ctrl ...
                    } //  ... ignore them
                else if (rec.Event.KeyEvent.uChar.UnicodeChar
                    == ctrlChar('[')) { // ESC, set flag for later
                    escSeen = true;
                    continue;
                } else
                    // we got a real character, return it
                    return modifierKeys | rec.Event.KeyEvent.uChar.UnicodeChar;
            }
        }
#else

        static int
        kbhit()
        {
            struct timeval tv;
            fd_set fds;
            tv.tv_sec = 0;
            tv.tv_usec = 0;
            FD_ZERO(&fds);
            FD_SET(STDIN_FILENO, &fds); // STDIN_FILENO is 0
            select(STDIN_FILENO + 1, &fds, nullptr, nullptr, &tv);
            return FD_ISSET(STDIN_FILENO, &fds) || stopInputBuffer;
        }

        static char32_t
        readUnicodeCharacter()
        {
            char8_t string[5];
            std::size_t count = 0;

            void* veval = Nelson::NelsonConfiguration::getInstance()->getMainEvaluator();
            Nelson::Evaluator* eval = (Nelson::Evaluator*)veval;
            eval->commandQueue.clear();

            while (true) {
                char8_t c;
                ::ssize_t nread = 0;

                while (!kbhit()) {
                    try {
                        ProcessEventsDynamicFunctionWithoutWait();
                    } catch (const Nelson::Exception& e) {
                        e.printMe(eval->getInterface());
                        stopInputBuffer = true;
                    }
                    std::this_thread::sleep_for(std::chrono::milliseconds(10));
                    if (stopInputBuffer) {
                        return -1;
                    }
                    if (!eval->commandQueue.isEmpty()
                        || !Nelson::CallbackQueue::getInstance()->isEmpty()) {
                        return -1;
                    }
                }
                if (stopInputBuffer) {
                    stopInputBuffer = false;
                    return -1;
                }
                nread = ::read(STDIN_FILENO, &c, 1);
                /* Continue reading if interrupted by signal. */
                if (nread <= 0)
                    return 0;
                if (c <= 0x7F) { // short circuit ASCII
                    count = 0;
                    return c;
                } else if (count < sizeof(string) / sizeof(char8_t) - 1) {
                    string[count++] = c;
                    string[count] = 0;
                    char32_t unicode_char[2];
                    const auto res = copyString8to32(unicode_char, 2, string, count);
                    if (std::get<0>(res) == conversion_result::succeeded && std::get<1>(res) != 0) {
                        count = 0;
                        return unicode_char[0];
                    }
                } else
                    count = 0; // this shouldn't happen: got four bytes but no UTF-8 character
            }
        }

        char32_t thisKeyMetaCtrl = 0; // holds pre-set Meta and/or Ctrl modifiers

        // Final dispatch routines -- return something
        //
        char32_t
        upArrowKeyRoutine() const
        {
            return thisKeyMetaCtrl | UP_ARROW_KEY;
        }
        char32_t
        downArrowKeyRoutine() const
        {
            return thisKeyMetaCtrl | DOWN_ARROW_KEY;
        }
        char32_t
        rightArrowKeyRoutine() const
        {
            return thisKeyMetaCtrl | RIGHT_ARROW_KEY;
        }
        char32_t
        leftArrowKeyRoutine() const
        {
            return thisKeyMetaCtrl | LEFT_ARROW_KEY;
        }
        char32_t
        homeKeyRoutine() const
        {
            return thisKeyMetaCtrl | HOME_KEY;
        }
        char32_t
        endKeyRoutine() const
        {
            return thisKeyMetaCtrl | END_KEY;
        }
        char32_t
        escFailureRoutine() const
        {
            beep();
            return -1;
        }

        // Handle ESC [ <more stuff> escape sequences
        //
        char32_t
        doA2FDispatch(char32_t c)
        {
            switch (c) {
            case 'A':
                return upArrowKeyRoutine();
            case 'B':
                return downArrowKeyRoutine();
            case 'C':
                return rightArrowKeyRoutine();
            case 'D':
                return leftArrowKeyRoutine();
            case 'H':
                return homeKeyRoutine();
            case 'F':
                return endKeyRoutine();
            default:
                return -1;
            }
        }

        // Initial ESC dispatch -- could be a Meta prefix or the start of an escape
        // sequence
        //
        char32_t
        doEscDispatch()
        {
            char32_t c;
            do {
                switch (c = readUnicodeCharacter()) {
                case 0:
                    return 0;
                case '[':
                    if (const auto a2f = doA2FDispatch(c = readUnicodeCharacter());
                        a2f != static_cast<char32_t>(-1))
                        return a2f;
                    switch (c) {
                    case 0:
                        return 0;
                    case '1': // escLeftBracket1Routine
                    case '3': // escLeftBracket3Routine
                    case '4': // escLeftBracket4Routine
                    case '5': // escLeftBracket5Routine
                    case '6': // escLeftBracket6Routine
                    case '7': // escLeftBracket7Routine
                    case '8': // escLeftBracket8Routine
                    {
                        const auto d = readUnicodeCharacter();
                        if (d == 0)
                            return 0;
                        else if (d == '~')
                            switch (c) {
                            case '3': // deleteKeyRoutine
                                return thisKeyMetaCtrl | DELETE_KEY;
                            case '5': // pageUpKeyRoutine
                                return thisKeyMetaCtrl | PAGE_UP_KEY;
                            case '6': // pageDownKeyRoutine
                                return thisKeyMetaCtrl | PAGE_DOWN_KEY;
                            case '1':
                            case '7':
                                return homeKeyRoutine();
                            case '4':
                            case '8':
                                return endKeyRoutine();
                            }
                        else if (d == ';' && c == '1') {
                            c = readUnicodeCharacter();
                            if (c == 0)
                                return 0;
                            else if (c == '3' || c == '5') {
                                thisKeyMetaCtrl |= (c == '3' ? META : CTRL);
                                switch (readUnicodeCharacter()) {
                                case 0:
                                    return 0;
                                case 'A':
                                    return upArrowKeyRoutine();
                                case 'B':
                                    return downArrowKeyRoutine();
                                case 'C':
                                    return rightArrowKeyRoutine();
                                case 'D':
                                    return leftArrowKeyRoutine();
                                }
                            }
                        }
                    }
                        [[fallthrough]];
                    case '0': // escLeftBracket0Routine
                    case '2': // escLeftBracket2Routine
                    case '9': // escLeftBracket9Routine
                    default:
                        return escFailureRoutine();
                    }
                case 'O':
                    if (const auto a2f = doA2FDispatch(c = readUnicodeCharacter());
                        a2f != static_cast<char32_t>(-1))
                        return a2f;
                    switch (c) {
                    case 0:
                        return 0;
                    case 'a': // ctrlUpArrowKeyRoutine
                        return thisKeyMetaCtrl | CTRL | UP_ARROW_KEY;
                    case 'b': // ctrlDownArrowKeyRoutine
                        return thisKeyMetaCtrl | CTRL | DOWN_ARROW_KEY;
                    case 'c': // ctrlRightArrowKeyRoutine
                        return thisKeyMetaCtrl | CTRL | RIGHT_ARROW_KEY;
                    case 'd': // ctrlLeftArrowKeyRoutine
                        return thisKeyMetaCtrl | CTRL | LEFT_ARROW_KEY;
                    }
                    return escFailureRoutine();
                }
                // Special handling for the ESC key because it does double duty
                // setMetaRoutine
                thisKeyMetaCtrl = META;
            } while (c == 0x1B); // another ESC, stay in ESC processing mode
            return EscapeSequenceProcessing(c);
        }

        char32_t
        EscapeSequenceProcessing(char32_t c)
        {
            switch (c) {
            case '\x1B':
                return doEscDispatch();
            case '\x7F':
                return thisKeyMetaCtrl | ctrlChar('H');
            }
            return thisKeyMetaCtrl | c; // normalKeyRoutine
        }

        char32_t
        linenoiseReadChar([[maybe_unused]] rawmode& rm)
        {
            char32_t c = readUnicodeCharacter();
            if (c == 0)
                return 0;

            return EscapeSequenceProcessing(c);
        }
#endif // #_WIN32

        /**
         * Incremental history search -- take over the prompt and keyboard as the user
         * types a search
         * string, deletes characters from it, changes direction, and either accepts the
         * found line (for
         * execution orediting) or cancels.
         * @param pi        PromptBase struct holding information about the (old,
         * static) prompt and our
         *                  screen position
         * @param startChar the character that began the search, used to set the initial
         * direction
         */
        void
        replace_history_back_with_buf32(hist& history)
        {
            if (history.index == static_cast<std::make_signed_t<std::size_t>>(history.size()) - 1)
                history.back()
                    = Utf32String::convert(std::u32string_view(buf32.data(), buf32.size()));
        }
        int
        incrementalHistorySearch(PromptBase& pi, int startChar, hist& history,
            std::u32string& previousSearchText, linse& ln)
        {
            auto&& rm = ln.rm;
            // if not already recalling, add the current line to the history list so we
            // don't have to
            // special case it
            replace_history_back_with_buf32(history);
            std::size_t historyLineLength = buf32.size();
            std::size_t historyLinePosition = pos;
            empty_refresh_line(pi, ln); // erase the old input first

            DynamicPrompt dp(pi, (startChar == ctrlChar('R')) ? -1 : 1);
            dp.promptPreviousLen = pi.promptPreviousLen;
            dp.promptPreviousInputLen = pi.promptPreviousInputLen;
            dynamicRefresh(dp, buf32.data(), (int)historyLineLength, (int)historyLinePosition,
                ln); // draw user's text with our prompt

            // loop until we get an exit character
            int c;
            bool keepLooping = true;
            bool useSearchedLine = true;
            bool searchAgain = false;
            std::u32string activeHistoryLine;
            while (keepLooping) {
                c = cleanupCtrl(linenoiseReadChar(rm)); // convert CTRL + <char> into normal ctrl

                switch (c) {
                // these characters keep the selected text but do not execute it
                case ctrlChar('A'): // ctrl-A, move cursor to start of line
                case HOME_KEY:
                case ctrlChar('B'): // ctrl-B, move cursor left by one character
                case LEFT_ARROW_KEY:
                case META + 'b': // meta-B, move cursor left by one word
                case META + 'B':
                case CTRL + LEFT_ARROW_KEY:
                case META + LEFT_ARROW_KEY: // Emacs allows Meta, bash & readline don't
                case ctrlChar('D'):
                case META + 'd': // meta-D, kill word to right of cursor
                case META + 'D':
                case ctrlChar('E'): // ctrl-E, move cursor to end of line
                case END_KEY:
                case ctrlChar('F'): // ctrl-F, move cursor right by one character
                case RIGHT_ARROW_KEY:
                case META + 'f': // meta-F, move cursor right by one word
                case META + 'F':
                case CTRL + RIGHT_ARROW_KEY:
                case META + RIGHT_ARROW_KEY: // Emacs allows Meta, bash & readline don't
                case META + ctrlChar('H'):
                case ctrlChar('J'):
                case ctrlChar('K'): // ctrl-K, kill from cursor to end of line
                case ctrlChar('M'):
                case ctrlChar('N'): // ctrl-N, recall next line in history
                case ctrlChar('P'): // ctrl-P, recall previous line in history
                case DOWN_ARROW_KEY:
                case UP_ARROW_KEY:
                case ctrlChar('T'): // ctrl-T, transpose characters
                case ctrlChar('U'): // ctrl-U, kill all characters to the left of the cursor
                case ctrlChar('W'):
                case META + 'y': // meta-Y, "yank-pop", rotate popped text
                case META + 'Y':
                case 127:
                case DELETE_KEY:
                case META + '<': // start of history
                case PAGE_UP_KEY:
                case META + '>': // end of history
                case PAGE_DOWN_KEY:
                    keepLooping = false;
                    break;

                // these characters revert the input line to its previous state
                case ctrlChar('C'): // ctrl-C, abort this line
                case ctrlChar('G'):
                case ctrlChar('L'): // ctrl-L, clear screen and redisplay line
                    keepLooping = false;
                    useSearchedLine = false;
                    if (c != ctrlChar('L'))
                        c = -1; // ctrl-C and ctrl-G just abort the search and do nothing
                                // else
                    break;

                // these characters stay in search mode and update the display
                case ctrlChar('S'):
                case ctrlChar('R'):
                    if (dp.searchText.size()
                        == 0) // if no current search text, recall previous text
                        if (previousSearchText.length())
                            dp.updateSearchText(previousSearchText.data());
                    if ((dp.direction == 1 && c == ctrlChar('R'))
                        || (dp.direction == -1 && c == ctrlChar('S'))) {
                        dp.direction = 0 - dp.direction; // reverse direction
                        dp.updateSearchPrompt(); // change the prompt
                    } else
                        searchAgain = true; // same direction, search again
                    break;

                    // job control is its own thing
#ifndef _WIN32
                case ctrlChar('Z'): // ctrl-Z, job control
                    rm.disable(); // Returning to Linux (whatever) shell, leave raw
                                  // mode
                    ::raise(SIGSTOP); // Break out in mid-line
                    rm.enable(); // Back from Linux shell, re-enter raw mode
                    {
                        const std::size_t bufferSize = historyLineLength + 1;
                        auto tempUnicode = std::make_unique<char32_t[]>(bufferSize);
                        copyString8to32(tempUnicode.get(), bufferSize,
                            history[history.index].data(), history[history.index].size());
                        dynamicRefresh(
                            dp, tempUnicode.get(), historyLineLength, historyLinePosition, ln);
                    }
                    continue;
                    // break;
#endif

                // these characters update the search string, and hence the selected input
                // line
                case ctrlChar('H'): // backspace/ctrl-H, delete char to left of cursor
                    if (dp.searchText.size() > 0) {
                        dp.searchText.pop_back();
                        dp.updateSearchPrompt();
                    } else
                        beep();
                    break;

                case ctrlChar('Y'): // ctrl-Y, yank killed text
                    break;

                default:
                    if (!isControlChar(c) && c <= 0x0010FFFF) { // not an action character
                        dp.searchText.push_back(c);
                        dp.updateSearchPrompt();
                    } else
                        beep();
                } // switch

                // if we are staying in search mode, search now
                if (keepLooping) {
                    activeHistoryLine = Utf32String::convert(history[history.index]);
                    if (dp.searchText.size() > 0) {
                        bool found = false;
                        int historySearchIndex = history.index;
                        std::size_t lineLength = activeHistoryLine.size();
                        std::make_signed_t<std::size_t> lineSearchPos
                            = static_cast<std::make_signed_t<std::size_t>>(historyLinePosition);
                        if (std::exchange(searchAgain, false))
                            lineSearchPos += dp.direction;
                        while (true) {
                            while ((dp.direction > 0) ? (lineSearchPos
                                       < static_cast<std::make_signed_t<std::size_t>>(lineLength))
                                                      : (lineSearchPos >= 0)) {
                                if (strncmp(dp.searchText.data(), &activeHistoryLine[lineSearchPos],
                                        dp.searchText.size())
                                    == 0) {
                                    found = true;
                                    break;
                                }
                                lineSearchPos += dp.direction;
                            }
                            if (found) {
                                history.index = historySearchIndex;
                                historyLineLength = static_cast<int>(lineLength);
                                historyLinePosition = lineSearchPos;
                                break;
                            } else if ((dp.direction > 0) ? (historySearchIndex
                                           < static_cast<std::make_signed_t<std::size_t>>(
                                                 history.size())
                                               - 1)
                                                          : (historySearchIndex > 0)) {
                                historySearchIndex += dp.direction;
                                activeHistoryLine
                                    = Utf32String::convert(history[historySearchIndex]);
                                lineLength = activeHistoryLine.size();
                                lineSearchPos = (dp.direction > 0)
                                    ? 0
                                    : static_cast<int>(lineLength - dp.searchText.size());
                            } else {
                                beep();
                                break;
                            }
                        } // while
                    }
                    activeHistoryLine = Utf32String::convert(history[history.index]);
                    dynamicRefresh(dp, activeHistoryLine.data(), (int)historyLineLength,
                        (int)historyLinePosition, ln); // draw user's text with our prompt
                }
            } // while

            // leaving history search, restore previous prompt, maybe make searched line
            // current
            PromptBase pb;
            pb.promptChars = pi.promptIndentation;
            pb.promptBytes = pi.promptBytes;
            std::u32string tempUnicode(&pi.promptText[pi.promptLastLinePosition],
                pb.promptBytes - pi.promptLastLinePosition);
            pb.promptText = std::move(tempUnicode);
            pb.promptExtraLines = 0;
            pb.promptIndentation = pi.promptIndentation;
            pb.promptLastLinePosition = 0;
            pb.promptPreviousInputLen = (int)historyLineLength;
            pb.promptCursorRowOffset = dp.promptCursorRowOffset;
            pb.promptScreenColumns = pi.promptScreenColumns;
            pb.promptPreviousLen = dp.promptChars;
            if (useSearchedLine && !activeHistoryLine.empty()) {
                history.recallMostRecent = true;
                buf32.assign(activeHistoryLine.cbegin(), activeHistoryLine.cend());
                pos = historyLinePosition;
            }
            dynamicRefresh(pb, buf32.data(), (int)buf32.size(), (int)pos,
                ln); // redraw the original prompt with current input
            pi.promptPreviousInputLen = (int)buf32.size();
            pi.promptCursorRowOffset = pi.promptExtraLines + pb.promptCursorRowOffset;
            previousSearchText
                = dp.searchText; // save search text for possible reuse on ctrl-R ctrl-R
            return c; // pass a character or -1 back to main loop
        }
        void
        refresh_at_tail(PromptBase& pi, linse& ln)
        {
            const int backup = (int)pos;
            pos = buf32.size();
            refreshLine(pi, ln);
            pos = backup;
        }
        [[nodiscard]] int
        read_char_and_cleanup_ctrl(rawmode& rm)
        {
            char32_t c;
            do {
                c = cleanupCtrl(linenoiseReadChar(rm));
            } while (c == static_cast<char32_t>(-1));
            return c;
        }
        /**
         * Handle command completion, using a completionCallback() routine to provide
         * possible substitutions
         * This routine handles the mechanics of updating the user's input buffer with
         * possible replacement
         * of text as the user selects a proposed completion string, or cancels the
         * completion attempt.
         * @param pi     PromptBase struct holding information about the prompt and our
         * screen position
         */
        int
        completeLine(PromptBase& pi, linse& ln)
        {
            auto&& rm = ln.rm;

            completions lc = [&] {
                std::vector<char> widths(pos);
                recomputeCharacterWidths(buf32.data(), widths.data(), (int)pos);
                return ln.completion_callback(
                    Utf32String::convert(std::u32string_view { buf32.data(), buf32.size() }),
                    std::accumulate(widths.begin(), widths.end(), 0));
            }();
            int itemLength = (int)pos;
            int startIndex = (int)pos;

            // if no completions, we are done
            if (lc.completionStrings.size() == 0) {
                beep();
                return 0;
            }

            // at least one completion
            std::size_t longestCommonPrefix = 0;
            int displayLength = 0;
            if (lc.completionStrings.size() == 1)
                longestCommonPrefix = lc.completionStrings[0].size();
            else {
                const auto shortest_length = std::accumulate(lc.completionStrings.begin(),
                    lc.completionStrings.end(), std::numeric_limits<std::size_t>::max(),
                    [](std::size_t a, const std::u32string& s) { return std::min(a, s.size()); });
                for (; longestCommonPrefix < shortest_length; ++longestCommonPrefix)
                    if (std::any_of(lc.completionStrings.begin() + 1, lc.completionStrings.end(),
                            [&](const std::u32string& str) {
                                return lc.completionStrings[0][longestCommonPrefix]
                                    != str[longestCommonPrefix];
                            }))
                        break;
                beep(); // beep if ambiguous
            }

            // if we can extend the item, extend it and return to main loop
            if (longestCommonPrefix != 0) {
                displayLength = (int)(buf32.size() + longestCommonPrefix);
                {
                    std::vector<char32_t> remain(buf32.begin() + pos,
                        buf32.begin() + pos + (displayLength - (startIndex + longestCommonPrefix)));
                    buf32.resize(startIndex);
                    buf32.insert(buf32.end(), lc.completionStrings[0].cbegin(),
                        lc.completionStrings[0].cbegin() + longestCommonPrefix);
                    buf32.insert(buf32.end(), remain.begin(), remain.end());
                }
                pos = startIndex + longestCommonPrefix;
                refreshLine(pi, ln);
                return 0;
            }

            // we can't complete any further, wait for second tab
            char32_t c = read_char_and_cleanup_ctrl(rm);

            // if any character other than tab, pass it to the main loop
            if (c != ctrlChar('I'))
                return c;

            // maximum number of completions to display without asking
            static constexpr std::size_t completionCountCutoff = 100;

            // we got a second tab, maybe show list of possible completions
            bool showCompletions = true;
            bool onNewLine = false;
            if (lc.completionStrings.size() > completionCountCutoff) {
                refresh_at_tail(pi, ln); // move cursor to EOL to avoid overwriting the command line
                std::printf(
                    "\nDisplay all %zu possibilities? (y or n)", lc.completionStrings.size());
                std::fflush(stdout);
                onNewLine = true;
                while (c != 'y' && c != 'Y' && c != 'n' && c != 'N' && c != ctrlChar('C'))
                    c = read_char_and_cleanup_ctrl(rm);
                switch (c) {
                case 'n':
                case 'N':
                    showCompletions = false;
                    break;
                case ctrlChar('C'):
                    showCompletions = false;
                    if (write(STDOUT_FILENO, "^C", 2) == -1) // Display the ^C we got
                        return -1;
                    c = 0;
                    break;
                }
            }

            // if showing the list, do it the way readline does it
            bool stopList = false;
            if (showCompletions) {
                int longestCompletion = 0;
                for (const auto& s : lc.completionStrings) {
                    itemLength = static_cast<int>(s.length());
                    if (itemLength > longestCompletion)
                        longestCompletion = itemLength;
                }
                longestCompletion += 2 + (int)lc.prefix.size();
                int columnCount = pi.promptScreenColumns / longestCompletion;
                if (columnCount < 1)
                    columnCount = 1;
                if (!onNewLine) // skip this if we showed "Display all %d possibilities?"
                    refresh_at_tail(
                        pi, ln); // move cursor to EOL to avoid overwriting the command line
                std::size_t pauseRow = getScreenRows() - 1;
                std::size_t rowCount
                    = (lc.completionStrings.size() + columnCount - 1) / columnCount;
                for (std::size_t row = 0; row < rowCount; ++row) {
                    if (row == pauseRow) {
                        printf("\n--More--");
                        fflush(stdout);
                        c = read_char_and_cleanup_ctrl(rm);
                        while (c != ' ' && c != '\r' && c != '\n' && c != 'y' && c != 'Y'
                            && c != 'n' && c != 'N' && c != 'q' && c != 'Q' && c != ctrlChar('C')) {
                            beep();
                            c = read_char_and_cleanup_ctrl(rm);
                        }
                        switch (c) {
                        case ' ':
                        case 'y':
                        case 'Y':
                            printf("\r        \r");
                            pauseRow += getScreenRows() - 1;
                            break;
                        case '\r':
                        case '\n':
                            printf("\r        \r");
                            ++pauseRow;
                            break;
                        case 'n':
                        case 'N':
                        case 'q':
                        case 'Q':
                            printf("\r        \r");
                            stopList = true;
                            break;
                        case ctrlChar('C'):
                            if (write(STDOUT_FILENO, "^C", 2) == -1) // Display the ^C we got
                                return -1;
                            stopList = true;
                            break;
                        }
                    } else
                        printf("\n");
                    if (stopList)
                        break;
                    for (int column = 0; column < columnCount; ++column) {
                        std::size_t index = (column * rowCount) + row;
                        if (index < lc.completionStrings.size()) {
                            itemLength = static_cast<int>(lc.completionStrings[index].length());
                            std::fflush(stdout);
                            if (ln.write32(STDOUT_FILENO, lc.prefix.data(), lc.prefix.size()) == -1)
                                return -1;
                            if (ln.write32(
                                    STDOUT_FILENO, lc.completionStrings[index].data(), itemLength)
                                == -1)
                                return -1;
                            if (index + rowCount < lc.completionStrings.size())
                                for (int k = static_cast<int>(lc.prefix.size()) + itemLength;
                                     k < longestCompletion; ++k)
                                    std::printf(" ");
                        }
                    }
                }
                std::fflush(stdout);
            }

            // display the prompt on a new line, then redisplay the input buffer
            if (!stopList || c == ctrlChar('C'))
                if (write(STDOUT_FILENO, "\n", 1) == -1)
                    return 0;
            write_and_refresh(pi, ln);
            return 0;
        }

        struct dynamicRefreshBaseData
        {
            int prompt_indentation;
#ifdef _WIN32
            int prompt_extra_lines;
            int prompt_previous_input_len;
            int* prompt_previous_len;
#endif
        };

        struct dynamicRefreshBaseFinalizeData
        {
            int xCursorPos;
            int yCursorPos;
#ifndef _WIN32
            int xEndOfInput;
#endif
            int yEndOfInput;
        };

        static std::optional<dynamicRefreshBaseFinalizeData>
        dynamicRefreshBase(PromptBase& pi, char32_t* buf32, int len, int pos,
            std::pair<int, int> calcScrPosRes, const dynamicRefreshBaseData& rbd,
            [[maybe_unused]] rawmode& rm)
        {
            // calculate the position of the end of the input line
            const auto [xEndOfInput, yEndOfInput] = calculateScreenPosition(calcScrPosRes.first,
                calcScrPosRes.second, pi.promptScreenColumns, calculateColumnPosition(buf32, len));
            // calculate the desired position of the cursor
            const auto [xCursorPos, yCursorPos] = calculateScreenPosition(calcScrPosRes.first,
                calcScrPosRes.second, pi.promptScreenColumns, calculateColumnPosition(buf32, pos));

#ifdef _WIN32
            // position at the start of the prompt, clear to end of previous input
            ::CONSOLE_SCREEN_BUFFER_INFO inf;
            ::GetConsoleScreenBufferInfo(rm.get_console_out(), &inf);
            inf.dwCursorPosition.X = rbd.prompt_indentation;
            inf.dwCursorPosition.Y -= pi.promptCursorRowOffset - rbd.prompt_extra_lines;
            ::SetConsoleCursorPosition(rm.get_console_out(), inf.dwCursorPosition);
            ::DWORD count;
            if (len < rbd.prompt_previous_input_len)
                ::FillConsoleOutputCharacterA(rm.get_console_out(), ' ',
                    *rbd.prompt_previous_len + pi.promptPreviousInputLen, inf.dwCursorPosition,
                    &count);
            *rbd.prompt_previous_len = pi.promptIndentation;
            pi.promptPreviousInputLen = len;
            return dynamicRefreshBaseFinalizeData { xCursorPos, yCursorPos, yEndOfInput };
#else // _WIN32
            char seq[64];
            const int cursorRowMovement = pi.promptCursorRowOffset - pi.promptExtraLines;
            if (cursorRowMovement > 0) { // move the cursor up as required
                snprintf(seq, sizeof seq, "\x1b[%dA", cursorRowMovement);
                if (::write(STDOUT_FILENO, seq, strlen(seq)) == -1)
                    return std::nullopt;
            }
            // position at the start of the prompt, clear to end of screen
            snprintf(seq, sizeof seq, "\x1b[%dG\x1b[J",
                rbd.prompt_indentation + 1); // 1-based on VT100
            if (::write(STDOUT_FILENO, seq, strlen(seq)) == -1)
                return std::nullopt;
            return dynamicRefreshBaseFinalizeData { xCursorPos, yCursorPos, xEndOfInput,
                yEndOfInput };
#endif
        }

        static void
        dynamicRefreshBaseFinalize(PromptBase& pi, const dynamicRefreshBaseFinalizeData& rbfd,
            [[maybe_unused]] rawmode& rm)
        {
#ifdef _WIN32
            // position the cursor
            ::CONSOLE_SCREEN_BUFFER_INFO inf;
            ::GetConsoleScreenBufferInfo(rm.get_console_out(), &inf);
            inf.dwCursorPosition.X = rbfd.xCursorPos; // 0-based on Win32
            inf.dwCursorPosition.Y -= rbfd.yEndOfInput - rbfd.yCursorPos;
            ::SetConsoleCursorPosition(rm.get_console_out(), inf.dwCursorPosition);
#else
            char seq[64];
            // we have to generate our own newline on line wrap
            if (rbfd.xEndOfInput == 0 && rbfd.yEndOfInput > 0)
                if (::write(STDOUT_FILENO, "\n", 1) == -1)
                    return;

            // position the cursor
            const int cursorRowMovement = rbfd.yEndOfInput - rbfd.yCursorPos;
            if (cursorRowMovement > 0) { // move the cursor up as required
                snprintf(seq, sizeof seq, "\x1b[%dA", cursorRowMovement);
                if (::write(STDOUT_FILENO, seq, strlen(seq)) == -1)
                    return;
            }
            // position the cursor within the line
            snprintf(seq, sizeof seq, "\x1b[%dG", rbfd.xCursorPos + 1); // 1-based on VT100
            if (::write(STDOUT_FILENO, seq, strlen(seq)) == -1)
                return;
#endif
            pi.promptCursorRowOffset
                = pi.promptExtraLines + rbfd.yCursorPos; // remember row for next pass
        }

        template <bool enhancedDisplay>
        static std::uint16_t
        setDisplayAttribute(
            [[maybe_unused]] rawmode& rm, [[maybe_unused]] std::uint16_t oldDisplayAttribute = 0u)
        {
#ifdef _WIN32
            if constexpr (enhancedDisplay) {
                ::CONSOLE_SCREEN_BUFFER_INFO inf;
                ::GetConsoleScreenBufferInfo(rm.get_console_out(), &inf);
                BYTE oldLowByte = inf.wAttributes & 0xFF;
                BYTE newLowByte;
                switch (oldLowByte) {
                case 0x07:
                    // newLowByte = FOREGROUND_BLUE | FOREGROUND_INTENSITY;  // too dim
                    // newLowByte = FOREGROUND_BLUE;                         // even dimmer
                    newLowByte
                        = FOREGROUND_BLUE | FOREGROUND_GREEN; // most similar to xterm appearance
                    break;
                case 0x70:
                    newLowByte = BACKGROUND_BLUE | BACKGROUND_INTENSITY;
                    break;
                default:
                    newLowByte = oldLowByte ^ 0xFF; // default to inverse video
                    break;
                }
                ::SetConsoleTextAttribute(
                    rm.get_console_out(), (inf.wAttributes & 0xFF00) | newLowByte);
                return inf.wAttributes;
            } else {
                ::SetConsoleTextAttribute(rm.get_console_out(), oldDisplayAttribute);
                return 0u;
            }
#else
            if constexpr (enhancedDisplay) {
                ssize_t written = ::write(
                    STDOUT_FILENO, "\x1b[1;34m", 7); /* bright blue (visible with both B&W bg) */
            } else {
                ssize_t written = ::write(STDOUT_FILENO, "\x1b[0m", 4); /* reset */
            }
            return 0u;
#endif
        }
        /**
         * Display the dynamic incremental search prompt and the current user input
         * line.
         * @param pi   PromptBase struct holding information about the prompt and our
         * screen position
         * @param buf32  input buffer to be displayed
         * @param len  count of characters in the buffer
         * @param pos  current cursor position within the buffer (0 <= pos <= len)
         */
        static void
        dynamicRefresh(PromptBase& pi, char32_t* buf32, int len, int pos, linse& ln)
        {
            auto&& rm = ln.rm;
            // calculate the position of the end of the prompt
            const auto endOfPrompt
                = calculateScreenPosition(0, 0, pi.promptScreenColumns, pi.promptChars);
            pi.promptIndentation = endOfPrompt.first;

            const auto rbfd = dynamicRefreshBase(pi, buf32, len, pos, endOfPrompt,
                { 0
#ifdef _WIN32
                    ,
                    0, len + 1, &pi.promptPreviousLen
#endif
                },
                rm);
            if (!rbfd)
                return;

            // display the prompt
            if (!pi.write(ln))
                return;

            // display the input line
            if (ln.write32(STDOUT_FILENO, buf32, len) == -1)
                return;

            dynamicRefreshBaseFinalize(pi, *rbfd, rm);
        }

        /**
         * Refresh the user's input line: the prompt is already onscreen and is not
         * redrawn here
         * @param pi   PromptBase struct holding information about the prompt and our
         * screen position
         */
        void
        refreshLine(PromptBase& pi, linse& ln)
        {
            auto&& rm = ln.rm;
            // check for a matching brace/bracket/paren, remember its position if found
            int highlight = -1;
            if (pos < buf32.size()) {
                /* this scans for a brace matching buf32[pos] to highlight */
                enum class scan_target : std::uint8_t{
        paren = '(',
        brace = '{',
        bracket = '['
      }target = scan_target::bracket;
                int scanDirection = 0;
                switch (buf32[pos]) {
                case '(':
                case '{':
                case '[':
                    scanDirection = 1;
                    break;
                case ')':
                case '}':
                case ']':
                    scanDirection = -1;
                }
                switch (buf32[pos]) {
                case '(':
                case ')':
                    target = scan_target::paren;
                    break;
                case '{':
                case '}':
                    target = scan_target::brace;
                    break;
                case '[':
                case ']':
                    break;
                }

                if (scanDirection) {
                    int unmatched = scanDirection;
                    for (int i = (int)(pos + scanDirection);
                         i >= 0 && i < static_cast<std::make_signed_t<std::size_t>>(buf32.size());
                         i += scanDirection) {
                        switch (buf32[i]) {
                        case ')':
                            if (target == scan_target::paren)
                                --unmatched;
                            break;
                        case '}':
                            if (target == scan_target::brace)
                                --unmatched;
                            break;
                        case ']':
                            if (target == scan_target::bracket)
                                --unmatched;
                            break;
                        case '(':
                            if (target == scan_target::paren)
                                ++unmatched;
                            break;
                        case '{':
                            if (target == scan_target::brace)
                                ++unmatched;
                            break;
                        case '[':
                            if (target == scan_target::bracket)
                                ++unmatched;
                            break;
                        }

                        if (unmatched == 0) {
                            highlight = i;
                            break;
                        }
                    }
                }
            }

            // calculate the position of the end of the prompt
#ifdef _WIN32
            int dummy = 0;
#endif
            const auto rbfd = dynamicRefreshBase(pi, buf32.data(), (int)buf32.size(), (int)pos,
                { pi.promptIndentation, 0 },
                { pi.promptIndentation
#ifdef _WIN32
                    ,
                    pi.promptExtraLines, pi.promptPreviousInputLen, &dummy
#endif
                },
                rm);
            if (!rbfd)
                return;

            // display the input line
            if (highlight == -1) {
                if (ln.write32(STDOUT_FILENO, buf32.data(), buf32.size()) == -1)
                    return;
            } else {
                if (ln.write32(STDOUT_FILENO, buf32.data(), highlight) == -1)
                    return;
                const auto oldattr
                    = setDisplayAttribute<true>(rm); /* bright blue (visible with both B&W bg) */
                if (ln.write32(STDOUT_FILENO, &buf32[highlight], 1) == -1)
                    return;
                setDisplayAttribute<false>(rm, oldattr);
                if (ln.write32(
                        STDOUT_FILENO, buf32.data() + highlight + 1, buf32.size() - highlight - 1)
                    == -1)
                    return;
            }

            dynamicRefreshBaseFinalize(pi, *rbfd, rm);
        }
        void
        write_and_refresh(PromptBase& pi, linse& ln)
        {
            if (!pi.write(ln))
                return;
#ifndef _WIN32
            // we have to generate our own newline on line wrap on Linux
            if (pi.promptIndentation == 0 && pi.promptExtraLines > 0)
                if (write(STDOUT_FILENO, "\n", 1) == -1)
                    return;
#endif
            pi.promptCursorRowOffset = pi.promptExtraLines;
            refreshLine(pi, ln);
        }
        void
        empty_refresh_line(PromptBase& pi, linse& ln)
        {
            auto&& rm = ln.rm;
            // calculate the position of the end of the prompt
#ifdef _WIN32
            int dummy = 0;
#endif
            const auto rbfd
                = dynamicRefreshBase(pi, buf32.data(), 0, 0, { pi.promptIndentation, 0 },
                    { pi.promptIndentation
#ifdef _WIN32
                        ,
                        pi.promptExtraLines, pi.promptPreviousInputLen, &dummy
#endif
                    },
                    rm);
            if (!rbfd)
                return;

            // display the input line
            if (ln.write32(STDOUT_FILENO, buf32.data(), buf32.size()) == -1)
                return dynamicRefreshBaseFinalize(pi, *rbfd, rm);
        }
        void
        copy_from_8(const char* p)
        {
            buf32.clear();
            buf32.resize(buf32.capacity());
            const auto [ret, new_pos] = copyString8to32(buf32.data(), buf32.capacity(), p);
            if (ret == conversion_result::succeeded)
                pos = new_pos;
            buf32.resize(pos);
            while (!buf32.empty() && buf32.back() == '\0') {
                buf32.pop_back(); //\0
                if (pos != 0)
                    --pos;
            }
        }

    public:
        InputBuffer() { buf32.reserve(LINSE_DEFAULT_LINE_CAPACITY); }
        void
        preloadBuffer(const char* preloadText)
        {
            copy_from_8(preloadText);
        }

        template <typename = void>
        int
        getInputLine(PromptBase& pi, linse& ln)
        {
            auto&& history = ln.history;
            auto&& killRing = ln.killRing;
            auto&& rm = ln.rm;
            auto&& completionCallback = ln.completion_callback;
            auto&& previousSearchText = ln.previousSearchText;
            // The latest history entry is always our current buffer
            if (buf32.size() > 0) {
                std::vector<char> tempBuffer(sizeof(char32_t) * buf32.size() + 1);
                copyString32to8(reinterpret_cast<UTF8*>(tempBuffer.data()), tempBuffer.size(),
                    buf32.data(), buf32.size());
                history.add(tempBuffer.data());
            } else
                history.add("");
            history.index = static_cast<int>(history.size() - 1);
            history.recallMostRecent = false;

            // display the prompt
            if (!pi.write(ln))
                return -1;

#ifndef _WIN32
            // we have to generate our own newline on line wrap on Linux
            if (pi.promptIndentation == 0 && pi.promptExtraLines > 0)
                if (write(STDOUT_FILENO, "\n", 1) == -1)
                    return -1;
#endif

            // the cursor starts out at the end of the prompt
            pi.promptCursorRowOffset = pi.promptExtraLines;

            // kill and yank start in "other" mode
            killRing.lastAction = KillRing::action::other;

            // when history search returns control to us, we execute its terminating
            // keystroke
            int terminatingKeystroke = -1;

            // if there is already text in the buffer, display it first
            if (buf32.size() > 0)
                refreshLine(pi, ln);

            // loop collecting characters, respond to line editing characters
            while (true) {
                int c;
                if (terminatingKeystroke == -1) {
                    c = linenoiseReadChar(rm); // get a new keystroke

#ifndef _WIN32
                    if (c == 0 && ln.gotResize) {
                        // caught a window resize event
                        // now redraw the prompt and line
                        ln.gotResize = false;
                        pi.promptScreenColumns = getScreenColumns();
                        dynamicRefresh(pi, buf32.data(), buf32.size(), pos,
                            ln); // redraw the original prompt with current input
                        continue;
                    }
#endif
                } else {
                    c = terminatingKeystroke; // use the terminating keystroke from search
                    terminatingKeystroke = -1; // clear it once we've used it
                }

                c = cleanupCtrl(c); // convert CTRL + <char> into normal ctrl

                if (c == 0) {
                    return (int)buf32.size();
                }
                if (c == -1) {
                    if (pos > 0) { // TO_DO
                        history.recallMostRecent = false;
                        killRing.kill(&buf32[0], pos, false);
                        buf32.erase(buf32.begin(), buf32.begin() + pos);
                        pos = 0;
                        refreshLine(pi, ln);
                    }
                    return -1;
                }

                if (c == -2) {
                    if (!pi.write(ln))
                        return -1;
                    refreshLine(pi, ln);
                    continue;
                }

                // ctrl-I/tab, command completion, needs to be before switch statement
                if (c == ctrlChar('I') && completionCallback) {
                    killRing.lastAction = KillRing::action::other;
                    history.recallMostRecent = false;

                    // completeLine does the actual completion and replacement
                    c = completeLine(pi, ln);

                    if (c < 0) // return on error
                        return (int)buf32.size();

                    if (c == 0) // read next character when 0
                        continue;

                    // deliberate fall-through here, so we use the terminating character
                }

                switch (c) {
                case ctrlChar('A'): // ctrl-A, move cursor to start of line
                case HOME_KEY:
                    killRing.lastAction = KillRing::action::other;
                    pos = 0;
                    refreshLine(pi, ln);
                    break;

                case ctrlChar('B'): // ctrl-B, move cursor left by one character
                case LEFT_ARROW_KEY:
                    killRing.lastAction = KillRing::action::other;
                    if (pos > 0) {
                        --pos;
                        refreshLine(pi, ln);
                    }
                    break;

                case META + 'b': // meta-B, move cursor left by one word
                case META + 'B':
                case CTRL + LEFT_ARROW_KEY:
                case META + LEFT_ARROW_KEY: // Emacs allows Meta, bash & readline don't
                    killRing.lastAction = KillRing::action::other;
                    if (pos > 0) {
                        while (pos > 0 && !isCharacterAlphanumeric(buf32[pos - 1]))
                            --pos;
                        while (pos > 0 && isCharacterAlphanumeric(buf32[pos - 1]))
                            --pos;
                        refreshLine(pi, ln);
                    }
                    break;

                case ctrlChar('C'): { // ctrl-C, abort this line
                    killRing.lastAction = KillRing::action::other;
                    history.recallMostRecent = false;
                    errno = EAGAIN;
                    history.pop_back();
                    // we need one last refresh with the cursor at the end of the line
                    // so we don't display the next prompt over the previous input line
                    pos = buf32.size(); // pass len as pos for EOL
                    refreshLine(pi, ln);
                    ssize_t written = write(STDOUT_FILENO, "^C", 2); // Display the ^C we got
                    return -1;
                } break;

                case META + 'c': // meta-C, give word initial Cap
                case META + 'C':
                    killRing.lastAction = KillRing::action::other;
                    history.recallMostRecent = false;
                    if (pos < buf32.size()) {
                        while (pos < buf32.size() && !isCharacterAlphanumeric(buf32[pos]))
                            ++pos;
                        if (pos < buf32.size() && isCharacterAlphanumeric(buf32[pos])) {
                            if (::islower(buf32[pos]))
                                buf32[pos] = ::toupper(buf32[pos]);
                            ++pos;
                        }
                        while (pos < buf32.size() && isCharacterAlphanumeric(buf32[pos])) {
                            if (::isupper(buf32[pos]))
                                buf32[pos] = ::tolower(buf32[pos]);
                            ++pos;
                        }
                        refreshLine(pi, ln);
                    }
                    break;

                // ctrl-D, delete the character under the cursor
                // on an empty line, exit the shell
                case ctrlChar('D'):
                    killRing.lastAction = KillRing::action::other;
                    if (buf32.size() != 0 && pos < buf32.size()) {
                        history.recallMostRecent = false;
                        buf32.erase(buf32.begin() + pos);
                        refreshLine(pi, ln);
                    } else if (buf32.empty()) {
                        history.pop_back();
                        return -1;
                    }
                    break;

                case META + 'd': // meta-D, kill word to right of cursor
                case META + 'D':
                    if (pos < buf32.size()) {
                        history.recallMostRecent = false;
                        auto endingPos = pos;
                        while (
                            endingPos < buf32.size() && !isCharacterAlphanumeric(buf32[endingPos]))
                            ++endingPos;
                        while (
                            endingPos < buf32.size() && isCharacterAlphanumeric(buf32[endingPos]))
                            ++endingPos;
                        killRing.kill(buf32.data() + pos, endingPos - pos, true);
                        buf32.erase(buf32.begin() + pos, buf32.begin() + endingPos);
                        refreshLine(pi, ln);
                    }
                    killRing.lastAction = KillRing::action::kill;
                    break;

                case ctrlChar('E'): // ctrl-E, move cursor to end of line
                case END_KEY:
                    killRing.lastAction = KillRing::action::other;
                    pos = buf32.size();
                    refreshLine(pi, ln);
                    break;

                case ctrlChar('F'): // ctrl-F, move cursor right by one character
                case RIGHT_ARROW_KEY:
                    killRing.lastAction = KillRing::action::other;
                    if (pos < buf32.size()) {
                        ++pos;
                        refreshLine(pi, ln);
                    }
                    break;

                case META + 'f': // meta-F, move cursor right by one word
                case META + 'F':
                case CTRL + RIGHT_ARROW_KEY:
                case META + RIGHT_ARROW_KEY: // Emacs allows Meta, bash & readline don't
                    killRing.lastAction = KillRing::action::other;
                    if (pos < buf32.size()) {
                        while (pos < buf32.size() && !isCharacterAlphanumeric(buf32[pos]))
                            ++pos;
                        while (pos < buf32.size() && isCharacterAlphanumeric(buf32[pos]))
                            ++pos;
                        refreshLine(pi, ln);
                    }
                    break;

                case ctrlChar('H'): // backspace/ctrl-H, delete char to left of cursor
                    killRing.lastAction = KillRing::action::other;
                    if (pos > 0) {
                        history.recallMostRecent = false;
                        buf32.erase(buf32.begin() + pos - 1);
                        --pos;
                        refreshLine(pi, ln);
                    }
                    break;

                // meta-Backspace, kill word to left of cursor
                case META + ctrlChar('H'):
                    if (pos > 0) {
                        history.recallMostRecent = false;
                        int startingPos = (int)pos;
                        while (pos > 0 && !isCharacterAlphanumeric(buf32[pos - 1]))
                            --pos;
                        while (pos > 0 && isCharacterAlphanumeric(buf32[pos - 1]))
                            --pos;
                        killRing.kill(&buf32[pos], startingPos - pos, false);
                        buf32.erase(buf32.begin() + pos, buf32.begin() + startingPos);
                        refreshLine(pi, ln);
                    }
                    killRing.lastAction = KillRing::action::kill;
                    break;

                case ctrlChar('J'): // ctrl-J/linefeed/newline, accept line
                case ctrlChar('M'): // ctrl-M/return/enter
                    killRing.lastAction = KillRing::action::other;
                    // we need one last refresh with the cursor at the end of the line
                    // so we don't display the next prompt over the previous input line
                    pos = buf32.size(); // pass len as pos for EOL
                    refreshLine(pi, ln);
                    history.previousIndex = history.recallMostRecent ? history.index : -2;
                    history.pop_back();
                    return (int)buf32.size();

                case ctrlChar('K'): // ctrl-K, kill from cursor to end of line
                    if ((int)buf32.size() - pos >= 0) {
                        killRing.kill(&buf32[pos], buf32.size() - pos, true);
                        buf32.erase(buf32.begin() + pos, buf32.end());
                    }
                    refreshLine(pi, ln);
                    killRing.lastAction = KillRing::action::kill;
                    history.recallMostRecent = false;
                    break;

                case ctrlChar('L'): // ctrl-L, clear screen and redisplay line
                    clearScreen(pi, ln);
                    break;

                case META + 'l': // meta-L, lowercase word
                case META + 'L':
                    killRing.lastAction = KillRing::action::other;
                    if (pos < buf32.size()) {
                        history.recallMostRecent = false;
                        while (pos < buf32.size() && !isCharacterAlphanumeric(buf32[pos]))
                            ++pos;
                        while (pos < buf32.size() && isCharacterAlphanumeric(buf32[pos])) {
                            if (::isupper(buf32[pos]))
                                buf32[pos] = ::tolower(buf32[pos]);
                            ++pos;
                        }
                        refreshLine(pi, ln);
                    }
                    break;

                case ctrlChar('N'): // ctrl-N, recall next line in history
                case ctrlChar('P'): // ctrl-P, recall previous line in history
                case DOWN_ARROW_KEY:
                case UP_ARROW_KEY:
                    killRing.lastAction = KillRing::action::other;
                    // if not already recalling, add the current line to the history list so
                    // we don't
                    // have to special case it
                    replace_history_back_with_buf32(history);
                    if (history.size() > 1) {
                        if (c == UP_ARROW_KEY)
                            c = ctrlChar('P');
                        if (history.previousIndex != -2 && c != ctrlChar('P'))
                            history.index = 1 + history.previousIndex; // emulate Windows down-arrow
                        else
                            history.index += (c == ctrlChar('P')) ? -1 : 1;
                        history.previousIndex = -2;
                        if (history.index < 0) {
                            history.index = 0;
                            break;
                        } else if (history.index
                            >= static_cast<std::make_signed_t<std::size_t>>(history.size())) {
                            history.index = static_cast<int>(history.size() - 1);
                            break;
                        }
                        history.recallMostRecent = true;
                        copy_from_8(history[history.index].data());
                        refreshLine(pi, ln);
                    }
                    break;

                case ctrlChar('R'): // ctrl-R, reverse history search
                case ctrlChar('S'): // ctrl-S, forward history search
                    terminatingKeystroke
                        = incrementalHistorySearch(pi, c, history, previousSearchText, ln);
                    break;

                case ctrlChar('T'): // ctrl-T, transpose characters
                    killRing.lastAction = KillRing::action::other;
                    if (pos > 0 && buf32.size() > 1) {
                        history.recallMostRecent = false;
                        std::size_t leftCharPos = (pos == buf32.size()) ? pos - 2 : pos - 1;
                        std::swap(buf32[leftCharPos], buf32[leftCharPos + 1]);
                        if (pos != buf32.size())
                            ++pos;
                        refreshLine(pi, ln);
                    }
                    break;

                case ctrlChar('U'): // ctrl-U, kill all characters to the left of the cursor
                    if (pos > 0) { // TO_DO
                        history.recallMostRecent = false;
                        killRing.kill(&buf32[0], pos, false);
                        buf32.erase(buf32.begin(), buf32.begin() + pos);
                        pos = 0;
                        refreshLine(pi, ln);
                    }
                    killRing.lastAction = KillRing::action::kill;
                    break;

                case META + 'u': // meta-U, uppercase word
                case META + 'U':
                    killRing.lastAction = KillRing::action::other;
                    if (pos < buf32.size()) {
                        history.recallMostRecent = false;
                        while (pos < buf32.size() && !isCharacterAlphanumeric(buf32[pos]))
                            ++pos;
                        while (pos < buf32.size() && isCharacterAlphanumeric(buf32[pos])) {
                            if (::islower(buf32[pos]))
                                buf32[pos] = ::toupper(buf32[pos]);
                            ++pos;
                        }
                        refreshLine(pi, ln);
                    }
                    break;

                // ctrl-W, kill to whitespace (not word) to left of cursor
                case ctrlChar('W'):
                    if (pos > 0) {
                        history.recallMostRecent = false;
                        int startingPos = (int)pos;
                        while (pos > 0 && buf32[pos - 1] == ' ')
                            --pos;
                        while (pos > 0 && buf32[pos - 1] != ' ')
                            --pos;
                        killRing.kill(buf32.data() + pos, startingPos - pos, false);
                        buf32.erase(buf32.begin() + pos, buf32.begin() + startingPos);
                        refreshLine(pi, ln);
                    }
                    killRing.lastAction = KillRing::action::kill;
                    break;

                case ctrlChar('Y'): // ctrl-Y, yank killed text
                    history.recallMostRecent = false;
                    {
                        std::u32string* restoredText = killRing.yank();
                        if (restoredText) {
                            std::size_t ucharCount = restoredText->size();
                            buf32.insert(
                                buf32.begin() + pos, restoredText->cbegin(), restoredText->cend());
                            pos += ucharCount;
                            refreshLine(pi, ln);
                            killRing.lastAction = KillRing::action::yank;
                            killRing.lastYankSize = ucharCount;
                        } else
                            beep();
                    }
                    break;

                case META + 'y': // meta-Y, "yank-pop", rotate popped text
                case META + 'Y':
                    if (killRing.lastAction == KillRing::action::yank) {
                        history.recallMostRecent = false;
                        std::u32string* restoredText = killRing.yankPop();
                        if (restoredText) {
                            std::size_t ucharCount = restoredText->size();
                            if (ucharCount > killRing.lastYankSize) {
                                const auto next
                                    = std::copy_n(restoredText->cbegin(), killRing.lastYankSize,
                                        buf32.begin() + pos - killRing.lastYankSize);
                                buf32.insert(next, restoredText->cbegin() + killRing.lastYankSize,
                                    restoredText->cend());
                            } else {
                                const auto next = std::copy_n(restoredText->cbegin(), ucharCount,
                                    buf32.begin() + pos - killRing.lastYankSize);
                                buf32.erase(next, next + (killRing.lastYankSize - ucharCount));
                            }
                            pos += ucharCount - killRing.lastYankSize;
                            killRing.lastYankSize = ucharCount;
                            refreshLine(pi, ln);
                            break;
                        }
                    }
                    beep();
                    break;

#ifndef _WIN32
                case ctrlChar('Z'): // ctrl-Z, job control
                    rm.disable(); // Returning to Linux (whatever) shell, leave raw
                                  // mode
                    raise(SIGSTOP); // Break out in mid-line
                    rm.enable(); // Back from Linux shell, re-enter raw mode
                    if (!pi.write(ln))
                        break; // Redraw prompt
                    refreshLine(pi, ln); // Refresh the line
                    break;
#endif

                // DEL, delete the character under the cursor
                case 127:
                case DELETE_KEY:
                    killRing.lastAction = KillRing::action::other;
                    if (buf32.size() > 0 && pos < buf32.size()) {
                        history.recallMostRecent = false;
                        buf32.erase(buf32.begin() + pos);
                        refreshLine(pi, ln);
                    }
                    break;

                case META + '<': // meta-<, beginning of history
                case PAGE_UP_KEY: // Page Up, beginning of history
                case META + '>': // meta->, end of history
                case PAGE_DOWN_KEY: // Page Down, end of history
                    killRing.lastAction = KillRing::action::other;
                    // if not already recalling, add the current line to the history list so
                    // we don't
                    // have to special case it
                    replace_history_back_with_buf32(history);
                    if (history.size() > 1) {
                        history.index = (c == META + '<' || c == PAGE_UP_KEY)
                            ? 0
                            : static_cast<int>(history.size() - 1);
                        history.previousIndex = -2;
                        history.recallMostRecent = true;
                        copy_from_8(history[history.index].data());
                        refreshLine(pi, ln);
                    }
                    break;

                // not one of our special characters, maybe insert it in the buffer
                default:
                    killRing.lastAction = KillRing::action::other;
                    history.recallMostRecent = false;
                    if (c & (META | CTRL)) { // beep on unknown Ctrl and/or Meta keys
                        beep();
                        break;
                    }
                    if (isControlChar(c)) { // don't insert control characters
                        beep();
                        break;
                    }
                    if (buf32.size() == pos) { // at end of buffer
                        buf32.insert(buf32.begin() + pos, c);
                        ++pos;
                        int inputLen = calculateColumnPosition(buf32.data(), (int)buf32.size());
                        if (pi.promptIndentation + inputLen < pi.promptScreenColumns) {
                            if (inputLen > pi.promptPreviousInputLen)
                                pi.promptPreviousInputLen = inputLen;
                            /* Avoid a full update of the line in the
                             * trivial case. */
                            if (ln.write32(STDOUT_FILENO, reinterpret_cast<char32_t*>(&c), 1) == -1)
                                return -1;
                        } else {
                            refreshLine(pi, ln);
                        }
                    } else { // not at end of buffer, have to move characters to our
                             // right
                        buf32.insert(buf32.begin() + pos, c);
                        ++pos;
                        refreshLine(pi, ln);
                    }
                    break;
                }
            }
            return (int)buf32.size();
        }
        std::u32string_view
        buffer_view() const
        {
            return { buf32.data(), buf32.size() };
        }
        void
        buffer_clear()
        {
            buf32.clear();
            pos = 0;
        }
    };

#ifndef _WIN32
    bool gotResize = false;
#endif

    std::string preloadedBufferContents; // used with linenoisePreloadBuffer
    std::string preloadErrorMessage;

    static inline std::optional<std::string>
    input_using_istream()
    {
        std::string buf8;
        if (!std::getline(std::cin, buf8))
            return std::nullopt;
        while (!buf8.empty() && (buf8.back() == '\n' || buf8.back() == '\r'))
            buf8.pop_back();
        return buf8;
    }

    InputBuffer ib;

public:
    linse() = default;

    /**
     * linenoisePreloadBuffer provides text to be inserted into the command buffer
     *
     * the provided text will be processed to be usable and will be used to preload
     * the input buffer on the next call to linenoise()
     *
     * @param preloadText text to begin with on the next call to linenoise()
     */
    void
    preload_buffer(const char* preloadText)
    {
        if (!preloadText)
            return;
        std::vector<char> tempBuffer(strlen(preloadText) + 1);
        strncpy(tempBuffer.data(), preloadText, tempBuffer.size());

        // remove characters that won't display correctly
        char* pIn = tempBuffer.data();
        char* pOut = pIn;
        bool controlsStripped = false;
        bool whitespaceSeen = false;
        while (*pIn) {
            unsigned char c = *pIn++; // we need unsigned so chars 0x80 and above are allowed
            if (c == '\r') // silently skip CR
                continue;
            if (c == '\n' || c == '\t') { // note newline or tab
                whitespaceSeen = true;
                continue;
            }
            if (isControlChar(c)) { // remove other control characters, flag for message
                controlsStripped = true;
                *pOut++ = ' ';
                continue;
            }
            if (whitespaceSeen) { // convert whitespace to a single space
                *pOut++ = ' ';
                whitespaceSeen = false;
            }
            *pOut++ = c;
        }
        *pOut = 0;
        preloadedBufferContents = tempBuffer.data();
        if (controlsStripped)
            preloadErrorMessage += " [Edited line: control characters were converted to spaces]\n";
    }

    /**
     * linenoise is a readline replacement.
     *
     * call it with a prompt to display and it will return a line of input from the
     * user
     *
     * @param prompt text of prompt to display to the user
     * @return       the returned string belongs to the caller on return and must be
     * freed to prevent
     *               memory leaks
     */
    std::optional<std::string>
    operator()(const char* prompt)
    {
#ifndef _WIN32
        gotResize = false;
#endif
        if (isatty(STDIN_FILENO)) { // input is from a terminal
            if (!preloadErrorMessage.empty()) {
                std::printf("%s", std::exchange(preloadErrorMessage, std::string {}).c_str());
                std::fflush(stdout);
            }
            PromptInfo pi(prompt, getScreenColumns());
            if (isUnsupportedTerm()) {
                if (!pi.write(*this))
                    return std::nullopt;
                std::fflush(stdout);
                if (preloadedBufferContents.empty())
                    return input_using_istream();
                else
                    return std::exchange(preloadedBufferContents, std::string {});
            } else {
                if (!rm.enable())
                    return std::nullopt;
                if (!preloadedBufferContents.empty())
                    ib.preloadBuffer(
                        std::exchange(preloadedBufferContents, std::string {}).c_str());
                int count = ib.getInputLine(pi, *this);
                rm.disable();
                if (count == -1) {
                    return std::nullopt;
                }
                std::fputc('\n', stdout);
                std::u32string_view sv = ib.buffer_view();
                ib.buffer_clear();
                return Utf32String::convert(sv);
            }
        } else // input not from a terminal, we should work with piped input, i.e.
               // redirected stdin
            return input_using_istream();
    }

private:
#ifndef _WIN32
    static inline linse* lthis;
    static void
    WindowSizeChanged(int)
    {
        // do nothing here but setting this flag
        lthis->gotResize = true;
    }
    using errno_t = int;
#else
    using errno_t = ::errno_t;
#endif

public:
    errno_t
    install_window_change_handler()
    {
#ifndef _WIN32
        lthis = this;
        struct sigaction sa;
        sigemptyset(&sa.sa_mask);
        sa.sa_flags = 0;
        sa.sa_handler = &WindowSizeChanged;

        if (sigaction(SIGWINCH, &sa, nullptr) == -1)
            return errno;
#endif
        return 0;
    }

    template <typename F> class word_completion
    {
        F f;

    public:
        word_completion(F&& f) : f { std::forward<F>(f) } { }
        linse::completions
        operator()(std::string_view data, std::size_t pos) const
            noexcept(noexcept(f(data).set_prefix(data)))
        {
            // completionCallback() expects a parsable entity, so find the previous break
            // character and
            // extract a copy to parse.  we also handle the case where tab is hit while
            // not at end-of-line.

            // break characters that may precede items to be completed
            static constexpr char breakChars[] = " =+-/\\*?\"'`&<>;|@{([])}";
            auto startIndex = data.find_last_of(breakChars, pos);
            if (startIndex == std::string_view::npos)
                startIndex = 0;

            const auto prefix = data.substr(startIndex, pos - startIndex);

            // get a list of completions
            return f(prefix).set_prefix(prefix);
        }
    };
};

#ifdef _WIN32
#if defined(_MSC_VER) && _MSC_VER < 1900
#undef snprintf
#endif
#undef strcasecmp
#undef isatty
#undef write
#undef STDOUT_FILENO
#undef STDIN_FILENO
#endif
