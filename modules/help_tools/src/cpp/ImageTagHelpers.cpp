//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of the Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/crc.hpp>
#include <boost/regex.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <sstream>
#include <fstream>
#include "FileSystemWrapper.hpp"
#include "ImageTagHelpers.hpp"
#include "characters_encoding.hpp"
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isValidImageTag(const std::wstring& tag)
{
    boost::xpressive::wsregex token = boost::xpressive::wsregex::compile(
        L"<img[^>]*?src\\s*=\\s*[\"\"']?([^'\"\" >]+?)[ '\"\"][^>]*?>");
    boost::xpressive::wsregex_iterator cur(tag.begin(), tag.end(), token);
    boost::xpressive::wsregex_iterator end;
    return !(cur == end);
}
//=============================================================================
bool
parseImageTag(const std::wstring& tag, const std::wstring& srcDirectory, std::wstring& oldPath,
    std::wstring& newPath)
{
    boost::xpressive::wsregex tokenUrl = boost::xpressive::wsregex::compile(
        L"((http(|s):\\/\\/)?[^\"'=\n\r]+\\.(jpg|png|jpeg|gif|svg))");
    boost::xpressive::wsregex_iterator cur2(tag.begin(), tag.end(), tokenUrl);
    boost::xpressive::wsregex_iterator end;
    if (cur2 != end) {
        boost::xpressive::wsmatch const& what = *cur2;
        oldPath = what[0];
        std::string errorMessage;
        if (!boost::algorithm::istarts_with(oldPath, L"http")) {
            if (boost::ends_with(srcDirectory, L"/") || boost::ends_with(srcDirectory, L"\\")) {
                FileSystemWrapper::Path absolutePath
                    = FileSystemWrapper::Path::canonical(srcDirectory + oldPath, errorMessage);
                newPath = absolutePath.generic_wstring();

            } else {
                FileSystemWrapper::Path absolutePath = FileSystemWrapper::Path::canonical(
                    srcDirectory + L"/" + oldPath, errorMessage);
                newPath = absolutePath.generic_wstring();
            }

            if (FileSystemWrapper::Path::is_regular_file(newPath)) {
                return true;
            }
            newPath.clear();
        }
    }
    return false;
}
//=============================================================================
bool
findImageTag(const std::wstring& text, wstringVector& imagesTag)
{
    bool bRes = false;
    boost::xpressive::wsregex token = boost::xpressive::wsregex::compile(
        L"<img[^>]*?src\\s*=\\s*[\"\"']?([^'\"\" >]+?)[ '\"\"][^>]*?>");
    boost::xpressive::wsregex_iterator cur(text.begin(), text.end(), token);
    boost::xpressive::wsregex_iterator end;
    for (; cur != end; ++cur) {
        boost::xpressive::wsmatch const& what = *cur;
        std::wstring submatch = what[0];
        imagesTag.push_back(submatch);
        bRes = true;
    }
    return bRes;
}
//=============================================================================
bool
copyImages(const wstringVector& srcImages, const wstringVector& dstImages)
{
    bool bRes = true;
    for (size_t k = 0; k < srcImages.size(); k++) {
        bool bIsFile = FileSystemWrapper::Path::is_regular_file(srcImages[k]);
        if (bIsFile) {
            FileSystemWrapper::Path::copy_file(srcImages[k], dstImages[k]);
        } else {
            bRes = false;
        }
    }
    return bRes;
}
//=============================================================================
bool
copyImage(const std::wstring& srcImage, const std::wstring& dstImage)
{
    wstringVector srcImages;
    wstringVector dstImages;
    srcImages.push_back(srcImage);
    dstImages.push_back(dstImage);
    return copyImages(srcImages, dstImages);
}
//=============================================================================
#ifndef PRIVATE_BUFFER_SIZE
#define PRIVATE_BUFFER_SIZE 1024
#endif
std::wstring
crcFile(const std::wstring& filename)
{
    std::wstring res = L"";
#ifdef _MSC_VER
    std::ifstream ifs(filename.c_str(), std::ios_base::binary);
#else
    std::ifstream ifs(wstring_to_utf8(filename).c_str(), std::ios_base::binary);
#endif
    if (ifs) {
        boost::crc_32_type result;
        do {
            char buffer[PRIVATE_BUFFER_SIZE];
            ifs.read(buffer, PRIVATE_BUFFER_SIZE);
            result.process_bytes(buffer, (size_t)ifs.gcount());
        } while (ifs);
        ifs.close();
        std::stringstream ss;
        ss << std::hex << std::uppercase << result.checksum();
        res = utf8_to_wstring(ss.str());
    }
    return res;
}
//=============================================================================
}
//=============================================================================
