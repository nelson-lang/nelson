//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#include <regex>
#include "StringHelpers.hpp"
#include <boost/algorithm/string/predicate.hpp>
#include <boost/crc.hpp>
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
    std::wregex token(L"<img[^>]*?src\\s*=\\s*[\"\"']?([^'\"\" >]+?)[ '\"\"][^>]*?>");
    std::wsregex_iterator cur(tag.begin(), tag.end(), token);
    std::wsregex_iterator end;
    return !(cur == end);
}
//=============================================================================
bool
parseImageTag(const std::wstring& tag, const std::wstring& srcDirectory, std::wstring& oldPath,
    std::wstring& newPath)
{
    std::wregex tokenUrl(L"((http(|s):\\/\\/)?[^\"'=\n\r]+\\.(jpg|png|jpeg|gif|svg))");
    std::wsregex_iterator cur2(tag.begin(), tag.end(), tokenUrl);
    std::wsregex_iterator end;
    if (cur2 != end) {
        std::wsmatch const& what = *cur2;
        oldPath = what[0];
        std::string errorMessage;
        if (!StringHelpers::istarts_with(oldPath, L"http")) {
            if (StringHelpers::ends_with(srcDirectory, L"/")
                || StringHelpers::ends_with(srcDirectory, L"\\")) {
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
    std::wregex token(L"<img[^>]*?src\\s*=\\s*[\"\"']?([^'\"\" >]+?)[ '\"\"][^>]*?>");
    std::wsregex_iterator cur(text.begin(), text.end(), token);
    std::wsregex_iterator end;
    for (; cur != end; ++cur) {
        std::wsmatch const& what = *cur;
        std::wstring submatch = what.str();
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
