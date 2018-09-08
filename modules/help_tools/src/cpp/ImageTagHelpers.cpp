//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#define _SCL_SECURE_NO_WARNINGS
#include "ImageTagHelpers.hpp"
#include "characters_encoding.hpp"
#include <boost/algorithm/string.hpp>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/crc.hpp>
#include <boost/filesystem.hpp>
#include <boost/regex.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_generators.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <boost/xpressive/xpressive.hpp>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
bool
isValidImageTag(std::wstring tag)
{
    boost::xpressive::wsregex token = boost::xpressive::wsregex::compile(
        L"<img[^>]*?src\\s*=\\s*[\"\"']?([^'\"\" >]+?)[ '\"\"][^>]*?>");
    boost::xpressive::wsregex_iterator cur(tag.begin(), tag.end(), token);
    boost::xpressive::wsregex_iterator end;
    return !(cur == end);
}
//=============================================================================
bool
parseImageTag(
    std::wstring tag, std::wstring srcDirectory, std::wstring& oldPath, std::wstring& newPath)
{
    boost::xpressive::wsregex tokenUrl = boost::xpressive::wsregex::compile(
        L"((http(|s):\\/\\/)?[^\"'=\n\r]+\\.(jpg|png|jpeg|gif|svg))");
    boost::xpressive::wsregex_iterator cur2(tag.begin(), tag.end(), tokenUrl);
    boost::xpressive::wsregex_iterator end;
    if (cur2 != end) {
        boost::xpressive::wsmatch const& what = *cur2;
        oldPath = what[0];
        if (!boost::algorithm::istarts_with(oldPath, L"http")) {
            boost::filesystem::path absolutePath;
            try {
                absolutePath = boost::filesystem::canonical(oldPath, srcDirectory);
            } catch (const boost::filesystem::filesystem_error& e) {
                e.what();
            }
            newPath = absolutePath.generic_wstring();
            bool bIsFile
                = boost::filesystem::exists(newPath) && !boost::filesystem::is_directory(newPath);
            if (!bIsFile) {
                newPath = L"";
            }
            return bIsFile;
        }
    }
    return false;
}
//=============================================================================
bool
findImageTag(std::wstring text, wstringVector& imagesTag)
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
copyImages(wstringVector srcImages, wstringVector dstImages)
{
    bool bRes = true;
    for (size_t k = 0; k < srcImages.size(); k++) {
        bool bIsFile = boost::filesystem::exists(srcImages[k])
            && !boost::filesystem::is_directory(srcImages[k]);
        if (bIsFile) {
            try {
                boost::filesystem::copy_file(srcImages[k], dstImages[k],
                    boost::filesystem::copy_option::overwrite_if_exists);
            } catch (const boost::filesystem::filesystem_error& e) {
                e.what();
            }
        } else {
            bRes = false;
        }
    }
    return bRes;
}
//=============================================================================
bool
copyImage(std::wstring srcImage, std::wstring dstImage)
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
crcFile(std::wstring filename)
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
            result.process_bytes(buffer, ifs.gcount());
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
