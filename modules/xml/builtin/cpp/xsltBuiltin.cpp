//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "xsltBuiltin.hpp"
#include "XmlBuiltinHelpers.hpp"
#include "XmlCore.hpp"
#include "XmlTransform.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "characters_encoding.hpp"
#include <chrono>
#include <filesystem>
#include <fstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
static std::wstring
makeXsltTempFilename()
{
    auto ticks = std::chrono::high_resolution_clock::now().time_since_epoch().count();
    std::filesystem::path path = std::filesystem::temp_directory_path()
        / ("nelson_xslt_" + std::to_string(static_cast<long long>(ticks)) + ".xml");
    return path.wstring();
}
//=============================================================================
static bool
writeXsltTempFile(const std::wstring& filename, const std::wstring& text)
{
    std::ofstream file(wstring_to_utf8(filename), std::ios::binary);
    if (!file.is_open()) {
        return false;
    }
    std::string data = wstring_to_utf8(text);
    file.write(data.data(), static_cast<std::streamsize>(data.size()));
    return file.good();
}
//=============================================================================
static std::wstring
sourceToFilename(const ArrayOf& source, std::wstring& tempFile, std::wstring& errorMessage)
{
    if (source.isClassType()) {
        std::wstring text;
        if (!XmlDocumentToText(source, text, errorMessage)) {
            return L"";
        }
        tempFile = makeXsltTempFilename();
        if (!writeXsltTempFile(tempFile, text)) {
            errorMessage = Nelson::resolveMessageTemplate(
                L"Nelson:xml:unableWriteTemporaryXml", L"Unable to write temporary XML source.");
            return L"";
        }
        return tempFile;
    }
    return source.asWideString();
}
//=============================================================================
ArrayOfVector
Nelson::XmlGateway::xsltBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2);
    nargoutcheck(nLhs, 0, 2);
    std::wstring errorMessage;
    std::wstring tempSource;
    std::wstring source = sourceToFilename(argIn[0], tempSource, errorMessage);
    if (!errorMessage.empty()) {
        Error(L"Nelson:xslt:transformFailed", errorMessage);
    }
    std::wstring style = argIn[1].asWideString();
    std::wstring dest = argIn.size() >= 3 ? argIn[2].asWideString() : L"";
    for (size_t k = 3; k < argIn.size(); k++) {
        if (argIn[k].isRowVectorCharacterArray() && argIn[k].asWideString() == L"-web") {
            continue;
        }
        if (k + 1 < argIn.size() && normalizedXmlNameValueName(argIn[k]) == "xmlengine") {
            k++;
            continue;
        }
        Error(L"Nelson:xml:propertyNotSupported", L"Property name not supported.");
    }
    ArrayOfVector retval;
    if (dest == L"-tostring") {
        std::wstring output;
        if (!XmlTransformToText(source, style, output, errorMessage)) {
            Error(L"Nelson:xslt:transformFailed", errorMessage);
        }
        retval << ArrayOf::characterArray(output);
    } else {
        if (dest.empty()) {
            dest = makeXsltTempFilename();
        }
        bool ok = XmlTransform(source, style, dest, true, DOCUMENT_OUTPUT::UNDEFINED, errorMessage);
        if (!ok) {
            Error(L"Nelson:xslt:transformFailed", errorMessage);
        }
        retval << ArrayOf::characterArray(dest);
    }
    if (nLhs > 1) {
        retval << ArrayOf::empty();
    }
    if (!tempSource.empty()) {
        std::error_code code;
        std::filesystem::remove(std::filesystem::path(tempSource), code);
    }
    return retval;
}
//=============================================================================
