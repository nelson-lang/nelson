//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "JuliaEngine.hpp"
#include "JuliaLibraryWrapper.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "NelsonConfiguration.hpp"
#include "JuliaEnvironment.hpp"
#include <fstream>
//=============================================================================
#ifdef _MSC_VER
#pragma warning(disable : 4244)
#pragma warning(disable : 4359)
#endif
//=============================================================================
namespace Nelson {
//=============================================================================
bool
initializeJuliaEngine()
{
    if (isJuliaLibraryLoaded()) {
        return true;
    }

    auto readFileIntoContent = [&](const std::wstring& filename, std::string& content) -> bool {
        std::ifstream inputFile(
#ifdef _MSC_VER
            filename, std::ios::in | std::ios::binary
#else
            wstring_to_utf8(filename), std::ios::in | std::ios::binary
#endif
        );

        if (!inputFile.is_open()) {
            return false;
        }

        inputFile.seekg(0, std::ios::end);
        size_t fileSize = inputFile.tellg();
        inputFile.seekg(0, std::ios::beg);
        content.resize(fileSize);
        inputFile.read(content.data(), fileSize);
        inputFile.close();

        return true;
    };

    JuliaEnvironment* juliaEnvironment = JuliaEnvironment::getInstance();
    configureJuliaEnvironment(juliaEnvironment);
    loadJuliaLibrary(juliaEnvironment->getLibrary());

    if (!isJuliaLibraryLoaded()) {
        juliaEnvironment->setStatus(0);
        return false;
    }
    juliaEnvironment->setStatus(1);

    NLSjl_init();

    std::wstring standardInOutRedirectionFullFilename
        = NelsonConfiguration::getInstance()->getNelsonModulesDirectory()
        + L"/julia_engine/resources/julia/standardInOutRedirection.jl";

    std::string content;
    if (!readFileIntoContent(standardInOutRedirectionFullFilename, content)) {
        return false;
    }

    jl_value_t* result = NLSjl_eval_string(content.c_str());
    if (!result) {
        return false;
    }
    return true;
}
//=============================================================================
}
//=============================================================================
