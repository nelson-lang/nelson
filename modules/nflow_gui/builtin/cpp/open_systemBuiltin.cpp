//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#define _CRT_SECURE_NO_WARNINGS
#include "open_systemBuiltin.hpp"
#include "nflow_guiBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "characters_encoding.hpp"
#include "NFlowModelDocument.hpp"
#include "NFlowModelRegistry.hpp"
#include <nlohmann/json.hpp>
#include <filesystem>
#include <fstream>
#include <sstream>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace fs = std::filesystem;
//=============================================================================
// open_system(modelOrFile)
//
// Opens the nflow editor bound to the live NFlowModelDocument in the registry, so
// GUI edits are visible to get_param/find_system and scripted mutations reach the
// editor. No temporary snapshot file is created. A model already in memory (by
// name or handle) opens in place; a file path is loaded into the registry first,
// like load_system.
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::open_systemBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 0);
    nargincheck(argIn, 1, 1);

    auto& reg = Nelson::NFlow::NFlowModelRegistry::instance();
    Nelson::NFlow::NFlowModelDocument* doc = nullptr;
    if (argIn[0].isNumeric() && !argIn[0].isEmpty()) {
        doc = reg.resolveByHandle(argIn[0].asDoubleScalar());
        if (doc == nullptr) {
            Error("Nelson:nflow_gui:invalidHandle", "Invalid nflow model handle.");
        }
    } else if (argIn[0].isRowVectorCharacterArray() || argIn[0].isScalarStringArray()) {
        std::string name = argIn[0].asUtf8String();
        auto pos = name.find('/');
        if (pos != std::string::npos) {
            name = name.substr(0, pos);
        }
        doc = reg.resolveByName(name);
    }

    std::string modelName;
    std::string realPath;
    if (doc != nullptr) {
        // In-memory model: open it in place. Keep its real file path (empty for a
        // never-saved new_system); the editor runs Save As when there is none.
        modelName = doc->name();
        realPath = doc->filePath();
    } else {
        // Name the model like load_system: the JSON model name, else the file stem.
        fs::path fp(argIn[0].asWideString());
        if (fp.extension().empty()) {
            fp.replace_extension(".nflow");
        }
        if (!fs::exists(fp)) {
            Error(L"Nelson:nflow_gui:modelFileNotFound",
                L"Model not loaded and file does not exist: %s", fp.wstring());
        }
        std::ifstream ifs(fp, std::ios::binary);
        if (!ifs.is_open()) {
            Error(L"Nelson:nflow_gui:cannotReadFile", L"Cannot open model file: %s", fp.wstring());
        }
        std::ostringstream ss;
        ss << ifs.rdbuf();
        const std::string content = ss.str();
        std::string name = fp.stem().string();
        try {
            const nlohmann::json parsed = nlohmann::json::parse(content);
            name = parsed.value("name", name);
        } catch (...) {
        }
        doc = reg.resolveByName(name);
        if (doc == nullptr) {
            auto loaded = std::make_unique<Nelson::NFlow::NFlowModelDocument>(name);
            std::string err;
            if (!Nelson::NFlow::NFlowModelDocument::fromJsonString(content, name, *loaded, err)) {
                Error("Nelson:nflow_gui:openSystemFailed", err);
            }
            loaded->setFilePath(fp.string());
            double handle = 0;
            if (!reg.add(std::move(loaded), handle, err)) {
                Error("Nelson:nflow_gui:openSystemFailed", err);
            }
            doc = reg.resolveByName(name);
        }
        modelName = name;
        realPath = doc != nullptr ? doc->filePath() : fp.string();
    }

    reg.setCurrentSystem(modelName);

    ArrayOfVector openArgs;
    openArgs << ArrayOf::characterArray(realPath);
    openArgs << ArrayOf::characterArray(modelName);
    Nelson::NFlowGateway::nflow_guiBuiltin(eval, 0, openArgs);
    return ArrayOfVector();
}
//=============================================================================
