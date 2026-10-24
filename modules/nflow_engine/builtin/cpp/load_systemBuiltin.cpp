//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "load_systemBuiltin.hpp"
#include "NFlowModelRegistry.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <filesystem>
#include <fstream>
#include <sstream>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace fs = std::filesystem;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static std::string
readFileUtf8(const std::wstring& path);
//=============================================================================
ArrayOfVector
load_systemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    auto& reg = NFlowModelRegistry::instance();
    std::string arg = argIn[0].asUtf8String();
    double handle = 0;
    if (reg.handleOf(arg, handle)) {
        ArrayOfVector retval;
        retval << ArrayOf::doubleScalar(handle);
        return retval;
    }
    std::wstring wpath = argIn[0].asWideString();
    fs::path p(wpath);
    if (p.extension().empty()) {
        p.replace_extension(".nflow");
    }
    if (!fs::exists(p)) {
        Error(L"Nelson:nflow_engine:modelFileNotFound", L"The model file does not exist: %s",
            p.wstring());
    }
    std::string content = readFileUtf8(p.wstring());
    std::string name = p.stem().string();
    // Prefer the model name stored in the document over the file stem.
    try {
        auto peek = nlohmann::json::parse(content);
        if (peek.contains("name") && peek["name"].is_string()
            && !peek["name"].get<std::string>().empty()) {
            name = peek["name"].get<std::string>();
        }
    } catch (...) {
    }
    std::string err;
    auto doc = std::make_unique<NFlowModelDocument>(name);
    if (!NFlowModelDocument::fromJsonString(content, name, *doc, err)) {
        Error("Nelson:nflow_engine:loadSystemFailed", err);
    }
    doc->setFilePath(p.string());
    doc->setDirty(false);
    if (!reg.add(std::move(doc), handle, err)) {
        Error("Nelson:nflow_engine:loadSystemFailed", err);
    }
    reg.setCurrentSystem(name);
    ArrayOfVector retval;
    retval << ArrayOf::doubleScalar(handle);
    return retval;
}
//=============================================================================
static std::string
readFileUtf8(const std::wstring& path)
{
    std::ifstream ifs(fs::path(path), std::ios::binary);
    if (!ifs.is_open()) {
        Error(L"Nelson:nflow_engine:cannotReadFile", L"Cannot open file for reading: %s", path);
    }
    std::ostringstream ss;
    ss << ifs.rdbuf();
    return ss.str();
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
