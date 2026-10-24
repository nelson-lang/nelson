//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "save_systemBuiltin.hpp"
#include "NFlowBuiltinHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include <filesystem>
#include <fstream>
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow;
//=============================================================================
namespace fs = std::filesystem;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
static void
writeFileAtomic(const fs::path& target, const std::string& content);
//=============================================================================
ArrayOfVector
save_systemBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 2);
    nargoutcheck(nLhs, 0, 1);
    std::string modelName;
    NFlowModelDocument& doc = requireModel(argIn[0], modelName);
    fs::path target;
    if (argIn.size() >= 2) {
        target = fs::path(argIn[1].asWideString());
    } else if (!doc.filePath().empty()) {
        target = fs::path(doc.filePath());
    } else {
        target = fs::path(modelName);
    }
    if (target.extension().empty()) {
        target.replace_extension(".nflow");
    }
    writeFileAtomic(target, doc.toJsonString());
    doc.setFilePath(target.string());
    doc.setDirty(false);
    ArrayOfVector retval;
    retval << ArrayOf::characterArray(target.string());
    return retval;
}
//=============================================================================
static void
writeFileAtomic(const fs::path& target, const std::string& content)
{
    fs::path tmp = target;
    tmp += ".tmp";
    {
        std::ofstream ofs(tmp, std::ios::binary | std::ios::trunc);
        if (!ofs.is_open()) {
            Error("Nelson:nflow_engine:cannotWriteFile:cannotOpenFileWritingValue",
                "Cannot open file for writing: %s", target.string());
        }
        ofs << content;
    }
    std::error_code ec;
    fs::rename(tmp, target, ec);
    if (ec) {
        fs::remove(target, ec);
        fs::rename(tmp, target, ec);
        if (ec) {
            Error("Nelson:nflow_engine:cannotWriteFile", "Cannot write model file: %s",
                target.string());
        }
    }
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
