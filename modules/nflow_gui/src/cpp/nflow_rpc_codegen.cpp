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
#include "nflow_rpc_codegen.hpp"
#include "BlockRegistry.hpp"
#include "BuildCodeHelpers.hpp"
#include "FieldNames.hpp"
#include "FileSystemWrapper.hpp"
#include "NFlowCodegenC.hpp"
#include "NFlowCodegenFlatten.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowCodegenPipeline.hpp"
#include "NFlowCodegenRust.hpp"
#include "NFlowNelsonBridge.hpp"
#include "SimEngine.hpp" // ensureBuiltinsRegistered
#include "characters_encoding.hpp"
#include <algorithm>
#include <cctype>
#include <climits>
#include <cmath>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <tuple>
#include <vector>
//=============================================================================
namespace Nelson::NFlowUiBindings {
//=============================================================================
namespace {
    //=============================================================================
    static nlohmann::json
    errorResult(const std::string& message)
    {
        return nlohmann::json { { "success", false }, { "error", message } };
    }
    //=============================================================================
    static bool
    readText(const std::filesystem::path& path, std::string& out)
    {
#ifdef _MSC_VER
        std::ifstream file(path.wstring(), std::ios::binary);
#else
        std::ifstream file(path.string(), std::ios::binary);
#endif
        if (!file.is_open()) {
            return false;
        }
        std::ostringstream ss;
        ss << file.rdbuf();
        out = ss.str();
        return true;
    }
    //=============================================================================
    static bool
    writeText(const FileSystemWrapper::Path& path, const std::string& content)
    {
        std::ofstream file(path.string(), std::ios::binary | std::ios::trunc);
        if (!file) {
            return false;
        }
        file << content;
        return true;
    }
    //=============================================================================
    static std::string
    lowerAscii(std::string value)
    {
        std::transform(value.begin(), value.end(), value.begin(),
            [](unsigned char c) { return static_cast<char>(std::tolower(c)); });
        return value;
    }
    //=============================================================================
    static std::string
    sanitizeIdentifier(const std::string& value)
    {
        std::string out;
        out.reserve(value.size());
        for (unsigned char c : value) {
            out.push_back(std::isalnum(c) ? static_cast<char>(std::tolower(c)) : '_');
        }
        return out.empty() ? "subsystem" : out;
    }
    //=============================================================================
    static bool
    loadNBlockForUserType(const std::string& blockType, nlohmann::json& outSpec)
    {
        if (blockType.rfind("user.", 0) != 0) {
            return false;
        }
        auto& reg = Nelson::NFlow::BlockRegistry::instance();
        for (const auto& rawManifest : reg.discoveredManifests()) {
            nlohmann::json manifest;
            try {
                manifest = nlohmann::json::parse(rawManifest);
            } catch (...) {
                continue;
            }
            const std::string dirUtf8 = manifest.value("_dir", "");
            if (dirUtf8.empty() || !manifest.contains("blocks") || !manifest["blocks"].is_array()) {
                continue;
            }
            std::string nblockName;
            bool typeFound = false;
            for (const auto& blockDef : manifest["blocks"]) {
                if (!blockDef.is_object() || blockDef.value("type", "") != blockType) {
                    continue;
                }
                typeFound = true;
                nblockName = blockDef.value("nblock", "");
                break;
            }
            if (!typeFound) {
                continue;
            }
            std::vector<std::filesystem::path> candidates;
            const std::filesystem::path libDir(dirUtf8);
            if (!nblockName.empty()) {
                candidates.push_back(libDir / nblockName);
            }
            candidates.push_back(libDir / (blockType.substr(5) + ".nblock"));
            try {
                for (const auto& entry : std::filesystem::directory_iterator(libDir)) {
                    if (entry.is_regular_file() && entry.path().extension() == ".nblock") {
                        candidates.push_back(entry.path());
                    }
                }
            } catch (...) {
            }
            for (const auto& candidate : candidates) {
                try {
                    if (!std::filesystem::exists(candidate)) {
                        continue;
                    }
                    std::string content;
                    if (!readText(candidate, content)) {
                        continue;
                    }
                    nlohmann::json spec = nlohmann::json::parse(content);
                    if (spec.contains(nflow::kBlocks) && spec[nflow::kBlocks].is_array()
                        && spec.contains(nflow::kConnections)
                        && spec[nflow::kConnections].is_array()) {
                        outSpec = std::move(spec);
                        return true;
                    }
                } catch (...) {
                }
            }
        }
        return false;
    }
    //=============================================================================
    static bool
    inlineGeneratedUserBlocks(nlohmann::json& diagram)
    {
        if (!diagram.contains(nflow::kBlocks) || !diagram[nflow::kBlocks].is_array()) {
            return false;
        }
        bool changed = false;
        for (auto& block : diagram[nflow::kBlocks]) {
            try {
                const std::string type = block.value(nflow::kType, "");
                if (type.rfind("user.", 0) != 0) {
                    continue;
                }
                nlohmann::json spec;
                if (!loadNBlockForUserType(type, spec)) {
                    continue;
                }
                if (inlineGeneratedUserBlocks(spec)) {
                    changed = true;
                }
                nlohmann::json params = block.value(nflow::kParams, nlohmann::json::object());
                params[nflow::kName] = spec.value("name", type);
                params[nflow::kSubsystem] = spec;
                block[nflow::kType] = nflow::kSubsystem;
                block[nflow::kParams] = params;
                changed = true;
            } catch (...) {
            }
        }
        return changed;
    }
    //=============================================================================
    static void
    applyCommonOptions(const std::string& optionsJson, nflow::CodegenOptions& opts,
        std::string& lang, std::wstring& buildConfig)
    {
        if (optionsJson.empty()) {
            return;
        }
        auto optObj = nlohmann::json::parse(optionsJson);
        if (optObj.contains("sampleTime") && optObj["sampleTime"].is_number()) {
            opts.sampleTime = optObj["sampleTime"].get<double>();
        }
        if (optObj.contains("includeMain") && optObj["includeMain"].is_boolean()) {
            opts.includeMain = optObj["includeMain"].get<bool>();
        }
        if (optObj.contains("lang") && optObj["lang"].is_string()) {
            lang = lowerAscii(optObj["lang"].get<std::string>());
        }
        if (optObj.contains("build") && optObj["build"].is_boolean()) {
            opts.build = optObj["build"].get<bool>();
        }
        if (optObj.contains("buildConfig") && optObj["buildConfig"].is_string()) {
            buildConfig
                = optObj["buildConfig"].get<std::string>() == "Release" ? L"Release" : L"Debug";
        }
    }
    //=============================================================================
    static std::string
    extractTypedef(const std::string& src, const std::string& typeName)
    {
        size_t pos = 0;
        while (true) {
            size_t tpos = src.find("typedef struct", pos);
            if (tpos == std::string::npos) {
                return {};
            }
            size_t bracePos = src.find('{', tpos);
            if (bracePos == std::string::npos) {
                pos = tpos + 1;
                continue;
            }
            int depth = 1;
            size_t i = bracePos + 1;
            for (; i < src.size(); ++i) {
                if (src[i] == '{') {
                    ++depth;
                } else if (src[i] == '}' && --depth == 0) {
                    break;
                }
            }
            size_t semi = i < src.size() ? src.find(';', i) : std::string::npos;
            if (semi == std::string::npos) {
                return {};
            }
            std::string candidate = src.substr(tpos, semi - tpos + 1);
            if (candidate.find(typeName) != std::string::npos) {
                return candidate;
            }
            pos = semi + 1;
        }
    }
    //=============================================================================
    static std::vector<std::string>
    parseFieldNames(const std::string& block)
    {
        std::vector<std::string> names;
        const size_t lbrace = block.find('{');
        const size_t rbrace = block.rfind('}');
        if (lbrace == std::string::npos || rbrace == std::string::npos || rbrace <= lbrace) {
            return names;
        }
        std::istringstream iss(block.substr(lbrace + 1, rbrace - lbrace - 1));
        std::string line;
        while (std::getline(iss, line)) {
            size_t start = line.find_first_not_of(" \t\r\n");
            size_t end = line.find_last_not_of(" \t\r\n;");
            if (start == std::string::npos || end == std::string::npos || end <= start) {
                continue;
            }
            std::string token = line.substr(start, end - start + 1);
            size_t posSpace = token.find_last_of(' ');
            if (posSpace == std::string::npos) {
                continue;
            }
            std::string name = token.substr(posSpace + 1);
            size_t br = name.find('[');
            if (br != std::string::npos) {
                name = name.substr(0, br);
            }
            if (!name.empty()) {
                names.push_back(name);
            }
        }
        return names;
    }
    //=============================================================================
    using PortPoint = std::tuple<int, int, std::string>;
    static void
    collectExternalPorts(const nlohmann::json& subspec, std::vector<PortPoint>& inputs,
        std::vector<PortPoint>& outputs, int& minY, int& maxY, int& minX, int& maxX)
    {
        if (!subspec.contains("blocks") || !subspec["blocks"].is_array()) {
            return;
        }
        for (const auto& block : subspec["blocks"]) {
            try {
                const auto& params = block.at("params");
                if (!params.value("isExternalPort", false)) {
                    continue;
                }
                const int x = block.value("x", 0);
                const int y = block.value("y", 0);
                const std::string name = params.value("name", "");
                const std::string type = block.value("type", "");
                if (type == "labelSource") {
                    inputs.emplace_back(x, y, name);
                } else if (type == "labelSink") {
                    outputs.emplace_back(x, y, name);
                }
                minY = std::min(minY, y);
                maxY = std::max(maxY, y);
                minX = std::min(minX, x);
                maxX = std::max(maxX, x);
            } catch (...) {
            }
        }
    }
    //=============================================================================
    static nlohmann::json
    makePortArray(
        std::vector<PortPoint>& points, int count, int finalWidth, int finalHeight, bool output)
    {
        nlohmann::json ports = nlohmann::json::array();
        if (count <= 0) {
            return ports;
        }
        std::sort(points.begin(), points.end(), [](const auto& a, const auto& b) {
            if (std::get<1>(a) != std::get<1>(b)) {
                return std::get<1>(a) < std::get<1>(b);
            }
            if (std::get<0>(a) != std::get<0>(b)) {
                return std::get<0>(a) < std::get<0>(b);
            }
            return std::get<2>(a) < std::get<2>(b);
        });
        const int extN = static_cast<int>(points.size());
        for (int i = 0; i < count; ++i) {
            double y = 10.0
                + static_cast<double>(i + 1) * static_cast<double>(finalHeight - 20)
                    / static_cast<double>(count + 1);
            nlohmann::json port;
            port["x"] = output ? finalWidth : 0;
            port["y"] = static_cast<int>(std::round(y));
            port["side"] = output ? "right" : "left";
            if (extN > 0) {
                int pick = extN == 1 ? 0
                    : count == 1     ? extN / 2
                                 : static_cast<int>(std::round(static_cast<double>(i) * (extN - 1)
                                       / static_cast<double>(count - 1)));
                pick = std::max(0, std::min(pick, extN - 1));
                port["name"] = std::get<2>(points[pick]);
            }
            ports.push_back(port);
        }
        return ports;
    }
    //=============================================================================
    static nlohmann::json
    generateCodeToDirImpl(const nlohmann::json& params)
    {
        const std::string diagramJson = params.value("diagramJson", "");
        const std::string optionsJson = params.value("optionsJson", "{}");
        const std::string destDirUtf8
            = params.value("destinationDirectory", params.value("destDir", ""));
        auto diagram = nlohmann::json::parse(diagramJson);
        nflow::CodegenOptions opts;
        std::string lang = "c";
        std::wstring buildConfig = L"Debug";
        applyCommonOptions(optionsJson, opts, lang, buildConfig);
        opts.target = lang == "rust" ? "rust" : "C";
        inlineGeneratedUserBlocks(diagram);
        // The shared pipeline owns the full gate sequence. The interpreter-backed hooks marshal
        // through the bridge installed at nflow() launch. Errors come back as the canonical
        // English message plus the structured gate/blockId for the editor.
        nflow::CodegenPipelineRequest pipelineRequest;
        pipelineRequest.diagramJson = diagram.dump();
        pipelineRequest.options = opts;
        nflow::CodegenPipelineResult pipelineResult = nflow::runCodegenPipeline(
            pipelineRequest, Nelson::NFlowGateway::makeInterpreterCodegenHooks());
        nlohmann::json warnings = nlohmann::json::array();
        for (const auto& d : pipelineResult.diagnostics) {
            if (d.severity == nflow::CodegenDiagnostic::Severity::Warning) {
                warnings.push_back(nflow::composeCodegenMessageEnglish(d));
            }
        }
        if (const nflow::CodegenDiagnostic* e = pipelineResult.firstError()) {
            nlohmann::json err = errorResult(nflow::composeCodegenMessageEnglish(*e));
            err["gate"] = e->gate;
            if (!e->blockId.empty()) {
                err["blockId"] = e->blockId;
            }
            if (!warnings.empty()) {
                err["warnings"] = warnings;
            }
            return err;
        }
        FileSystemWrapper::Path crateDir = FileSystemWrapper::Path(destDirUtf8) / "";
        if (!crateDir.is_directory()) {
            FileSystemWrapper::Path::create_directories(crateDir);
        }
        if (opts.target == "rust") {
            const nflow::CodegenRustResult& rustResult = pipelineResult.rust;
            FileSystemWrapper::Path srcDir = crateDir / FileSystemWrapper::Path(L"src");
            if (!srcDir.is_directory()) {
                FileSystemWrapper::Path::create_directories(srcDir);
            }
            FileSystemWrapper::Path cargoPath = crateDir / FileSystemWrapper::Path(L"Cargo.toml");
            if (!writeText(srcDir / FileSystemWrapper::Path(L"lib.rs"), rustResult.rustSource)) {
                return errorResult("Cannot open lib.rs for writing");
            }
            if (opts.includeMain && !rustResult.mainSource.empty()
                && !writeText(
                    srcDir / FileSystemWrapper::Path(L"main.rs"), rustResult.mainSource)) {
                return errorResult("Cannot open main.rs for writing");
            }
            if (!writeText(cargoPath, rustResult.cargoToml)) {
                return errorResult("Cannot open Cargo.toml for writing");
            }
            nlohmann::json res { { "success", true }, { "dir", crateDir.string() } };
            if (!warnings.empty()) {
                res["warnings"] = warnings;
            }
            if (opts.build) {
                std::wstring buildError;
                bool buildOk = nflow::callCargoToml(cargoPath.wstring(), L"",
                    (crateDir / FileSystemWrapper::Path(L"target")).wstring(), buildError);
                res["buildSuccess"] = buildOk;
                if (!buildOk) {
                    res["buildError"] = wstring_to_utf8(buildError);
                }
            }
            return res;
        }
        const nflow::CodegenCResult& cres = pipelineResult.c;
        if (!writeText(crateDir / FileSystemWrapper::Path(L"model.c"), cres.modelSource)) {
            return errorResult("Cannot open model.c for writing");
        }
        if (opts.includeMain && !cres.mainSource.empty()
            && !writeText(crateDir / FileSystemWrapper::Path(L"main.c"), cres.mainSource)) {
            return errorResult("Cannot open main.c for writing");
        }
        FileSystemWrapper::Path cmakePath = crateDir / FileSystemWrapper::Path(L"CMakeLists.txt");
        if (!writeText(cmakePath, cres.cmake)) {
            return errorResult("Cannot open CMakeLists.txt for writing");
        }
        nlohmann::json res { { "success", true }, { "dir", crateDir.string() } };
        if (!warnings.empty()) {
            res["warnings"] = warnings;
        }
        if (opts.build) {
            std::wstring buildError;
            bool buildOk = nflow::callCMake(cmakePath.wstring(), L"",
                (crateDir / FileSystemWrapper::Path(L"build")).wstring(), buildError, buildConfig);
            res["buildSuccess"] = buildOk;
            if (!buildOk) {
                res["buildError"] = wstring_to_utf8(buildError);
            }
        }
        return res;
    }
    //=============================================================================
    static nlohmann::json
    generateBlockToDirImpl(const nlohmann::json& params)
    {
        const std::string subsystemJson = params.value("subsystemJson", "");
        const std::string optionsJson = params.value("optionsJson", "{}");
        const std::string destDirUtf8
            = params.value("destinationDirectory", params.value("destDir", ""));
        auto subspec = nlohmann::json::parse(subsystemJson);
        nflow::CodegenOptions opts;
        std::string lang = "c";
        std::wstring buildConfig = L"Debug";
        int hostBlockWidth = 0;
        int hostBlockHeight = 0;
        bool haveHostBlockSize = false;
        if (!optionsJson.empty()) {
            auto optObj = nlohmann::json::parse(optionsJson);
            applyCommonOptions(optionsJson, opts, lang, buildConfig);
            if (optObj.contains("hostBlock") && optObj["hostBlock"].is_object()) {
                const auto& hb = optObj["hostBlock"];
                hostBlockWidth = hb.value("width", 0);
                hostBlockHeight = hb.value("height", 0);
                haveHostBlockSize = hostBlockWidth > 0 && hostBlockHeight > 0;
            }
        }
        inlineGeneratedUserBlocks(subspec);
        // A packaged subsystem spec carries its gate at the SPEC ROOT (there is
        // no host block whose params the conditional gate could inspect): a gated
        // spec would silently generate an ungated plugin, so reject it here.
        {
            const bool rootGated
                = (subspec.contains("controlPort") && subspec["controlPort"].is_object()
                      && !subspec["controlPort"].empty())
                || (subspec.contains("params") && subspec["params"].is_object()
                    && subspec["params"].contains("controlPort")
                    && subspec["params"]["controlPort"].is_object()
                    && !subspec["params"]["controlPort"].empty());
            if (rootGated) {
                nlohmann::json err = errorResult(
                    "Code generation is not yet supported for conditional execution "
                    "(conditional subsystem, packaged spec root): the control-port gate "
                    "has no host block to lower against. Package the subsystem without "
                    "its control port, or generate the full model instead.");
                err["gate"] = "conditional";
                return err;
            }
        }
        // Same shared gate sequence as generateCodeToDir. Without it, a subsystem with an
        // un-lowerable block silently produced a plugin outputting 0.0.
        nflow::CodegenPipelineRequest pipelineRequest;
        pipelineRequest.diagramJson = subspec.dump();
        pipelineRequest.options = opts;
        nflow::CodegenPipelineResult pipelineResult = nflow::runCodegenPipeline(
            pipelineRequest, Nelson::NFlowGateway::makeInterpreterCodegenHooks());
        nlohmann::json blockWarnings = nlohmann::json::array();
        for (const auto& dWarn : pipelineResult.diagnostics) {
            if (dWarn.severity == nflow::CodegenDiagnostic::Severity::Warning) {
                blockWarnings.push_back(nflow::composeCodegenMessageEnglish(dWarn));
            }
        }
        if (const nflow::CodegenDiagnostic* e = pipelineResult.firstError()) {
            nlohmann::json err = errorResult(nflow::composeCodegenMessageEnglish(*e));
            err["gate"] = e->gate;
            if (!e->blockId.empty()) {
                err["blockId"] = e->blockId;
            }
            if (!blockWarnings.empty()) {
                err["warnings"] = blockWarnings;
            }
            return err;
        }
        const nflow::CodegenCResult& cres = pipelineResult.c;
        const std::string name = subspec.value("name", "subsystem");
        const std::string id = sanitizeIdentifier(name);
        const std::string fqType = "user." + id;
        FileSystemWrapper::Path crateDir
            = FileSystemWrapper::Path(destDirUtf8) / FileSystemWrapper::Path(utf8_to_wstring(id));
        if (!crateDir.is_directory()) {
            FileSystemWrapper::Path::create_directories(crateDir);
        }
        if (!writeText(crateDir / FileSystemWrapper::Path(L"model.c"), cres.modelSource)) {
            return errorResult("Cannot open model.c for writing");
        }
        std::string ms = extractTypedef(cres.modelSource, "ModelState");
        std::string mi = extractTypedef(cres.modelSource, "ModelInput");
        std::string mo = extractTypedef(cres.modelSource, "ModelOutput");
        std::string modelH = "#pragma once\n\n#ifdef __cplusplus\nextern \"C\" {\n#endif\n\n";
        modelH += !ms.empty() ? ms + "\n\n" : "typedef struct ModelState ModelState;\n\n";
        modelH += !mi.empty() ? mi + "\n\n" : "typedef struct ModelInput ModelInput;\n\n";
        modelH += !mo.empty() ? mo + "\n\n" : "typedef struct ModelOutput ModelOutput;\n\n";
        modelH += "void nflow_init(ModelState* s);\n";
        modelH += "void nflow_step(ModelState* s, const ModelInput* in, ModelOutput* out, double "
                  "t);\n\n";
        modelH += "#ifdef __cplusplus\n}\n#endif\n";
        if (!writeText(crateDir / FileSystemWrapper::Path(L"model.h"), modelH)) {
            return errorResult("Cannot open model.h for writing");
        }
        const std::vector<std::string> inputFields = parseFieldNames(mi);
        const std::vector<std::string> outputFields = parseFieldNames(mo);
        std::string pluginCpp;
        pluginCpp += "#include \"SimEngineTypes.hpp\"\n#include \"model.h\"\n#include "
                     "\"BlockPlugin.h\"\n";
        pluginCpp += "#include <unordered_map>\n#include <vector>\n#include <cstring>\n#include "
                     "<cstdlib>\n\n";
        pluginCpp += "using namespace Nelson::NFlow;\n\nstatic std::unordered_map<BlockId, "
                     "ModelState*> g_states;\n\n";
        pluginCpp += "static bool handle_" + id + "(SimCtx& ctx, const Block& b, Phase phase) {\n";
        pluginCpp += "    if (phase == Phase::INIT) {\n";
        pluginCpp += "        if (g_states.find(b.nid) == g_states.end()) {\n";
        pluginCpp += "            ModelState* s = (ModelState*)std::malloc(sizeof(ModelState));\n";
        pluginCpp
            += "            std::memset(s, 0, sizeof(ModelState));\n            nflow_init(s);\n";
        pluginCpp
            += "            g_states[b.nid] = s;\n        }\n        return false;\n    }\n\n";
        pluginCpp += "    if (phase == Phase::OUTPUT) {\n";
        pluginCpp += "        auto it = g_states.find(b.nid);\n        if (it == g_states.end()) "
                     "return false;\n";
        pluginCpp += "        ModelState* s = it->second;\n        ModelInput inVal;\n        "
                     "ModelOutput outVal = {};\n";
        for (size_t i = 0; i < inputFields.size(); ++i) {
            pluginCpp += "        inVal." + inputFields[i] + " = getInput(ctx, b.nid, "
                + std::to_string(static_cast<int>(i)) + ", 0.0);\n";
        }
        pluginCpp += "        nflow_step(s, &inVal, &outVal, ctx.t);\n";
        for (size_t i = 0; i < outputFields.size(); ++i) {
            pluginCpp += "        setOutput(ctx, b.nid, outVal." + outputFields[i] + ", "
                + std::to_string(static_cast<int>(i)) + ");\n";
        }
        pluginCpp += "        return true;\n    }\n    return false;\n}\n\n";
        pluginCpp += "static int " + id + "Handler(void* ctx, const void* block, int phase) {\n";
        pluginCpp += "    SimCtx& sctx = *static_cast<SimCtx*>(ctx);\n";
        pluginCpp += "    const Block& blk = *static_cast<const Block*>(block);\n";
        pluginCpp
            += "    return handle_" + id + "(sctx, blk, static_cast<Phase>(phase)) ? 1 : 0;\n}\n\n";
        pluginCpp += "extern \"C\" {\n";
        pluginCpp += "int nfGetAbiVersion(void) { return NF_PLUGIN_ABI_VERSION; }\n";
        pluginCpp += "const NfBlockHandlerEntry* nfGetBlockHandlers(int* count) {\n";
        pluginCpp += "    static const NfBlockHandlerEntry handlers[] = { { \"" + fqType + "\", "
            + id + "Handler } };\n";
        pluginCpp += "    if (count) *count = 1;\n    return handlers;\n}\n";
        pluginCpp += "const char* nfGetPhaseInfo(void) { return \"{\\\"" + fqType
            + "\\\": [\\\"INIT\\\",\\\"OUTPUT\\\"]}\"; }\n}\n";
        if (!writeText(crateDir / FileSystemWrapper::Path(L"plugin.cpp"), pluginCpp)) {
            return errorResult("Cannot open plugin.cpp for writing");
        }
        std::string cmake = "cmake_minimum_required(VERSION 3.16)\n";
        cmake += "project(nflow_" + id + " VERSION 1.0.0 LANGUAGES C CXX)\n";
        cmake += "set(CMAKE_CXX_STANDARD 17)\n\n";
        cmake += "if(NOT DEFINED NFLOW_BLOCKS_INCLUDE)\n";
        cmake += "  if(DEFINED NFLOW_ROOT)\n    set(NFLOW_BLOCKS_INCLUDE "
                 "\"${NFLOW_ROOT}/src/include\")\n";
        cmake += "  else()\n    get_filename_component(NFLOW_ROOT "
                 "\"${CMAKE_CURRENT_SOURCE_DIR}/..\" ABSOLUTE)\n";
        cmake += "    set(NFLOW_BLOCKS_INCLUDE \"${NFLOW_ROOT}/src/include\")\n  "
                 "endif()\nendif()\n\n";
        cmake += "if(NOT DEFINED NFLOW_COMMONS_INCLUDE)\n";
        cmake
            += "  set(NFLOW_COMMONS_INCLUDE \"${NFLOW_ROOT}/../commons/src/include\")\nendif()\n\n";
        cmake += "add_library(nflow_" + id + " SHARED plugin.cpp model.c)\n";
        cmake += "target_include_directories(nflow_" + id
            + " PRIVATE ${NFLOW_BLOCKS_INCLUDE} ${NFLOW_COMMONS_INCLUDE})\n";
        cmake += "set_target_properties(nflow_" + id
            + " PROPERTIES PREFIX \"\" WINDOWS_EXPORT_ALL_SYMBOLS ON)\n\n";
        cmake += "if(MSVC)\n  set_target_properties(nflow_" + id
            + " PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})\n";
        cmake += "else()\n  set_target_properties(nflow_" + id
            + " PROPERTIES LIBRARY_OUTPUT_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})\nendif()\n\n";
        cmake += "add_custom_command(TARGET nflow_" + id
            + " POST_BUILD\n  COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:nflow_" + id
            + "> ${CMAKE_CURRENT_SOURCE_DIR}\n)\n";
        FileSystemWrapper::Path cmakePath = crateDir / FileSystemWrapper::Path(L"CMakeLists.txt");
        if (!writeText(cmakePath, cmake)) {
            return errorResult("Cannot open CMakeLists.txt for writing");
        }
        std::vector<PortPoint> extInputs;
        std::vector<PortPoint> extOutputs;
        int minY = INT_MAX, maxY = INT_MIN, minX = INT_MAX, maxX = INT_MIN;
        collectExternalPorts(subspec, extInputs, extOutputs, minY, maxY, minX, maxX);
        int inCount
            = std::max(static_cast<int>(inputFields.size()), static_cast<int>(extInputs.size()));
        int outCount
            = std::max(static_cast<int>(outputFields.size()), static_cast<int>(extOutputs.size()));
        int finalWidth = haveHostBlockSize ? hostBlockWidth : 200;
        int finalHeight = haveHostBlockSize ? hostBlockHeight
                                            : std::max(60, std::max(inCount, outCount) * 24 + 20);
        if (!haveHostBlockSize && minX != INT_MAX && maxX != INT_MIN) {
            finalWidth = std::max(finalWidth, (maxX - minX) + 40);
        }
        nlohmann::json blockDesc;
        blockDesc["type"] = fqType;
        blockDesc["label"] = name;
        blockDesc["nblock"] = id + ".nblock";
        blockDesc["phases"] = nlohmann::json::array({ "INIT", "OUTPUT" });
        blockDesc["width"] = finalWidth;
        blockDesc["height"] = finalHeight;
        blockDesc["render"] = nlohmann::json { { "type", "math" }, { "formula", name },
            { "bodyClass", "nf-block generated-subsystem" },
            { "mathGroupClass", "nf-generated-subsystem-formula" },
            { "backgroundColor", "#fff8d6" }, { "borderColor", "#d4b200" },
            { "textColor", "#333333" }, { "textSize", 16 } };
        if (inCount > 0) {
            blockDesc["inputs"] = makePortArray(extInputs, inCount, finalWidth, finalHeight, false);
        }
        if (outCount > 0) {
            blockDesc["outputs"]
                = makePortArray(extOutputs, outCount, finalWidth, finalHeight, true);
        }
        nlohmann::json lib;
        lib["id"] = fqType;
        lib["title"] = name + " (generated)";
        lib["format"] = "nflow-1";
        lib["native"] = "nflow_" + id;
        lib["version"] = "1.0.0";
        lib["blocks"] = nlohmann::json::array({ blockDesc });
        if (!writeText(crateDir / FileSystemWrapper::Path(L"library.json"), lib.dump(2))) {
            return errorResult("Cannot open library.json for writing");
        }
        nlohmann::json nblock = subspec;
        nblock["format"] = "nblock-1";
        nblock["version"] = "1.0.0";
        if (!writeText(crateDir / FileSystemWrapper::Path(utf8_to_wstring(id + ".nblock")),
                nblock.dump(2))) {
            return errorResult("Cannot open nblock file for writing");
        }
        nlohmann::json res { { "success", true }, { "dir", crateDir.string() } };
        if (opts.build) {
            std::wstring buildError;
            bool buildOk = nflow::callCMake(cmakePath.wstring(), nflow::getNflowCMakeDefines(),
                (crateDir / FileSystemWrapper::Path(L"build")).wstring(), buildError, buildConfig);
            res["buildSuccess"] = buildOk;
            if (!buildOk) {
                res["buildError"] = wstring_to_utf8(buildError);
            }
        }
        if (!blockWarnings.empty()) {
            res["warnings"] = blockWarnings;
        }
        return res;
    }
    //=============================================================================
} // namespace
//=============================================================================
std::string
generateCodeToDirRpc(const nlohmann::json& params)
{
    try {
        return generateCodeToDirImpl(params).dump();
    } catch (const std::exception& ex) {
        return errorResult(ex.what()).dump();
    } catch (...) {
        return errorResult("unknown exception").dump();
    }
}
//=============================================================================
std::string
generateBlockToDirRpc(const nlohmann::json& params)
{
    try {
        return generateBlockToDirImpl(params).dump();
    } catch (const std::exception& ex) {
        return errorResult(ex.what()).dump();
    } catch (...) {
        return errorResult("unknown exception").dump();
    }
}
//=============================================================================
bool
inlineUserBlocksForCodegen(nlohmann::json& diagram)
{
    return inlineGeneratedUserBlocks(diagram);
}
//=============================================================================
} // namespace Nelson::NFlowUiBindings
//=============================================================================
