//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_exportFmuBuiltin.hpp"
#include "NFlowCodegenC.hpp"
#include "NFlowCodegenFlatten.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "NFlowFmuExport.hpp"
#include "SimEngine.hpp"
#include "BlockRegistry.hpp"
#include "BuildCodeHelpers.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "characters_encoding.hpp"
#include "NFlowNelsonBridge.hpp"
#include "NFlowWorkspaceIO.hpp"
#include <cmath>
#include <fstream>
#include <iterator>
//=============================================================================
using namespace Nelson;
//=============================================================================
namespace Nelson::NFlowGateway {
//=============================================================================
// Sanitize a model name into a C identifier usable as the FMI modelIdentifier.
static std::string
sanitizeIdentifier(const std::string& name)
{
    std::string out;
    for (char c : name) {
        if ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
            || c == '_') {
            out += c;
        } else {
            out += '_';
        }
    }
    if (out.empty() || (out[0] >= '0' && out[0] <= '9')) {
        out = "m_" + out;
    }
    return out;
}
//=============================================================================
static void
writeTextFile(const FileSystemWrapper::Path& path, const std::string& content)
{
    std::ofstream f(path.string(), std::ios::binary);
    if (!f) {
        Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenOutputFileWritingValue",
            L"Cannot open output file for writing: %s", path.wstring());
    }
    f << content;
    f.close();
}
//=============================================================================
// FMI platform tuple and shared-library extension for the current platform.
// The tuple is <arch>-<os> per the FMI 3.0 layered standard, so an aarch64 host
// (Windows on ARM, Apple Silicon, ARM Linux) is tagged correctly.
static void
fmiPlatform(std::string& tuple, std::string& ext)
{
    std::string arch;
#if defined(__aarch64__) || defined(_M_ARM64)
    arch = "aarch64";
#elif defined(__arm__) || defined(_M_ARM)
    arch = "aarch32";
#elif defined(__x86_64__) || defined(_M_X64) || defined(_M_AMD64)
    arch = "x86_64";
#elif defined(__i386__) || defined(_M_IX86)
    arch = "x86";
#else
    arch = "x86_64";
#endif
#if defined(_WIN32)
    tuple = arch + "-windows";
    ext = ".dll";
#elif defined(__APPLE__)
    tuple = arch + "-darwin";
    ext = ".dylib";
#else
    tuple = arch + "-linux";
    ext = ".so";
#endif
}
//=============================================================================
// __nflow_export_fmu_sources__(modelFile, stagingDir)
//   Generates the FMI 3.0 Co-Simulation source-FMU tree under stagingDir:
//     stagingDir/modelDescription.xml
//     stagingDir/sources/{model.c, fmi3Functions.c, buildDescription.xml}
//   Returns the model identifier (used by NFlow.exportfmu to name the .fmu).
//   Float64 I/O only; other signal types (integer, bus) are rejected.
//=============================================================================
ArrayOfVector
nflow_exportFmuSourcesBuiltin(Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 2, 3);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring modelFilename = argIn[0].asWideString();
    const std::wstring stagingDir = argIn[1].asWideString();
    // Optional: path to the FMI 3.0 headers. When given (and a C toolchain is
    // present) the source FMU is also compiled to a platform binary.
    std::wstring fmiIncludeDir;
    if (argIn.size() >= 3) {
        fmiIncludeDir = argIn[2].asWideString();
    }

    std::string diagramStr;
    {
#ifdef _MSC_VER
        std::ifstream inputFile(modelFilename);
#else
        std::ifstream inputFile(wstring_to_utf8(modelFilename));
#endif
        if (!inputFile) {
            Error(L"Nelson:nflow_engine:cannotReadFile", L"Cannot open file for reading: %s",
                modelFilename);
        }
        diagramStr.assign(
            (std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());
    }
    // Pre-parse only for the export-specific model attributes (stopTime); the
    // shared pipeline re-parses after its interpreter passes.
    nflow::json diagram;
    try {
        diagram = nflow::json::parse(diagramStr);
    } catch (const nlohmann::json::exception&) {
        Error(L"Nelson:nflow_engine:parseError", L"Error parsing .nflow model file: %s",
            modelFilename);
    }

    nflow::CodegenOptions opts;
    opts.target = "C";
    opts.includeMain = false;
    if (diagram.contains("sampleTime") && diagram["sampleTime"].is_number()) {
        const double dt = diagram["sampleTime"].get<double>();
        if (std::isfinite(dt) && dt > 0.0) {
            opts.sampleTime = dt;
        }
    }
    double stopTime = 10.0;
    if (diagram.contains("duration") && diagram["duration"].is_number()) {
        stopTime = diagram["duration"].get<double>();
    }

    // The shared pipeline owns the gate sequence and runs the interpreter passes
    // (workspace baking, modelica/acausal expansion, linear-island reduction), so an
    // acausal model whose islands reduce or lower natively exports. This exporter
    // owns the FMU wording; every Error() throws.
    if (eval != nullptr) {
        ensureNelsonWorkspaceBridge(eval);
    }
    nflow::CodegenPipelineRequest pipelineRequest;
    pipelineRequest.diagramJson = std::move(diagramStr);
    pipelineRequest.options = opts;
    nflow::CodegenPipelineResult pipelineResult = nflow::runCodegenPipeline(pipelineRequest,
        (eval != nullptr) ? makeInterpreterCodegenHooks() : nflow::CodegenPipelineHooks {});
    for (const auto& d : pipelineResult.diagnostics) {
        if (d.severity == nflow::CodegenDiagnostic::Severity::Warning) {
            RaiseWarning(L"Nelson:nflow:codegenWarning",
                utf8_to_wstring(nflow::composeCodegenMessageEnglish(d)));
        }
    }
    if (const nflow::CodegenDiagnostic* e = pipelineResult.firstError()) {
        const nflow::CodegenDiagnostic& d = *e;
        if (d.gate == "parse") {
            Error(L"Nelson:nflow_engine:parseError", L"Error parsing .nflow model file: %s",
                modelFilename);
        }
        if (d.gate == "solver") {
            Error(L"Nelson:nflow_engine:fmuExportUnsupported:fMUExportNotSupportedVariableStep",
                L"FMU export is not supported for the variable-step solver '%s': it has no "
                L"embedded lowering, so the exported FMU would integrate fixed-step and differ "
                L"from the simulation. Select the fixed-step 'ode4' (RK4) / 'ode1' or the adaptive "
                L"'ode45' solver for FMU export.",
                utf8_to_wstring(d.detail));
        }
        if (d.gate == "conditional") {
            Error(L"Nelson:nflow_engine:fmuExportUnsupported",
                L"FMU export is not supported for conditional execution (%s, block '%s').",
                utf8_to_wstring(d.blockType), utf8_to_wstring(d.blockId));
        }
        if (d.gate == "nonGeneratable" || d.gate == "modelica") {
            Error(L"Nelson:nflow_engine:fmuExportUnsupportedBlocks",
                L"FMU export is not supported for FMU / nelsonFunction blocks; remove them before "
                L"exporting.");
        }
        if (d.gate == "multirate") {
            Error(L"Nelson:nflow_engine:fmuExportUnsupported:fMUExportNotYetSupportedA",
                L"FMU export is not yet supported for a multi-rate SampleTime on this block type "
                L"(block '%s'). It is supported on discrete state-holding blocks (unit delay, "
                L"difference, counters, edge and change detectors); for other blocks remove the "
                L"per-block SampleTime.",
                utf8_to_wstring(d.blockId));
        }
        if (d.gate == "unknownTypes") {
            Error(L"Nelson:nflow_engine:fmuExportUnsupported:fMUExportModelContainsBlockTypes",
                L"FMU export: the model contains block types with no code generation.");
        }
        if (!d.message.empty()) {
            Error(L"Nelson:nflow_engine:fmuExportFailed", L"FMU export: %s",
                utf8_to_wstring(d.message));
        }
        Error(L"Nelson:nflow_engine:fmuExportFailed", L"FMU export: %s",
            utf8_to_wstring(nflow::composeCodegenMessageEnglish(d)));
    }
    const nflow::CodegenCResult& cres = pipelineResult.c;

    // The wrapper maps scalar FMI types (Float64 / Int64 / UInt64). A bus or
    // other non-scalar boundary signal has no FMI type (empty) and is rejected.
    auto requireScalarFmiType = [](const std::vector<std::pair<std::string, std::string>>& io) {
        for (const auto& v : io) {
            if (v.second != "Float64" && v.second != "Int64" && v.second != "UInt64") {
                Error(
                    L"Nelson:nflow_engine:fmuExportUnsupported:fMUExportSupportsScalarFloat64Int64",
                    L"FMU export supports scalar Float64 / Int64 / UInt64 boundary signals; the "
                    L"port '%s' has no scalar FMI type (e.g. a bus). Split it before exporting.",
                    utf8_to_wstring(v.first));
            }
        }
    };
    requireScalarFmiType(cres.fmuInputs);
    requireScalarFmiType(cres.fmuOutputs);

    const std::string modelName
        = sanitizeIdentifier(FileSystemWrapper::Path(modelFilename).stem().string());

    std::vector<Nelson::NFlow::FmuInterfaceVar> inputs;
    for (const auto& v : cres.fmuInputs) {
        inputs.push_back({ v.first, v.second });
    }
    std::vector<Nelson::NFlow::FmuInterfaceVar> outputs;
    for (const auto& v : cres.fmuOutputs) {
        outputs.push_back({ v.first, v.second });
    }

    Nelson::NFlow::FmuModelInfo info = Nelson::NFlow::buildFmuModelInfo(
        modelName, "nflow-" + modelName, opts.sampleTime, stopTime, inputs, outputs);
    // stopSimulation hook: when the generated ModelState carries the stop
    // flag, fmi3DoStep stops sub-stepping on it and reports
    // terminateSimulation to the importer.
    info.hasStopFlag = cres.modelSource.find("nflow_stop_requested") != std::string::npos;

    // Lay out the FMU tree: modelDescription.xml at the root, C sources under
    // sources/.
    FileSystemWrapper::Path root(stagingDir);
    if (!root.is_directory()) {
        FileSystemWrapper::Path::create_directories(root);
    }
    FileSystemWrapper::Path sources = root / FileSystemWrapper::Path(L"sources");
    if (!sources.is_directory()) {
        FileSystemWrapper::Path::create_directories(sources);
    }

    writeTextFile(root / FileSystemWrapper::Path(L"modelDescription.xml"),
        Nelson::NFlow::buildModelDescriptionXml(info));
    writeTextFile(sources / FileSystemWrapper::Path(L"model.c"), cres.modelSource);
    writeTextFile(sources / FileSystemWrapper::Path(L"fmi3Functions.c"),
        Nelson::NFlow::buildFmi3FunctionsC(info));
    writeTextFile(sources / FileSystemWrapper::Path(L"buildDescription.xml"),
        Nelson::NFlow::buildBuildDescriptionXml(modelName));

    // Best-effort binary build: compile the source FMU into a loadable shared
    // library under binaries/<platform>/. Requires the FMI headers (3rd arg) and
    // a C toolchain; on any failure the source-only FMU is still produced.
    std::wstring tcErr;
    if (!fmiIncludeDir.empty() && nflow::haveCToolChain(tcErr)) {
        FileSystemWrapper::Path buildRoot = root / FileSystemWrapper::Path(L"_fmubuild");
        if (!buildRoot.is_directory()) {
            FileSystemWrapper::Path::create_directories(buildRoot);
        }
        const std::string srcPath
            = (sources / FileSystemWrapper::Path(L"fmi3Functions.c")).generic_string();
        std::string cml;
        cml += "cmake_minimum_required(VERSION 3.16)\n";
        // Statically link the C runtime so the FMU binary is self-contained and
        // loads from any importer's extraction directory (CMP0091 NEW makes the
        // MSVC_RUNTIME_LIBRARY property authoritative).
        cml += "cmake_policy(SET CMP0091 NEW)\n";
        cml += "project(" + modelName + " C)\n";
        cml += "set(CMAKE_C_STANDARD 99)\n";
        cml += "set(CMAKE_MSVC_RUNTIME_LIBRARY \"MultiThreaded\")\n";
        cml += "add_library(" + modelName + " SHARED \"" + srcPath + "\")\n";
        cml += "set_target_properties(" + modelName + " PROPERTIES PREFIX \"\")\n";
        cml += "target_include_directories(" + modelName + " PRIVATE \""
            + wstring_to_utf8(fmiIncludeDir) + "\")\n";
        writeTextFile(buildRoot / FileSystemWrapper::Path(L"CMakeLists.txt"), cml);

        std::wstring buildErr;
        FileSystemWrapper::Path buildDir = buildRoot / FileSystemWrapper::Path(L"build");
        const bool ok
            = nflow::callCMake((buildRoot / FileSystemWrapper::Path(L"CMakeLists.txt")).wstring(),
                L"", buildDir.wstring(), buildErr, L"Release");
        if (ok) {
            std::string tuple, ext;
            fmiPlatform(tuple, ext);
            const std::wstring wname = utf8_to_wstring(modelName);
            const std::wstring wext = utf8_to_wstring(ext);
            const FileSystemWrapper::Path candidates[] = { buildDir
                    / FileSystemWrapper::Path(L"Release") / FileSystemWrapper::Path(wname + wext),
                buildDir / FileSystemWrapper::Path(wname + wext) };
            for (const auto& cand : candidates) {
                if (cand.is_regular_file()) {
                    FileSystemWrapper::Path binDir = root / FileSystemWrapper::Path(L"binaries")
                        / FileSystemWrapper::Path(utf8_to_wstring(tuple));
                    if (!binDir.is_directory()) {
                        FileSystemWrapper::Path::create_directories(binDir);
                    }
                    FileSystemWrapper::Path::copy_file(
                        cand, binDir / FileSystemWrapper::Path(wname + wext));
                    break;
                }
            }
        } else {
            Warning(L"Nelson:nflow:fmuBinaryBuildFailed",
                L"FMU export: binary build failed; a source-only FMU was produced.\n%s", buildErr);
        }
        // The build tree is not part of the FMU.
        FileSystemWrapper::Path::remove_all(buildRoot);
    }

    ArrayOfVector retval;
    retval << ArrayOf::characterArray(modelName);
    return retval;
}
//=============================================================================
} // namespace Nelson::NFlowGateway
//=============================================================================
