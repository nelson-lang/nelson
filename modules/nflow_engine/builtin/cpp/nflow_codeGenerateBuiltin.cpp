//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "nflow_codeGenerateBuiltin.hpp"
#include "Error.hpp"
#include "ErrorID.hpp"
#include "Warning.hpp"
#include "i18n.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FileSystemWrapper.hpp"
#include "NFlowCodegenPipeline.hpp"
#include "NFlowCodegenHelpers.hpp"
#include "BlockRegistry.hpp"
#include "SimEngine.hpp"
#include "characters_encoding.hpp"
#include "BuildCodeHelpers.hpp"
#include "NFlowNelsonBridge.hpp"
#include <fstream>
#include <iterator>
//=============================================================================
using namespace Nelson;
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_codeGenerateBuiltin(
    Evaluator* eval, int nLhs, const ArrayOfVector& argIn)
{
    nargoutcheck(nLhs, 0, 1);
    nargincheck(argIn, 1, 5);
    ArrayOfVector retval;
    std::wstring language = L"C";
    std::wstring modelFilename = argIn[0].asWideString();
    if (!FileSystemWrapper::Path(modelFilename).is_regular_file()) {
        Error(L"Nelson:nflow_engine:modelFileNotFound", L"The model file does not exist: %s",
            modelFilename);
    }
    std::wstring destinationDirectory;
    if (argIn.size() > 1) {
        destinationDirectory = argIn[1].asWideString();
    } else {
        destinationDirectory = FileSystemWrapper::Path(modelFilename).current_path().wstring();
    }
    bool generateMain = false;
    if (argIn.size() > 2) {
        generateMain = argIn[2].asLogicalScalar();
    }
    if (argIn.size() > 3) {
        language = argIn[3].asWideString();
    }

    nflow::CodegenOptions opts;

    if (language != L"C" && language != L"rust") {
        Error(L"Nelson:nflow_engine:unsupportedLanguage", L"Unsupported language: %s", language);
    } else if (language == L"rust") {
        opts.target = "rust";
    } else {
        opts.target = "C";
    }
    opts.includeMain = generateMain;

    // Optional 5th argument: scalar struct with build (logical, default false),
    // buildConfig ('Release' | 'Debug', default 'Release') and failOnWarning
    // (logical, default false: promote pipeline warnings such as an RK4 downgrade
    // to errors).
    bool failOnWarning = false;
    std::wstring buildConfig = L"Release";
    if (argIn.size() > 4) {
        ArrayOf optionsArg = argIn[4];
        if (!optionsArg.isStruct() || !optionsArg.isScalar()) {
            Error(L"Nelson:nflow_engine:invalidArgument:optionsArgumentAScalarStruct",
                L"The options argument must be a scalar struct.");
        }
        if (optionsArg.isField("build")) {
            opts.build = optionsArg.field("build").asLogicalScalar();
        }
        if (optionsArg.isField("buildConfig")) {
            std::wstring cfg = optionsArg.field("buildConfig").asWideString();
            if (cfg != L"Release" && cfg != L"Debug") {
                Error(L"Nelson:nflow_engine:invalidValue:optionsBuildConfigReleaseDebug",
                    L"options.buildConfig must be 'Release' or 'Debug'.");
            }
            buildConfig = cfg;
        }
        if (optionsArg.isField("failOnWarning")) {
            failOnWarning = optionsArg.field("failOnWarning").asLogicalScalar();
        }
    }

#ifdef _MSC_VER
    std::ifstream inputFile(modelFilename);
#else
    std::ifstream inputFile(wstring_to_utf8(modelFilename));
#endif
    if (!inputFile) {
        Error(L"Nelson:nflow_engine:cannotReadFile", L"Cannot open file for reading: %s",
            modelFilename);
    }
    std::string diagramStr(
        (std::istreambuf_iterator<char>(inputFile)), std::istreambuf_iterator<char>());

    // The shared pipeline owns the whole gate sequence and the generation. This
    // builtin owns the translated wording and the file writing.
    if (eval != nullptr) {
        ensureNelsonWorkspaceBridge(eval);
    }
    nflow::CodegenPipelineRequest pipelineRequest;
    pipelineRequest.diagramJson = std::move(diagramStr);
    pipelineRequest.options = opts;
    pipelineRequest.failOnWarning = failOnWarning;
    nflow::CodegenPipelineResult pipelineResult = nflow::runCodegenPipeline(pipelineRequest,
        (eval != nullptr) ? makeInterpreterCodegenHooks() : nflow::CodegenPipelineHooks {});

    // Warnings first (visible, non-fatal), then the first error stops the builtin
    // before any file is written. Gate wording lives here, segmented through _W so
    // catalogs keep translating it; every Error() throws.
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
        if (d.gate == "modelica" || (d.gate == "nonGeneratable" && d.blockType == "modelica")) {
            Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationModelicaBlocks",
                L"Code generation is not supported for Modelica blocks; they are compiled to FMUs "
                L"for simulation, and generated standalone code cannot embed the FMU runtime. "
                L"Remove Modelica blocks before generating code.");
        }
        if (d.gate == "solver") {
            Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationNotSupportedVariableStep",
                L"Code generation is not supported for the variable-step solver '%s': it has no "
                L"embedded lowering, so the generated fixed-step code would differ from the "
                L"simulation. Select the fixed-step 'ode4' (RK4) / 'ode1' or the adaptive 'ode45' "
                L"solver for code generation, or simulate this model.",
                utf8_to_wstring(d.detail));
        }
        if (d.gate == "conditional") {
            Error(
                L"Nelson:nflow_engine:codegenUnsupported:codeGenerationNotYetSupportedConditional",
                L"Code generation is not yet supported for conditional execution (%s, block '%s'): "
                L"the control-port gate and if/switchCase/merge action selection have no C/Rust "
                L"lowering. Simulate these models with the solver instead.",
                utf8_to_wstring(d.blockType), utf8_to_wstring(d.blockId));
        }
        if (d.gate == "nonGeneratable") {
            if (d.blockType == "nelsonFunction") {
                Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationNelsonFunctionBlocks",
                    L"Code generation is not supported for nelsonFunction blocks (they run "
                    L"interpreted Nelson code). Replace them with the expression block or an "
                    L"equivalent subsystem.");
            }
            if (d.blockType == "integrator") {
                Error(
                    L"Nelson:nflow_engine:codegenUnsupported:codeGenerationIntegratorExternalReset",
                    L"Code generation is not supported for an integrator with an external reset or "
                    L"an external initial condition ('%s'): a discrete event on a continuous state "
                    L"has no C / Rust lowering, and emitting an integrator that never resets would "
                    L"not be this model. Simulate it, or reset through a resettable subsystem.",
                    utf8_to_wstring(d.detail));
            }
            if (d.blockType == "constraint") {
                Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationConstraintBlocks",
                    L"Code generation is not supported for constraint (differential-algebraic) "
                    L"blocks; they are solved by the IDAS runtime. Simulate these models with the "
                    L"'dae' solver instead.");
            }
            if (d.blockType == "physicalIsland") {
                Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationPhysicalIsland",
                    L"Code generation for this physical (acausal) island is not supported: the "
                    L"embedded-Newton lowering (C and Rust) covers "
                    L"resistor/conductor/capacitor/inductor/source/sensor/diode/radiation islands, "
                    L"but this one uses a component kind outside that set. Simulate these models "
                    L"with the 'dae' solver instead.");
            }
            if (d.blockType == "planarMechanicalIsland") {
                Error(L"Nelson:nflow_engine:codegenUnsupported:"
                      L"codeGenerationThisPlanarMechanicalIsland",
                    L"Code generation for this planar mechanical island is not supported: the "
                    L"joint-free lowering (C and Rust) covers bodies, loads, spring-dampers and "
                    L"position/speed/frame sensors, but this island uses the component kind '%s', "
                    L"which needs the runtime DAE solver. Simulate these models with the 'dae' "
                    L"solver instead.",
                    utf8_to_wstring(d.detail));
            }
            Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationFmuBlocks",
                L"Code generation is not supported for FMU (FMI Co-Simulation) blocks; the "
                L"generated program cannot embed the FMU runtime. Remove FMU blocks before "
                L"generating code.");
        }
        if (d.gate == "multirate") {
            Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationNotYetSupportedA",
                L"Code generation is not yet supported for a multi-rate SampleTime on this block "
                L"type (block '%s'). It is supported on discrete state-holding blocks (unit delay, "
                L"difference, counters, edge and change detectors); for other blocks remove the "
                L"per-block SampleTime or simulate with the solver instead.",
                utf8_to_wstring(d.blockId));
        }
        if (d.gate == "unknownTypes") {
            Error(L"Nelson:nflow_engine:codegenUnsupported:codeGenerationNotImplementedBlockTypes",
                L"Code generation is not implemented for these block types: %s",
                utf8_to_wstring(d.detail));
        }
        if (d.gate == "vectorExpand") {
            RaiseError(
                formatI18nMessage(Nelson::resolveMessageTemplate(
                                      L"Nelson:nflow_engine:codegenFailed", L"Code generation: %s"),
                    utf8_to_wstring(d.message))
                    + (d.blockId.empty() ? std::wstring {}
                                         : (L" ("
                                               + formatI18nMessage(
                                                   Nelson::resolveMessageTemplate(
                                                       L"Nelson:nflow_engine:block", L"block '%s'"),
                                                   utf8_to_wstring(d.blockId))
                                               + L")")),
                L"Nelson:nflow_engine:codegenFailed");
        }
        // workspace / expandModelica / expandAcausal / flatten / complexLower
        // and the generator findings (algebraicLoop, typedPort,
        // expressionRejected) carry an English detail in `message`;
        // hookMissing composes its own full text (prefix included).
        if (!d.message.empty()) {
            Error(L"Nelson:nflow_engine:codegenFailed", L"Code generation: %s",
                utf8_to_wstring(d.message));
        }
        RaiseError(utf8_to_wstring(nflow::composeCodegenMessageEnglish(d)),
            L"Nelson:nflow_engine:codegenFailed");
    }

    if (opts.target == "rust") {
        const nflow::CodegenRustResult& rustResult = pipelineResult.rust;

        // Root of the generated crate:  <destinationDirectory>/<modelStem>/
        FileSystemWrapper::Path outDir(destinationDirectory);
        FileSystemWrapper::Path stem = FileSystemWrapper::Path(modelFilename).stem();
        FileSystemWrapper::Path crateDir = outDir / stem / "";
        if (!crateDir.is_directory()) {
            FileSystemWrapper::Path::create_directories(crateDir);
        }

        FileSystemWrapper::Path srcDir = crateDir / FileSystemWrapper::Path(L"src");
        if (!srcDir.is_directory()) {
            FileSystemWrapper::Path::create_directories(srcDir);
        }

        FileSystemWrapper::Path libPath = srcDir / FileSystemWrapper::Path(L"lib.rs");
        FileSystemWrapper::Path mainPath = srcDir / FileSystemWrapper::Path(L"main.rs");
        FileSystemWrapper::Path cargoPath = crateDir / FileSystemWrapper::Path(L"Cargo.toml");

        std::ofstream libFile(libPath.string());
        if (!libFile) {
            Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenOutputFileWritingValue",
                L"Cannot open output file for writing: %s", libPath.wstring());
        }
        libFile << rustResult.rustSource;
        libFile.flush();
        libFile.close();

        if (opts.includeMain && !rustResult.mainSource.empty()) {
            std::ofstream mainFile(mainPath.string());
            if (!mainFile) {
                Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenMainRsWritingValue",
                    L"Cannot open main.rs for writing: %s", mainPath.wstring());
            }
            mainFile << rustResult.mainSource;
            mainFile.flush();
            mainFile.close();
        }

        std::ofstream cargoFile(cargoPath.string());
        if (!cargoFile) {
            Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenCargoTomlWritingValue",
                L"Cannot open Cargo.toml for writing: %s", cargoPath.wstring());
        }
        cargoFile << rustResult.cargoToml;
        cargoFile.flush();
        cargoFile.close();

        if (opts.build) {
            std::wstring errorMessage;

            std::wstring buildOptions = L"--release";

            bool ok = nflow::callCargoToml(cargoPath.wstring(), buildOptions,
                (cargoPath.parent_path() / L"target").wstring(), errorMessage);

            if (!ok) {
                Error(L"Nelson:nflow_engine:buildRustFailed", L"Failed to build Rust crate: %s",
                    errorMessage);
            }
        }

        if (nLhs > 0) {
            retval << ArrayOf::characterArray(crateDir.string());
        }
        return retval;
    }
    const nflow::CodegenCResult& cres = pipelineResult.c;

    // Root of the generated output: <destinationDirectory>/<modelStem>/
    FileSystemWrapper::Path outDir(destinationDirectory);
    FileSystemWrapper::Path stem = FileSystemWrapper::Path(modelFilename).stem();
    FileSystemWrapper::Path crateDir = outDir / stem / "";
    if (!crateDir.is_directory()) {
        FileSystemWrapper::Path::create_directories(crateDir);
    }

    FileSystemWrapper::Path modelPath = crateDir / FileSystemWrapper::Path(L"model.c");
    std::ofstream modelFile(modelPath.string());
    if (!modelFile) {
        Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenOutputFileWritingValue",
            L"Cannot open output file for writing: %s", modelPath.wstring());
    }
    modelFile << cres.modelSource;
    modelFile.flush();
    modelFile.close();

    if (opts.includeMain && !cres.mainSource.empty()) {
        FileSystemWrapper::Path mainPath = crateDir / FileSystemWrapper::Path(L"main.c");
        std::ofstream mainFile(mainPath.string());
        if (!mainFile) {
            Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenMainCWritingValue",
                L"Cannot open main.c for writing: %s", mainPath.wstring());
        }
        mainFile << cres.mainSource;
        mainFile.flush();
        mainFile.close();
    }

    FileSystemWrapper::Path cmakePath = crateDir / FileSystemWrapper::Path(L"CMakeLists.txt");
    std::ofstream cmakeFile(cmakePath.string());
    if (!cmakeFile) {
        Error(L"Nelson:nflow_engine:cannotWriteFile:cannotOpenCMakeListsTxtWritingValue",
            L"Cannot open CMakeLists.txt for writing: %s", cmakePath.wstring());
    }
    cmakeFile << cres.cmake;
    cmakeFile.flush();
    cmakeFile.close();

    if (opts.build) {
        std::wstring errorMessage;
        FileSystemWrapper::Path buildDir = crateDir / L"build";
        // callCMake takes the CMakeLists.txt FILE path (it derives the source dir)
        // and drives the multi-config generator through buildConfig.
        bool buildSuccess = nflow::callCMake(
            cmakePath.wstring(), L"", buildDir.wstring(), errorMessage, buildConfig);
        if (!buildSuccess) {
            Error(L"Nelson:nflow_engine:buildCFailed", L"Failed to build generated C code: %s",
                errorMessage);
        }
    }
    if (nLhs > 0) {
        retval << ArrayOf::characterArray(crateDir.string());
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_codegenToolchainBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);
    const std::wstring lang = argIn[0].asWideString();
    std::wstring probeError;
    bool available = false;
#ifdef __EMSCRIPTEN__
    if (lang == L"C" || lang == L"c" || lang == L"rust") {
        available = false;
    } else {
        Error(L"Nelson:nflow_engine:unsupportedLanguage", L"Unsupported language: %s", lang);
    }
#else
    if (lang == L"C" || lang == L"c") {
        available = nflow::haveCToolChain(probeError);
    } else if (lang == L"rust") {
        available = nflow::haveRustToolChain(probeError);
    } else {
        Error(L"Nelson:nflow_engine:unsupportedLanguage", L"Unsupported language: %s", lang);
    }
#endif
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(available);
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_codegenSupportBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 2);
    const std::string type = wstring_to_utf8(argIn[0].asWideString());
    // Registration is lazy: populate the block registry (handlers + codegen
    // templates) before querying getCodegen, or the registry is empty.
    Nelson::NFlow::ensureBuiltinsRegistered();
    // A block type supports a target when it can SIMULATE (a registered handler;
    // codegen-only helpers like luScalar, or a template registered without a
    // handler, must not report support) AND one of:
    //  - a codegen template is registered for (type, lang) and is not the
    //    explicit "unimplemented" placeholder;
    //  - the generators lower it through a native emitter branch or a
    //    pre-generation pass (subsystem flattening, complex lowering, vector
    //    expansion erasure, dashboard plan): hasNativeCodegenPath;
    //  - the vector-expansion pass lowers it into scalar primitive blocks
    //    (vector-math reductions, lookup helpers): expandsToGeneratableCode.
    const bool haveSim = Nelson::NFlow::BlockRegistry::instance().getHandler(type) != nullptr;
    Nelson::NFlow::BlockCodegenTemplate tpl;
    const bool cSupported = haveSim
        && ((Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "c", tpl)
                && !tpl.notImplemented)
            || nflow::hasNativeCodegenPath(type) || nflow::expandsToGeneratableCode(type, "c"));
    const bool rustSupported = haveSim
        && ((Nelson::NFlow::BlockRegistry::instance().getCodegen(type, "rust", tpl)
                && !tpl.notImplemented)
            || nflow::hasNativeCodegenPath(type) || nflow::expandsToGeneratableCode(type, "rust"));
    ArrayOfVector retval;
    retval << ArrayOf::logicalScalar(cSupported);
    if (nLhs > 1) {
        retval << ArrayOf::logicalScalar(rustSupported);
    }
    return retval;
}
//=============================================================================
ArrayOfVector
Nelson::NFlowGateway::nflow_codegenAsymmetriesBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 0, 0);
    nargoutcheck(nLhs, 0, 1);
    Nelson::NFlow::ensureBuiltinsRegistered();
    const std::vector<std::string> bad
        = Nelson::NFlow::BlockRegistry::instance().codegenLanguageAsymmetries();
    Dimensions dims(1, bad.size());
    ArrayOf* elements = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, dims.elementCount(), stringVector(), false));
    ArrayOf cell(NLS_CELL_ARRAY, dims, elements);
    for (size_t k = 0; k < bad.size(); ++k) {
        elements[k] = ArrayOf::characterArray(bad[k]);
    }
    ArrayOfVector retval;
    retval << cell;
    return retval;
}
//=============================================================================
