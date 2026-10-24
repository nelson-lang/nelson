//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#if _MSC_VER
#pragma warning(disable : 4251)
#endif
//=============================================================================
#include <cstdint>
#include <memory>
#include <string>
#include <vector>
#include "nlsNflow_fmi_exports.h"
//=============================================================================
namespace Nelson::NFlow::Fmi {
//=============================================================================
class FmiInstance;
//=============================================================================
// Backend-neutral view of an FMI 3.0 scalar variable. No fmi4c types leak
// through this header: the enum-like fields are plain ints mirrored from the
// fmi4c causality / data-type enumerations by the implementation.
//=============================================================================
enum class FmiCausality : int
{
    Parameter = 0,
    CalculatedParameter,
    Input,
    Output,
    Local,
    Independent,
    StructuralParameter,
    Unknown
};
//=============================================================================
enum class FmiDataType : int
{
    Float64 = 0,
    Float32,
    Int64,
    Int32,
    Int16,
    Int8,
    UInt64,
    UInt32,
    UInt16,
    UInt8,
    Boolean,
    String,
    Binary,
    Clock,
    Enumeration,
    Unknown
};
//=============================================================================
// One <Dimension> of an array variable: either a fixed size (hasStart) or
// bound to a structural-parameter variable's value (valueReference).
struct FmiDimension
{
    bool hasStart = false;
    uint64_t start = 1; // fixed size, or the bound variable's default start
    uint32_t valueReference = 0; // size variable VR when !hasStart
};
//=============================================================================
struct FmiVariable
{
    std::string name;
    uint32_t valueReference = 0;
    FmiCausality causality = FmiCausality::Unknown;
    FmiDataType dataType = FmiDataType::Unknown;
    std::string description;
    bool hasStartValue = false;
    double startValue = 0.0; // Float64 start value when hasStartValue is true
    // Number of array elements (product of the dimension sizes, with a
    // value-reference-bound dimension resolved to the bound structural
    // parameter's start value). 1 for a scalar variable.
    int arraySize = 1;
    // Per-dimension descriptors (empty for a scalar variable). A dimension
    // bound to a structural parameter carries both the parameter's VR and
    // its default start size, so the effective size under a parameter
    // override can be recomputed (see effectiveArraySize).
    std::vector<FmiDimension> dimensions;
};
//=============================================================================
// Array size of `v` when the structural parameters listed in
// `overridesByVr` (dimension VR -> overridden value) differ from their
// start values. Falls back to the declared start size per dimension.
NLSNFLOW_FMI_IMPEXP int
effectiveArraySize(
    const FmiVariable& v, const std::vector<std::pair<uint32_t, uint64_t>>& overridesByVr);
//=============================================================================
struct FmiModelInfo
{
    std::string modelIdentifier;
    std::string fmiVersion; // e.g. "3.0"
    bool supportsCoSimulation = false;
    bool supportsModelExchange = false;
    bool supportsScheduledExecution = false;
    // FMU declares canGetAndSetFMUState: its full state can be saved and restored
    // (needed for iterative co-simulation coupling and step rejection).
    bool canGetAndSetState = false;
    std::vector<FmiVariable> variables;
};
//=============================================================================
// NFlow extracts the FMU itself (hardened extractor, content-addressed cache)
// and hands fmi4c only an unzipped directory through loadUnzipped(), so the
// archive code inside fmi4c never runs.
//=============================================================================
class NLSNFLOW_FMI_IMPEXP FmiPackage
{
public:
    // Loads an already-extracted FMU directory. Returns nullptr and fills
    // errorMessage on failure (not an FMI 3.0 package, missing binary, ...).
    static std::unique_ptr<FmiPackage>
    loadUnzipped(
        const std::string& instanceName, const std::string& unzippedDir, std::string& errorMessage);

    ~FmiPackage();

    FmiPackage(const FmiPackage&) = delete;
    FmiPackage&
    operator=(const FmiPackage&)
        = delete;

    const FmiModelInfo&
    info() const;

    // Instantiates a Co-Simulation instance of this FMU. Returns nullptr and
    // fills errorMessage on failure. The returned instance borrows this
    // package, which must outlive it.
    std::unique_ptr<FmiInstance>
    instantiateCoSimulation(
        const std::string& instanceName, bool loggingOn, std::string& errorMessage);

    std::unique_ptr<FmiInstance>
    instantiateModelExchange(
        const std::string& instanceName, bool loggingOn, std::string& errorMessage);

private:
    FmiPackage();
    struct Impl;
    std::unique_ptr<Impl> pImpl;
};
//=============================================================================
// True when the module was built with the fmi4c importer. Always true today
// (fmi4c is vendored); kept as a seam for a possible build without it.
//=============================================================================
NLSNFLOW_FMI_IMPEXP bool
fmiSupportCompiledIn();
//=============================================================================
NLSNFLOW_FMI_IMPEXP std::string
fmi4cBuildInfo();
//=============================================================================
} // namespace Nelson::NFlow::Fmi
//=============================================================================
