//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include <cmath>
#include "nflow_fmi_inspectBuiltin.hpp"
#include "InputOutputArgumentsCheckers.hpp"
#include "FmiPackage.hpp"
#include "Error.hpp"
#include "characters_encoding.hpp"
#include "ErrorID.hpp"
//=============================================================================
using namespace Nelson;
using namespace Nelson::NFlow::Fmi;
//=============================================================================
static std::string
causalityToString(FmiCausality c)
{
    switch (c) {
    case FmiCausality::Parameter:
        return "parameter";
    case FmiCausality::CalculatedParameter:
        return "calculatedParameter";
    case FmiCausality::Input:
        return "input";
    case FmiCausality::Output:
        return "output";
    case FmiCausality::Local:
        return "local";
    case FmiCausality::Independent:
        return "independent";
    case FmiCausality::StructuralParameter:
        return "structuralParameter";
    default:
        return "unknown";
    }
}
//=============================================================================
static std::string
dataTypeToString(FmiDataType t)
{
    switch (t) {
    case FmiDataType::Float64:
        return "Float64";
    case FmiDataType::Float32:
        return "Float32";
    case FmiDataType::Int64:
        return "Int64";
    case FmiDataType::Int32:
        return "Int32";
    case FmiDataType::Int16:
        return "Int16";
    case FmiDataType::Int8:
        return "Int8";
    case FmiDataType::UInt64:
        return "UInt64";
    case FmiDataType::UInt32:
        return "UInt32";
    case FmiDataType::UInt16:
        return "UInt16";
    case FmiDataType::UInt8:
        return "UInt8";
    case FmiDataType::Boolean:
        return "Boolean";
    case FmiDataType::String:
        return "String";
    case FmiDataType::Binary:
        return "Binary";
    case FmiDataType::Clock:
        return "Clock";
    case FmiDataType::Enumeration:
        return "Enumeration";
    default:
        return "Unknown";
    }
}
//=============================================================================
// __nflow_fmi_inspect__(unzippedFmuDir) -> struct
//
// Reads the modelDescription of an already-extracted FMI 3.0 FMU directory and
// returns its model info + variable table. NFlow owns FMU extraction; this
// builtin operates on the extracted directory.
//=============================================================================
ArrayOfVector
Nelson::NFlowFmiGateway::nflow_fmi_inspectBuiltin(int nLhs, const ArrayOfVector& argIn)
{
    nargincheck(argIn, 1, 1);
    nargoutcheck(nLhs, 0, 1);

    if (!fmiSupportCompiledIn()) {
        Error("Nelson:nflow_fmi:notCompiled", "FMI support is not compiled in this build.");
    }

    const std::wstring wdir = argIn[0].asWideString();
    const std::string dir = wstring_to_utf8(wdir);

    std::string errorMessage;
    std::unique_ptr<FmiPackage> pkg = FmiPackage::loadUnzipped("nelson_inspect", dir, errorMessage);
    if (!pkg) {
        RaiseError(errorMessage, "Nelson:nflow_fmi:loadFailed");
    }

    const FmiModelInfo& mi = pkg->info();
    const size_t nVars = mi.variables.size();

    ArrayOf* nameCells = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nVars, stringVector(), false));
    ArrayOf* causCells = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nVars, stringVector(), false));
    ArrayOf* typeCells = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nVars, stringVector(), false));
    ArrayOf* descCells = static_cast<ArrayOf*>(
        ArrayOf::allocateArrayOf(NLS_CELL_ARRAY, nVars, stringVector(), false));
    double* vrefs
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nVars, stringVector(), false));
    // Float64 start value per variable; NaN when the variable declares none.
    double* starts
        = static_cast<double*>(ArrayOf::allocateArrayOf(NLS_DOUBLE, nVars, stringVector(), false));

    for (size_t i = 0; i < nVars; ++i) {
        const FmiVariable& v = mi.variables[i];
        nameCells[i] = ArrayOf::characterArray(v.name);
        causCells[i] = ArrayOf::characterArray(causalityToString(v.causality));
        typeCells[i] = ArrayOf::characterArray(dataTypeToString(v.dataType));
        descCells[i] = ArrayOf::characterArray(v.description);
        vrefs[i] = static_cast<double>(v.valueReference);
        starts[i] = v.hasStartValue ? v.startValue : std::nan("");
    }

    Dimensions colDim(nVars, 1);
    ArrayOf namesArray(NLS_CELL_ARRAY, colDim, nameCells);
    ArrayOf causArray(NLS_CELL_ARRAY, colDim, causCells);
    ArrayOf typeArray(NLS_CELL_ARRAY, colDim, typeCells);
    ArrayOf descArray(NLS_CELL_ARRAY, colDim, descCells);
    ArrayOf vrefArray(NLS_DOUBLE, colDim, vrefs);
    ArrayOf startArray(NLS_DOUBLE, colDim, starts);

    stringVector fieldnames = { "modelIdentifier", "fmiVersion", "coSimulation", "modelExchange",
        "scheduledExecution", "variableNames", "variableValueReferences", "variableCausality",
        "variableDataType", "variableDescriptions", "variableStartValues" };
    ArrayOfVector fieldvalues;
    fieldvalues << ArrayOf::characterArray(mi.modelIdentifier);
    fieldvalues << ArrayOf::characterArray(mi.fmiVersion);
    fieldvalues << ArrayOf::logicalScalar(mi.supportsCoSimulation);
    fieldvalues << ArrayOf::logicalScalar(mi.supportsModelExchange);
    fieldvalues << ArrayOf::logicalScalar(mi.supportsScheduledExecution);
    fieldvalues << namesArray;
    fieldvalues << vrefArray;
    fieldvalues << causArray;
    fieldvalues << typeArray;
    fieldvalues << descArray;
    fieldvalues << startArray;

    ArrayOfVector retval;
    retval << ArrayOf::structScalar(fieldnames, fieldvalues);
    return retval;
}
//=============================================================================
