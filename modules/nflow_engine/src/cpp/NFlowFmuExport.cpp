//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#include "NFlowFmuExport.hpp"
#include "NFlowVersion.hpp"
#include <cmath>
#include <cstdio>
#include <cstdlib>
#include <sstream>
//=============================================================================
namespace Nelson {
//=============================================================================
namespace NFlow {
    //=============================================================================
    static std::string
    xmlEscape(const std::string& s)
    {
        std::string out;
        out.reserve(s.size());
        for (char c : s) {
            switch (c) {
            case '&':
                out += "&amp;";
                break;
            case '<':
                out += "&lt;";
                break;
            case '>':
                out += "&gt;";
                break;
            case '"':
                out += "&quot;";
                break;
            case '\'':
                out += "&apos;";
                break;
            default:
                out += c;
                break;
            }
        }
        return out;
    }
    //=============================================================================
    // Locale-independent double for an XML attribute, preferring a readable plain
    // form: an integer-valued double prints without a fraction ("10", "0"), other
    // values use the shortest round-trippable decimal ("0.01", not the %g
    // scientific "1e-02" nor "0.010000000000000000").
    static std::string
    fmtDouble(double v)
    {
        char buf[64];
        if (std::isfinite(v) && v == std::floor(v) && std::fabs(v) < 1e15) {
            std::snprintf(buf, sizeof(buf), "%.0f", v);
            return std::string(buf);
        }
        for (int prec = 1; prec < 17; ++prec) {
            std::snprintf(buf, sizeof(buf), "%.*g", prec, v);
            if (std::strtod(buf, nullptr) == v) {
                return std::string(buf);
            }
        }
        std::snprintf(buf, sizeof(buf), "%.17g", v);
        return std::string(buf);
    }
    //=============================================================================
    static std::string
    variabilityOf(const std::string& type)
    {
        return (type == "Float64") ? "continuous" : "discrete";
    }
    //=============================================================================
    FmuModelInfo
    buildFmuModelInfo(const std::string& modelName, const std::string& instantiationToken,
        double stepSize, double stopTime, const std::vector<FmuInterfaceVar>& inputs,
        const std::vector<FmuInterfaceVar>& outputs)
    {
        FmuModelInfo info;
        info.modelName = modelName;
        info.instantiationToken = instantiationToken;
        info.generationTool = "Nelson/nflow " NFLOW_PRODUCT_VERSION;
        info.stepSize = stepSize;
        info.stopTime = stopTime;

        unsigned vr = 0;
        FmuExportVar t;
        t.name = "time";
        t.causality = "independent";
        t.type = "Float64";
        t.valueReference = vr++;
        t.start = "0";
        info.variables.push_back(t);

        for (const auto& in : inputs) {
            FmuExportVar v;
            v.name = in.name;
            v.causality = "input";
            v.type = in.type;
            v.valueReference = vr++;
            v.start = "0";
            info.variables.push_back(v);
        }
        for (const auto& out : outputs) {
            FmuExportVar v;
            v.name = out.name;
            v.causality = "output";
            v.type = out.type;
            v.valueReference = vr++;
            // Calculated output: no start.
            info.variables.push_back(v);
        }
        return info;
    }
    //=============================================================================
    std::string
    buildModelDescriptionXml(const FmuModelInfo& info)
    {
        std::ostringstream os;
        os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        os << "<fmiModelDescription fmiVersion=\"3.0\" modelName=\"" << xmlEscape(info.modelName)
           << "\" instantiationToken=\"" << xmlEscape(info.instantiationToken)
           << "\" generationTool=\"" << xmlEscape(info.generationTool) << "\">\n";
        os << "  <CoSimulation modelIdentifier=\"" << xmlEscape(info.modelName)
           << "\" canHandleVariableCommunicationStepSize=\"true\" canGetAndSetFMUState=\"true\""
           << " fixedInternalStepSize=\"" << fmtDouble(info.stepSize) << "\"/>\n";
        os << "  <DefaultExperiment startTime=\"0\" stopTime=\"" << fmtDouble(info.stopTime)
           << "\" stepSize=\"" << fmtDouble(info.stepSize) << "\"/>\n";

        os << "  <ModelVariables>\n";
        for (const auto& v : info.variables) {
            os << "    <" << v.type << " name=\"" << xmlEscape(v.name) << "\" valueReference=\""
               << v.valueReference << "\" causality=\"" << v.causality << "\" variability=\""
               << variabilityOf(v.type) << "\"";
            if (!v.start.empty()) {
                os << " start=\"" << xmlEscape(v.start) << "\"";
            }
            os << "/>\n";
        }
        os << "  </ModelVariables>\n";

        os << "  <ModelStructure>\n";
        for (const auto& v : info.variables) {
            if (v.causality == "output") {
                os << "    <Output valueReference=\"" << v.valueReference << "\"/>\n";
            }
        }
        for (const auto& v : info.variables) {
            if (v.causality == "output") {
                os << "    <InitialUnknown valueReference=\"" << v.valueReference << "\"/>\n";
            }
        }
        os << "  </ModelStructure>\n";

        os << "</fmiModelDescription>\n";
        return os.str();
    }
    //=============================================================================
    std::string
    buildBuildDescriptionXml(const std::string& modelIdentifier)
    {
        std::ostringstream os;
        os << "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        os << "<fmiBuildDescription fmiVersion=\"3.0\">\n";
        os << "  <BuildConfiguration modelIdentifier=\"" << xmlEscape(modelIdentifier) << "\">\n";
        os << "    <SourceFileSet language=\"C99\">\n";
        os << "      <SourceFile name=\"model.c\"/>\n";
        os << "      <SourceFile name=\"fmi3Functions.c\"/>\n";
        os << "    </SourceFileSet>\n";
        os << "  </BuildConfiguration>\n";
        os << "</fmiBuildDescription>\n";
        return os.str();
    }
    //=============================================================================
    std::string
    buildFmi3FunctionsC(const FmuModelInfo& info)
    {
        std::ostringstream os;
        os << "/* Generated by Nelson/nflow - FMI 3.0 Co-Simulation wrapper. */\n";
        os << "#include <stdlib.h>\n";
        os << "#include <string.h>\n";
        os << "#include \"fmi3Functions.h\"\n";
        os << "#include \"model.c\"\n\n";

        os << "typedef struct {\n";
        os << "    ModelState state;\n";
        os << "    ModelInput input;\n";
        os << "    ModelOutput output;\n";
        os << "    double time;\n";
        os << "} nflow_fmu_instance;\n\n";

        os << "const char* fmi3GetVersion(void) { return \"3.0\"; }\n\n";

        os << "fmi3Status fmi3SetDebugLogging(fmi3Instance instance, fmi3Boolean loggingOn,\n";
        os << "        size_t nCategories, const fmi3String categories[]) {\n";
        os << "    (void)instance; (void)loggingOn; (void)nCategories; (void)categories;\n";
        os << "    return fmi3OK;\n}\n\n";

        os << "fmi3Instance fmi3InstantiateCoSimulation(\n";
        os << "        fmi3String instanceName, fmi3String instantiationToken,\n";
        os << "        fmi3String resourcePath, fmi3Boolean visible, fmi3Boolean loggingOn,\n";
        os << "        fmi3Boolean eventModeUsed, fmi3Boolean earlyReturnAllowed,\n";
        os << "        const fmi3ValueReference requiredIntermediateVariables[],\n";
        os << "        size_t nRequiredIntermediateVariables,\n";
        os << "        fmi3InstanceEnvironment instanceEnvironment,\n";
        os << "        fmi3LogMessageCallback logMessage,\n";
        os << "        fmi3IntermediateUpdateCallback intermediateUpdate) {\n";
        os << "    (void)instanceName; (void)instantiationToken; (void)resourcePath;\n";
        os << "    (void)visible; (void)loggingOn; (void)eventModeUsed;\n";
        os << "    (void)earlyReturnAllowed; (void)requiredIntermediateVariables;\n";
        os << "    (void)nRequiredIntermediateVariables; (void)instanceEnvironment;\n";
        os << "    (void)logMessage; (void)intermediateUpdate;\n";
        os << "    nflow_fmu_instance* c =\n";
        os << "        (nflow_fmu_instance*)calloc(1, sizeof(nflow_fmu_instance));\n";
        os << "    if (c) { nflow_init(&c->state); c->time = 0.0; }\n";
        os << "    return c;\n}\n\n";

        os << "void fmi3FreeInstance(fmi3Instance instance) {\n";
        os << "    if (instance) {\n";
        os << "        nflow_terminate(&((nflow_fmu_instance*)instance)->state);\n";
        os << "        free(instance);\n";
        os << "    }\n}\n\n";

        os << "fmi3Status fmi3EnterInitializationMode(fmi3Instance instance,\n";
        os << "        fmi3Boolean toleranceDefined, fmi3Float64 tolerance,\n";
        os << "        fmi3Float64 startTime, fmi3Boolean stopTimeDefined,\n";
        os << "        fmi3Float64 stopTime) {\n";
        os << "    (void)toleranceDefined; (void)tolerance;\n";
        os << "    (void)stopTimeDefined; (void)stopTime;\n";
        os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
        os << "    if (!c) return fmi3Error;\n";
        os << "    c->time = startTime;\n";
        os << "    return fmi3OK;\n}\n\n";

        os << "fmi3Status fmi3ExitInitializationMode(fmi3Instance instance) {\n";
        os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
        os << "    if (!c) return fmi3Error;\n";
        os << "    nflow_step(&c->state, &c->input, &c->output, c->time);\n";
        os << "    return fmi3OK;\n}\n\n";

        os << "fmi3Status fmi3EnterStepMode(fmi3Instance instance) {\n";
        os << "    (void)instance; return fmi3OK;\n}\n\n";

        os << "fmi3Status fmi3Terminate(fmi3Instance instance) {\n";
        os << "    (void)instance; return fmi3OK;\n}\n\n";

        os << "fmi3Status fmi3Reset(fmi3Instance instance) {\n";
        os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
        os << "    if (!c) return fmi3Error;\n";
        os << "    nflow_init(&c->state);\n";
        os << "    memset(&c->input, 0, sizeof(c->input));\n";
        os << "    memset(&c->output, 0, sizeof(c->output));\n";
        os << "    c->time = 0.0;\n";
        os << "    return fmi3OK;\n}\n\n";

        os << "fmi3Status fmi3DoStep(fmi3Instance instance,\n";
        os << "        fmi3Float64 currentCommunicationPoint,\n";
        os << "        fmi3Float64 communicationStepSize,\n";
        os << "        fmi3Boolean noSetFMUStatePriorToCurrentPoint,\n";
        os << "        fmi3Boolean* eventHandlingNeeded, fmi3Boolean* terminateSimulation,\n";
        os << "        fmi3Boolean* earlyReturn, fmi3Float64* lastSuccessfulTime) {\n";
        os << "    (void)noSetFMUStatePriorToCurrentPoint;\n";
        os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
        os << "    if (!c) return fmi3Error;\n";
        // Sub-step at the model's fixed internal rate so a communication step larger
        // than the model step runs several updates (inputs held: zero-order hold).
        os << "    {\n";
        os << "        const double model_dt = " << fmtDouble(info.stepSize) << ";\n";
        os << "        const double t_end = currentCommunicationPoint + communicationStepSize;\n";
        os << "        int nsub = (model_dt > 0.0)\n";
        os << "            ? (int)(communicationStepSize / model_dt + 0.5) : 1;\n";
        os << "        if (nsub < 1) nsub = 1;\n";
        os << "        for (int k = 0; k < nsub; ++k) {\n";
        os << "            c->time = currentCommunicationPoint + (double)(k + 1) * model_dt;\n";
        os << "            nflow_step(&c->state, &c->input, &c->output, c->time);\n";
        if (info.hasStopFlag) {
            // stopSimulation hook: a raised flag ends the run cleanly; stop
            // sub-stepping and report terminateSimulation to the importer.
            os << "            if (c->state.nflow_stop_requested != 0.0) break;\n";
        }
        os << "        }\n";
        os << "        c->time = t_end;\n";
        os << "    }\n";
        os << "    if (eventHandlingNeeded) *eventHandlingNeeded = fmi3False;\n";
        if (info.hasStopFlag) {
            os << "    if (terminateSimulation) *terminateSimulation = "
                  "(c->state.nflow_stop_requested != 0.0) ? fmi3True : fmi3False;\n";
        } else {
            os << "    if (terminateSimulation) *terminateSimulation = fmi3False;\n";
        }
        os << "    if (earlyReturn) *earlyReturn = fmi3False;\n";
        os << "    if (lastSuccessfulTime) *lastSuccessfulTime = c->time;\n";
        os << "    return fmi3OK;\n}\n\n";

        // Typed get/set, one pair per FMI type. Each switches on the value
        // references of variables of that type into the matching ModelInput /
        // ModelOutput field (fmi3Float64/Int64/UInt64 are the exact C field
        // types). getFloat64 also serves the independent variable (time). A type
        // with no variables yields just the default branch, i.e. a conforming
        // "unsupported reference" response.
        auto emitGet = [&](const std::string& fmiType, const std::string& cType) {
            os << "fmi3Status fmi3Get" << fmiType << "(fmi3Instance instance,\n";
            os << "        const fmi3ValueReference valueReferences[], size_t nValueReferences,\n";
            os << "        " << cType << " values[], size_t nValues) {\n";
            os << "    (void)nValues;\n";
            os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
            os << "    if (!c) return fmi3Error;\n";
            os << "    for (size_t i = 0; i < nValueReferences; i++) {\n";
            os << "        switch (valueReferences[i]) {\n";
            if (fmiType == "Float64") {
                os << "        case 0: values[i] = (" << cType << ")c->time; break;\n";
            }
            for (const auto& v : info.variables) {
                if (v.causality == "independent" || v.type != fmiType) {
                    continue;
                }
                const std::string src = (v.causality == "input") ? "c->input." : "c->output.";
                os << "        case " << v.valueReference << ": values[i] = " << src << v.name
                   << "; break;\n";
            }
            os << "        default: return fmi3Error;\n        }\n    }\n    return fmi3OK;\n}\n\n";
        };
        auto emitSet = [&](const std::string& fmiType, const std::string& cType) {
            os << "fmi3Status fmi3Set" << fmiType << "(fmi3Instance instance,\n";
            os << "        const fmi3ValueReference valueReferences[], size_t nValueReferences,\n";
            os << "        const " << cType << " values[], size_t nValues) {\n";
            os << "    (void)nValues;\n";
            os << "    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;\n";
            os << "    if (!c) return fmi3Error;\n";
            os << "    for (size_t i = 0; i < nValueReferences; i++) {\n";
            os << "        switch (valueReferences[i]) {\n";
            for (const auto& v : info.variables) {
                if (v.causality != "input" || v.type != fmiType) {
                    continue;
                }
                os << "        case " << v.valueReference << ": c->input." << v.name
                   << " = values[i]; break;\n";
            }
            os << "        default: return fmi3Error;\n        }\n    }\n    return fmi3OK;\n}\n\n";
        };
        emitGet("Float64", "fmi3Float64");
        emitSet("Float64", "fmi3Float64");
        emitGet("Int64", "fmi3Int64");
        emitSet("Int64", "fmi3Int64");
        emitGet("UInt64", "fmi3UInt64");
        emitSet("UInt64", "fmi3UInt64");

        // FMI 3.0 requires the full function set: the operations this FMU does not
        // use are conforming stubs so any importer resolves every symbol.
        os << R"FMISTUB(
fmi3Instance fmi3InstantiateModelExchange(fmi3String instanceName, fmi3String instantiationToken, fmi3String resourcePath, fmi3Boolean visible, fmi3Boolean loggingOn, fmi3InstanceEnvironment instanceEnvironment, fmi3LogMessageCallback logMessage) { return NULL; }
fmi3Instance fmi3InstantiateScheduledExecution(fmi3String instanceName, fmi3String instantiationToken, fmi3String resourcePath, fmi3Boolean visible, fmi3Boolean loggingOn, fmi3InstanceEnvironment instanceEnvironment, fmi3LogMessageCallback logMessage, fmi3ClockUpdateCallback clockUpdate, fmi3LockPreemptionCallback lockPreemption, fmi3UnlockPreemptionCallback unlockPreemption) { return NULL; }
fmi3Status fmi3EnterEventMode(fmi3Instance instance) { return fmi3Error; }
fmi3Status fmi3GetOutputDerivatives(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Int32 orders[], fmi3Float64 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetFloat32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Float32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetInt8(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Int8 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetUInt8(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt8 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetInt16(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Int16 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetUInt16(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt16 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetInt32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Int32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetUInt32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetBoolean(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Boolean values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetString(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3String values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetBinary(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, size_t valueSizes[], fmi3Binary values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3GetClock(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Clock values[]) { return fmi3Error; }
fmi3Status fmi3SetFloat32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Float32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetInt8(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Int8 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetUInt8(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3UInt8 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetInt16(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Int16 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetUInt16(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3UInt16 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetInt32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Int32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetUInt32(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3UInt32 values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetBoolean(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Boolean values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetString(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3String values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetBinary(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const size_t valueSizes[], const fmi3Binary values[], size_t nValues) { return fmi3Error; }
fmi3Status fmi3SetClock(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Clock values[]) { return fmi3Error; }
fmi3Status fmi3GetNumberOfVariableDependencies(fmi3Instance instance, fmi3ValueReference valueReference, size_t* nDependencies) { return fmi3Error; }
fmi3Status fmi3GetVariableDependencies(fmi3Instance instance, fmi3ValueReference dependent, size_t elementIndicesOfDependent[], fmi3ValueReference independents[], size_t elementIndicesOfIndependents[], fmi3DependencyKind dependencyKinds[], size_t nDependencies) { return fmi3Error; }
fmi3Status fmi3GetFMUState(fmi3Instance instance, fmi3FMUState* FMUState) {
    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;
    if (!c || !FMUState) return fmi3Error;
    nflow_fmu_instance* snap = (nflow_fmu_instance*)*FMUState;
    if (!snap) {
        snap = (nflow_fmu_instance*)malloc(sizeof(nflow_fmu_instance));
        if (!snap) return fmi3Error;
    }
    *snap = *c; /* the instance is a flat, self-contained struct */
    *FMUState = snap;
    return fmi3OK;
}
fmi3Status fmi3SetFMUState(fmi3Instance instance, fmi3FMUState FMUState) {
    nflow_fmu_instance* c = (nflow_fmu_instance*)instance;
    nflow_fmu_instance* snap = (nflow_fmu_instance*)FMUState;
    if (!c || !snap) return fmi3Error;
    *c = *snap;
    return fmi3OK;
}
fmi3Status fmi3FreeFMUState(fmi3Instance instance, fmi3FMUState* FMUState) {
    (void)instance;
    if (FMUState && *FMUState) { free(*FMUState); *FMUState = (fmi3FMUState)0; }
    return fmi3OK;
}
fmi3Status fmi3SerializedFMUStateSize(fmi3Instance instance, fmi3FMUState FMUState, size_t* size) { return fmi3Error; }
fmi3Status fmi3SerializeFMUState(fmi3Instance instance, fmi3FMUState FMUState, fmi3Byte serializedState[], size_t size) { return fmi3Error; }
fmi3Status fmi3DeserializeFMUState(fmi3Instance instance, const fmi3Byte serializedState[], size_t size, fmi3FMUState* FMUState) { return fmi3Error; }
fmi3Status fmi3GetDirectionalDerivative(fmi3Instance instance, const fmi3ValueReference unknowns[], size_t nUnknowns, const fmi3ValueReference knowns[], size_t nKnowns, const fmi3Float64 seed[], size_t nSeed, fmi3Float64 sensitivity[], size_t nSensitivity) { return fmi3Error; }
fmi3Status fmi3GetAdjointDerivative(fmi3Instance instance, const fmi3ValueReference unknowns[], size_t nUnknowns, const fmi3ValueReference knowns[], size_t nKnowns, const fmi3Float64 seed[], size_t nSeed, fmi3Float64 sensitivity[], size_t nSensitivity) { return fmi3Error; }
fmi3Status fmi3EnterConfigurationMode(fmi3Instance instance) { return fmi3Error; }
fmi3Status fmi3ExitConfigurationMode(fmi3Instance instance) { return fmi3Error; }
fmi3Status fmi3GetIntervalDecimal(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Float64 intervals[], fmi3IntervalQualifier qualifiers[]) { return fmi3Error; }
fmi3Status fmi3GetIntervalFraction(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt64 counters[], fmi3UInt64 resolutions[], fmi3IntervalQualifier qualifiers[]) { return fmi3Error; }
fmi3Status fmi3GetShiftDecimal(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Float64 shifts[]) { return fmi3Error; }
fmi3Status fmi3GetShiftFraction(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt64 counters[], fmi3UInt64 resolutions[]) { return fmi3Error; }
fmi3Status fmi3SetIntervalDecimal(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Float64 intervals[]) { return fmi3Error; }
fmi3Status fmi3SetIntervalFraction(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3UInt64 counters[], const fmi3UInt64 resolutions[]) { return fmi3Error; }
fmi3Status fmi3SetShiftDecimal(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Float64 shifts[]) { return fmi3Error; }
fmi3Status fmi3SetShiftFraction(fmi3Instance instance, const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3UInt64 counters[], const fmi3UInt64 resolutions[]) { return fmi3Error; }
)FMISTUB";
        return os.str();
    }
    //=============================================================================
} // namespace NFlow
//=============================================================================
} // namespace Nelson
//=============================================================================
