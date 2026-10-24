#ifndef FMIC_H
#define FMIC_H

#include "fmi4c_types.h"
#include "fmi4c_types_fmi1.h"
#include "fmi4c_types_fmi2.h"
#include "fmi4c_types_fmi3.h"
#include "fmi4c_functions_fmi1.h"
#include "fmi4c_functions_fmi2.h"
#include "fmi4c_functions_fmi3.h"
#ifndef FMI4C_H_INTERNAL_INCLUDE
#include "fmi4c_public.h"
#endif

#include <stdio.h>
#include <stdlib.h>

#ifdef _WIN32
#include <direct.h>
#include <windows.h>
#endif

#undef fmiVersion

#ifdef _WIN32
#if defined(FMI4C_STATIC)
#define FMI4C_DLLAPI
#elif defined(FMI4C_DLLEXPORT)
#define FMI4C_DLLAPI __declspec(dllexport)
#else
#define FMI4C_DLLAPI __declspec(dllimport)
#endif
#else
#define FMI4C_DLLAPI
#endif

#ifdef __cplusplus
extern "C"
{
#endif

    // FMU access functions

    FMI4C_DLLAPI void
    fmi4c_setMessageFunction(void (*func)(const char*));
    FMI4C_DLLAPI fmiVersion_t
    fmi4c_getFmiVersion(fmuHandle* fmu);
    FMI4C_DLLAPI fmuHandle*
    fmi4c_loadUnzippedFmu(const char* instanceName, const char* unzipLocation);
    FMI4C_DLLAPI fmuHandle*
    fmi4c_loadFmu(const char* fmufile, const char* instanceName);
    FMI4C_DLLAPI void
    fmi4c_freeFmu(fmuHandle* fmu);

    // FMI 1 wrapper functions
    FMI4C_DLLAPI fmi1Type
    fmi1_getType(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getModelName(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getGuid(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getDescription(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getAuthor(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getGenerationTool(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getGenerationDateAndTime(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi1_getNumberOfContinuousStates(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi1_getNumberOfEventIndicators(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi1_defaultStartTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi1_defaultStopTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi1_defaultToleranceDefined(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi1_getDefaultStartTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi1_getDefaultStopTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi1_getDefaultTolerance(fmuHandle* fmu);

    FMI4C_DLLAPI int
    fmi1_getNumberOfVariables(fmuHandle* fmu);
    FMI4C_DLLAPI fmi1VariableHandle*
    fmi1_getVariableByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI fmi1VariableHandle*
    fmi1_getVariableByValueReference(fmuHandle* fmu, fmi1ValueReference vr);
    FMI4C_DLLAPI fmi1VariableHandle*
    fmi1_getVariableByName(fmuHandle* fmu, fmi1String name);
    FMI4C_DLLAPI const char*
    fmi1_getVariableName(fmi1VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi1_getVariableDescription(fmi1VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi1_getVariableQuantity(fmi1VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi1_getVariableUnit(fmi1VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi1_getVariableDisplayUnit(fmi1VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi1_getVariableRelativeQuantity(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Real
    fmi1_getVariableMin(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Real
    fmi1_getVariableMax(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Real
    fmi1_getVariableNominal(fmi1VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi1_getVariableHasStartValue(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Real
    fmi1_getVariableStartReal(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Integer
    fmi1_getVariableStartInteger(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Boolean
    fmi1_getVariableStartBoolean(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1String
    fmi1_getVariableStartString(fmi1VariableHandle* var);
    FMI4C_DLLAPI long
    fmi1_getVariableValueReference(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Causality
    fmi1_getVariableCausality(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Variability
    fmi1_getVariableVariability(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1Alias
    fmi1_getAlias(fmi1VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi1_getVariableIsFixed(fmi1VariableHandle* var);
    FMI4C_DLLAPI fmi1DataType
    fmi1_getVariableDataType(fmi1VariableHandle* var);

    FMI4C_DLLAPI const char*
    fmi1_getTypesPlatform(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi1_getVersion(fmuHandle* fmu);
    FMI4C_DLLAPI fmi1Status
    fmi1_setDebugLogging(fmi1InstanceHandle* instance, fmi1Boolean loggingOn);

    FMI4C_DLLAPI int
    fmi1_getNumberOfBaseUnits(fmuHandle* fmu);
    FMI4C_DLLAPI fmi1BaseUnitHandle*
    fmi1_getBaseUnitByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI const char*
    fmi1_getBaseUnitUnit(fmi1BaseUnitHandle* baseUnit);
    FMI4C_DLLAPI int
    fmi1_getNumberOfDisplayUnits(fmi1BaseUnitHandle* baseUnit);
    FMI4C_DLLAPI void
    fmi1_getDisplayUnitByIndex(fmi1BaseUnitHandle* baseUnit, int id, const char** displayUnit,
        double* gain, double* offset);

    FMI4C_DLLAPI fmi1Status
    fmi1_getReal(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, fmi1Real values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_getInteger(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, fmi1Integer values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_getBoolean(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, fmi1Boolean values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_getString(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, fmi1String values[]);

    FMI4C_DLLAPI fmi1Status
    fmi1_setReal(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1Real values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_setInteger(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1Integer values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_setBoolean(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1Boolean values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_setString(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1String values[]);

    FMI4C_DLLAPI fmi1InstanceHandle*
    fmi1_instantiateSlave(fmuHandle* fmu, fmi1String mimeType, fmi1Real timeOut,
        fmi1Boolean visible, fmi1Boolean interactive, fmi1CallbackLogger_t logger,
        fmi1CallbackAllocateMemory_t allocateMemory, fmi1CallbackFreeMemory_t freeMemory,
        fmi1StepFinished_t stepFinished, fmi1Boolean loggingOn);
    FMI4C_DLLAPI fmi1Status
    fmi1_initializeSlave(fmi1InstanceHandle* instance, fmi1Real startTime,
        fmi1Boolean stopTimeDefined, fmi1Real stopTime);
    FMI4C_DLLAPI fmi1Status
    fmi1_terminateSlave(fmi1InstanceHandle* instance);
    FMI4C_DLLAPI fmi1Status
    fmi1_resetSlave(fmi1InstanceHandle* instance);
    FMI4C_DLLAPI void
    fmi1_freeSlaveInstance(fmi1InstanceHandle* instance);

    FMI4C_DLLAPI fmi1Status
    fmi1_setRealInputDerivatives(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1Integer orders[], const fmi1Real values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_getRealOutputDerivatives(fmi1InstanceHandle* i, const fmi1ValueReference valueReferences[],
        size_t nValueReferences, const fmi1Integer orders[], fmi1Real values[]);
    FMI4C_DLLAPI fmi1Status
    fmi1_cancelStep(fmi1InstanceHandle* instance);
    FMI4C_DLLAPI fmi1Status
    fmi1_doStep(fmi1InstanceHandle* i, fmi1Real currentCommunicationPoint,
        fmi1Real communicationStepSize, fmi1Boolean newStep);
    FMI4C_DLLAPI fmi1Status
    fmi1_getStatus(
        fmi1InstanceHandle* instance, const fmi1StatusKind statusKind, fmi1Status* value);
    FMI4C_DLLAPI fmi1Status
    fmi1_getRealStatus(
        fmi1InstanceHandle* instance, const fmi1StatusKind statusKind, fmi1Real* value);
    FMI4C_DLLAPI fmi1Status
    fmi1_getIntegerStatus(
        fmi1InstanceHandle* instance, const fmi1StatusKind statusKind, fmi1Integer* value);
    FMI4C_DLLAPI fmi1Status
    fmi1_getBooleanStatus(
        fmi1InstanceHandle* instance, const fmi1StatusKind statusKind, fmi1Boolean* value);
    FMI4C_DLLAPI fmi1Status
    fmi1_getStringStatus(
        fmi1InstanceHandle* instance, const fmi1StatusKind statusKind, fmi1String* value);

    FMI4C_DLLAPI const char*
    fmi1_getModelTypesPlatform(fmuHandle* fmu);
    FMI4C_DLLAPI fmi1InstanceHandle*
    fmi1_instantiateModel(fmuHandle* fmu, fmi1CallbackLogger_t logger,
        fmi1CallbackAllocateMemory_t allocateMemory, fmi1CallbackFreeMemory_t freeMemory,
        fmi1Boolean loggingOn);
    FMI4C_DLLAPI void
    fmi1_freeModelInstance(fmi1InstanceHandle* i);
    FMI4C_DLLAPI fmi1Status
    fmi1_setTime(fmi1InstanceHandle* i, fmi1Real time);
    FMI4C_DLLAPI fmi1Status
    fmi1_setContinuousStates(fmi1InstanceHandle* i, const fmi1Real values[], size_t nStates);
    FMI4C_DLLAPI fmi1Status
    fmi1_completedIntegratorStep(fmi1InstanceHandle* i, fmi1Boolean* callEventUpdate);
    FMI4C_DLLAPI fmi1Status
    fmi1_initialize(fmi1InstanceHandle* i, fmi1Boolean toleranceControlled,
        fmi1Real relativeTolerance, fmi1EventInfo* eventInfo);
    FMI4C_DLLAPI fmi1Status
    fmi1_getDerivatives(fmi1InstanceHandle* i, fmi1Real derivatives[], size_t nDerivatives);
    FMI4C_DLLAPI fmi1Status
    fmi1_getEventIndicators(fmi1InstanceHandle* i, fmi1Real indicators[], size_t nIndicators);
    FMI4C_DLLAPI fmi1Status
    fmi1_eventUpdate(
        fmi1InstanceHandle* i, fmi1Boolean intermediateResults, fmi1EventInfo* eventInfo);
    FMI4C_DLLAPI fmi1Status
    fmi1_getContinuousStates(fmi1InstanceHandle* i, fmi1Real states[], size_t nStates);
    FMI4C_DLLAPI fmi1Status
    fmi1_getNominalContinuousStates(fmi1InstanceHandle* i, fmi1Real nominals[], size_t nNominals);
    FMI4C_DLLAPI fmi1Status
    fmi1_getStateValueReferences(
        fmi1InstanceHandle* i, fmi1ValueReference valueReferences[], size_t nValueReferences);
    FMI4C_DLLAPI fmi1Status
    fmi1_terminate(fmi1InstanceHandle* i);

    // FMI 2 wrapper functions

    FMI4C_DLLAPI bool
    fmi2_defaultStartTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2_defaultStopTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2_defaultToleranceDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2_defaultStepSizeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi2_getDefaultStartTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi2_getDefaultStopTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi2_getDefaultTolerance(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi2_getDefaultStepSize(fmuHandle* fmu);

    FMI4C_DLLAPI int
    fmi2_getNumberOfVariables(fmuHandle* fmu);
    FMI4C_DLLAPI fmi2VariableHandle*
    fmi2_getVariableByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI fmi2VariableHandle*
    fmi2_getVariableByValueReference(fmuHandle* fmu, fmi2ValueReference vr);
    FMI4C_DLLAPI fmi2VariableHandle*
    fmi2_getVariableByName(fmuHandle* fmu, fmi2String name);
    FMI4C_DLLAPI const char*
    fmi2_getVariableName(fmi2VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi2_getVariableDescription(fmi2VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi2_getFmiVersion(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getAuthor(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getModelName(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getModelDescription(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getCopyright(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getLicense(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getGenerationTool(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getGenerationDateAndTime(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getVariableNamingConvention(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi2_getVariableDerivativeIndex(fmi2VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi2_getVariableQuantity(fmi2VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi2_getVariableUnit(fmi2VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi2_getVariableDisplayUnit(fmi2VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi2_getVariableRelativeQuantity(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Real
    fmi2_getVariableMin(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Real
    fmi2_getVariableMax(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Real
    fmi2_getVariableNominal(fmi2VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi2_getVariableUnbounded(fmi2VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi2_getVariableHasStartValue(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Real
    fmi2_getVariableStartReal(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Integer
    fmi2_getVariableStartInteger(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Boolean
    fmi2_getVariableStartBoolean(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2String
    fmi2_getVariableStartString(fmi2VariableHandle* var);
    FMI4C_DLLAPI long
    fmi2_getVariableValueReference(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Causality
    fmi2_getVariableCausality(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Variability
    fmi2_getVariableVariability(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2Initial
    fmi2_getVariableInitial(fmi2VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi2_getVariableCanHandleMultipleSetPerTimeInstant(fmi2VariableHandle* var);
    FMI4C_DLLAPI fmi2DataType
    fmi2_getVariableDataType(fmi2VariableHandle* var);

    FMI4C_DLLAPI const char*
    fmi2_getTypesPlatform(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi2_getVersion(fmuHandle* fmu);
    FMI4C_DLLAPI fmi2Status
    fmi2_setDebugLogging(fmi2InstanceHandle* instance, fmi2Boolean loggingOn, size_t nCategories,
        const fmi2String categories[]);
    FMI4C_DLLAPI const char*
    fmi2_getGuid(fmuHandle* fmu);

    FMI4C_DLLAPI const char*
    fmi2cs_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanHandleVariableCommunicationStepSize(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanInterpolateInputs(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi2cs_getMaxOutputDerivativeOrder(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanRunAsynchronuously(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getNeedsExecutionTool(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanBeInstantiatedOnlyOncePerProcess(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanNotUseMemoryManagementFunctions(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanGetAndSetFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getCanSerializeFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2cs_getProvidesDirectionalDerivative(fmuHandle* fmu);

    FMI4C_DLLAPI const char*
    fmi2me_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getCompletedIntegratorStepNotNeeded(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getNeedsExecutionTool(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getCanBeInstantiatedOnlyOncePerProcess(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getCanNotUseMemoryManagementFunctions(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getCanGetAndSetFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getCanSerializeFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2me_getProvidesDirectionalDerivative(fmuHandle* fmu);

    FMI4C_DLLAPI int
    fmi2_getNumberOfContinuousStates(fmuHandle* fmu);
    FMI4C_DLLAPI void
    fmi2_getContinuousStateValueReferences(
        fmuHandle* fmu, fmi2ValueReference valueReferences[], size_t nValueReferences);
    FMI4C_DLLAPI int
    fmi2_getNumberOfEventIndicators(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2_getSupportsCoSimulation(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi2_getSupportsModelExchange(fmuHandle* fmu);

    FMI4C_DLLAPI int
    fmi2_getNumberOfModelStructureOutputs(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi2_getNumberOfModelStructureDerivatives(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi2_getNumberOfModelStructureInitialUnknowns(fmuHandle* fmu);
    FMI4C_DLLAPI fmi2ModelStructureHandle*
    fmi2_getModelStructureOutput(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi2ModelStructureHandle*
    fmi2_getModelStructureDerivative(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi2ModelStructureHandle*
    fmi2_getModelStructureInitialUnknown(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI int
    fmi2_getModelStructureIndex(fmi2ModelStructureHandle* handle);
    FMI4C_DLLAPI int
    fmi2_getModelStructureNumberOfDependencies(fmi2ModelStructureHandle* handle);
    FMI4C_DLLAPI bool
    fmi2_getModelStructureDependencyKindsDefined(fmi2ModelStructureHandle* handle);
    FMI4C_DLLAPI void
    fmi2_getModelStructureDependencies(
        fmi2ModelStructureHandle* handle, int* dependencies, size_t numberOfDependencies);
    FMI4C_DLLAPI void
    fmi2_getModelStructureDependencyKinds(
        fmi2ModelStructureHandle* handle, int* dependencyKinds, size_t numberOfDependencies);

    FMI4C_DLLAPI fmi2InstanceHandle*
    fmi2_instantiate(fmuHandle* fmu, fmi2Type type, fmi2CallbackLogger logger,
        fmi2CallbackAllocateMemory allocateMemory, fmi2CallbackFreeMemory freeMemory,
        fmi2StepFinished stepFinished, fmi2ComponentEnvironment componentEnvironment,
        fmi2Boolean visible, fmi2Boolean loggingOn);
    FMI4C_DLLAPI void
    fmi2_freeInstance(fmi2InstanceHandle* instance);

    FMI4C_DLLAPI fmi2Status
    fmi2_setupExperiment(fmi2InstanceHandle* instance, fmi2Boolean toleranceDefined,
        fmi2Real tolerance, fmi2Real startTime, fmi2Boolean stopTimeDefined, fmi2Real stopTime);
    FMI4C_DLLAPI fmi2Status
    fmi2_enterInitializationMode(fmi2InstanceHandle* instance);
    FMI4C_DLLAPI fmi2Status
    fmi2_exitInitializationMode(fmi2InstanceHandle* instance);
    FMI4C_DLLAPI fmi2Status
    fmi2_terminate(fmi2InstanceHandle* instance);
    FMI4C_DLLAPI fmi2Status
    fmi2_reset(fmi2InstanceHandle* instance);

    FMI4C_DLLAPI int
    fmi2_getNumberOfUnits(fmuHandle* fmu);
    FMI4C_DLLAPI fmi2UnitHandle*
    fmi2_getUnitByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI const char*
    fmi2_getUnitName(fmi2UnitHandle* unit);
    FMI4C_DLLAPI bool
    fmi2_hasBaseUnit(fmi2UnitHandle* unit);
    FMI4C_DLLAPI void
    fmi2_getBaseUnit(fmi2UnitHandle* unit, double* factor, double* offset, int* kg, int* m, int* s,
        int* A, int* K, int* mol, int* cd, int* rad);
    FMI4C_DLLAPI int
    fmi2_getNumberOfDisplayUnits(fmi2UnitHandle* unit);
    FMI4C_DLLAPI void
    fmi2_getDisplayUnitByIndex(
        fmi2UnitHandle* unit, int id, const char** name, double* factor, double* offset);

    FMI4C_DLLAPI fmi2Status
    fmi2_getReal(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, fmi2Real values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_getInteger(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, fmi2Integer values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_getBoolean(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, fmi2Boolean values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_getString(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, fmi2String values[]);

    FMI4C_DLLAPI fmi2Status
    fmi2_setReal(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, const fmi2Real values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_setInteger(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, const fmi2Integer values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_setBoolean(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, const fmi2Boolean values[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_setString(fmi2InstanceHandle* instance, const fmi2ValueReference valueReferences[],
        size_t nValueReferences, const fmi2String values[]);

    FMI4C_DLLAPI fmi2Status
    fmi2_getFMUstate(fmi2InstanceHandle* instance, fmi2FMUstate* FMUstate);
    FMI4C_DLLAPI fmi2Status
    fmi2_setFMUstate(fmi2InstanceHandle* instance, fmi2FMUstate FMUstate);
    FMI4C_DLLAPI fmi2Status
    fmi2_freeFMUstate(fmi2InstanceHandle* instance, fmi2FMUstate* FMUstate);
    FMI4C_DLLAPI fmi2Status
    fmi2_serializedFMUstateSize(fmi2InstanceHandle* instance, fmi2FMUstate FMUstate, size_t* size);
    FMI4C_DLLAPI fmi2Status
    fmi2_serializeFMUstate(fmi2InstanceHandle* instance, fmi2FMUstate FMUstate,
        fmi2Byte serializedState[], size_t size);
    FMI4C_DLLAPI fmi2Status
    fmi2_deSerializeFMUstate(fmi2InstanceHandle* instance, const fmi2Byte serializedState[],
        size_t size, fmi2FMUstate* FMUstate);

    FMI4C_DLLAPI fmi2Status
    fmi2_getDirectionalDerivative(fmi2InstanceHandle* instance,
        const fmi2ValueReference unknownReferences[], size_t nUnknown,
        const fmi2ValueReference knownReferences[], size_t nKnown, const fmi2Real dvKnown[],
        fmi2Real dvUnknown[]);

    FMI4C_DLLAPI fmi2Status
    fmi2_enterEventMode(fmi2InstanceHandle* instance);
    FMI4C_DLLAPI fmi2Status
    fmi2_newDiscreteStates(fmi2InstanceHandle* instance, fmi2EventInfo* eventInfo);
    FMI4C_DLLAPI fmi2Status
    fmi2_enterContinuousTimeMode(fmi2InstanceHandle* instance);
    FMI4C_DLLAPI fmi2Status
    fmi2_completedIntegratorStep(fmi2InstanceHandle* instance,
        fmi2Boolean noSetFMUStatePriorToCurrentPoint, fmi2Boolean* enterEventMode,
        fmi2Boolean* terminateSimulation);

    FMI4C_DLLAPI fmi2Status
    fmi2_setTime(fmi2InstanceHandle* instance, fmi2Real time);
    FMI4C_DLLAPI fmi2Status
    fmi2_setContinuousStates(fmi2InstanceHandle* instance, const fmi2Real x[], size_t nx);

    FMI4C_DLLAPI fmi2Status
    fmi2_getDerivatives(fmi2InstanceHandle* instance, fmi2Real derivatives[], size_t nx);
    FMI4C_DLLAPI fmi2Status
    fmi2_getEventIndicators(fmi2InstanceHandle* instance, fmi2Real eventIndicators[], size_t ni);
    FMI4C_DLLAPI fmi2Status
    fmi2_getContinuousStates(fmi2InstanceHandle* instance, fmi2Real x[], size_t nx);
    FMI4C_DLLAPI fmi2Status
    fmi2_getNominalsOfContinuousStates(
        fmi2InstanceHandle* instance, fmi2Real x_nominal[], size_t nx);

    FMI4C_DLLAPI fmi2Status
    fmi2_setRealInputDerivatives(fmi2InstanceHandle* instance, const fmi2ValueReference vr[],
        size_t nvr, const fmi2Integer order[], const fmi2Real value[]);
    FMI4C_DLLAPI fmi2Status
    fmi2_getRealOutputDerivatives(fmi2InstanceHandle* instance, const fmi2ValueReference vr[],
        size_t nvr, const fmi2Integer order[], fmi2Real value[]);

    FMI4C_DLLAPI fmi2Status
    fmi2_doStep(fmi2InstanceHandle* instance, fmi2Real currentCommunicationPoint,
        fmi2Real communicationStepSize, fmi2Boolean noSetFMUStatePriorToCurrentPoint);
    FMI4C_DLLAPI fmi2Status
    fmi2_cancelStep(fmi2InstanceHandle* instance);

    FMI4C_DLLAPI fmi2Status
    fmi2_getStatus(fmi2InstanceHandle* instance, const fmi2StatusKind s, fmi2Status* value);
    FMI4C_DLLAPI fmi2Status
    fmi2_getRealStatus(fmi2InstanceHandle* instance, const fmi2StatusKind s, fmi2Real* value);
    FMI4C_DLLAPI fmi2Status
    fmi2_getIntegerStatus(fmi2InstanceHandle* instance, const fmi2StatusKind s, fmi2Integer* value);
    FMI4C_DLLAPI fmi2Status
    fmi2_getBooleanStatus(fmi2InstanceHandle* instance, const fmi2StatusKind s, fmi2Boolean* value);
    FMI4C_DLLAPI fmi2Status
    fmi2_getStringStatus(fmi2InstanceHandle* instance, const fmi2StatusKind s, fmi2String* value);

    FMI4C_DLLAPI int
    fmi3_getNumberOfVariables(fmuHandle* fmu);
    FMI4C_DLLAPI fmi3VariableHandle*
    fmi3_getVariableByName(fmuHandle* fmu, fmi3String name);
    FMI4C_DLLAPI fmi3VariableHandle*
    fmi3_getVariableByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI fmi3VariableHandle*
    fmi3_getVariableByValueReference(fmuHandle* fmu, fmi3ValueReference vr);
    FMI4C_DLLAPI const char*
    fmi3_getVariableName(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Causality
    fmi3_getVariableCausality(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Variability
    fmi3_getVariableVariability(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Initial
    fmi3_getVariableInitial(fmi3VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi3_getVariableSupportsIntermediateUpdate(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3DataType
    fmi3_getVariableDataType(fmi3VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi3_getVariableDescription(fmi3VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi3_getVariableQuantity(fmi3VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi3_getVariableUnit(fmi3VariableHandle* var);
    FMI4C_DLLAPI const char*
    fmi3_getVariableDisplayUnit(fmi3VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi3_getVariableHasStartValue(fmi3VariableHandle* var);
    FMI4C_DLLAPI int
    fmi3_getVariableDerivativeIndex(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Float64
    fmi3_getVariableStartFloat64(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Float32
    fmi3_getVariableStartFloat32(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Int64
    fmi3_getVariableStartInt64(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Int32
    fmi3_getVariableStartInt32(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Int16
    fmi3_getVariableStartInt16(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Int8
    fmi3_getVariableStartInt8(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3UInt64
    fmi3_getVariableStartUInt64(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3UInt32
    fmi3_getVariableStartUInt32(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3UInt16
    fmi3_getVariableStartUInt16(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3UInt8
    fmi3_getVariableStartUInt8(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Boolean
    fmi3_getVariableStartBoolean(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3String
    fmi3_getVariableStartString(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3Binary
    fmi3_getVariableStartBinary(fmi3VariableHandle* var);
    FMI4C_DLLAPI fmi3ValueReference
    fmi3_getVariableValueReference(fmi3VariableHandle* var);

    // NFlow-local extension: FMI3 array dimensions (<Dimension> elements). Each
    // dimension is a fixed size (hasStart -> Start) or bound to a variable value
    // (else -> DimensionValueReference). Upstream fmi4c drops these; re-apply this
    // patch after any upstream update.
    FMI4C_DLLAPI int
    fmi3_getVariableNumberOfDimensions(fmi3VariableHandle* var);
    FMI4C_DLLAPI bool
    fmi3_getVariableDimensionHasStart(fmi3VariableHandle* var, int index);
    FMI4C_DLLAPI uint64_t
    fmi3_getVariableDimensionStart(fmi3VariableHandle* var, int index);
    FMI4C_DLLAPI fmi3ValueReference
    fmi3_getVariableDimensionValueReference(fmi3VariableHandle* var, int index);

    FMI4C_DLLAPI const char*
    fmi3_modelName(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_instantiationToken(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_description(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_author(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_version(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_copyright(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_license(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_generationTool(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_generationDateAndTime(fmuHandle* fmu);
    FMI4C_DLLAPI const char*
    fmi3_variableNamingConvention(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_supportsModelExchange(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_supportsScheduledExecution(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_supportsCoSimulation(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_defaultStartTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_defaultStopTimeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_defaultToleranceDefined(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3_defaultStepSizeDefined(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi3_getDefaultStartTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi3_getDefaultStopTime(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi3_getDefaultTolerance(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi3_getDefaultStepSize(fmuHandle* fmu);

    FMI4C_DLLAPI int
    fmi3_getNumberOfUnits(fmuHandle* fmu);
    FMI4C_DLLAPI fmi3UnitHandle*
    fmi3_getUnitByIndex(fmuHandle* fmu, int i);
    FMI4C_DLLAPI const char*
    fmi3_getUnitName(fmi3UnitHandle* unit);
    FMI4C_DLLAPI bool
    fmi3_hasBaseUnit(fmi3UnitHandle* unit);
    FMI4C_DLLAPI void
    fmi3_getBaseUnit(fmi3UnitHandle* unit, double* factor, double* offset, int* kg, int* m, int* s,
        int* A, int* K, int* mol, int* cd, int* rad);
    FMI4C_DLLAPI int
    fmi3_getNumberOfDisplayUnits(fmi3UnitHandle* unit);
    FMI4C_DLLAPI void
    fmi3_getDisplayUnitByIndex(fmi3UnitHandle* unit, int id, const char** name, double* factor,
        double* offset, bool* inverse);

    FMI4C_DLLAPI void
    fmi3_getFloat64Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, const char** unit, const char** displayUnit, bool* relativeQuantity,
        bool* unbounded, double* min, double* max, double* nominal);
    FMI4C_DLLAPI void
    fmi3_getFloat32Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, const char** unit, const char** displayUnit, bool* relativeQuantity,
        bool* unbounded, float* min, float* max, float* nominal);
    FMI4C_DLLAPI void
    fmi3_getInt64Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, int64_t* min, int64_t* max);
    FMI4C_DLLAPI void
    fmi3_getInt32Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, int32_t* min, int32_t* max);
    FMI4C_DLLAPI void
    fmi3_getInt16Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, int16_t* min, int16_t* max);
    FMI4C_DLLAPI void
    fmi3_getInt8Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, int8_t* min, int8_t* max);
    FMI4C_DLLAPI void
    fmi3_getUInt64Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, uint64_t* min, uint64_t* max);
    FMI4C_DLLAPI void
    fmi3_getUInt32Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, uint32_t* min, uint32_t* max);
    FMI4C_DLLAPI void
    fmi3_getUInt16Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, uint16_t* min, uint16_t* max);
    FMI4C_DLLAPI void
    fmi3_getUInt8Type(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, uint8_t* min, uint8_t* max);
    FMI4C_DLLAPI void
    fmi3_getBooleanType(fmuHandle* fmu, const char* name, const char** description);
    FMI4C_DLLAPI void
    fmi3_getStringType(fmuHandle* fmu, const char* name, const char** description);
    FMI4C_DLLAPI void
    fmi3_getBinaryType(fmuHandle* fmu, const char* name, const char** description,
        const char** mimeType, uint32_t* maxSize);
    FMI4C_DLLAPI void
    fmi3_getEnumerationType(fmuHandle* fmu, const char* name, const char** description,
        const char** quantity, int64_t* min, int64_t* max, int* numberOfItems);
    FMI4C_DLLAPI void
    fmi3_getEnumerationItem(fmuHandle* fmu, const char* typeName, int itemId, const char** itemName,
        int64_t* value, const char** description);
    FMI4C_DLLAPI void
    fmi3_getClockType(fmuHandle* fmu, const char* name, const char** description,
        bool* canBeDeactivated, uint32_t* priority, fmi3IntervalVariability* intervalVariability,
        float* intervalDecimal, float* shiftDecimal, bool* supportsFraction, uint64_t* resolution,
        uint64_t* intervalCounter, uint64_t* shiftCounter);

    FMI4C_DLLAPI int
    fmi3_getNumberOfLogCategories(fmuHandle* fmu);
    FMI4C_DLLAPI void
    fmi3_getLogCategory(fmuHandle* fmu, int id, const char** name, const char** description);

    FMI4C_DLLAPI int
    fmi3_getNumberOfModelStructureOutputs(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi3_getNumberOfModelStructureContinuousStateDerivatives(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi3_getNumberOfModelStructureClockedStates(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi3_getNumberOfModelStructureInitialUnknowns(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi3_getNumberOfModelStructureEventIndicators(fmuHandle* fmu);
    FMI4C_DLLAPI fmi3ModelStructureHandle*
    fmi3_getModelStructureOutput(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi3ModelStructureHandle*
    fmi3_getModelStructureContinuousStateDerivative(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi3ModelStructureHandle*
    fmi3_getModelStructureClockedState(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi3ModelStructureHandle*
    fmi3_getModelStructureInitialUnknown(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi3ModelStructureHandle*
    fmi3_getModelStructureEventIndicator(fmuHandle* fmu, size_t i);
    FMI4C_DLLAPI fmi3ValueReference
    fmi3_getModelStructureValueReference(fmi3ModelStructureHandle* handle);
    FMI4C_DLLAPI int
    fmi3_getModelStructureNumberOfDependencies(fmi3ModelStructureHandle* handle);
    FMI4C_DLLAPI bool
    fmi3_getModelStructureDependencyKindsDefined(fmi3ModelStructureHandle* handle);
    FMI4C_DLLAPI void
    fmi3_getModelStructureDependencies(
        fmi3ModelStructureHandle* handle, int* dependencies, size_t numberOfDependencies);
    FMI4C_DLLAPI void
    fmi3_getModelStructureDependencyKinds(
        fmi3ModelStructureHandle* handle, int* dependencyKinds, size_t numberOfDependencies);

    FMI4C_DLLAPI const char*
    fmi3cs_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getNeedsExecutionTool(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getCanBeInstantiatedOnlyOncePerProcess(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getCanGetAndSetFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getCanSerializeFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getProvidesDirectionalDerivative(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getProvidesAdjointDerivatives(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getProvidesPerElementDependencies(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getProvidesIntermediateUpdate(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getProvidesEvaluateDiscreteStates(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getHasEventMode(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getRecommendedIntermediateInputSmoothness(fmuHandle* fmu);
    FMI4C_DLLAPI int
    fmi3cs_getMaxOutputDerivativeOrder(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getCanHandleVariableCommunicationStepSize(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3cs_getCanReturnEarlyAfterIntermediateUpdate(fmuHandle* fmu);
    FMI4C_DLLAPI double
    fmi3cs_getFixedInternalStepSize(fmuHandle* fmu);

    FMI4C_DLLAPI const char*
    fmi3me_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getNeedsExecutionTool(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getCanBeInstantiatedOnlyOncePerProcess(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getCanGetAndSetFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getCanSerializeFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getProvidesDirectionalDerivative(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getProvidesAdjointDerivatives(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getProvidesPerElementDependencies(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getProvidesEvaluateDiscreteStates(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3me_getNeedsCompletedIntegratorStep(fmuHandle* fmu);

    FMI4C_DLLAPI const char*
    fmi3se_getModelIdentifier(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getNeedsExecutionTool(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getCanBeInstantiatedOnlyOncePerProcess(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getCanGetAndSetFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getCanSerializeFMUState(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getProvidesDirectionalDerivative(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getProvidesAdjointDerivatives(fmuHandle* fmu);
    FMI4C_DLLAPI bool
    fmi3se_getProvidesPerElementDependencies(fmuHandle* fmu);

    FMI4C_DLLAPI fmi3InstanceHandle*
    fmi3_instantiateCoSimulation(fmuHandle* fmu, fmi3Boolean visible, fmi3Boolean loggingOn,
        fmi3Boolean eventModeUsed, fmi3Boolean earlyReturnAllowed,
        const fmi3ValueReference requiredIntermediateVariables[],
        size_t nRequiredIntermediateVariables, fmi3InstanceEnvironment instanceEnvironment,
        fmi3LogMessageCallback logMessage, fmi3IntermediateUpdateCallback intermediateUpdate);

    FMI4C_DLLAPI fmi3InstanceHandle*
    fmi3_instantiateModelExchange(fmuHandle* fmu, fmi3Boolean visible, fmi3Boolean loggingOn,
        fmi3InstanceEnvironment instanceEnvironment, fmi3LogMessageCallback logMessage);

    FMI4C_DLLAPI const char*
    fmi3_getVersion(fmuHandle* fmu);

    FMI4C_DLLAPI fmi3Status
    fmi3_setDebugLogging(fmi3InstanceHandle* instance, fmi3Boolean loggingOn, size_t nCategories,
        const fmi3String categories[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_getFloat64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Float64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setFloat64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Float64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_enterInitializationMode(fmi3InstanceHandle* instance, fmi3Boolean toleranceDefined,
        fmi3Float64 tolerance, fmi3Float64 startTime, fmi3Boolean stopTimeDefined,
        fmi3Float64 stopTime);

    FMI4C_DLLAPI fmi3Status
    fmi3_exitInitializationMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_terminate(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI void
    fmi3_freeInstance(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_doStep(fmi3InstanceHandle* instance, fmi3Float64 currentCommunicationPoint,
        fmi3Float64 communicationStepSize, fmi3Boolean noSetFMUStatePriorToCurrentPoint,
        fmi3Boolean* eventEncountered, fmi3Boolean* terminateSimulation, fmi3Boolean* earlyReturn,
        fmi3Float64* lastSuccessfulTime);

    FMI4C_DLLAPI fmi3Status
    fmi3_enterEventMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_reset(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_getFloat32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Float32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getInt8(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Int8 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getUInt8(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3UInt8 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getInt16(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Int16 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getUInt16(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3UInt16 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getInt32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Int32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getUInt32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3UInt32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getInt64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Int64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getUInt64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3UInt64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getBoolean(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Boolean values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getString(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3String values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getBinary(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, size_t valueSizes[], fmi3Binary values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_getClock(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Clock values[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_setFloat32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Float32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setInt8(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Int8 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setUInt8(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3UInt8 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setInt16(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Int16 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setUInt16(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3UInt16 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setInt32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Int32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setUInt32(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3UInt32 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setInt64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Int64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setUInt64(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3UInt64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setBoolean(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Boolean values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setString(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3String values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setBinary(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const size_t valueSizes[], const fmi3Binary values[],
        size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_setClock(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, const fmi3Clock values[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_getNumberOfVariableDependencies(
        fmi3InstanceHandle* instance, fmi3ValueReference valueReference, size_t* nDependencies);

    FMI4C_DLLAPI fmi3Status
    fmi3_getVariableDependencies(fmi3InstanceHandle* instance, fmi3ValueReference dependent,
        size_t elementIndicesOfDependent[], fmi3ValueReference independents[],
        size_t elementIndicesOfIndependents[], fmi3DependencyKind dependencyKinds[],
        size_t nDependencies);

    FMI4C_DLLAPI fmi3Status
    fmi3_getFMUState(fmi3InstanceHandle* instance, fmi3FMUState* FMUState);

    FMI4C_DLLAPI fmi3Status
    fmi3_setFMUState(fmi3InstanceHandle* instance, fmi3FMUState FMUState);

    FMI4C_DLLAPI fmi3Status
    fmi3_freeFMUState(fmi3InstanceHandle* instance, fmi3FMUState* FMUState);

    FMI4C_DLLAPI fmi3Status
    fmi3_serializedFMUStateSize(fmi3InstanceHandle* instance, fmi3FMUState FMUState, size_t* size);

    FMI4C_DLLAPI fmi3Status
    fmi3_serializeFMUState(fmi3InstanceHandle* instance, fmi3FMUState FMUState,
        fmi3Byte serializedState[], size_t size);

    FMI4C_DLLAPI fmi3Status
    fmi3_deserializeFMUState(fmi3InstanceHandle* instance, const fmi3Byte serializedState[],
        size_t size, fmi3FMUState* FMUState);

    FMI4C_DLLAPI fmi3Status
    fmi3_getDirectionalDerivative(fmi3InstanceHandle* instance, const fmi3ValueReference unknowns[],
        size_t nUnknowns, const fmi3ValueReference knowns[], size_t nKnowns,
        const fmi3Float64 seed[], size_t nSeed, fmi3Float64 sensitivity[], size_t nSensitivity);

    FMI4C_DLLAPI fmi3Status
    fmi3_getAdjointDerivative(fmi3InstanceHandle* instance, const fmi3ValueReference unknowns[],
        size_t nUnknowns, const fmi3ValueReference knowns[], size_t nKnowns,
        const fmi3Float64 seed[], size_t nSeed, fmi3Float64 sensitivity[], size_t nSensitivity);

    FMI4C_DLLAPI fmi3Status
    fmi3_enterConfigurationMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_exitConfigurationMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_getIntervalDecimal(fmi3InstanceHandle* instance,
        const fmi3ValueReference valueReferences[], size_t nValueReferences,
        fmi3Float64 intervals[], fmi3IntervalQualifier qualifiers[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_getIntervalFraction(fmi3InstanceHandle* instance,
        const fmi3ValueReference valueReferences[], size_t nValueReferences,
        fmi3UInt64 intervalCounters[], fmi3UInt64 resolutions[],
        fmi3IntervalQualifier qualifiers[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_getShiftDecimal(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3Float64 shifts[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_getShiftFraction(fmi3InstanceHandle* instance, const fmi3ValueReference valueReferences[],
        size_t nValueReferences, fmi3UInt64 shiftCounters[], fmi3UInt64 resolutions[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_setIntervalDecimal(fmi3InstanceHandle* instance,
        const fmi3ValueReference valueReferences[], size_t nValueReferences,
        const fmi3Float64 intervals[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_setIntervalFraction(fmi3InstanceHandle* instance,
        const fmi3ValueReference valueReferences[], size_t nValueReferences,
        const fmi3UInt64 intervalCounters[], const fmi3UInt64 resolutions[]);

    FMI4C_DLLAPI fmi3Status
    fmi3_evaluateDiscreteStates(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_updateDiscreteStates(fmi3InstanceHandle* instance, fmi3Boolean* discreteStatesNeedUpdate,
        fmi3Boolean* terminateSimulation, fmi3Boolean* nominalsOfContinuousStatesChanged,
        fmi3Boolean* valuesOfContinuousStatesChanged, fmi3Boolean* nextEventTimeDefined,
        fmi3Float64* nextEventTime);

    FMI4C_DLLAPI fmi3Status
    fmi3_enterContinuousTimeMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_completedIntegratorStep(fmi3InstanceHandle* instance,
        fmi3Boolean noSetFMUStatePriorToCurrentPoint, fmi3Boolean* enterEventMode,
        fmi3Boolean* terminateSimulation);

    FMI4C_DLLAPI fmi3Status
    fmi3_setTime(fmi3InstanceHandle* instance, fmi3Float64 time);

    FMI4C_DLLAPI fmi3Status
    fmi3_setContinuousStates(fmi3InstanceHandle* instance, const fmi3Float64 continuousStates[],
        size_t nContinuousStates);

    FMI4C_DLLAPI fmi3Status
    fmi3_getContinuousStateDerivatives(
        fmi3InstanceHandle* instance, fmi3Float64 derivatives[], size_t nContinuousStates);

    FMI4C_DLLAPI fmi3Status
    fmi3_getEventIndicators(
        fmi3InstanceHandle* instance, fmi3Float64 eventIndicators[], size_t nEventIndicators);

    FMI4C_DLLAPI fmi3Status
    fmi3_getContinuousStates(
        fmi3InstanceHandle* instance, fmi3Float64 continuousStates[], size_t nContinuousStates);

    FMI4C_DLLAPI fmi3Status
    fmi3_getNominalsOfContinuousStates(
        fmi3InstanceHandle* instance, fmi3Float64 nominals[], size_t nContinuousStates);

    FMI4C_DLLAPI fmi3Status
    fmi3_getNumberOfEventIndicators(fmi3InstanceHandle* instance, size_t* nEventIndicators);

    FMI4C_DLLAPI fmi3Status
    fmi3_getNumberOfContinuousStates(fmi3InstanceHandle* instance, size_t* nContinuousStates);

    FMI4C_DLLAPI fmi3Status
    fmi3_enterStepMode(fmi3InstanceHandle* instance);

    FMI4C_DLLAPI fmi3Status
    fmi3_getOutputDerivatives(fmi3InstanceHandle* instance,
        const fmi3ValueReference valueReferences[], size_t nValueReferences,
        const fmi3Int32 orders[], fmi3Float64 values[], size_t nValues);

    FMI4C_DLLAPI fmi3Status
    fmi3_activateModelPartition(fmi3InstanceHandle* instance, fmi3ValueReference clockReference,
        fmi3Float64 activationTime);

#ifdef __cplusplus
}
#endif

#endif // FMIC_H
