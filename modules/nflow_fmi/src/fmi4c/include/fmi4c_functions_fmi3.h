#ifndef FMIC_FUNCTIONS_FMI3_H
#define FMIC_FUNCTIONS_FMI3_H

#include "fmi4c_types_fmi3.h"
#include <stddef.h>

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

// Callback functions
typedef void (*fmi3LogMessageCallback)(
    fmi3InstanceEnvironment, fmi3Status status, fmi3String category, fmi3String message);

typedef void (*fmi3IntermediateUpdateCallback)(fmi3InstanceEnvironment instanceEnvironment,
    fmi3Float64 intermediateUpdateTime, fmi3Boolean intermediateVariableSetRequested,
    fmi3Boolean intermediateVariableGetAllowed, fmi3Boolean intermediateStepFinished,
    fmi3Boolean canReturnEarly, fmi3Boolean* earlyReturnRequested, fmi3Float64* earlyReturnTime);

typedef void (*fmi3CallbackLockPreemption)();
typedef void (*fmi3CallbackUnlockPreemption)();

// API functions
typedef const char*(STDCALL* fmi3GetVersion_t)();
typedef fmi3Status(STDCALL* fmi3SetDebugLogging_t)(
    fmi3Component*, fmi3Boolean, size_t, const fmi3String[]);
typedef fmi3Component*(STDCALL* fmi3InstantiateModelExchange_t)(fmi3String, fmi3String, fmi3String,
    fmi3Boolean, fmi3Boolean, fmi3InstanceEnvironment, fmi3LogMessageCallback);
typedef fmi3Component*(STDCALL* fmi3InstantiateCoSimulation_t)(fmi3String, fmi3String, fmi3String,
    fmi3Boolean, fmi3Boolean, fmi3Boolean, fmi3Boolean, const fmi3ValueReference[], size_t,
    fmi3InstanceEnvironment, fmi3LogMessageCallback, fmi3IntermediateUpdateCallback);
typedef fmi3InstanceHandle*(STDCALL* fmi3InstantiateScheduledExecution_t)(fmi3String, fmi3String,
    fmi3String, fmi3Boolean, fmi3Boolean, fmi3InstanceEnvironment, fmi3LogMessageCallback,
    fmi3IntermediateUpdateCallback, fmi3CallbackLockPreemption, fmi3CallbackUnlockPreemption);
typedef void(STDCALL* fmi3FreeInstance_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3EnterInitializationMode_t)(
    fmi3Component*, fmi3Boolean, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Float64);
typedef fmi3Status(STDCALL* fmi3ExitInitializationMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3Terminate_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3GetFloat64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3SetFloat64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3DoStep_t)(fmi3Component*, fmi3Float64, fmi3Float64, fmi3Boolean,
    fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
typedef fmi3Status(STDCALL* fmi3EnterEventMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3Reset_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3GetFloat32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Float32[], size_t);
typedef fmi3Status(STDCALL* fmi3GetInt8_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Int8[], size_t);
typedef fmi3Status(STDCALL* fmi3GetUInt8_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3UInt8[], size_t);
typedef fmi3Status(STDCALL* fmi3GetInt16_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Int16[], size_t);
typedef fmi3Status(STDCALL* fmi3GetUInt16_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3UInt16[], size_t);
typedef fmi3Status(STDCALL* fmi3GetInt32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Int32[], size_t);
typedef fmi3Status(STDCALL* fmi3GetUInt32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3UInt32[], size_t);
typedef fmi3Status(STDCALL* fmi3GetInt64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Int64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetUInt64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3UInt64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetBoolean_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Boolean[], size_t);
typedef fmi3Status(STDCALL* fmi3GetString_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3String[], size_t);
typedef fmi3Status(STDCALL* fmi3GetBinary_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, size_t[], fmi3Binary[], size_t);
typedef fmi3Status(STDCALL* fmi3GetClock_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Clock[]);
typedef fmi3Status(STDCALL* fmi3SetFloat32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Float32[], size_t);
typedef fmi3Status(STDCALL* fmi3SetInt8_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Int8[], size_t);
typedef fmi3Status(STDCALL* fmi3SetUInt8_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3UInt8[], size_t);
typedef fmi3Status(STDCALL* fmi3SetInt16_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Int16[], size_t);
typedef fmi3Status(STDCALL* fmi3SetUInt16_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3UInt16[], size_t);
typedef fmi3Status(STDCALL* fmi3SetInt32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Int32[], size_t);
typedef fmi3Status(STDCALL* fmi3SetUInt32_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3UInt32[], size_t);
typedef fmi3Status(STDCALL* fmi3SetInt64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Int64[], size_t);
typedef fmi3Status(STDCALL* fmi3SetUInt64_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3UInt64[], size_t);
typedef fmi3Status(STDCALL* fmi3SetBoolean_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Boolean[], size_t);
typedef fmi3Status(STDCALL* fmi3SetString_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3String[], size_t);
typedef fmi3Status(STDCALL* fmi3SetBinary_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const size_t[], const fmi3Binary[], size_t);
typedef fmi3Status(STDCALL* fmi3SetClock_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Clock[]);
typedef fmi3Status(STDCALL* fmi3GetNumberOfVariableDependencies_t)(
    fmi3Component*, fmi3ValueReference, size_t*);
typedef fmi3Status(STDCALL* fmi3GetVariableDependencies_t)(fmi3Component*, fmi3ValueReference,
    size_t[], fmi3ValueReference[], size_t[], fmi3DependencyKind[], size_t);
typedef fmi3Status(STDCALL* fmi3GetFMUState_t)(fmi3Component*, fmi3FMUState*);
typedef fmi3Status(STDCALL* fmi3SetFMUState_t)(fmi3Component*, fmi3FMUState);
typedef fmi3Status(STDCALL* fmi3FreeFMUState_t)(fmi3Component*, fmi3FMUState*);
typedef fmi3Status(STDCALL* fmi3SerializedFMUStateSize_t)(fmi3Component*, fmi3FMUState, size_t*);
typedef fmi3Status(STDCALL* fmi3SerializeFMUState_t)(
    fmi3Component*, fmi3FMUState, fmi3Byte[], size_t);
typedef fmi3Status(STDCALL* fmi3DeserializeFMUState_t)(
    fmi3Component*, const fmi3Byte[], size_t, fmi3FMUState*);
typedef fmi3Status(STDCALL* fmi3GetDirectionalDerivative_t)(fmi3Component*,
    const fmi3ValueReference[], size_t, const fmi3ValueReference[], size_t, const fmi3Float64[],
    size_t, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetAdjointDerivative_t)(fmi3Component*, const fmi3ValueReference[],
    size_t, const fmi3ValueReference[], size_t, const fmi3Float64[], size_t, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3EnterConfigurationMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3ExitConfigurationMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3GetIntervalDecimal_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Float64[], fmi3IntervalQualifier[]);
typedef fmi3Status(STDCALL* fmi3GetIntervalFraction_t)(fmi3Component*, const fmi3ValueReference[],
    size_t, fmi3UInt64[], fmi3UInt64[], fmi3IntervalQualifier[]);
typedef fmi3Status(STDCALL* fmi3GetShiftDecimal_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3Float64[]);
typedef fmi3Status(STDCALL* fmi3GetShiftFraction_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, fmi3UInt64[], fmi3UInt64[]);
typedef fmi3Status(STDCALL* fmi3SetIntervalDecimal_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Float64[]);
typedef fmi3Status(STDCALL* fmi3SetIntervalFraction_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3UInt64[], const fmi3UInt64[]);
typedef fmi3Status(STDCALL* fmi3SetShiftDecimal_t)(fmi3Component* instance,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    const fmi3Float64 shifts[]);
typedef fmi3Status(STDCALL* fmi3SetShiftFraction_t)(fmi3Component* instance,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    const fmi3UInt64 counters[], const fmi3UInt64 resolutions[]);

typedef fmi3Status(STDCALL* fmi3EvaluateDiscreteStates_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3UpdateDiscreteStates_t)(fmi3Component*, fmi3Boolean*, fmi3Boolean*,
    fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
typedef fmi3Status(STDCALL* fmi3EnterContinuousTimeMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3CompletedIntegratorStep_t)(
    fmi3Component*, fmi3Boolean, fmi3Boolean*, fmi3Boolean*);
typedef fmi3Status(STDCALL* fmi3SetTime_t)(fmi3Component*, fmi3Float64);
typedef fmi3Status(STDCALL* fmi3SetContinuousStates_t)(fmi3Component*, const fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetContinuousStateDerivatives_t)(
    fmi3Component*, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetEventIndicators_t)(fmi3Component*, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetContinuousStates_t)(fmi3Component*, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetNominalsOfContinuousStates_t)(
    fmi3Component*, fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3GetNumberOfEventIndicators_t)(fmi3Component*, size_t*);
typedef fmi3Status(STDCALL* fmi3GetNumberOfContinuousStates_t)(fmi3Component*, size_t*);
typedef fmi3Status(STDCALL* fmi3EnterStepMode_t)(fmi3Component*);
typedef fmi3Status(STDCALL* fmi3GetOutputDerivatives_t)(
    fmi3Component*, const fmi3ValueReference[], size_t, const fmi3Int32[], fmi3Float64[], size_t);
typedef fmi3Status(STDCALL* fmi3ActivateModelPartition_t)(
    fmi3Component*, fmi3ValueReference, fmi3Float64);

#endif // FMIC_FUNCTIONS_FMI3_H
