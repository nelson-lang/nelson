#ifndef FMIC_FUNCTIONS_FMI2_H
#define FMIC_FUNCTIONS_FMI2_H

#include "fmi4c_types_fmi2.h"
#include <stddef.h>

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

// Callback functions
typedef void (*fmi2CallbackLogger)(
    fmi2ComponentEnvironment, fmi2String, fmi2Status, fmi2String, fmi2String, ...);
typedef void* (*fmi2CallbackAllocateMemory)(size_t, size_t);
typedef void (*fmi2CallbackFreeMemory)(void*);
typedef void (*fmi2StepFinished)(fmi2ComponentEnvironment, fmi2Status);

// Structs
typedef struct
{
    fmi2CallbackLogger logger;
    fmi2CallbackAllocateMemory allocateMemory;
    fmi2CallbackFreeMemory freeMemory;
    fmi2StepFinished stepFinished;
    fmi2ComponentEnvironment componentEnvironment;
} fmi2CallbackFunctions;

typedef struct
{
    fmi2Boolean newDiscreteStatesNeeded;
    fmi2Boolean terminateSimulation;
    fmi2Boolean nominalsOfContinuousStatesChanged;
    fmi2Boolean valuesOfContinuousStatesChanged;
    fmi2Boolean nextEventTimeDefined;
    fmi2Real nextEventTime;
} fmi2EventInfo;

// API functions
typedef const char*(STDCALL* fmi2GetTypesPlatform_t)(void);

typedef const char*(STDCALL* fmi2GetVersion_t)(void);

typedef fmi2Status(STDCALL* fmi2SetDebugLogging_t)(
    fmi2Component*, fmi2Boolean, size_t, const fmi2String[]);

typedef fmi2Component(STDCALL* fmi2Instantiate_t)(fmi2String, fmi2Type, fmi2String, fmi2String,
    const fmi2CallbackFunctions*, fmi2Boolean, fmi2Boolean);

typedef void(STDCALL* fmi2FreeInstance_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2SetupExperiment_t)(
    fmi2Component*, fmi2Boolean, fmi2Real, fmi2Real, fmi2Boolean, fmi2Real);
typedef fmi2Status(STDCALL* fmi2EnterInitializationMode_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2ExitInitializationMode_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2Terminate_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2Reset_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2GetReal_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, fmi2Real[]);
typedef fmi2Status(STDCALL* fmi2GetInteger_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, fmi2Integer[]);
typedef fmi2Status(STDCALL* fmi2GetBoolean_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, fmi2Boolean[]);
typedef fmi2Status(STDCALL* fmi2GetString_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, fmi2String[]);
typedef fmi2Status(STDCALL* fmi2SetReal_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2Real[]);
typedef fmi2Status(STDCALL* fmi2SetInteger_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2Integer[]);
typedef fmi2Status(STDCALL* fmi2SetBoolean_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2Boolean[]);
typedef fmi2Status(STDCALL* fmi2SetString_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2String[]);
typedef fmi2Status(STDCALL* fmi2GetFMUstate_t)(fmi2Component*, fmi2FMUstate*);
typedef fmi2Status(STDCALL* fmi2SetFMUstate_t)(fmi2Component*, fmi2FMUstate);
typedef fmi2Status(STDCALL* fmi2FreeFMUstate_t)(fmi2Component*, fmi2FMUstate*);
typedef fmi2Status(STDCALL* fmi2SerializedFMUstateSize_t)(fmi2Component*, fmi2FMUstate, size_t*);
typedef fmi2Status(STDCALL* fmi2SerializeFMUstate_t)(
    fmi2Component*, fmi2FMUstate, fmi2Byte[], size_t);
typedef fmi2Status(STDCALL* fmi2DeSerializeFMUstate_t)(
    fmi2Component*, const fmi2Byte[], size_t, fmi2FMUstate*);
typedef fmi2Status(STDCALL* fmi2GetDirectionalDerivative_t)(fmi2Component*,
    const fmi2ValueReference[], size_t, const fmi2ValueReference[], size_t, const fmi2Real[],
    fmi2Real[]);
typedef fmi2Status(STDCALL* fmi2EnterEventMode_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2NewDiscreteStates_t)(fmi2Component*, fmi2EventInfo*);
typedef fmi2Status(STDCALL* fmi2EnterContinuousTimeMode_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2CompletedIntegratorStep_t)(
    fmi2Component*, fmi2Boolean, fmi2Boolean*, fmi2Boolean*);
typedef fmi2Status(STDCALL* fmi2SetTime_t)(fmi2Component*, fmi2Real);
typedef fmi2Status(STDCALL* fmi2SetContinuousStates_t)(fmi2Component*, const fmi2Real[], size_t);
typedef fmi2Status(STDCALL* fmi2GetDerivatives_t)(fmi2Component*, fmi2Real[], size_t);
typedef fmi2Status(STDCALL* fmi2GetEventIndicators_t)(fmi2Component*, fmi2Real[], size_t);
typedef fmi2Status(STDCALL* fmi2GetContinuousStates_t)(fmi2Component*, fmi2Real[], size_t);
typedef fmi2Status(STDCALL* fmi2GetNominalsOfContinuousStates_t)(
    fmi2Component*, fmi2Real[], size_t);
typedef fmi2Status(STDCALL* fmi2SetRealInputDerivatives_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2Integer[], const fmi2Real[]);
typedef fmi2Status(STDCALL* fmi2GetRealOutputDerivatives_t)(
    fmi2Component*, const fmi2ValueReference[], size_t, const fmi2Integer[], fmi2Real[]);
typedef fmi2Status(STDCALL* fmi2DoStep_t)(fmi2Component*, fmi2Real, fmi2Real, fmi2Boolean);
typedef fmi2Status(STDCALL* fmi2CancelStep_t)(fmi2Component*);
typedef fmi2Status(STDCALL* fmi2GetStatus_t)(fmi2Component*, const fmi2StatusKind, fmi2Status*);
typedef fmi2Status(STDCALL* fmi2GetRealStatus_t)(fmi2Component*, const fmi2StatusKind, fmi2Real*);
typedef fmi2Status(STDCALL* fmi2GetIntegerStatus_t)(
    fmi2Component*, const fmi2StatusKind, fmi2Integer*);
typedef fmi2Status(STDCALL* fmi2GetBooleanStatus_t)(
    fmi2Component*, const fmi2StatusKind, fmi2Boolean*);
typedef fmi2Status(STDCALL* fmi2GetStringStatus_t)(
    fmi2Component*, const fmi2StatusKind, fmi2String*);

#endif // FMIC_FUNCTIONS_FMI2_H
