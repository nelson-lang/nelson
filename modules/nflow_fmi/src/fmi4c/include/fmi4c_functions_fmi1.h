#ifndef FMIC_FUNCTIONS_FMI1_H
#define FMIC_FUNCTIONS_FMI1_H

#include "fmi4c_types_fmi1.h"
#include <stddef.h>

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

// Callback functions
typedef void (*fmi1CallbackLogger_t)(fmi1Component*, fmi1String instanceName, fmi1Status status,
    fmi1String category, fmi1String message, ...);
typedef void* (*fmi1CallbackAllocateMemory_t)(size_t nobj, size_t size);
typedef void (*fmi1CallbackFreeMemory_t)(void* obj);
typedef void (*fmi1StepFinished_t)(fmi1Component*, fmi1Status status);

// Structs
typedef struct
{
    fmi1CallbackLogger_t logger;
    fmi1CallbackAllocateMemory_t allocateMemory;
    fmi1CallbackFreeMemory_t freeMemory;
} fmi1CallbackFunctionsModelExchange;

typedef struct
{
    fmi1CallbackLogger_t logger;
    fmi1CallbackAllocateMemory_t allocateMemory;
    fmi1CallbackFreeMemory_t freeMemory;
    fmi1StepFinished_t stepFinished;
} fmi1CallbackFunctionsCoSimulation;

typedef struct
{
    fmi1Boolean iterationConverged;
    fmi1Boolean stateValueReferencesChanged;
    fmi1Boolean stateValuesChanged;
    fmi1Boolean terminateSimulation;
    fmi1Boolean upcomingTimeEvent;
    fmi1Real nextEventTime;
} fmi1EventInfo;

// API functions
typedef const char*(STDCALL* fmiGetTypesPlatform_t)();
typedef const char*(STDCALL* fmiGetVersion_t)();
typedef fmi1Status(STDCALL* fmiSetDebugLogging_t)(fmi1Component*, fmi1Boolean);
typedef fmi1Status(STDCALL* fmiGetReal_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, fmi1Real[]);
typedef fmi1Status(STDCALL* fmiGetInteger_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, fmi1Integer[]);
typedef fmi1Status(STDCALL* fmiGetBoolean_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, fmi1Boolean[]);
typedef fmi1Status(STDCALL* fmiGetString_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, fmi1String[]);
typedef fmi1Status(STDCALL* fmiSetReal_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1Real[]);
typedef fmi1Status(STDCALL* fmiSetInteger_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1Integer[]);
typedef fmi1Status(STDCALL* fmiSetBoolean_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1Boolean[]);
typedef fmi1Status(STDCALL* fmiSetString_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1String[]);
typedef fmi1InstanceHandle*(STDCALL* fmiInstantiateSlave_t)(fmi1String, fmi1String, fmi1String,
    fmi1String, fmi1Real, fmi1Boolean, fmi1Boolean, fmi1CallbackFunctionsCoSimulation, fmi1Boolean);
typedef fmi1Status(STDCALL* fmiInitializeSlave_t)(fmi1Component*, fmi1Real, fmi1Boolean, fmi1Real);
typedef fmi1Status(STDCALL* fmiTerminateSlave_t)(fmi1Component*);
typedef fmi1Status(STDCALL* fmiResetSlave_t)(fmi1Component*);
typedef void(STDCALL* fmiFreeSlaveInstance_t)(fmi1Component*);
typedef fmi1Status(STDCALL* fmiSetRealInputDerivatives_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1Integer[], const fmi1Real[]);
typedef fmi1Status(STDCALL* fmiGetRealOutputDerivatives_t)(
    fmi1Component*, const fmi1ValueReference[], size_t, const fmi1Integer[], fmi1Real[]);
typedef fmi1Status(STDCALL* fmiCancelStep_t)(fmi1Component*);
typedef fmi1Status(STDCALL* fmiDoStep_t)(fmi1Component*, fmi1Real, fmi1Real, fmi1Boolean);
typedef fmi1Status(STDCALL* fmiGetStatus_t)(fmi1Component*, const fmi1StatusKind, fmi1Status*);
typedef fmi1Status(STDCALL* fmiGetRealStatus_t)(fmi1Component*, const fmi1StatusKind, fmi1Real*);
typedef fmi1Status(STDCALL* fmiGetIntegerStatus_t)(
    fmi1Component*, const fmi1StatusKind, fmi1Integer*);
typedef fmi1Status(STDCALL* fmiGetBooleanStatus_t)(
    fmi1Component*, const fmi1StatusKind, fmi1Boolean*);
typedef fmi1Status(STDCALL* fmiGetStringStatus_t)(
    fmi1Component*, const fmi1StatusKind, fmi1String*);
typedef const char*(STDCALL* fmiGetModelTypesPlatform_t)();
typedef fmi1InstanceHandle*(STDCALL* fmiInstantiateModel_t)(
    fmi1String, fmi1String, fmi1CallbackFunctionsModelExchange, fmi1Boolean);
typedef void(STDCALL* fmiFreeModelInstance_t)(fmi1Component*);
typedef fmi1Status(STDCALL* fmiSetTime_t)(fmi1Component*, fmi1Real);
typedef fmi1Status(STDCALL* fmiSetContinuousStates_t)(fmi1Component*, const fmi1Real[], size_t);
typedef fmi1Status(STDCALL* fmiCompletedIntegratorStep_t)(fmi1Component*, fmi1Boolean*);
typedef fmi1Status(STDCALL* fmiInitialize_t)(fmi1Component*, fmi1Boolean, fmi1Real, fmi1EventInfo*);
typedef fmi1Status(STDCALL* fmiGetDerivatives_t)(fmi1Component*, fmi1Real[], size_t);
typedef fmi1Status(STDCALL* fmiGetEventIndicators_t)(fmi1Component*, fmi1Real[], size_t);
typedef fmi1Status(STDCALL* fmiEventUpdate_t)(fmi1Component*, fmi1Boolean, fmi1EventInfo*);
typedef fmi1Status(STDCALL* fmiGetContinuousStates_t)(fmi1Component*, fmi1Real[], size_t);
typedef fmi1Status(STDCALL* fmiGetNominalContinuousStates_t)(fmi1Component*, fmi1Real[], size_t);
typedef fmi1Status(STDCALL* fmiGetStateValueReferences_t)(
    fmi1Component*, fmi1ValueReference[], size_t);
typedef fmi1Status(STDCALL* fmiTerminate_t)(fmi1Component*);

#endif // FMIC_FUNCTIONS_FMI1_H
