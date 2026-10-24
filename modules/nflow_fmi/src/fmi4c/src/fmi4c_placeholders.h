#ifndef FMI4C_PLACEHOLDERS_H
#define FMI4C_PLACEHOLDERS_H

#include "fmi4c_common.h"
#include "fmi4c_types_fmi1.h"
#include "fmi4c_types_fmi2.h"
#include "fmi4c_types_fmi3.h"
#include "fmi4c_functions_fmi1.h"
#include "fmi4c_functions_fmi2.h"
#include "fmi4c_functions_fmi3.h"
#include "fmi/fmiPlatformTypes.h"
#include "fmi/fmiFunctions.h"

#include <stdio.h>

fmi1Status STDCALL
placeholder(fmi1Component component, fmi1Boolean x)
{
    return fmi1Error;
}

#define NOT_IMPLEMENTED(FUNCNAME)                                                                  \
    printf("Function \"%s\" is not provided by this FMU.\n", #FUNCNAME);

const char* STDCALL
placeholder_fmiGetVersion()
{
    NOT_IMPLEMENTED(fmiGetVersion);
    return NULL;
}

const char* STDCALL
placeholder_fmiGetTypesPlatform()
{
    NOT_IMPLEMENTED(fmiGetTypesPlatform);
    return NULL;
}

fmi1Status STDCALL
placeholder_fmiSetDebugLogging(fmi1Component* component, fmi1Boolean loggingOn)
{
    UNUSED(component);
    UNUSED(loggingOn);
    NOT_IMPLEMENTED(fmiSetDebugLogging);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetReal(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, fmi1Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetReal);
    return fmi1Error;
}
fmi1Status STDCALL
placeholder_fmiGetInteger(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, fmi1Integer value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetInteger);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetBoolean(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, fmi1Boolean value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetBoolean);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetString(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, fmi1String value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetString);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiSetReal(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, const fmi1Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiSetReal);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiSetInteger(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, const fmi1Integer value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiSetInteger);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiSetBoolean(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, const fmi1Boolean value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiSetBoolean);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiSetString(
    fmi1Component* component, const fmi1ValueReference vr[], size_t nvr, const fmi1String value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiSetString);
    return fmi1Error;
}
fmi1InstanceHandle* STDCALL
placeholder_fmiInstantiateSlave(fmi1String instanceName, fmi1String fmuGUID, fmi1String fmuLocation,
    fmi1String mimeType, fmi1Real timeout, fmi1Boolean visible, fmi1Boolean interactive,
    fmi1CallbackFunctionsCoSimulation functions, fmi1Boolean loggingOn)
{
    UNUSED(instanceName);
    UNUSED(fmuGUID);
    UNUSED(fmuLocation);
    UNUSED(mimeType);
    UNUSED(timeout);
    UNUSED(visible);
    UNUSED(interactive);
    UNUSED(functions);
    UNUSED(loggingOn);
    NOT_IMPLEMENTED(fmiInstantiateSlave);
    return NULL;
}

fmi1Status STDCALL
placeholder_fmiInitializeSlave(
    fmi1Component* component, fmi1Real tStart, fmi1Boolean StopTimeDefined, fmi1Real tStop)
{
    UNUSED(component);
    UNUSED(tStart);
    UNUSED(StopTimeDefined);
    UNUSED(tStop);
    NOT_IMPLEMENTED(fmiInitializeSlave);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiTerminateSlave(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiTerminateSlave);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiResetSlave(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiResetSlave);
    return fmi1Error;
}

void STDCALL
placeholder_fmiFreeSlaveInstance(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiFreeSlaveInstance);
}

fmi1Status STDCALL
placeholder_fmiSetRealInputDerivatives(fmi1Component* component, const fmiValueReference vr[],
    size_t nvr, const fmi1Integer order[], const fmi1Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(order);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiSetRealInputDerivatives);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetRealOutputDerivatives(fmi1Component* component, const fmi1ValueReference vr[],
    size_t nvr, const fmi1Integer order[], fmi1Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(order);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetRealOutputDerivatives);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiCancelStep(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiCancelStep);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiDoStep(fmi1Component* component, fmi1Real currentCommunicationPoint,
    fmi1Real communicationStepSize, fmi1Boolean newStep)
{
    UNUSED(component);
    UNUSED(currentCommunicationPoint);
    UNUSED(communicationStepSize);
    UNUSED(newStep);
    NOT_IMPLEMENTED(fmiDoStep);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetStatus(fmi1Component* component, const fmi1StatusKind s, fmi1Status* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetStatus);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetRealStatus(fmi1Component* component, const fmi1StatusKind s, fmi1Real* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetRealStatus);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetIntegerStatus(
    fmi1Component* component, const fmi1StatusKind s, fmi1Integer* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetIntegerStatus);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetBooleanStatus(
    fmi1Component* component, const fmi1StatusKind s, fmi1Boolean* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetBooleanStatus);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetStringStatus(fmi1Component* component, const fmi1StatusKind s, fmi1String* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmiGetStringStatus);
    return fmi1Error;
}

const char* STDCALL
placeholder_fmiGetModelTypesPlatform()
{
    NOT_IMPLEMENTED(fmiGetModelTypesPlatform);
    return NULL;
}

fmi1InstanceHandle* STDCALL
placeholder_fmiInstantiateModel(fmi1String instanceName, fmi1String fmuGUID,
    fmi1CallbackFunctionsModelExchange functions, fmi1Boolean loggingOn)
{
    UNUSED(instanceName);
    UNUSED(fmuGUID);
    UNUSED(functions);
    UNUSED(loggingOn);
    NOT_IMPLEMENTED(fmiInstantiateModel);
    return NULL;
}

void STDCALL
placeholder_fmiFreeModelInstance(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiFreeModelInstance);
}

fmi1Status STDCALL
placeholder_fmiSetTime(fmi1Component* component, fmi1Real time)
{
    UNUSED(component);
    UNUSED(time);
    NOT_IMPLEMENTED(fmiSetTime);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiSetContinuousStates(fmi1Component* component, const fmi1Real x[], size_t nx)
{
    UNUSED(component);
    UNUSED(x);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmiSetContinuousStates);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiCompletedIntegratorStep(fmi1Component* component, fmi1Boolean* callEventUpdate)
{
    UNUSED(component);
    UNUSED(callEventUpdate);
    NOT_IMPLEMENTED(fmiCompletedIntegratorStep);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiInitialize(fmi1Component* component, fmi1Boolean toleranceControlled,
    fmi1Real relativeTolerance, fmi1EventInfo* eventInfo)
{
    UNUSED(component);
    UNUSED(toleranceControlled);
    UNUSED(relativeTolerance);
    UNUSED(eventInfo);
    NOT_IMPLEMENTED(fmiInitialize);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetDerivatives(fmi1Component* component, fmi1Real derivatives[], size_t nx)
{
    UNUSED(component);
    UNUSED(derivatives);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmiGetDerivatives);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetEventIndicators(fmi1Component* component, fmi1Real eventIndicators[], size_t ni)
{
    UNUSED(component);
    UNUSED(eventIndicators);
    UNUSED(ni);
    NOT_IMPLEMENTED(fmiGetEventIndicators);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiEventUpdate(
    fmi1Component* component, fmi1Boolean intermediateResults, fmi1EventInfo* eventInfo)
{
    UNUSED(component);
    UNUSED(intermediateResults);
    UNUSED(eventInfo);
    NOT_IMPLEMENTED(fmiEventUpdate);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetContinuousStates(fmi1Component* component, fmi1Real states[], size_t nx)
{
    UNUSED(component);
    UNUSED(states);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmiGetContinuousStates);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetNominalContinuousStates(fmi1Component* component, fmi1Real x_nominal[], size_t nx)
{
    UNUSED(component);
    UNUSED(x_nominal);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmiGetNominalContinuousStates);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiGetStateValueReferences(
    fmi1Component* component, fmi1ValueReference vrx[], size_t nx)
{
    UNUSED(component);
    UNUSED(vrx);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmiGetStateValueReferences);
    return fmi1Error;
}

fmi1Status STDCALL
placeholder_fmiTerminate(fmi1Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmiTerminate);
    return fmi1Error;
}

const char* STDCALL
placeholder_fmi2_getTypesPlatform(void)
{
    NOT_IMPLEMENTED(fmi2_getTypesPlatform);
    return NULL;
}

const char* STDCALL
placeholder_fmi2_getVersion(void)
{
    NOT_IMPLEMENTED(fmi2_getVersion);
    return NULL;
}

fmi2Status STDCALL
placeholder_fmi2_setDebugLogging(fmi2Component* component, fmi2Boolean loggingOn,
    size_t nCategories, const fmi2String categories[])
{
    UNUSED(component);
    UNUSED(loggingOn);
    UNUSED(nCategories);
    UNUSED(categories);
    NOT_IMPLEMENTED(fmi2_setDebugLogging);
    return fmi2Error;
}

/* Creation and destruction of FMU instances and setting debug status */
fmi2Component STDCALL
placeholder_fmi2Instantiate(fmi2String instanceName, fmi2Type fmuType, fmi2String fmuGUID,
    fmi2String fmuResourceLocation, const fmi2CallbackFunctions* functions, fmi2Boolean visible,
    fmi2Boolean loggingOn)
{
    UNUSED(instanceName);
    UNUSED(fmuType);
    UNUSED(fmuGUID);
    UNUSED(fmuResourceLocation);
    UNUSED(functions);
    UNUSED(visible);
    UNUSED(loggingOn);
    NOT_IMPLEMENTED(fmi2Instantiate);
    return NULL;
}

void STDCALL
placeholder_fmi2FreeInstance(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2FreeInstance);
}

/* Enter and exit initialization mode, terminate and reset */
fmi2Status STDCALL
placeholder_fmi2_setupExperiment(fmi2Component* component, fmi2Boolean toleranceDefined,
    fmi2Real tolerance, fmi2Real startTime, fmi2Boolean stopTimeDefined, fmi2Real stopTime)
{
    UNUSED(component);
    UNUSED(toleranceDefined);
    UNUSED(tolerance);
    UNUSED(startTime);
    UNUSED(stopTimeDefined);
    UNUSED(stopTime);
    NOT_IMPLEMENTED(fmi2_setupExperiment);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2EnterInitializationMode(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2EnterInitializationMode);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2ExitInitializationMode(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2ExitInitializationMode);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2Terminate(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2Terminate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2Reset(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2Reset);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getReal(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, fmi2Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getReal);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getInteger(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, fmi2Integer value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getInteger);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getBoolean(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, fmi2Boolean value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getBoolean);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getString(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, fmi2String value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getString);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setReal(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, const fmi2Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_setReal);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setInteger(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, const fmi2Integer value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_setInteger);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setBoolean(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, const fmi2Boolean value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_setBoolean);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setString(
    fmi2Component* component, const fmi2ValueReference vr[], size_t nvr, const fmi2String value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_setString);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2EnterEventMode(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2EnterEventMode);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2NewDiscreteStates(fmi2Component* component, fmi2EventInfo* eventInfo)
{
    UNUSED(component);
    UNUSED(eventInfo);
    NOT_IMPLEMENTED(fmi2NewDiscreteStates);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2EnterContinuousTimeMode(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2EnterContinuousTimeMode);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2CompletedIntegratorStep(fmi2Component* component,
    fmi2Boolean noSetFMUStatePriorToCurrentPoint, fmi2Boolean* enterEventMode,
    fmi2Boolean* terminateSimulation)
{
    UNUSED(component);
    UNUSED(noSetFMUStatePriorToCurrentPoint);
    UNUSED(enterEventMode);
    UNUSED(terminateSimulation);
    NOT_IMPLEMENTED(fmi2CompletedIntegratorStep);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setTime(fmi2Component* component, fmi2Real time)
{
    UNUSED(component);
    UNUSED(time);
    NOT_IMPLEMENTED(fmi2_setTime);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setContinuousStates(
    fmi2Component* component, const fmi2Real states[], size_t nStates)
{
    UNUSED(component);
    UNUSED(states);
    UNUSED(nStates);
    NOT_IMPLEMENTED(fmi2_setContinuousStates);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getDerivatives(fmi2Component* component, fmi2Real derivatives[], size_t nx)
{
    UNUSED(component);
    UNUSED(derivatives);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmi2_getDerivatives);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getEventIndicators(fmi2Component* component, fmi2Real eventIndicators[], size_t ni)
{
    UNUSED(component);
    UNUSED(eventIndicators);
    UNUSED(ni);
    NOT_IMPLEMENTED(fmi2_getEventIndicators);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getContinuousStates(fmi2Component* component, fmi2Real states[], size_t nStates)
{
    UNUSED(component);
    UNUSED(states);
    UNUSED(nStates);
    NOT_IMPLEMENTED(fmi2_getContinuousStates);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getNominalsOfContinuousStates(
    fmi2Component* component, fmi2Real x_nominal[], size_t nx)
{
    UNUSED(component);
    UNUSED(x_nominal);
    UNUSED(nx);
    NOT_IMPLEMENTED(fmi2_getNominalsOfContinuousStates);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2DoStep(fmi2Component* component, fmi2Real currentCommunicationPoint,
    fmi2Real communicationStepSize, fmi2Boolean noSetFMUStatePriorToCurrentPoint)
{
    UNUSED(component);
    UNUSED(currentCommunicationPoint);
    UNUSED(communicationStepSize);
    UNUSED(noSetFMUStatePriorToCurrentPoint);
    NOT_IMPLEMENTED(fmi2DoStep);
    return fmi2OK;
}

fmi2Status STDCALL
placeholder_fmi2_getFMUstate(fmi2Component* component, fmi2FMUstate* FMUstate)
{
    UNUSED(component);
    UNUSED(FMUstate);
    NOT_IMPLEMENTED(fmi2_getFMUstate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setFMUstate(fmi2Component* component, fmi2FMUstate FMUstate)
{
    UNUSED(component);
    UNUSED(FMUstate);
    NOT_IMPLEMENTED(fmi2_setFMUstate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2FreeFMUstate(fmi2Component* component, fmi2FMUstate* FMUstate)
{
    UNUSED(component);
    UNUSED(FMUstate);
    NOT_IMPLEMENTED(fmi2FreeFMUstate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2SerializedFMUstateSize(
    fmi2Component* component, fmi2FMUstate FMUstate, size_t* size)
{
    UNUSED(component);
    UNUSED(FMUstate);
    UNUSED(size);
    NOT_IMPLEMENTED(fmi2SerializedFMUstateSize);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2SerializeFMUstate(
    fmi2Component* component, fmi2FMUstate FMUstate, fmi2Byte serializedState[], size_t size)
{
    UNUSED(component);
    UNUSED(FMUstate);
    UNUSED(serializedState);
    UNUSED(size);
    NOT_IMPLEMENTED(fmi2SerializeFMUstate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2DeSerializeFMUstate(
    fmi2Component* component, const fmi2Byte serializedState[], size_t size, fmi2FMUstate* FMUstate)
{
    UNUSED(component);
    UNUSED(serializedState);
    UNUSED(size);
    UNUSED(FMUstate);
    NOT_IMPLEMENTED(fmi2DeSerializeFMUstate);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getDirectionalDerivative(fmi2Component* component,
    const fmi2ValueReference vUnknownRef[], size_t nUnknown, const fmi2ValueReference vKnownRef[],
    size_t nKnown, const fmi2Real dvKnown[], fmi2Real dvUnknown[])
{
    UNUSED(component);
    UNUSED(vUnknownRef);
    UNUSED(nUnknown);
    UNUSED(vKnownRef);
    UNUSED(nKnown);
    UNUSED(dvKnown);
    UNUSED(dvUnknown);
    NOT_IMPLEMENTED(fmi2_getDirectionalDerivative);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_setRealInputDerivatives(fmi2Component* component, const fmi2ValueReference vr[],
    size_t nvr, const fmi2Integer order[], const fmi2Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(order);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_setRealInputDerivatives);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getRealOutputDerivatives(fmi2Component* component, const fmi2ValueReference vr[],
    size_t nvr, const fmi2Integer order[], fmi2Real value[])
{
    UNUSED(component);
    UNUSED(vr);
    UNUSED(nvr);
    UNUSED(order);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getRealOutputDerivatives);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2CancelStep(fmi2Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi2CancelStep);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getStatus(fmi2Component* component, const fmi2StatusKind s, fmi2Status* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getStatus);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getRealStatus(fmi2Component* component, const fmi2StatusKind s, fmi2Real* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getRealStatus);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getIntegerStatus(
    fmi2Component* component, const fmi2StatusKind s, fmi2Integer* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getIntegerStatus);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getBooleanStatus(
    fmi2Component* component, const fmi2StatusKind s, fmi2Boolean* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getBooleanStatus);
    return fmi2Error;
}

fmi2Status STDCALL
placeholder_fmi2_getStringStatus(
    fmi2Component* component, const fmi2StatusKind s, fmi2String* value)
{
    UNUSED(component);
    UNUSED(s);
    UNUSED(value);
    NOT_IMPLEMENTED(fmi2_getStringStatus);
    return fmi2Error;
}

const char* STDCALL
placeholder_fmi3GetVersion()
{
    NOT_IMPLEMENTED(fmi3GetVersion);
    return NULL;
}

fmi3Status STDCALL
placeholder_fmi3SetDebugLogging(fmi3Component* component, fmi3Boolean loggingOn, size_t nCategories,
    const fmi3String categories[])
{
    UNUSED(component);
    UNUSED(loggingOn);
    UNUSED(nCategories);
    UNUSED(categories);
    NOT_IMPLEMENTED(fmi3SetDebugLogging);
    return fmi3Error;
}

fmi3Component* STDCALL
placeholder_fmi3InstantiateCoSimulation(fmi3String instanceName, fmi3String instantiationToken,
    fmi3String resourcePath, fmi3Boolean visible, fmi3Boolean loggingOn, fmi3Boolean eventModeUsed,
    fmi3Boolean earlyReturnAllowed, const fmi3ValueReference requiredIntermediateVariables[],
    size_t nRequiredIntermediateVariables, fmi3InstanceEnvironment instanceEnvironment,
    fmi3LogMessageCallback logMessage, fmi3IntermediateUpdateCallback intermediateUpdate)
{
    UNUSED(instanceName);
    UNUSED(instantiationToken);
    UNUSED(resourcePath);
    UNUSED(visible);
    UNUSED(loggingOn);
    UNUSED(eventModeUsed);
    UNUSED(earlyReturnAllowed);
    UNUSED(requiredIntermediateVariables);
    UNUSED(nRequiredIntermediateVariables);
    UNUSED(instanceEnvironment);
    UNUSED(logMessage);
    UNUSED(intermediateUpdate);
    NOT_IMPLEMENTED(fmi3InstantiateCoSimulation);
    return NULL;
}

fmi3Component* STDCALL
placeholder_fmi3InstantiateModelExchange(fmi3String instanceName, fmi3String instantiationToken,
    fmi3String resourcePath, fmi3Boolean visible, fmi3Boolean loggingOn,
    fmi3InstanceEnvironment instanceEnvironment, fmi3LogMessageCallback logMessage)
{
    UNUSED(instanceName);
    UNUSED(instantiationToken);
    UNUSED(resourcePath);
    UNUSED(visible);
    UNUSED(loggingOn);
    UNUSED(instanceEnvironment);
    UNUSED(logMessage);
    NOT_IMPLEMENTED(fmi3InstantiateModelExchange);
    return NULL;
}

fmi3InstanceHandle* STDCALL
placeholder_fmi3InstantiateScheduledExecution(fmi3String instanceName,
    fmi3String instantiationToken, fmi3String resourcePath, fmi3Boolean visible,
    fmi3Boolean loggingOn, fmi3InstanceEnvironment instanceEnvironment,
    fmi3LogMessageCallback logMessage, fmi3IntermediateUpdateCallback intermediateUpdate,
    fmi3CallbackLockPreemption lockPreemption, fmi3CallbackUnlockPreemption unlockPreemption)
{
    UNUSED(instanceName);
    UNUSED(instantiationToken);
    UNUSED(resourcePath);
    UNUSED(visible);
    UNUSED(loggingOn);
    UNUSED(instanceEnvironment);
    UNUSED(logMessage);
    UNUSED(intermediateUpdate);
    UNUSED(lockPreemption);
    UNUSED(unlockPreemption);
    NOT_IMPLEMENTED(fmi3InstantiateScheduledExecution);
    return NULL;
}

void STDCALL
placeholder_fmi3FreeInstance(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3FreeInstance);
}

fmi3Status STDCALL
placeholder_fmi3EnterInitializationMode(fmi3Component* component, fmi3Boolean toleranceDefined,
    fmi3Float64 tolerance, fmi3Float64 startTime, fmi3Boolean stopTimeDefined, fmi3Float64 stopTime)
{
    UNUSED(component);
    UNUSED(toleranceDefined);
    UNUSED(tolerance);
    UNUSED(startTime);
    UNUSED(stopTimeDefined);
    UNUSED(stopTime);
    NOT_IMPLEMENTED(fmi3EnterInitializationMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3ExitInitializationMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3ExitInitializationMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3Terminate(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3Terminate);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3DoStep(fmi3Component* component, fmi3Float64 currentCommunicationPoint,
    fmi3Float64 communicationStepSize, fmi3Boolean noSetFMUStatePriorToCurrentPoint,
    fmi3Boolean* eventHandlingNeeded, fmi3Boolean* terminateSimulation, fmi3Boolean* earlyReturn,
    fmi3Float64* lastSuccessfulTime)
{
    UNUSED(component);
    UNUSED(currentCommunicationPoint);
    UNUSED(communicationStepSize);
    UNUSED(noSetFMUStatePriorToCurrentPoint);
    UNUSED(eventHandlingNeeded);
    UNUSED(terminateSimulation);
    UNUSED(earlyReturn);
    UNUSED(lastSuccessfulTime);
    NOT_IMPLEMENTED(fmi3DoStep);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3EnterEventMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3EnterEventMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3Reset(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3Reset);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetFloat64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Float64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetFloat64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetFloat64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Float64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetFloat64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetFloat32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Float32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetFloat32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetInt8(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Int8 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetInt8);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetUInt8(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3UInt8 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetUInt8);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetInt16(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Int16 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetInt16);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetUInt16(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3UInt16 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetUInt16);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetInt32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Int32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetInt32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetUInt32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3UInt32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetUInt32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetInt64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Int64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetInt64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetUInt64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3UInt64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetUInt64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetBoolean(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Boolean values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetBoolean);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetString(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3String values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetString);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetBinary(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, size_t valueSizes[], fmi3Binary values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(valueSizes);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetBinary);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetClock(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, fmi3Clock values[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    NOT_IMPLEMENTED(fmi3GetClock);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetFloat32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Float32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetFloat32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetInt8(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Int8 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetInt8);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetUInt8(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3UInt8 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetUInt8);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetInt16(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Int16 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetInt16);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetUInt16(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3UInt16 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetUInt16);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetInt32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Int32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetInt32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetUInt32(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3UInt32 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetUInt32);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetInt64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Int64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetInt64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetUInt64(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3UInt64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetUInt64);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetBoolean(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Boolean values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetBoolean);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetString(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3String values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetString);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetBinary(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const size_t valueSizes[], const fmi3Binary values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(valueSizes);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3SetBinary);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetClock(fmi3Component* component, const fmi3ValueReference valueReferences[],
    size_t nValueReferences, const fmi3Clock values[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(values);
    NOT_IMPLEMENTED(fmi3SetClock);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetNumberOfVariableDependencies(
    fmi3Component* component, fmi3ValueReference valueReference, size_t* nDependencies)
{
    UNUSED(component);
    UNUSED(valueReference);
    UNUSED(nDependencies);
    NOT_IMPLEMENTED(fmi3GetNumberOfVariableDependencies);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetVariableDependencies(fmi3Component* component, fmi3ValueReference dependent,
    size_t elementIndicesOfDependent[], fmi3ValueReference independents[],
    size_t elementIndicesOfIndependents[], fmi3DependencyKind dependencyKinds[],
    size_t nDependencies)
{
    UNUSED(component);
    UNUSED(dependent);
    UNUSED(elementIndicesOfDependent);
    UNUSED(independents);
    UNUSED(elementIndicesOfIndependents);
    UNUSED(dependencyKinds);
    UNUSED(nDependencies);
    NOT_IMPLEMENTED(fmi3GetVariableDependencies);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetFMUState(fmi3Component* component, fmi3FMUState* FMUState)
{
    UNUSED(component);
    UNUSED(FMUState);
    NOT_IMPLEMENTED(fmi3GetFMUState);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetFMUState(fmi3Component* component, fmi3FMUState FMUState)
{
    UNUSED(component);
    UNUSED(FMUState);
    NOT_IMPLEMENTED(fmi3SetFMUState);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3FreeFMUState(fmi3Component* component, fmi3FMUState* FMUState)
{
    UNUSED(component);
    UNUSED(FMUState);
    NOT_IMPLEMENTED(fmi3FreeFMUState);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SerializedFMUStateSize(
    fmi3Component* component, fmi3FMUState FMUState, size_t* size)
{
    UNUSED(component);
    UNUSED(FMUState);
    UNUSED(size);
    NOT_IMPLEMENTED(fmi3SerializedFMUStateSize);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SerializeFMUState(
    fmi3Component* component, fmi3FMUState FMUState, fmi3Byte serializedState[], size_t size)
{
    UNUSED(component);
    UNUSED(FMUState);
    UNUSED(serializedState);
    UNUSED(size);
    NOT_IMPLEMENTED(fmi3SerializeFMUState);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3DeserializeFMUState(
    fmi3Component* component, const fmi3Byte serializedState[], size_t size, fmi3FMUState* FMUState)
{
    UNUSED(component);
    UNUSED(serializedState);
    UNUSED(size);
    UNUSED(FMUState);
    NOT_IMPLEMENTED(fmi3DeserializeFMUState);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetDirectionalDerivative(fmi3Component* component,
    const fmi3ValueReference unknowns[], size_t nUnknowns, const fmi3ValueReference knowns[],
    size_t nKnowns, const fmi3Float64 seed[], size_t nSeed, fmi3Float64 sensitivity[],
    size_t nSensitivity)
{
    UNUSED(component);
    UNUSED(unknowns);
    UNUSED(nUnknowns);
    UNUSED(knowns);
    UNUSED(nKnowns);
    UNUSED(seed);
    UNUSED(nSeed);
    UNUSED(sensitivity);
    UNUSED(nSensitivity);
    NOT_IMPLEMENTED(fmi3GetDirectionalDerivative);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetAdjointDerivative(fmi3Component* component, const fmi3ValueReference unknowns[],
    size_t nUnknowns, const fmi3ValueReference knowns[], size_t nKnowns, const fmi3Float64 seed[],
    size_t nSeed, fmi3Float64 sensitivity[], size_t nSensitivity)
{
    UNUSED(component);
    UNUSED(unknowns);
    UNUSED(nUnknowns);
    UNUSED(knowns);
    UNUSED(nKnowns);
    UNUSED(seed);
    UNUSED(nSeed);
    UNUSED(sensitivity);
    UNUSED(nSensitivity);
    NOT_IMPLEMENTED(fmi3GetAdjointDerivative);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3EnterConfigurationMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3EnterConfigurationMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3ExitConfigurationMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3ExitConfigurationMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetIntervalDecimal(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Float64 intervals[],
    fmi3IntervalQualifier qualifiers[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(intervals);
    UNUSED(qualifiers);
    NOT_IMPLEMENTED(fmi3GetIntervalDecimal);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetIntervalFraction(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    fmi3UInt64 intervalCounters[], fmi3UInt64 resolutions[], fmi3IntervalQualifier qualifiers[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(intervalCounters);
    UNUSED(resolutions);
    UNUSED(qualifiers);
    NOT_IMPLEMENTED(fmi3GetIntervalFraction);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetShiftDecimal(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3Float64 shifts[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(shifts);
    NOT_IMPLEMENTED(fmi3GetShiftDecimal);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetShiftFraction(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences, fmi3UInt64 shiftCounters[],
    fmi3UInt64 resolutions[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(shiftCounters);
    UNUSED(resolutions);
    NOT_IMPLEMENTED(fmi3GetShiftFraction);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetIntervalDecimal(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    const fmi3Float64 intervals[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(intervals);
    NOT_IMPLEMENTED(fmi3SetIntervalDecimal);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetIntervalFraction(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    const fmi3UInt64 intervalCounters[], const fmi3UInt64 resolutions[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(intervalCounters);
    UNUSED(resolutions);
    NOT_IMPLEMENTED(fmi3SetIntervalFraction);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetShiftDecimal(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Float64 shifts[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(shifts);
    NOT_IMPLEMENTED(STDCALL placeholder_fmi3SetShiftDecimal);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetShiftFraction(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences,
    const fmi3UInt64 counters[], const fmi3UInt64 resolutions[])
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(counters);
    UNUSED(resolutions);
    NOT_IMPLEMENTED(STDCALL placeholder_fmi3SetShiftFraction);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3EvaluateDiscreteStates(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3EvaluateDiscreteStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3UpdateDiscreteStates(fmi3Component* component,
    fmi3Boolean* discreteStatesNeedUpdate, fmi3Boolean* terminateSimulation,
    fmi3Boolean* nominalsOfContinuousStatesChanged, fmi3Boolean* valuesOfContinuousStatesChanged,
    fmi3Boolean* nextEventTimeDefined, fmi3Float64* nextEventTime)
{
    UNUSED(component);
    UNUSED(discreteStatesNeedUpdate);
    UNUSED(terminateSimulation);
    UNUSED(nominalsOfContinuousStatesChanged);
    UNUSED(valuesOfContinuousStatesChanged);
    UNUSED(nextEventTimeDefined);
    UNUSED(nextEventTime);
    NOT_IMPLEMENTED(fmi3UpdateDiscreteStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3EnterContinuousTimeMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3EnterContinuousTimeMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3CompletedIntegratorStep(fmi3Component* component,
    fmi3Boolean noSetFMUStatePriorToCurrentPoint, fmi3Boolean* enterEventMode,
    fmi3Boolean* terminateSimulation)
{
    UNUSED(component);
    UNUSED(noSetFMUStatePriorToCurrentPoint);
    UNUSED(enterEventMode);
    UNUSED(terminateSimulation);
    NOT_IMPLEMENTED(fmi3CompletedIntegratorStep);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetTime(fmi3Component* component, fmi3Float64 time)
{
    UNUSED(component);
    UNUSED(time);
    NOT_IMPLEMENTED(fmi3SetTime);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3SetContinuousStates(
    fmi3Component* component, const fmi3Float64 continuousStates[], size_t nContinuousStates)
{
    UNUSED(component);
    UNUSED(continuousStates);
    UNUSED(nContinuousStates);
    NOT_IMPLEMENTED(fmi3SetContinuousStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetContinuousStateDerivatives(
    fmi3Component* component, fmi3Float64 derivatives[], size_t nContinuousStates)
{
    UNUSED(component);
    UNUSED(derivatives);
    UNUSED(nContinuousStates);
    NOT_IMPLEMENTED(fmi3GetContinuousStateDerivatives);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetEventIndicators(
    fmi3Component* component, fmi3Float64 eventIndicators[], size_t nEventIndicators)
{
    UNUSED(component);
    UNUSED(eventIndicators);
    UNUSED(nEventIndicators);
    NOT_IMPLEMENTED(fmi3GetEventIndicators);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetContinuousStates(
    fmi3Component* component, fmi3Float64 continuousStates[], size_t nContinuousStates)
{
    UNUSED(component);
    UNUSED(continuousStates);
    UNUSED(nContinuousStates);
    NOT_IMPLEMENTED(fmi3GetContinuousStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetNominalsOfContinuousStates(
    fmi3Component* component, fmi3Float64 nominals[], size_t nContinuousStates)
{
    UNUSED(component);
    UNUSED(nominals);
    UNUSED(nContinuousStates);
    NOT_IMPLEMENTED(fmi3GetNominalsOfContinuousStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetNumberOfEventIndicators(fmi3Component* component, size_t* nEventIndicators)
{
    UNUSED(component);
    UNUSED(nEventIndicators);
    NOT_IMPLEMENTED(fmi3GetNumberOfEventIndicators);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetNumberOfContinuousStates(fmi3Component* component, size_t* nContinuousStates)
{
    UNUSED(component);
    UNUSED(nContinuousStates);
    NOT_IMPLEMENTED(fmi3GetNumberOfContinuousStates);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3EnterStepMode(fmi3Component* component)
{
    UNUSED(component);
    NOT_IMPLEMENTED(fmi3EnterStepMode);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3GetOutputDerivatives(fmi3Component* component,
    const fmi3ValueReference valueReferences[], size_t nValueReferences, const fmi3Int32 orders[],
    fmi3Float64 values[], size_t nValues)
{
    UNUSED(component);
    UNUSED(valueReferences);
    UNUSED(nValueReferences);
    UNUSED(orders);
    UNUSED(values);
    UNUSED(nValues);
    NOT_IMPLEMENTED(fmi3GetOutputDerivatives);
    return fmi3Error;
}

fmi3Status STDCALL
placeholder_fmi3ActivateModelPartition(
    fmi3Component* component, fmi3ValueReference clockReference, fmi3Float64 activationTime)
{
    UNUSED(component);
    UNUSED(clockReference);
    UNUSED(activationTime);
    NOT_IMPLEMENTED(fmi3ActivateModelPartition);
    return fmi3Error;
}

#endif // FMI4C_PLACEHOLDERS_H
