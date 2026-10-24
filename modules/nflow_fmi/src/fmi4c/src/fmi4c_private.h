#ifndef FMIC_PRIVATE_H
#define FMIC_PRIVATE_H

#if defined(_MSC_VER)
#pragma warning(disable : 4101 4244 4267)
#endif

#include "fmi4c_types.h"
#include "fmi4c_types_fmi1.h"
#include "fmi4c_types_fmi2.h"
#include "fmi4c_types_fmi3.h"
#include "fmi4c_functions_fmi1.h"
#include "fmi4c_functions_fmi2.h"
#include "fmi4c_functions_fmi3.h"

#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef DEBUG
#define TRACEFUNC printf("In function: %s\n", __func__);
#else
#define TRACEFUNC
#endif

void
fmi4c_printMessage(const char* msg);

typedef struct
{
    fmi1DataType datatype;
    const char* name;
    const char* description;
    const char* quantity;
    const char* unit;
    const char* displayUnit;
    bool relativeQuantity;
    double min;
    double max;
    double nominal;
    int64_t valueReference;
    bool hasStartValue;
    fmi1Real startReal;
    fmi1Integer startInteger;
    fmi1Boolean startBoolean;
    fmi1String startString;
    fmi1Causality causality;
    fmi1Variability variability;
    fmi1Alias alias;
    bool fixed;
} fmi1VariableHandle;

typedef struct
{
    const char* displayUnit;
    double gain;
    double offset;
} fmi1DisplayUnitHandle;

typedef struct
{
    const char* unit;
    fmi1DisplayUnitHandle* displayUnits;
    size_t numberOfDisplayUnits;
} fmi1BaseUnitHandle;

typedef struct
{
    fmi2DataType datatype;
    const char* name;
    const char* description;
    const char* quantity;
    const char* unit;
    const char* displayUnit;
    bool relativeQuantity;
    fmi2Real min;
    fmi2Real max;
    fmi2Real nominal;
    bool unbounded;
    bool hasStartValue;
    fmi2Real startReal;
    fmi2Integer startInteger;
    fmi2Boolean startBoolean;
    fmi2String startString;
    fmi2Integer startEnumeration;
    int64_t valueReference;
    fmi2Causality causality;
    fmi2Variability variability;
    fmi2Initial initial;
    bool canHandleMultipleSetPerTimeInstant;
    fmi2ValueReference derivative;
} fmi2VariableHandle;

typedef struct
{
    int kg;
    int m;
    int s;
    int A;
    int K;
    int mol;
    int cd;
    int rad;
    double factor;
    double offset;
} fmi2BaseUnitHandle;

typedef struct
{
    const char* name;
    double factor;
    double offset;
} fmi2DisplayUnitHandle;

typedef struct
{
    const char* name;
    fmi2BaseUnitHandle* baseUnit;
    fmi2DisplayUnitHandle* displayUnits;
    size_t numberOfDisplayUnits;
} fmi2UnitHandle;

typedef struct
{
    fmi3DataType datatype;
    const char* name;
    const char* description;
    const char* quantity;
    const char* unit;
    const char* displayUnit;
    bool relativeQuantity;
    bool unbounded;
    double min;
    double max;
    double nominal;
    bool hasStartValue;
    fmi3Float64 startFloat64;
    fmi3Float32 startFloat32;
    fmi3Int64 startInt64;
    fmi3Int32 startInt32;
    fmi3Int16 startInt16;
    fmi3Int8 startInt8;
    fmi3UInt64 startUInt64;
    fmi3UInt32 startUInt32;
    fmi3UInt16 startUInt16;
    fmi3UInt8 startUInt8;
    fmi3Boolean startBoolean;
    fmi3Binary startBinary;
    fmi3Clock startClock;
    fmi3String startString;
    fmi3Int64 startEnumeration;
    unsigned int derivative;
    bool reInit;
    int64_t valueReference;
    fmi3Causality causality;
    fmi3Variability variability;
    fmi3Initial initial;
    bool canHandleMultipleSetPerTimeInstant;
    bool intermediateUpdate;
    unsigned int previous;
    const char* declaredType;
    const char* mimeType;
    int maxSize;
    bool canBeDeactivated;
    int priority;
    fmi3IntervalVariability intervalVariability;
    double intervalDecimal;
    double shiftDecimal;
    bool supportsFraction;
    int64_t resolution;
    int64_t intervalCounter;
    int64_t shiftCounter;
    int numberOfClocks;
    int* clocks;

    // Array dimensions (FMI3 <Dimension> elements). Each dimension is either a
    // fixed size (hasStart) or bound to another variable's value (valueReference).
    // NFlow-local extension: fmi4c upstream parses variables but drops <Dimension>.
    int numberOfDimensions;
    bool* dimensionHasStart; // [numberOfDimensions]
    uint64_t* dimensionStart; // [numberOfDimensions] (valid when hasStart)
    uint32_t* dimensionValueRef; // [numberOfDimensions] (valid when !hasStart)

} fmi3VariableHandle;

typedef struct
{
    int kg;
    int m;
    int s;
    int A;
    int K;
    int mol;
    int cd;
    int rad;
    double factor;
    double offset;
} fmi3BaseUnit;

typedef struct
{
    const char* name;
    double factor;
    double offset;
    bool inverse;
} fmi3DisplayUnitHandle;

typedef struct
{
    const char* name;
    fmi3BaseUnit* baseUnit;
    fmi3DisplayUnitHandle* displayUnits;
    size_t numberOfDisplayUnits;
} fmi3UnitHandle;

typedef struct
{
    const char* modelName;
    const char* modelIdentifier;
    const char* guid;
    const char* description;
    const char* author;
    const char* version;
    const char* generationTool;
    const char* generationDateAndTime;
    const char* variableNamingConvention;
    int numberOfContinuousStates;
    int numberOfEventIndicators;

    bool canHandleVariableCommunicationStepSize;
    bool canHandleEvents;
    bool canRejectSteps;
    bool canInterpolateInputs;
    int maxOutputDerivativeOrder;
    bool canRunAsynchronuously;
    bool canSignalEvents;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canNotUseMemoryManagementFunctions;
    bool hasRealVariables;
    bool hasIntegerVariables;
    bool hasStringVariables;
    bool hasBooleanVariables;

    bool defaultStartTimeDefined;
    bool defaultStopTimeDefined;
    bool defaultToleranceDefined;

    double defaultStartTime;
    double defaultStopTime;
    double defaultTolerance;

    int numberOfBaseUnits;
    fmi1BaseUnitHandle* baseUnits;

    int numberOfVariables;
    fmi1VariableHandle* variables;
    int variablesSize;

    fmi1Type type;

    fmi1Component component;

    fmi1CallbackFunctionsCoSimulation callbacksCoSimulation;
    fmi1CallbackFunctionsModelExchange callbacksModelExchange;

    fmiGetVersion_t getVersion;
    fmiGetTypesPlatform_t getTypesPlatform;
    fmiSetDebugLogging_t setDebugLogging;
    fmiGetReal_t getReal;
    fmiGetInteger_t getInteger;
    fmiGetBoolean_t getBoolean;
    fmiGetString_t getString;
    fmiSetReal_t setReal;
    fmiSetInteger_t setInteger;
    fmiSetBoolean_t setBoolean;
    fmiSetString_t setString;
    fmiInstantiateSlave_t instantiateSlave;
    fmiInitializeSlave_t initializeSlave;
    fmiTerminateSlave_t terminateSlave;
    fmiResetSlave_t resetSlave;
    fmiFreeSlaveInstance_t freeSlaveInstance;
    fmiSetRealInputDerivatives_t setRealInputDerivatives;
    fmiGetRealOutputDerivatives_t getRealOutputDerivatives;
    fmiCancelStep_t cancelStep;
    fmiDoStep_t doStep;
    fmiGetStatus_t getStatus;
    fmiGetRealStatus_t getRealStatus;
    fmiGetIntegerStatus_t getIntegerStatus;
    fmiGetBooleanStatus_t getBooleanStatus;
    fmiGetStringStatus_t getStringStatus;
    fmiGetModelTypesPlatform_t getModelTypesPlatform;
    fmiInstantiateModel_t instantiateModel;
    fmiFreeModelInstance_t freeModelInstance;
    fmiSetTime_t setTime;
    fmiSetContinuousStates_t setContinuousStates;
    fmiCompletedIntegratorStep_t completedIntegratorStep;
    fmiInitialize_t initialize;
    fmiGetDerivatives_t getDerivatives;
    fmiGetEventIndicators_t getEventIndicators;
    fmiEventUpdate_t eventUpdate;
    fmiGetContinuousStates_t getContinuousStates;
    fmiGetNominalContinuousStates_t getNominalContinuousStates;
    fmiGetStateValueReferences_t getStateValueReferences;
    fmiTerminate_t terminate;
} fmi1_data_t;

typedef struct
{
    const char* modelIdentifier;
    bool needsExecutionTool;
    bool canHandleVariableCommunicationStepSize;
    bool canInterpolateInputs;
    int maxOutputDerivativeOrder;
    bool canRunAsynchronuously;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canNotUseMemoryManagementFunctions;
    bool canGetAndSetFMUState;
    bool canSerializeFMUState;
    bool providesDirectionalDerivative;
} fmi2DataCs_t;

typedef struct
{
    const char* modelIdentifier;
    bool needsExecutionTool;
    bool completedIntegratorStepNotNeeded;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canNotUseMemoryManagementFunctions;
    bool canGetAndSetFMUState;
    bool canSerializeFMUState;
    bool providesDirectionalDerivative;
} fmi2DataMe_t;

typedef struct
{
    int index;
    int numberOfDependencies;
    bool dependencyKindsDefined;
    int* dependencies;
    fmi2DependencyKind* dependencyKinds;
} fmi2ModelStructureHandle;

typedef struct
{
    int numberOfOutputs;
    int numberOfDerivatives;
    int numberOfInitialUnknowns;
    fmi2ModelStructureHandle* outputs;
    fmi2ModelStructureHandle* derivatives;
    fmi2ModelStructureHandle* initialUnknowns;
} fmi2ModelStructureData_t;

typedef struct
{
    const char* fmiVersion_;
    const char* modelName;
    const char* guid;
    const char* description;
    const char* author;
    const char* version;
    const char* copyright;
    const char* license;
    const char* generationTool;
    const char* generationDateAndTime;
    const char* variableNamingConvention;
    int numberOfEventIndicators;

    bool supportsCoSimulation;
    bool supportsModelExchange;

    fmi2DataCs_t cs;
    fmi2DataMe_t me;

    bool hasRealVariables;
    bool hasIntegerVariables;
    bool hasBooleanVariables;
    bool hasStringVariables;
    bool hasEnumerationVariables;

    bool defaultStartTimeDefined;
    bool defaultStopTimeDefined;
    bool defaultToleranceDefined;
    bool defaultStepSizeDefined;

    double defaultStartTime;
    double defaultStopTime;
    double defaultTolerance;
    double defaultStepSize;

    int numberOfUnits;
    fmi2UnitHandle* units;

    int numberOfContinuousStates;

    int numberOfVariables;
    fmi2VariableHandle* variables;
    int variablesSize;

    fmi2ModelStructureData_t modelStructure;

    fmi2GetTypesPlatform_t getTypesPlatform;
    fmi2GetVersion_t getVersion;
    fmi2SetDebugLogging_t setDebugLogging;
    fmi2Instantiate_t instantiate;
    fmi2FreeInstance_t freeInstance;
    fmi2SetupExperiment_t setupExperiment;
    fmi2EnterInitializationMode_t enterInitializationMode;
    fmi2ExitInitializationMode_t exitInitializationMode;
    fmi2Terminate_t terminate;
    fmi2Reset_t reset;
    fmi2GetReal_t getReal;
    fmi2GetInteger_t getInteger;
    fmi2GetBoolean_t getBoolean;
    fmi2GetString_t getString;
    fmi2SetReal_t setReal;
    fmi2SetInteger_t setInteger;
    fmi2SetBoolean_t setBoolean;
    fmi2SetString_t setString;
    fmi2GetFMUstate_t getFMUstate;
    fmi2SetFMUstate_t setFMUstate;
    fmi2FreeFMUstate_t freeFMUstate;
    fmi2SerializedFMUstateSize_t serializedFMUstateSize;
    fmi2SerializeFMUstate_t serializeFMUstate;
    fmi2DeSerializeFMUstate_t deSerializeFMUstate;
    fmi2GetDirectionalDerivative_t getDirectionalDerivative;
    fmi2EnterEventMode_t enterEventMode;
    fmi2NewDiscreteStates_t newDiscreteStates;
    fmi2EnterContinuousTimeMode_t enterContinuousTimeMode;
    fmi2CompletedIntegratorStep_t completedIntegratorStep;
    fmi2SetTime_t setTime;
    fmi2SetContinuousStates_t setContinuousStates;
    fmi2GetDerivatives_t getDerivatives;
    fmi2GetEventIndicators_t getEventIndicators;
    fmi2GetContinuousStates_t getContinuousStates;
    fmi2GetNominalsOfContinuousStates_t getNominalsOfContinuousStates;
    fmi2SetRealInputDerivatives_t setRealInputDerivatives;
    fmi2GetRealOutputDerivatives_t getRealOutputDerivatives;
    fmi2DoStep_t doStep;
    fmi2CancelStep_t cancelStep;
    fmi2GetStatus_t getStatus;
    fmi2GetRealStatus_t getRealStatus;
    fmi2GetIntegerStatus_t getIntegerStatus;
    fmi2GetBooleanStatus_t getBooleanStatus;
    fmi2GetStringStatus_t getStringStatus;

    fmi2CallbackFunctions callbacks;
} fmi2Data_t;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    const char* unit;
    const char* displayUnit;
    bool relativeQuantity;
    bool unbounded;
    double min;
    double max;
    double nominal;
} fmi3Float64Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    const char* unit;
    const char* displayUnit;
    bool relativeQuantity;
    bool unbounded;
    float min;
    float max;
    float nominal;
} fmi3Float32Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    int64_t min;
    int64_t max;
} fmi3Int64Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    int32_t min;
    int32_t max;
} fmi3Int32Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    int16_t min;
    int16_t max;
} fmi3Int16Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    int8_t min;
    int8_t max;
} fmi3Int8Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    uint64_t min;
    uint64_t max;
} fmi3UInt64Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    uint32_t min;
    uint32_t max;
} fmi3UInt32Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    uint16_t min;
    uint16_t max;
} fmi3UInt16Type;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    uint8_t min;
    uint8_t max;
} fmi3UInt8Type;

typedef struct
{
    const char* name;
    const char* description;
} fmi3BooleanType;

typedef struct
{
    const char* name;
    const char* description;
} fmi3StringType;

typedef struct
{
    const char* name;
    const char* description;
    const char* mimeType;
    uint32_t maxSize;
} fmi3BinaryType;

typedef struct
{
    const char* name;
    int64_t value;
    const char* description;
} fmi3EnumerationItem;

typedef struct
{
    const char* name;
    const char* description;
    const char* quantity;
    int64_t min;
    int64_t max;
    int numberOfItems;
    fmi3EnumerationItem* items;
} fmi3EnumerationType;

typedef struct
{
    const char* name;
    const char* description;
    bool canBeDeactivated;
    uint32_t priority;
    fmi3IntervalVariability intervalVariability;
    float intervalDecimal;
    float shiftDecimal;
    bool supportsFraction;
    uint64_t resolution;
    uint64_t intervalCounter;
    uint64_t shiftCounter;
} fmi3ClockType;

typedef struct
{
    const char* name;
    const char* description;
} fmi3LogCategory;

typedef struct
{
    fmi3ValueReference valueReference;
    int numberOfDependencies;
    bool dependencyKindsDefined;
    fmi3ValueReference* dependencies;
    fmi3DependencyKind* dependencyKinds;
} fmi3ModelStructureHandle;

typedef struct
{
    const char* modelIdentifier;
    bool needsExecutionTool;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canGetAndSetFMUState;
    bool canSerializeFMUState;
    bool providesDirectionalDerivative;
    bool providesAdjointDerivatives;
    bool providesPerElementDependencies;
    int maxOutputDerivativeOrder;
    bool providesIntermediateUpdate;
    bool mightReturnEarlyFromDoStep;
    bool providesEvaluateDiscreteStates;
    int recommendedIntermediateInputSmoothness;
    bool canHandleVariableCommunicationStepSize;
    bool canReturnEarlyAfterIntermediateUpdate;
    double fixedInternalStepSize;
    bool hasEventMode;
} fmi3DataCs_t;

typedef struct
{
    const char* modelIdentifier;
    bool needsExecutionTool;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canGetAndSetFMUState;
    bool canSerializeFMUState;
    bool providesDirectionalDerivative;
    bool providesAdjointDerivatives;
    bool providesPerElementDependencies;
    bool needsCompletedIntegratorStep;
    bool providesEvaluateDiscreteStates;
} fmi3DataMe_t;

typedef struct
{
    const char* modelIdentifier;
    bool needsExecutionTool;
    bool canBeInstantiatedOnlyOncePerProcess;
    bool canGetAndSetFMUState;
    bool canSerializeFMUState;
    bool providesDirectionalDerivative;
    bool providesAdjointDerivatives;
    bool providesPerElementDependencies;
} fmi3DataSe_t;

typedef struct
{
    int numberOfOutputs;
    int numberOfContinuousStateDerivatives;
    int numberOfClockedStates;
    int numberOfInitialUnknowns;
    int numberOfEventIndicators;
    fmi3ModelStructureHandle* outputs;
    fmi3ModelStructureHandle* continuousStateDerivatives;
    fmi3ModelStructureHandle* clockedStates;
    fmi3ModelStructureHandle* initialUnknowns;
    fmi3ModelStructureHandle* eventIndicators;
} fmi3ModelStructureData_t;

typedef struct
{
    bool supportsModelExchange;
    bool supportsCoSimulation;
    bool supportsScheduledExecution;

    const char* modelName;
    const char* instantiationToken;
    const char* description;
    const char* author;
    const char* version;
    const char* copyright;
    const char* license;
    const char* generationTool;
    const char* generationDateAndTime;
    const char* variableNamingConvention;

    fmi3DataCs_t cs;
    fmi3DataMe_t me;
    fmi3DataSe_t se;

    bool hasFloat64Variables;
    bool hasFloat32Variables;
    bool hasInt64Variables;
    bool hasInt32Variables;
    bool hasInt16Variables;
    bool hasInt8Variables;
    bool hasUInt64Variables;
    bool hasUInt32Variables;
    bool hasUInt16Variables;
    bool hasUInt8Variables;
    bool hasBooleanVariables;
    bool hasStringVariables;
    bool hasBinaryVariables;
    bool hasEnumerationVariables;
    bool hasClockVariables;
    bool hasStructuralParameters;

    bool defaultStartTimeDefined;
    bool defaultStopTimeDefined;
    bool defaultToleranceDefined;
    bool defaultStepSizeDefined;

    double defaultStartTime;
    double defaultStopTime;
    double defaultTolerance;
    double defaultStepSize;

    int numberOfVariables;
    fmi3VariableHandle* variables;
    int variablesSize;

    fmi3InstanceHandle fmi3Instance;
    fmi3GetVersion_t getVersion;
    fmi3SetDebugLogging_t setDebugLogging;
    fmi3InstantiateModelExchange_t instantiateModelExchange;
    fmi3InstantiateCoSimulation_t instantiateCoSimulation;
    fmi3InstantiateScheduledExecution_t instantiateScheduledExecution;
    fmi3FreeInstance_t freeInstance;
    fmi3EnterInitializationMode_t enterInitializationMode;
    fmi3ExitInitializationMode_t exitInitializationMode;
    fmi3Terminate_t terminate;
    fmi3SetFloat64_t setFloat64;
    fmi3GetFloat64_t getFloat64;
    fmi3DoStep_t doStep;
    fmi3EnterEventMode_t enterEventMode;
    fmi3Reset_t reset;
    fmi3GetFloat32_t getFloat32;
    fmi3GetInt8_t getInt8;
    fmi3GetUInt8_t getUInt8;
    fmi3GetInt16_t getInt16;
    fmi3GetUInt16_t getUInt16;
    fmi3GetInt32_t getInt32;
    fmi3GetUInt32_t getUInt32;
    fmi3GetInt64_t getInt64;
    fmi3GetUInt64_t getUInt64;
    fmi3GetBoolean_t getBoolean;
    fmi3GetString_t getString;
    fmi3GetBinary_t getBinary;
    fmi3GetClock_t getClock;
    fmi3SetFloat32_t setFloat32;
    fmi3SetInt8_t setInt8;
    fmi3SetUInt8_t setUInt8;
    fmi3SetInt16_t setInt16;
    fmi3SetUInt16_t setUInt16;
    fmi3SetInt32_t setInt32;
    fmi3SetUInt32_t setUInt32;
    fmi3SetInt64_t setInt64;
    fmi3SetUInt64_t setUInt64;
    fmi3SetBoolean_t setBoolean;
    fmi3SetString_t setString;
    fmi3SetBinary_t setBinary;
    fmi3SetClock_t setClock;
    fmi3GetNumberOfVariableDependencies_t getNumberOfVariableDependencies;
    fmi3GetVariableDependencies_t getVariableDependencies;
    fmi3GetFMUState_t getFMUState;
    fmi3SetFMUState_t setFMUState;
    fmi3FreeFMUState_t freeFMUState;
    fmi3SerializedFMUStateSize_t serializedFMUStateSize;
    fmi3SerializeFMUState_t serializeFMUState;
    fmi3DeserializeFMUState_t deserializeFMUState;
    fmi3GetDirectionalDerivative_t getDirectionalDerivative;
    fmi3GetAdjointDerivative_t getAdjointDerivative;
    fmi3EnterConfigurationMode_t enterConfigurationMode;
    fmi3ExitConfigurationMode_t exitConfigurationMode;
    fmi3GetIntervalDecimal_t getIntervalDecimal;
    fmi3GetIntervalFraction_t getIntervalFraction;
    fmi3GetShiftDecimal_t getShiftDecimal;
    fmi3GetShiftFraction_t getShiftFraction;
    fmi3SetIntervalDecimal_t setIntervalDecimal;
    fmi3SetIntervalFraction_t setIntervalFraction;
    fmi3SetShiftDecimal_t setShiftDecimal;
    fmi3SetShiftFraction_t setShiftFraction;
    fmi3EvaluateDiscreteStates_t evaluateDiscreteStates;
    fmi3UpdateDiscreteStates_t updateDiscreteStates;
    fmi3EnterContinuousTimeMode_t enterContinuousTimeMode;
    fmi3CompletedIntegratorStep_t completedIntegratorStep;
    fmi3SetTime_t setTime;
    fmi3SetContinuousStates_t setContinuousStates;
    fmi3GetContinuousStateDerivatives_t getContinuousStateDerivatives;
    fmi3GetEventIndicators_t getEventIndicators;
    fmi3GetContinuousStates_t getContinuousStates;
    fmi3GetNominalsOfContinuousStates_t getNominalsOfContinuousStates;
    fmi3GetNumberOfEventIndicators_t getNumberOfEventIndicators;
    fmi3GetNumberOfContinuousStates_t getNumberOfContinuousStates;
    fmi3EnterStepMode_t enterStepMode;
    fmi3GetOutputDerivatives_t getOutputDerivatives;
    fmi3ActivateModelPartition_t activateModelPartition;

    size_t numberOfUnits;
    fmi3UnitHandle* units;

    size_t numberOfFloat64Types;
    size_t numberOfFloat32Types;
    size_t numberOfInt64Types;
    size_t numberOfInt32Types;
    size_t numberOfInt16Types;
    size_t numberOfInt8Types;
    size_t numberOfUInt64Types;
    size_t numberOfUInt32Types;
    size_t numberOfUInt16Types;
    size_t numberOfUInt8Types;
    size_t numberOfBooleanTypes;
    size_t numberOfStringTypes;
    size_t numberOfBinaryTypes;
    size_t numberOfEnumerationTypes;
    size_t numberOfClockTypes;
    fmi3Float64Type* float64Types;
    fmi3Float32Type* float32Types;
    fmi3Int64Type* int64Types;
    fmi3Int32Type* int32Types;
    fmi3Int16Type* int16Types;
    fmi3Int8Type* int8Types;
    fmi3UInt64Type* uint64Types;
    fmi3UInt32Type* uint32Types;
    fmi3UInt16Type* uint16Types;
    fmi3UInt8Type* uint8Types;
    fmi3BooleanType* booleanTypes;
    fmi3StringType* stringTypes;
    fmi3BinaryType* binaryTypes;
    fmi3EnumerationType* enumTypes;
    fmi3ClockType* clockTypes;

    int numberOfLogCategories;
    fmi3LogCategory* logCategories;

    fmi3ModelStructureData_t modelStructure;
} fmi3Data_t;

struct fmuHandle
{
    fmiVersion_t version;
    bool unzippedLocationIsTemporary;
    const char* unzippedLocation;
    const char* resourcesLocation;
    const char* instanceName;
#ifdef _WIN32
    HINSTANCE dll;
#else
    void* dll;
#endif

    fmi1_data_t fmi1;
    fmi2Data_t fmi2;
    fmi3Data_t fmi3;

    void** allocatedPointers;
    int numAllocatedPointers;
};
typedef struct fmuHandle fmuHandle;

bool
parseModelDescriptionFmi1(fmuHandle* fmuFile);
bool
parseModelDescriptionFmi2(fmuHandle* fmuFile);
bool
parseModelDescriptionFmi3(fmuHandle* fmuFile);

bool
loadFunctionsFmi1(fmuHandle* contents);
bool
loadFunctionsFmi2(fmuHandle* contents, fmi2Type fmuType);
bool
loadFunctionsFmi3(fmuHandle* contents, fmi3Type fmuType);

#endif // FMIC_PRIVATE_H
