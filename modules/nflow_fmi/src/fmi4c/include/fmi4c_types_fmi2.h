#ifndef FMIC_FMI2_TYPES_H
#define FMIC_FMI2_TYPES_H

// Defines
#define fmi2True 1
#define fmi2False 0

// Forward declarations
struct fmuHandle;
typedef struct fmuHandle fmuHandle;

// Types
typedef void* fmi2Component;
struct fmi2InstanceHandle
{
    fmi2Component component;
    fmuHandle* fmu;
};
typedef struct fmi2InstanceHandle fmi2InstanceHandle;
typedef void* fmi2ComponentEnvironment;
typedef void* fmi2FMUstate;
typedef unsigned int fmi2ValueReference;
typedef double fmi2Real;
typedef int fmi2Integer;
typedef int fmi2Boolean;
typedef char fmi2Char;
typedef const fmi2Char* fmi2String;
typedef char fmi2Byte;

// Enums
typedef enum
{
    fmi2OK,
    fmi2Warning,
    fmi2Discard,
    fmi2Error,
    fmi2Fatal,
    fmi2Pending
} fmi2Status;
typedef enum
{
    fmi2ModelExchange,
    fmi2CoSimulation
} fmi2Type;
typedef enum
{
    fmi2DoStepStatus,
    fmi2PendingStatus,
    fmi2LastSuccessfulTime,
    fmi2Terminated
} fmi2StatusKind;
typedef enum
{
    fmi2CausalityInput,
    fmi2CausalityOutput,
    fmi2CausalityParameter,
    fmi2CausalityCalculatedParameter,
    fmi2CausalityLocal,
    fmi2CausalityIndependent
} fmi2Causality;
typedef enum
{
    fmi2VariabilityFixed,
    fmi2VariabilityTunable,
    fmi2VariabilityConstant,
    fmi2VariabilityDiscrete,
    fmi2VariabilityContinuous
} fmi2Variability;
typedef enum
{
    fmi2InitialExact,
    fmi2InitialApprox,
    fmi2InitialCalculated,
    fmi2InitialUnknown
} fmi2Initial;
typedef enum
{
    fmi2DataTypeReal,
    fmi2DataTypeInteger,
    fmi2DataTypeBoolean,
    fmi2DataTypeString,
    fmi2DataTypeEnumeration
} fmi2DataType;
typedef enum
{
    fmi2Dependent,
    fmi2Constant,
    fmi2Fixed,
    fmi2Tunable,
    fmi2Discrete
} fmi2DependencyKind;

#endif // FMIC_FMI2_TYPES_H
