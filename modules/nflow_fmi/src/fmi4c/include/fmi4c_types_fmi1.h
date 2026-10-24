#ifndef FMIC_TYPES_FMI1_H
#define FMIC_TYPES_FMI1_H

// Defines
#define fmi1True 1
#define fmi1False 0

// Forward declarations
struct fmuHandle;
typedef struct fmuHandle fmuHandle;

// Types
typedef void* fmi1Component;
struct fmi1InstanceHandle
{
    fmi1Component component;
    fmuHandle* fmu;
};
typedef struct fmi1InstanceHandle fmi1InstanceHandle;

typedef unsigned int fmi1ValueReference;
typedef double fmi1Real;
typedef int fmi1Integer;
typedef char fmi1Boolean;
typedef const char* fmi1String;

// Enums
typedef enum
{
    fmi1OK,
    fmi1Warning,
    fmi1Discard,
    fmi1Error,
    fmi1Fatal,
    fmi1Pending
} fmi1Status;
typedef enum
{
    fmi1DoStepStatus,
    fmi1PendingStatus,
    fmi1LastSuccessfulTime
} fmi1StatusKind;
typedef enum
{
    fmi1ModelExchange,
    fmi1CoSimulationStandAlone,
    fmi1CoSimulationTool
} fmi1Type;
typedef enum
{
    fmi1CausalityInput,
    fmi1CausalityOutput,
    fmi1CausalityInternal,
    fmi1CausalityNone
} fmi1Causality;
typedef enum
{
    fmi1VariabilityConstant,
    fmi1VariabilityParameter,
    fmi1VariabilityDiscrete,
    fmi1VariabilityContinuous
} fmi1Variability;
typedef enum
{
    fmi1DataTypeReal,
    fmi1DataTypeInteger,
    fmi1DataTypeBoolean,
    fmi1DataTypeString,
    fmi1DataTypeEnumeration
} fmi1DataType;
typedef enum
{
    fmi1AliasNoAlias,
    fmi1AliasAlias,
    fmi1AliasNegatedAlias
} fmi1Alias;

#endif // FMIC_TYPES_FMI1_H
