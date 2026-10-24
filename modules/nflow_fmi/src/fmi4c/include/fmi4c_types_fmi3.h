#ifndef FMI_TYPES_FMI3_H
#define FMI_TYPES_FMI3_H

#include <stdbool.h>
#include <stdint.h>

// Defines
#define fmi3True true
#define fmi3False false

// Forward declarations
struct fmuHandle;
typedef struct fmuHandle fmuHandle;

// Types
typedef void* fmi3Component;
struct fmi3InstanceHandle
{
    fmi3Component component;
    fmuHandle* fmu;
};
typedef struct fmi3InstanceHandle fmi3InstanceHandle;
typedef void* fmi3InstanceEnvironment;
typedef void* fmi3FMUState;
typedef uint32_t fmi3ValueReference;
typedef float fmi3Float32;
typedef double fmi3Float64;
typedef int8_t fmi3Int8;
typedef uint8_t fmi3UInt8;
typedef int16_t fmi3Int16;
typedef uint16_t fmi3UInt16;
typedef int32_t fmi3Int32;
typedef uint32_t fmi3UInt32;
typedef int64_t fmi3Int64;
typedef uint64_t fmi3UInt64;
typedef bool fmi3Boolean;
typedef char fmi3Char;
typedef const fmi3Char* fmi3String;
typedef uint8_t fmi3Byte;
typedef fmi3Byte* fmi3Binary;
typedef bool fmi3Clock;

// Enums
typedef enum
{
    fmi3OK,
    fmi3Warning,
    fmi3Discard,
    fmi3Error,
    fmi3Fatal,
} fmi3Status;
typedef enum
{
    fmi3ModelExchange,
    fmi3CoSimulation,
    fmi3ScheduledExecution
} fmi3Type;
typedef enum
{
    fmi3Independent,
    fmi3Constant,
    fmi3Fixed,
    fmi3Tunable,
    fmi3Discrete,
    fmi3Dependent
} fmi3DependencyKind;
typedef enum
{
    fmi3IntervalNotYetKnown,
    fmi3IntervalUnchanged,
    fmi3IntervalChanged
} fmi3IntervalQualifier;
typedef enum
{
    fmi3CausalityInput,
    fmi3CausalityOutput,
    fmi3CausalityParameter,
    fmi3CausalityCalculatedParameter,
    fmi3CausalityLocal,
    fmi3CausalityIndependent,
    fmi3CausalityStructuralParameter
} fmi3Causality;
typedef enum
{
    fmi3VariabilityFixed,
    fmi3VariabilityTunable,
    fmi3VariabilityConstant,
    fmi3VariabilityDiscrete,
    fmi3VariabilityContinuous
} fmi3Variability;
typedef enum
{
    fmi3IntervalVariabilityConstant,
    fmi3IntervalVariabilityFixed,
    fmi3IntervalVariabilityCalculated,
    fmi3IntervalVariabilityTunable,
    fmi3IntervalVariabilityChanging,
    fmi3IntervalVariabilityCountdown,
    fmi3IntervalVariabilityTriggered
} fmi3IntervalVariability;

typedef enum
{
    fmi3InitialExact,
    fmi3InitialApprox,
    fmi3InitialCalculated,
    fmi3InitialUndefined
} fmi3Initial;
typedef enum
{
    fmi3DataTypeFloat64,
    fmi3DataTypeFloat32,
    fmi3DataTypeInt64,
    fmi3DataTypeInt32,
    fmi3DataTypeInt16,
    fmi3DataTypeInt8,
    fmi3DataTypeUInt64,
    fmi3DataTypeUInt32,
    fmi3DataTypeUInt16,
    fmi3DataTypeUInt8,
    fmi3DataTypeBoolean,
    fmi3DataTypeString,
    fmi3DataTypeBinary,
    fmi3DataTypeEnumeration,
    fmi3DataTypeClock
} fmi3DataType;

#endif // FMI_TYPES_FMI3_H
