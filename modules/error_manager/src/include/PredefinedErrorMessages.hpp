//=============================================================================
// Copyright (c) 2016-present Allan CORNET (Nelson)
//=============================================================================
// This file is part of Nelson.
//=============================================================================
// LICENCE_BLOCK_BEGIN
// SPDX-License-Identifier: LGPL-3.0-or-later
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "i18n.hpp"
//=============================================================================
// SIZE & DIMENSION ERRORS
//=============================================================================
#define ERROR_SIZE_SCALAR_EXPECTED _W("A scalar expected.")
// Dimensions module messages
#define ERROR_ILLEGAL_ARGUMENT_TO_DIMENSIONS_CONSTRUCTOR                                           \
    _W("Illegal argument to Dimensions constructor")
#define ERROR_INVALID_DIMENSION_POSITION _W("Invalid dimension position.")
#define ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED                                              \
    _W("Wrong size for input arguments: 2D matrix expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_OR_ROW_VECTOR_EXPECTED                                  \
    _W("Wrong size for #{} argument. a scalar or a row vector expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_A_B_VECTOR_EXPECTED                                            \
    _W("Wrong size for #{} argument. [a, b] expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_ROW_VECTOR_EXPECTED                                            \
    _W("Wrong size for #{} argument. row vector expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_2D_MATRIX_EXPECTED                                             \
    _W("Wrong size for argument #{}: 2D matrix expected.")
#define ERROR_WRONG_ARGUMENT_X_INVALID_VECTOR_SIZE _W("Wrong value for #{} :Invalid size.")
//=============================================================================
// TYPE ERRORS
//=============================================================================
#define ERROR_TYPE_LOGICAL_EXPECTED _W("logical expected.")
// Dynamic link additions
#define ERROR_INVALID_TYPE _W("Invalid type.")
#define ERROR_ONLY_NUMERICPTR_CAN_BE_RESHAPED _W("Only numericPtr can be reshaped.")
#define ERROR_TYPE_CLASS_EXPECTED _W("class expected.")
#define ERROR_WRONG_ARGUMENTS_TYPE _W("Wrong types for inputs arguments.")
#define ERROR_TYPE_CELL_OF_STRINGS _W("cell of strings")
#define ERROR_TYPE_X_EXPECTED _W("{} expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE _W("Wrong type: #{} argument.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED                             \
    _W("Wrong type for argument #{}: string or function handle expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_DOUBLE_EXPECTED                                      \
    _W("Wrong type for argument #{}: string or double expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_CELL_EXPECTED                                        \
    _W("Wrong type for argument #{}: string or cell expected.")
#define ERROR_MATRIX_INDEX_OUT_OF_RANGE _W("Matrix index is out of range.")
#define ERROR_ILLEGAL_ZERO_OR_NEGATIVE_INDEX _W("Illegal zero or negative index")
#define ERROR_CANNOT_CONVERT_STRING_DATA_TYPES_TO_INDICES                                          \
    _W("Cannot convert string data types to indices.")
#define ERROR_TOO_BIG_INDEX_ENCOUNTERED _W("Too big index encountered.")
#define ERROR_CANNOT_CONVERT_HANDLE_ARRAYS_TO_INDICES _W("Cannot convert handle arrays to indices.")
#define ERROR_CANNOT_CONVERT_CELL_ARRAYS_TO_INDICES _W("Cannot convert cell arrays to indices.")
#define ERROR_CANNOT_CONVERT_STRING_ARRAYS_TO_INDICES _W("Cannot convert string arrays to indices.")
#define ERROR_CANNOT_CONVERT_FUNCTION_HANDLE_ARRAYS_TO_INDICES                                     \
    _W("Cannot convert function_handle arrays to indices.")
#define ERROR_CANNOT_CONVERT_CLASS_ARRAYS_TO_INDICES _W("Cannot convert class arrays to indices.")
#define ERROR_CANNOT_CONVERT_STRUCTURE_ARRAYS_TO_INDICES                                           \
    _W("Cannot convert structure arrays to indices.")
#define ERROR_CANNOT_CONVERT_UNKNOWN_TYPE_TO_INDICES _W("Cannot convert unknown type to indices.")
#define ERROR_INDEX_EXCEEDS_ARRAY_BOUNDS _W("Index exceeds array bounds.")
#define ERROR_OPERATION_DOES_NOT_SUPPORT_SPARSE_MATRIX_ARGUMENTS                                   \
    _W("operation does not support sparse matrix arguments.")
#define ERROR_CANNOT_RESIZE_SPARSE_ARRAYS _W("Cannot resize sparse arrays.")
#define ERROR_RESHAPE_OPERATION_NOT_ALLOWED_FOR_OVERLOADED_TYPE                                    \
    _W("Reshape operation not allowed for overloaded type.")
#define ERROR_RESHAPE_OPERATION_NOT_ALLOWED_FOR_FUNCTION_HANDLE_TYPE                               \
    _W("Reshape operation not allowed for 'function_handle' type.")
#define ERROR_RESHAPE_OPERATION_CANNOT_CHANGE_NUMBER_OF_ELEMENTS                                   \
    _W("Reshape operation cannot change the number of elements in array.")
#define ERROR_RESHAPE_NOT_ALLOWED_ND_SPARSE_ARRAYS                                                 \
    _W("Reshape operation not allowed with N Dimensions sparse arrays.")
#define ERROR_REAL_INTEGER_EXPECTED _W("real integer expected.")
#define ERROR_REAL_POSITIVE_INTEGER_EXPECTED _W("real positive integer expected.")
#define ERROR_ONLY_ONE_UNKNOWN_DIMENSION_ALLOWED _W("only one unknown dimension allowed.")
#define ERROR_K_TH_DIAGONAL_INPUT_MUST_BE_AN_INTEGER_SCALAR                                        \
    _W("K-th diagonal input must be an integer scalar.")
#define ERROR_ORDER_MUST_HAVE_AT_LEAST_N_ELEMENTS_FOR_AN_ND_ARRAY                                  \
    _W("ORDER must have at least N elements for an N-D array.")
#define ERROR_SECOND_ARGUMENT_NOT_A_VALID_PERMUTATION                                              \
    _W("Second argument is not a valid permutation.")
#define ERROR_CHANGEDIMENSIONS_OPERATION_NOT_ALLOWED_FOR_OVERLOADED_TYPE                           \
    _W("changeDimensions operation not allowed for overloaded type.")
#define ERROR_CHANGEDIMENSIONS_OPERATION_NOT_ALLOWED_FOR_FUNCTION_HANDLE                           \
    _W("changeDimensions operation not allowed for 'function_handle' type.")
#define ERROR_CHANGEDIMENSIONS_CANNOT_CHANGE_NUMBER_OF_ELEMENTS                                    \
    _W("changeDimensions operation cannot change the number of elements in array.")
#define ERROR_INVALID_DATA_CLASS _W("Invalid data class.")
#define ERROR_BYTE_SIZE_CALCULATION_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                \
    _W("Byte size calculation not supported for sparse arrays.")
#define ERROR_ISPOSITIVE_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                           \
    _W("isPositive not supported for sparse arrays.")
#define ERROR_SWITCH_ARGUMENT_MUST_BE_SCALAR_OR_STRING                                             \
    _W("Switch argument must be a scalar or a string")
#define ERROR_SWITCH_ARGUMENT_CANNOT_BE_REFERENCE_TYPE                                             \
    _W("Switch argument cannot be a reference type (struct or cell array)")
#define ERROR_CASE_ARGUMENTS_MUST_BE_SCALAR_OR_CELL_ARRAY                                          \
    _W("Case arguments must either be a scalar or a cell array")
#define ERROR_COPYELEMENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                         \
    _W("copyElements not supported for sparse arrays.")
#define ERROR_INVALID_INDEX_VALUE_EXCEEDS_LIMIT_MAX _W("Invalid index value > limit max.")
#define ERROR_EXPECTED_POSITIVE_INTEGER_SCALAR _W("Expected a positive integer scalar.")
#define ERROR_EXPECTED_INTEGER _W("Expected a integer.")
#define ERROR_NAN_AND_INF_NOT_ALLOWED _W("NaN and Inf not allowed.")
#define ERROR_A_CELL_OF_STRING_EXPECTED _W("A cell of string expected.")
#define ERROR_A_VECTOR_EXPECTED _W("A vector expected.")
#define ERROR_AN_ROW_VECTOR_EXPECTED _W("An row vector expected.")
#define ERROR_A_COLUMN_VECTOR_EXPECTED _W("A column vector expected.")
#define ERROR_EXPECTED_A_REAL_VALUE _W("Expected a real value.")
#define ERROR_EXPECTED_A_REAL_VALUE_SCALAR _W("Expected a real value scalar.")
#define ERROR_A_REAL_INTEGER_VALUE_SCALAR_EXPECTED _W("A real integer value scalar expected.")
#define ERROR_INPUTS_PARAMETERS_MUST_BE_SCALARS _W("Inputs parameters must be scalars.")
#define ERROR_THIRD_ARGUMENT_SHOULD_BE_AN_INTEGER_VALUE                                            \
    _W("Third argument should be an integer value.")
#define ERROR_EXPECTED_A_REAL_VALUED_SCALAR _W("Expected a real valued scalar")
#define ERROR_DIMENSION_ARGUMENT_MUST_BE_POSITIVE_INTEGER_SCALAR_WITHIN_INDEXING_RANGE             \
    _W("Dimension argument must be a positive integer scalar within indexing range.")
#define ERROR_UNDEFINED_FUNCTION_NZMAX_FOR_INPUT_ARGS_OF_TYPE_CELL                                 \
    _W("Undefined function 'nzmax' for input arguments of type 'cell'.")
#define ERROR_UNDEFINED_FUNCTION_NZMAX_FOR_INPUT_ARGS_OF_TYPE_STRING                               \
    _W("Undefined function 'nzmax' for input arguments of type 'string'.")
#define ERROR_UNDEFINED_FUNCTION_NZMAX_FOR_INPUT_ARGS_OF_TYPE_CLASS                                \
    _W("Undefined function 'nzmax' for input arguments of type 'class'.")
#define ERROR_UNDEFINED_FUNCTION_NZMAX_FOR_INPUT_ARGS_OF_TYPE_STRUCT                               \
    _W("Undefined function 'nzmax' for input arguments of type 'struct'.")
#define ERROR_UNDEFINED_FUNCTION_NZMAX_FOR_INPUT_ARGUMENTS                                         \
    _W("Undefined function 'nzmax' for input arguments.")
#define ERROR_UNDEFINED_FUNCTION_NNZ_FOR_INPUT_ARGS_OF_TYPE_CELL                                   \
    _W("Undefined function 'nnz' for input arguments of type 'cell'.")
#define ERROR_UNDEFINED_FUNCTION_NNZ_FOR_INPUT_ARGS_OF_TYPE_STRING                                 \
    _W("Undefined function 'nnz' for input arguments of type 'string'.")
#define ERROR_UNDEFINED_FUNCTION_NNZ_FOR_INPUT_ARGS_OF_TYPE_CLASS                                  \
    _W("Undefined function 'nnz' for input arguments of type 'class'.")
#define ERROR_UNDEFINED_FUNCTION_NNZ_FOR_INPUT_ARGS_OF_TYPE_STRUCT                                 \
    _W("Undefined function 'nnz' for input arguments of type 'struct'.")
#define ERROR_UNDEFINED_FUNCTION_NNZ_FOR_INPUT_ARGUMENTS                                           \
    _W("Undefined function 'nnz' for input arguments.")
#define ERROR_UNDEFINED_FUNCTION_NDIMS_FOR_INPUT_ARGUMENTS_OF_TYPE                                 \
    _W("Undefined function 'ndims' for input arguments of type {0}")
#define ERROR_UNDEFINED_FUNCTION_CONJ_FOR_INPUT_ARGUMENTS_OF_TYPE                                  \
    _W("Undefined function 'conj' for input arguments of type {0}")
#define ERROR_INVALID_THIRD_INPUT_ARGUMENT_FIRST_OR_LAST_EXPECTED                                  \
    _W("Invalid third input argument. 'first' or 'last' expected.")
#define ERROR_CELL_DEF_MUST_HAVE_SAME_ELEMENTS_PER_ROW                                             \
    _W("Cell definition must have same number of elements in each row")
#define ERROR_ARRAYS_HAVE_INCOMPATIBLE_SIZES_FOR_THIS_OPERATION                                    \
    _W("Arrays have incompatible sizes for this operation.")
#define ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT                             \
    _W("Attempt to apply contents-indexing to non-cell array object.")
#define ERROR_EMPTY_CONTENTS_INDEXING_NOT_DEFINED _W("Empty contents indexing is not defined.")
#define ERROR_CONTENT_INDEXING_MUST_RETURN_SINGLE_VALUE                                            \
    _W("Content indexing must return a single value.")
#define ERROR_INDEX_EXCEEDS_CELL_ARRAY_DIMENSIONS _W("Index exceeds cell array dimensions")
#define ERROR_GETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                      \
    _W("getNDimContents not supported for sparse arrays.")
#define ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_ARRAY_OBJECT_VARIANT                     \
    _W("Attempt to apply contents-indexing to non cell-array object.")
#define ERROR_GETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                              \
    _W("getVectorContentsAsList not supported for sparse arrays.")
#define ERROR_ARRAYOF_INDEX_EXCEEDS_BOUNDS_OF_CELL_ARRAY                                           \
    _W("ArrayOf index exceeds bounds of cell-array")
#define ERROR_ATTEMPT_APPLY_CONTENTS_INDEXING_TO_NON_CELL_OR_STRING_ARRAY_OBJECT                   \
    _W("Attempt to apply contents-indexing to non cell or string array object.")
#define ERROR_GETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                \
    _W("getNDimContentsAsList not supported for sparse arrays.")
#define ERROR_CONVERSION_FROM_MISSING_TO_CHARACTER_VECTOR_NOT_SUPPORTED                            \
    _W("Conversion from <missing> to character vector is not supported.")
#define ERROR_SETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                    \
    _W("setVectorContents not supported for sparse arrays.")
#define ERROR_IN_EXPRESSION_A_BRACE_I_EQUALS_B_I_MUST_REFERENCE_SINGLE_ELEMENT                     \
    _W("In expression A{I} = B, I must reference a single element of cell-array A.")
#define ERROR_ILLEGAL_NEGATIVE_INDEX_IN_EXPRESSION_A_BRACE_I_EQUALS_B                              \
    _W("Illegal negative index in expression A{I} = B.")
#define ERROR_SETNDIMCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                      \
    _W("setNDimContents not supported for sparse arrays.")
#define ERROR_IN_EXPRESSION_A_BRACE_I1_I2_IN_EQUALS_B_MUST_REFERENCE                               \
    _W("In expression A{I1,I2,...,IN} = B, (I1,...,IN) must reference a ")
#define ERROR_SETVECTORCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                              \
    _W("setVectorContentsAsList not supported for sparse arrays.")
#define ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_SIDE_EXPRESSION               \
    _W("Not enough right hand side values to satisy left hand side expression.")
#define ERROR_ASSIGNMENT_EXPECTS_A_CHARACTER_VECTOR _W("{} assignment expects a character vector.")
#define ERROR_SETNDIMCONTENTSASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                \
    _W("setNDimContentsAsList not supported for sparse arrays.")
#define ERROR_REFERENCE_TO_NON_EXISTENT_FIELD _W("Reference to non-existent field {0}")
#define ERROR_SETFIELD_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                             \
    _W("setField not supported for sparse arrays.")
#define ERROR_CANNOT_APPLY_A_FIELD_NAME_EQUALS_B_TO_MULTI_ELEMENT_STRUCTURE_ARRAY_A                \
    _W("Cannot apply A.field_name = B to multi-element structure array A.")
#define ERROR_SETFIELDASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                       \
    _W("setFieldAsList not supported for sparse arrays.")
#define ERROR_NOT_ENOUGH_RIGHT_HAND_SIDE_VALUES_TO_SATISFY_LEFT_HAND_SIDE_EXPRESSION_VARIANT       \
    _W("Not enough right hand side values to satisfy left hand side expression")
#define ERROR_CANNOT_CONVERT_HANDLE_ARRAYS_TO_ANY_OTHER_TYPE                                       \
    _W("Cannot convert handle-arrays to any other type.")
#define ERROR_CANNOT_CONVERT_GRAPHIC_HANDLE_ARRAYS_TO_ANY_OTHER_TYPE                               \
    _W("Cannot convert graphic handle-arrays to any other type.")
#define ERROR_CANNOT_CONVERT_CELL_ARRAYS_TO_ANY_OTHER_TYPE                                         \
    _W("Cannot convert cell-arrays to any other type.")
#define ERROR_CANNOT_CONVERT_FUNCTION_HANDLE_TO_ANY_OTHER_TYPE                                     \
    _W("Cannot convert function_handle to any other type.")
#define ERROR_CANNOT_COMBINE_CLASSES_WITH_DIFFERENT_TYPES                                          \
    _W("Cannot combine classes with different types.")
#define ERROR_CANNOT_CONVERT_CLASS_TO_ANY_OTHER_TYPE _W("Cannot convert class to any other type.")
#define ERROR_CANNOT_COMBINE_STRUCTURES_DIFFERENT_FIELDS_DELETIONS                                 \
    _W("Cannot combine structures with different fields if the combination requires fields to be " \
       "deleted from one of the structures.")
#define ERROR_CANNOT_CONVERT_STRUCT_ARRAYS_TO_ANY_OTHER_TYPE                                       \
    _W("Cannot convert struct-arrays to any other type.")
#define ERROR_CANNOT_CONVERT_STRING_ARRAYS_TO_ANY_OTHER_TYPE                                       \
    _W("Cannot convert string-arrays to any other type.")
// StringToClass messages
#define ERROR_INPUT_MUST_BE_A_VALID_CLASS_NAME _W("input must be a valid class name.")
#define ERROR_SPARSE_TYPE_NOT_SUPPORTED _W("Sparse type not supported.")
#define ERROR_NUMERICS_TYPES_EXPECTED _W("Numerics types expected.")
#define ERROR_INTEGERS_CAN_ONLY_BE_COMBINED_WITH_INTEGERS_OR_SCALAR_DOUBLES                        \
    _W("Integers can only be combined with integers of the same class, or scalar doubles.")
#define ERROR_CANNOT_CONVERT_TO_STR_WITH_VALUE _W("Cannot convert: {0}")
#define ERROR_BASE_MUST_BE_INTEGER_BETWEEN_2_AND_36                                                \
    _W("The base must be an integer value between 2 and 36.")
#define ERROR_2D_CHAR_ARRAY_EXPECTED _W("2D char array expected.")
#define ERROR_WRONG_VALUE_ARG2_R_OR_C_EXPECTED                                                     \
    _W("Wrong value for argument #2. 'r' or 'c' expected")
#define ERROR_WRONG_TYPE_ARG2_NUMERIC_VALUES_EXPECTED                                              \
    _W("Wrong type for argument #2. numeric values expected")
#define ERROR_WRONG_SIZE_ARG2_ROW_VECTOR_OR_SCALAR_EXPECTED                                        \
    _W("Wrong size for argument #2. row vector or scalar expected")
#define ERROR_ARRAY_OF_INTEGERS_EXPECTED                                                           \
    _W("An array of integers values, 0 <= D <= flintmax expected.")
#define ERROR_THIRD_PARAMETER_POSITIVE_INTEGER_VALUE_EXPECTED                                      \
    _W("#3 parameter: positive integer value expected.")
#define ERROR_BINARY_CHAR_VECTOR_MUST_BE_0_1                                                       \
    _W("Binary character vector may consist only of characters 0 and 1.")
#define ERROR_INVALID_STRING_LENGTH_1_32_64 _W("Invalid string length: 1, 32, 64 expected.")
#define ERROR_UNSUPPORTED_DATA_CONVERSION _W("Unsupported data conversion.")
#define ERROR_INPUT_ARGUMENTS_MUST_BE_NUMERIC _W("Input arguments must be numeric.")
#define ERROR_INPUT_ARGUMENTS_MUST_HAVE_THE_SAME_SIZE _W("Input arguments must have the same size.")
// Table type messages
#define ERROR_TABLE_COLUMN_NOT_FOUND _W("Column '{0}' not found in table.")
#define ERROR_COLUMN_INDEX_OUT_OF_BOUNDS _W("Column index out of bounds.")
#define ERROR_TABLE_COLUMN_ALREADY_EXISTS                                                          \
    _W("Column '{0}' already exists. Use setTableColumn to modify.")
#define ERROR_COLUMN_DATA_SIZE_MUST_MATCH_TABLE_HEIGHT                                             \
    _W("Column data size must match table height.")
// Class builtin messages
#define ERROR_DECLARATION_ONLY_ALLOWED_FROM_CLASS_CONSTRUCTOR                                      \
    _W("This declaration is only allowed from a class constructor.")
// Time module messages
#define ERROR_CALENDAR_NOT_INITIALIZED _W("Calendar not initialized.")
#define ERROR_YEAR_VALUE_OUT_OF_RANGE _W("Year value is wrong [1400, 9999] expected.")
#define ERROR_MONTH_VALUE_OUT_OF_RANGE _W("Month value is wrong [1, 12] expected.")
#define ERROR_TIMEIT_INVALID_FUNCTION_HANDLE                                                       \
    _W("First argument must be a function handle that takes no input argument.")
//=============================================================================
// PYTHON ENGINE MESSAGES
//=============================================================================
#define ERROR_PYTHON_SAME_NAME_VALUE_EXPECTED _W("Same name, value numbers expected.")
#define ERROR_PYTHON_VALID_PYTHON_CODE_OBJECT_EXPECTED _W("Valid Python code object expected.")
// Additional Python engine messages added during migration
#define ERROR_PYTHON_CONVERSION_NOT_SUPPORTED _W("Conversion to Python is not supported.")
#define ERROR_PYTHON_CHAR_CONVERSION_1N_VECTORS                                                    \
    _W("Conversion of 'char' to Python is only supported for 1-N vectors.")
#define ERROR_PYTHON_INVALID_DICTIONARY _W("Invalid dictionary.")
#define ERROR_PYTHON_TYPE_NOT_SUPPORTED _W("Type not supported")
#define ERROR_PYTHON_CANNOT_CREATE_MEMORYVIEW _W("Cannot create MemoryView.")
#define ERROR_PYTHON_FAILED_TO_GET_BUFFER_FROM_MEMORY_VIEW                                         \
    _W("Failed to get buffer from memory view.")
#define ERROR_PYTHON_UNSUPPORTED_DATA_FORMAT _W("Unsupported data format.")
#define ERROR_PYKEYMATCH_INVALID_PYTHON_OBJECT _W("Invalid Python object.")
#define ERROR_PYENGINE_IMPOSSIBLE_TO_LOAD_PYTHON_LIBRARY _W("Impossible to load Python library.")
#define ERROR_PYENGINE_CANNOT_INITIALIZE_PYTHON_ENVIRONMENT                                        \
    _W("Cannot initialize python environment.")
// PythonEnvironment class messages
#define ERROR_PYTHON_ENVIRONMENT_OBJECT_EXPECTED _W("PythonEnvironment object expected.")
//=============================================================================
// VALUE ERRORS
//=============================================================================
#define ERROR_WRONG_ARGUMENT_X_POSITIVE_VALUE_EXPECTED                                             \
    _W("Wrong value for #{} argument. positive value expected.")
#define ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #{}. A finite scalar integer value expected.")
#define ERROR_WRONG_ARGUMENT_X_FINITE_VECTOR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #{}. A finite vector of integer values expected.")
#define ERROR_WRONG_ARGUMENT_X_A_MUST_BE_HIGHER_THAN_B                                             \
    _W("Wrong value for #2 argument. a must be higher than b ([a,b]).")
//=============================================================================
// ARGUMENT COUNT ERRORS
//=============================================================================
#define ERROR_WRONG_NUMBERS_INPUT_ARGS _W("Wrong number of input arguments.")
#define WARNING_OUTPUTS_NOT_ASSIGNED _W("Warning! one or more outputs not assigned in call.")
//=============================================================================
// SYNTAX & EXPRESSION ERRORS
//=============================================================================
#define ERROR_AST_SYNTAX_ERROR _W("AST - syntax error!")
#define ERROR_END_ILLEGAL _W("END keyword illegal!")
#define ERROR_UNRECOGNIZED_NODE _W("Unrecognized reserved node in expression tree!")
#define ERROR_UNRECOGNIZED_EXPRESSION _W("Unrecognized expression!")
#define ERROR_EMPTY_EXPRESSION _W("Empty expression!")
#define ERROR_UNRECOGNIZED_STATEMENT _W("Unrecognized statement type.")
#define ERROR_INDEX_EXPRESSION_EXPECTED _W("Expected indexing expression!")
#define ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION                                                \
    _W("Illegal left hand side in multifunction expression")
#define ERROR_MULTIPLE_ROWS_NOT_ALLOWED                                                            \
    _W("Multiple rows not allowed in left hand side of multifunction expression")
#define ERROR_PARENTHETICAL_EXPRESSION                                                             \
    _W("Parenthetical expression in the left hand side of a function call must resolve to a "      \
       "single "                                                                                   \
       "element.")
#define ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION _W("Illegal expression in function expression")
//=============================================================================
// FUNCTION & SCRIPT ERRORS
//=============================================================================
#define ERROR_ENDFUNCTION_WRONG_USE _W("endfunction cannot used here.")
#define ERROR_MUST_HAVE_LVALUE _W("Must have lvalue in argument passed by reference")
//=============================================================================
// STACK & SCOPE ERRORS
//=============================================================================
#define ERROR_STACK_DEPTH_EXCEEDED _W("Allowable stack depth exceeded...")
#define ERROR_POP_GLOBAL_SCOPE _W("Attempt to pop global scope off of context stack!")
//=============================================================================
// OBJECT & STRUCTURE ERRORS
//=============================================================================
#define ERROR_DYNAMIC_FIELD_STRING_EXPECTED                                                        \
    _W("dynamic field reference to structure requires a string argument")
#define ERROR_ASSIGN_TO_NON_STRUCT _W("Cannot apply A.field_name = B to non struct-array object A.")
#define ERROR_FIELDNAMES_MUST_MATCH _W("Fieldnames in structs must match.")
#define ERROR_NEED_OVERLOAD _W("Need to Overload!")
// Struct/Field related messages
#define ERROR_NUMBER_OF_FIELD_NAMES_MUST_MATCH                                                     \
    _W("Number of field names must match number of values in structure constructor.")
#define ERROR_ARRAYOF_DIMENSIONS_OF_NON_SCALAR_ENTRIES_MUST_AGREE                                  \
    _W("ArrayOf dimensions of non-scalar entries must agree in structure construction.")
#define ERROR_INSERTFIELDNAME_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                      \
    _W("insertFieldName not supported for sparse arrays.")
#define ERROR_CANNOT_DEREFERENCE_A_FIELD_OF_A_MULTI_ELEMENT_STRUCTURE_ARRAY                        \
    _W("Cannot dereference a field of a multi-element structure array.")
#define ERROR_GETFIELD_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                             \
    _W("getField not supported for sparse arrays.")
#define ERROR_ATTEMPT_APPLY_FIELD_INDEXING_TO_NON_STRUCTURE_ARRAY_OBJECT                           \
    _W("Attempt to apply field-indexing to non structure-array object.")
#define ERROR_GETFIELDASLIST_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                       \
    _W("getFieldAsList not supported for sparse arrays.")
//=============================================================================
// SWITCH STATEMENT ERRORS
//=============================================================================
#define ERROR_SWITCH_STATEMENTS _W("Switch statements support scalar and string arguments only.")
// Matrix/operation specific messages
#define ERROR_CANNOT_APPLY_NUMERIC_OPERATION_TO_REFERENCE_TYPES                                    \
    _W("Cannot apply numeric operation {0} to reference types.")
#define ERROR_CANNOT_APPLY_MATRIX_OPERATION_TO_N_DIMENSIONAL_ARRAYS                                \
    _W("Cannot apply matrix operation {0} to N-Dimensional arrays.")
#define ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO _W("Size mismatch on arguments to {0}")
#define ERROR_SAME_REFERENCE_TYPE_EXPECTED _W("Same reference type expected.")
// Simple size mismatch message (used by several operators)
// Colon operator messages
#define ERROR_COLON_OPERANDS_MUST_BE_SAME_TYPE_OR_MIXED_WITH_REAL_SCALAR_DOUBLES                   \
    _W("Colon operands must be all the same type, or mixed with real scalar doubles.")
#define ERROR_COLON_OPERANDS_MUST_BE_ALL_THE_SAME_TYPE_OR_MIXED_WITH_REAL_DOUBLE_SCALAR            \
    _W("Colon operands must be all the same type, or mixed with real double scalar.")
#define ERROR_COLON_CHAR_OPERANDS_FIRST_AND_LAST_MUST_BE_CHAR                                      \
    _W("For colon operator with char operands, first and last operands must be char.")
// Handle object messages
#define ERROR_HANDLE_MUST_HAVE_A_TYPE _W("handle must have a type.")
#define ERROR_HANDLE_MUST_HAVE_A_POINTER _W("handle must have a pointer.")
// Operators module messages
#define ERROR_FINITE_INTEGER_VALUES_EXPECTED _W("finite integer values expected.")
#define ERROR_FINITE_POSITIVE_INTEGER_VALUES_EXPECTED _W("finite positive integer values expected.")
#define ERROR_ASSUMEDTYPE_MUST_BE_INTEGER_TYPE_NAME _W("ASSUMEDTYPE must be an integer type name.")
#define ERROR_OPERANDS_TO_BIT_OPS_MUST_BE_NUMERIC _W("Operands to BIT Ops must be numeric.")
#define ERROR_INPUTS_MUST_BE_FULL _W("Inputs must be full.")
#define ERROR_INPUT_ARGUMENTS_MUST_HAVE_SAME_CLASS_OR_SCALAR_DOUBLES                               \
    _W("Input arguments must have the same class or scalar doubles.")
// Operator-specific messages
// Matrix multiplication messages
#define ERROR_AT_LEAST_ONE_INPUT_ARGUMENT_MUST_BE_SCALAR                                           \
    _W("At least one input argument must be scalar.")
#define ERROR_USING_OPERATOR_N_MATRIX_DIMENSIONS_MUST_AGREE                                        \
    _W("using operator '*' \n Matrix dimensions must agree.")
#define ERROR_ONLY_POSITIVE_INTEGERS_EXPECTED _W("Only positive integers expected.")
#define ERROR_UNHANDLED_TYPE_FOR_SECOND_ARGUMENT_TO_A_POW                                          \
    _W("Unhandled type for second argument to A^B")
#define ERROR_UNHANDLED_TYPE_COMBINATION_FOR_A_POW _W("Unhandled type combination for A^B")
//=============================================================================
// SYSTEM & RESOURCE ERRORS
//=============================================================================
#define ERROR_INVALID_FILEID _W("Invalid file id.")
#define MSG_CTRL_C_DETECTED _W("Interrupt (ctrl-c) encountered.")
// characters_encoding module messages
#define ERROR_TYPE_OR_DIMENSIONS_NOT_SUPPORTED _W("Type or dimensions not supported.")
#define ERROR_CANNOT_CONVERT_TO_UNICODE _W("Cannot convert to unicode.")
#define ERROR_INVALID_CHARSET _W("Invalid charset: {0}")
#define ERROR_CANNOT_CONVERT_STRING_TO_EXPECTED_CHARSET                                            \
    _W("Cannot convert string to expected charset.")
//=============================================================================
// AUTO-CONVERTED ERROR MESSAGES
#define ERROR_WRONG_VALUE_FOR_2_ARGUMENT _W("Wrong value for #2 argument.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_0_1_VALUES_EXPECTED                                       \
    _W("Wrong value for #1 argument. [0, 1] values expected.")
#define ERROR_WRONG_VALUE_OF_THE_FOURTH_ARGUMENT_UPPER_OR_LOWER_EXPECTED                           \
    _W("Wrong value of the fourth argument 'upper' or 'lower' expected.")
#define ERROR_INPUT_ARGUMENT_MUST_BE_DENSE_AND_REAL _W("Input argument must be dense and real.")
#define ERROR_LINEAR_METHOD_EXPECTED _W("'linear' method expected.")
#define ERROR_SPARSE_MATRIX_INDICES_MUST_BE_POSITIVE_INTEGERS                                      \
    _W("Sparse matrix indices must be positive integers.")
#define ERROR_IN_I_J_V_FORMAT_ALL_THREE_VECTORS_MUST_BE_THE_SAME_SIZE_OR_BE_SCALARS                \
    _W("in I, J, V format, all three vectors must be the same size or be scalars.")
#define ERROR_NAME_ALREADY_REGISTER _W("name already register.")
#define ERROR_RESERVED_NAME _W("reserved name")
#define ERROR_IMPOSSIBLE_TO_UNREGISTER_AN_RESERVED_EVENT_NAME                                      \
    _W("Impossible to unregister an reserved event name.")
#define ERROR_MUST_CONTAIN_GENERATOR_SETTINGS_CAPTURED_PREVIOUSLY                                  \
    _W("Must contain generator settings captured previously.")
#define ERROR_UNRECOGNIZED_OPTION _W("Unrecognized option: '{0}' expected.")
#define ERROR_NAME_NOT_FOUND _W("Name not found.")
#define ERROR_EVALUATOR_NOT_AVAILABLE _W("Evaluator not available.")
#define ERROR_FIELD_NAMES_MUST_BE_STRING_SCALARS_OR_CHARACTER_VECTORS                              \
    _W("Field names must be string scalars or character vectors.")
#define ERROR_PYTHON_OBJECT_EXPECTED _W("Python object expected.")
#define ERROR_OPTION_NOT_MANAGED _W("option not managed.")
#define ERROR_INVALID_VALUE _W("Invalid value.")
#define ERROR_PROFILE_STRUCT_EXPECTED _W("Profile struct expected.")
#define ERROR_FUTURE_HANDLE_EXPECTED _W("Future handle expected.")
#define ERROR_FEVALFUTURE_HANDLE_EXPECTED _W("FevalFuture handle expected.")
#define ERROR_FUNCTION_HANDLE_HANDLE_EXPECTED _W("function handle handle expected.")
#define ERROR_INVALID_ANONYMOUS_FUNCTION _W("Invalid anonymous function.")
#define ERROR_INTEGER_VALUE_EXPECTED _W("integer value expected.")
#define ERROR_NON_NEGATIVE_VALUE_EXPECTED _W("non negative value expected.")
#define ERROR_BACKGROUNDPOOL_HANDLE_EXPECTED _W("backgroundPool handle expected.")
#define ERROR_A_NUMERIC_SCALAR_VALUE_EXPECTED _W("a numeric scalar value expected.")
#define ERROR_FEVALQUEUE_HANDLE_EXPECTED _W("FevalQueue handle expected.")
#define ERROR_VALID_STATES_TO_WAIT_FOR_ARE_RUNNING_OR_FINISHED                                     \
    _W("Valid states to wait for are: 'running' or 'finished'.")
#define ERROR_EXPECTED_TIMEOUT_TO_BE_NON_NEGATIVE_REAL_NUMERICAL_SCALAR                            \
    _W("Expected timeout to be non-negative real numerical scalar.")
#define ERROR_CANNOT_WAIT_FOR_COMPLETION_OF_FUTURES_THAT_ARE_IN_STATE_UNAVAILABLE                  \
    _W("Cannot wait for completion of Futures that are in state 'unavailable'.")
#define ERROR_INVALID_BACKGROUNDPOOL_HANDLE _W("Invalid backgroundPool handle.")
#define ERROR_SYSTEM_DOES_NOT_RETURN_RESULT _W("system does not return result.")
#define ERROR_UNKNOWN_COMMAND_OPTION _W("Unknown command option.")
#define ERROR_ENVIRONMENT_NAMES_MUST_BE_A_STRING_ARRAY_CELL_ARRAY_OF_CHARACTER                     \
    _W("Environment names must be a string array, cell array of character vectors, or character "  \
       "array.")
#define ERROR_EVALUATOR_IS_NOT_INITIALIZED _W("Evaluator is not initialized.")
#define ERROR_CONTEXT_IS_NOT_INITIALIZED _W("Context is not initialized.")
#define ERROR_FUNCTION_DICTIONARY_NOT_FOUND_IN_THE_CURRENT_CONTEXT                                 \
    _W("Function 'dictionary' not found in the current context.")
#define ERROR_CANNOT_SET_ENVIRONMENT_VARIABLE _W("Cannot set environment variable.")
#define ERROR_FILENAME_NOT_ASSOCIATED_TO_AN_APPLICATION                                            \
    _W("Filename not associated to an application.")
#define ERROR_NAME_ARGUMENT_REQUIRES_3_INPUT_ARGUMENTS                                             \
    _W("'name' argument requires 3 input arguments.")
#define ERROR_COMPLEX_INTEGER_NOT_ALLOWED_FOR_ARITHMETIC_OPERATOR                                  \
    _W("Complex integer not allowed for arithmetic operator {0}.")
#define ERROR_SAME_CLASS_TYPE_EXPECTED _W("Same class type expected.")
#define ERROR_REQUESTED_DIVIDE_OPERATION_REQUIRES_ARGUMENTS_TO_HAVE_CORRECT_DIMENSIONS             \
    _W("Requested divide operation requires arguments to have correct dimensions.")
#define ERROR_CANNOT_NEGATE_NON_NUMERIC_TYPES _W("Cannot negate non-numeric types.")
#define ERROR_INPUT_ARGUMENT_MUST_BE_REAL _W("Input argument must be real.")
#define ERROR_SIZE_MISMATCH_ON_ARGUMENTS_TO_ARITHMETIC_OPERATOR                                    \
    _W("Size mismatch on arguments to arithmetic operator {0}.")
#define ERROR_SAME_TYPES_EXPECTED _W("Same types expected.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_A_STRUCTURE_WITH_TWO_FIELDS_WHOSE_NAMES_ARE_TYPE_AND         \
    _W("Second argument must be a structure with two fields whose names are '{0}' and '{1}'.")
#define ERROR_NO_SUPPORT_FOR_INDEX_CHAINING _W("No support for index chaining.")
#define ERROR_SUBS_FIELD_MUST_BE_A_CELL_ARRAY_OR_STRING                                            \
    _W("'subs' field must be a cell array or string.")
#define ERROR_FIRST_ARGUMENT_MUST_BE_A_STRUCT _W("First argument must be a struct.")
#define ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_OR_EXPECTED                                 \
    _W("Illegal indexing structure argument: type '.', '{}' or '()' expected.")
#define ERROR_UNSUPPORTED_TYPE_TO_REDUCE _W("Unsupported type to reduce.")
#define ERROR_UNSUPPORTED_OPERATOR_TYPE _W("Unsupported operator type.")
#define ERROR_UNSUPPORTED_TYPE_MUST_BE_A_NUMERICAL_TYPE                                            \
    _W("Unsupported Type: must be a numerical type.")
#define ERROR_MPI_COMM_NULL_NOT_ALLOWED _W("MPI_COMM_NULL not allowed.")
#define ERROR_MPI_COMM_SPLIT_FAILS _W("MPI_Comm_split fails.")
#define ERROR_ATTEMPTING_TO_USE_AN_MPI_ROUTINE_BEFORE_INITIALIZING_MPI                             \
    _W("Attempting to use an MPI routine before initializing MPI.")
#define ERROR_WRONG_ENGINE_MODE _W("Wrong engine mode.")
#define ERROR_INVALID_MODULE_NAME _W("Invalid module name: {0}")
#define ERROR_IMPOSSIBLE_TO_ASSOCIATE_MODULE_NAME_AND_LIBRARY_PATH                                 \
    _W("Impossible to associate module name and library path")
#define ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_NAME_ALREADY_USED                                   \
    _W("An existing module with the same name already used: {0}")
#define ERROR_AN_EXISTING_MODULE_WITH_THE_SAME_PATH_ALREADY_DEFINED                                \
    _W("An existing module with the same path already defined: {0}")
#define ERROR_STARTUP_M_DOES_NOT_EXIST _W("startup.m does not exist")
#define ERROR_PATH_DOES_NOT_EXIST _W("Path does not exist: {0}.")
#define ERROR_FINISH_M_DOES_NOT_EXIST _W("finish.m does not exist.")
#define ERROR_AN_EXISTING_MODULE_ROOT_PATH_EXPECTED                                                \
    _W("An existing module root path expected: {0}.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_REVERSE_EXPECTED                                          \
    _W("Wrong value for #1 argument, \'reverse\' expected.")
#define ERROR_ISPROTECTED_VALUE_EXPECTED _W("'isprotected' value expected.")
#define ERROR_ARGUMENT_2_MUST_BE_A_VALID_OPTION _W("Argument #2 must be a valid option.")
#define ERROR_CANNOT_COMPARE_VERSIONS _W("Cannot compare versions: {0}.")
#define ERROR_IMPOSSIBLE_TO_GET_EXTERNAL_MODULES_DIRECTORY                                         \
    _W("Impossible to get external modules directory.")
#define ERROR_INPUT_MUST_BE_ALL _W("Input must be 'all'.")
#define ERROR_WRONG_SCOPE _W("Wrong scope.")
#define ERROR_WHOS_FILE_FUNCTION_EXPECTED _W("whos file function expected.")
#define ERROR_FILENAME_DOES_NOT_EXIST _W("Filename does not exist.")
#define ERROR_1_ARGUMENT_MUST_CONTAIN_A_STRING_GLOBAL_BASE_LOCAL_OR_CALLER                         \
    _W("#1 Argument must contain a string: \'global\', \'base\', \'local\' or \'caller\' "         \
       "expected.")
#define ERROR_2_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME                                        \
    _W("#2 Argument must contain a valid variable name.")
#define ERROR_REDEFINING_PERMANENT_VARIABLE _W("Redefining permanent variable.")
#define ERROR_A_VALID_VARIABLE_NAME_EXPECTED _W("A valid variable name expected.")
#define ERROR_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME                                          \
    _W("Argument must contain a valid variable name.")
#define ERROR_VARIABLE_IS_LOCKED _W("variable is locked.")
#define ERROR_ARGUMENT_1_GLOBAL_BASE_LOCAL_OR_CALLER_EXPECTED                                      \
    _W("Argument #1 : 'global', 'base', 'local' or 'caller' expected.")
#define ERROR_A_PERSISTENT_DECLARATION_IS_ONLY_ALLOWED_IN_A_SCRIPT_FILE_FUNCTION                   \
    _W("A 'persistent' declaration is only allowed in a script file function.")
#define ERROR_2_ARGUMENT_MUST_BE_AN_EXISTING_VARIABLE_NAME                                         \
    _W("#2 Argument must be an existing variable name.")
#define ERROR_FILENAME_EXPECTED_AFTER_FILE _W("filename expected after '-file'.")
#define ERROR_FILE_MUST_BE_THE_FIRST_ARGUMENT _W("-file must be the first argument.")
#define ERROR_WHO_FILE_FUNCTION_EXPECTED _W("who file function expected.")
#define ERROR_INVALID_OPTION _W("Invalid option: {0}.")
#define ERROR_N_DIMENSIONAL_SPARSE_ARRAYS_ARE_NOT_SUPPORTED                                        \
    _W("N-dimensional sparse arrays are not supported.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_SINGLE_IS_NOT_POSSIBLE                                    \
    _W("Conversion to logical from single is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_WITH_NAN_IS_NOT_POSSIBLE                                       \
    _W("Conversion to logical with NaN is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_SPARSE_INTEGER_IS_NOT_POSSIBLE                            \
    _W("Conversion to logical from sparse integer is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_GRAPHICS_OBJECT_IS_NOT_POSSIBLE                           \
    _W("Conversion to logical from graphics_object is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_HANDLE_IS_NOT_POSSIBLE                                    \
    _W("Conversion to logical from handle is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_STRING_IS_NOT_POSSIBLE                                    \
    _W("Conversion to logical from string is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_CELL_IS_NOT_POSSIBLE                                      \
    _W("Conversion to logical from cell is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_FUNCTION_HANDLE_IS_NOT_POSSIBLE                           \
    _W("Conversion to logical from function_handle is not possible.")
#define ERROR_UNDEFINED_FUNCTION_LOGICAL_FOR_INPUT_ARGUMENTS_OF_TYPE                               \
    _W("Undefined function 'logical' for input arguments of type '{}'")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_STRUCT_IS_NOT_POSSIBLE                                    \
    _W("Conversion to logical from struct is not possible.")
#define ERROR_CONVERSION_TO_LOGICAL_FROM_COMPLEX_IS_NOT_POSSIBLE                                   \
    _W("Conversion to logical from complex is not possible.")
#define ERROR_INVALID_CONVERSION _W("Invalid conversion.")
#define ERROR_SIZE_MISMATCH_ON_ARGUMENTS _W("Size mismatch on arguments.")
#define ERROR_INPUT_FOLLOWING_LIKE_IS_NOT_A_LOGICAL_ARRAY                                          \
    _W("Input following 'like' is not a logical array.")
#define ERROR_ONE_ARGUMENT_HAD_AN_ILLEGAL_VALUE _W("One argument had an illegal value.")
#define ERROR_POSITIVE_FINITE_MATRIX_EXPECTED _W("Positive finite matrix expected.")
#define ERROR_UNSUPPORTED_MATRIX_TYPE _W("Unsupported matrix type.")
#define ERROR_INPUT_MUST_BE_FINITE _W("Input must be finite.")
#define ERROR_INPUT_TO_X_MUST_NOT_CONTAIN_NAN_OR_INF _W("Input to {0} must not contain NaN or Inf.")
#define ERROR_INPUT_TO_LU_MUST_NOT_CONTAIN_NAN_OR_INF _W("Input to LU must not contain NaN or Inf.")
#define ERROR_CANNOT_APPLY_EXPONENTIAL_OPERATION_TO_N_DIMENSIONAL_ARRAYS                           \
    _W("Cannot apply exponential operation to N-Dimensional arrays.")
#define ERROR_POWER_OPERATOR_CAN_ONLY_BE_APPLIED_TO_SCALAR_AND_SQUARE_ARGUMENTS                    \
    _W("Power (^) operator can only be applied to scalar and square arguments.")
#define ERROR_UNDEFINED_FUNCTION_X_FOR_INPUT_ARGUMENTS_OF_TYPE                                     \
    _W("Undefined function '{0}' for input arguments of type {1}.")
#define ERROR_INPUT_ARGUMENT_MUST_NOT_CONTAIN_NAN_OR_INF                                           \
    _W("Input argument must not contain NaN or Inf.")
#define ERROR_SVD_CANNOT_TAKE_SVD_OF_MATRIX_CONTAINING_INF_OR_NAN_VALUES                           \
    _W("svd: cannot take svd of matrix containing Inf or NaN values.")
#define ERROR_OPTION_2_MUST_BE_BALANCE_OR_NOBALANCE                                                \
    _W("option #2 must be 'balance', or 'nobalance'.")
#define ERROR_SECOND_INPUT_MUST_BE_SKEW_OR_NONSKEW _W("Second input must be 'skew' or 'nonskew.")
#define ERROR_SECOND_INPUT_MUST_BE_FINITE_AND_0 _W("Second input must be finite and >= 0.")
#define ERROR_VECTOR_OR_MATRIX_2D_EXPECTED _W("Vector or matrix 2D expected.")
#define ERROR_NOT_YET_IMPLEMENTED _W("Not yet implemented.")
#define ERROR_SINGLE_OR_DOUBLE_TYPE_EXPECTED _W("single or double type expected.")
#define ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_OR_COMPLEX                                        \
    _W("Second input argument must be 'real' or 'complex'.")
#define ERROR_SVD_X_0_OR_SVD_X_ECON_EXPECTED _W("svd(X, 0) or svd(X, 'econ') expected.")
#define ERROR_JULIA_OBJECT_EXPECTED _W("Julia object expected.")
#define ERROR_JULIAENVIRONMENT_OBJECT_EXPECTED _W("JuliaEnvironment object expected.")
#define ERROR_WRONG_VALUE_FOR_ARGUMENT_2_CONVERTINFANDNAN_EXPECTED                                 \
    _W("Wrong value for argument #2: 'ConvertInfAndNaN' expected.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_AVAILABLE_EXPECTED                                        \
    _W("Wrong value for #1 argument: 'available' expected.")
#define ERROR_PID_VALID_EXPECTED _W("PID valid expected.")
#define ERROR_COMMAND_NOT_SENT _W("Command not sent: {0}.")
#define ERROR_3_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME                                        \
    _W("#3 Argument must contain a valid variable name.")
#define ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_VARIABLE_NAME                                        \
    _W("#4 Argument must contain a valid variable name.")
#define ERROR_VARIABLE_NOT_SENT _W("Variable not sent.")
#define ERROR_4_ARGUMENT_MUST_CONTAIN_A_VALID_SCOPE_NAME                                           \
    _W("#4 Argument must contain a valid scope name.")
#define ERROR_2_PARAMETER_INVALID_PUT_GET_POST_OR_ISVAR_PARAMETER_EXPECTED                         \
    _W("#2 parameter invalid: 'put', 'get', 'post' or 'isvar' parameter expected.")
#define ERROR_2_PARAMETER_INVALID_PUT_PARAMETER_EXPECTED                                           \
    _W("#2 parameter invalid: 'put' parameter expected.")
#define ERROR_A_VALID_FUNCTION_DEFINITION_EXPECTED_WITH_NAME                                       \
    _W("a valid function definition expected.\n{0}")
#define ERROR_A_VALID_FUNCTION_DEFINITION_EXPECTED _W("a valid function definition expected.")
#define ERROR_SIMPLE_EXPRESSION_EXPECTED _W("simple expression expected.")
#define ERROR_COMPLEX_CANNOT_BE_CONVERTED_TO_LOGICAL _W("Complex cannot be converted to logical.")
#define ERROR_UNABLE_TO_CONVERT_VARIABLE_TYPE_TO_TEST_FOR_IF_WHILE_STATEMENT                       \
    _W("Unable to convert variable type to test for if/while statement")
#define ERROR_RIGHT_HAND_VALUES_MUST_SATISFY_LEFT_HAND_SIDE_EXPRESSION                             \
    _W("Right hand values must satisfy left hand side expression.")
#define ERROR_TYPE_FUNCTION_NOT_VALID _W("Type function not valid.")
#define ERROR_SUBTYPES_AND_SUBSINDICES_MUST_HAVE_THE_SAME_SIZE                                     \
    _W("Subtypes and subsindices must have the same size.")
#define ERROR_ASSIGNMENT_OPERATION_FAILED _W("Assignment operation failed.")
#define ERROR_INVALID_LHS _W("Invalid LHS.")
#define ERROR_ILLEGAL_USE_OF_THE_OPERATOR _W("Illegal use of the ':' operator")
#define ERROR_EXECUTION_OF_THE_FUTURE_WAS_CANCELLED _W("Execution of the future was cancelled.")
#define ERROR_OUT_OF_ORDER_ARGUMENT                                                                \
    _W("out-of-order argument {0} is not defined in the called function!")
#define ERROR_INSUFFICIENT_NUMBER_OF_OUTPUTS _W("Insufficient number of outputs.")
#define ERROR_CASE_NOT_MANAGED _W("Case not managed.")
#define ERROR_UNABLE_TO_RESOLVE _W("unable to resolve {0} to a function call.")
#define ERROR_UNDEFINED_VARIABLE_OR_FUNCTION _W("Undefined variable or function: {0}.")
#define ERROR_CANNOT_REINDEX_AN_EXPRESSION_THAT_RETURNS_MULTIPLE_VALUES                            \
    _W("Cannot reindex an expression that returns multiple values.")
#define ERROR_INVALID_INDEXING _W("Invalid indexing.")
#define ERROR_INDEX_EXPECTED _W("index expected.")
#define ERROR_PLEASE_DEFINE _W("Please define: {0}.")
#define ERROR_UNDEFINED_VARIABLE _W("Undefined variable:")
#define ERROR_SYNTAX_ERROR _W("Syntax error.")
#define ERROR_THE_SPECIAL_VARIABLE_VARARGOUT_WAS_NOT_DEFINED_AS_A_CELL_ARRAY                       \
    _W("The special variable 'varargout' was not defined as a cell-array.")
#define ERROR_THE_SPECIAL_VARIABLE_VARARGOUT_WAS_NOT_DEFINED_AS_EXPECTED                           \
    _W("The special variable 'varargout' was not defined as expected.")
#define ERROR_NOT_ENOUGH_OUTPUTS_IN_VARARGOUT_TO_SATISFY_CALL                                      \
    _W("Not enough outputs in varargout to satisfy call.")
#define ERROR_CANNOT_OPEN_WITH_FILENAME _W("Cannot open:\n {0}")
#define ERROR_OPERANDS_MUST_BE_REAL _W("Operands must be real.")
#define ERROR_ATTEMPT_TO_CONVERT_TO_UNIMPLEMENTED_SPARSE_TYPE                                      \
    _W("Attempt to convert to unimplemented sparse type")
#define ERROR_COMPLEX_VALUES_CANNOT_BE_CONVERTED_TO_CHARS                                          \
    _W("Complex values cannot be converted to chars.")
#define ERROR_POSITIVE_INTEGRAL_POWERS_EXPECTED _W("Positive integral powers expected.")
#define ERROR_COMPLEX_VALUES_CANNOT_CONVERTED_TO_LOGICALS                                          \
    _W("Complex values cannot converted to logicals.")
#define ERROR_OPERAND_TO_OPERATOR_MUST_BE_CONVERTIBLE_TO_LOGICAL_SCALAR_VALUES                     \
    _W("Operand to || operator must be convertible to logical scalar values.")
#define ERROR_FORMAT_NOT_SUPPORTED_HTML_OR_MD_EXPECTED                                             \
    _W("format not supported: 'html', or 'md' expected.")
#define ERROR_VALID_DEFLATE_VALUE_EXPECTED _W("Valid deflate value expected.")
#define ERROR_VALID_FILENAME_EXPECTED _W("Valid filename expected.")
#define ERROR_VALID_DATA_SET_NAME_EXPECTED _W("Valid data set name expected.")
#define ERROR_CHUNKSIZE_REQUIRED _W("ChunkSize required.")
#define ERROR_FILLVALUE_AND_DATA_SET_CLASS_MUST_BE_SAME                                            \
    _W("FillValue and data set class must be same.")
#define ERROR_LENGTH_CHUNKSIZE_AND_SIZE_MUST_BE_EQUAL _W("Length ChunkSize and Size must be equal.")
#define ERROR_CHUNKSIZE_LARGER_THAN_SIZE _W("ChunkSize larger than Size.")
#define ERROR_CHUNKSIZE_EXPECTED _W("ChunkSize expected.")
#define ERROR_HDF5_FORMAT_FILE_EXPECTED _W("HDF5 format file expected.")
#define ERROR_OPEN_FILE_FAILED _W("Open file failed.")
#define ERROR_DATA_SET_ALREADY_EXISTS _W("data set already exists.")
#define ERROR_H5SCREATE_SIMPLE_FAILS _W("H5Screate_simple fails.")
#define ERROR_H5PSET_LAYOUT_FAILS _W("H5Pset_layout fails.")
#define ERROR_H5PSET_CHUNK_FAILS _W("H5Pset_chunk fails.")
#define ERROR_H5DCREATE_FAILS _W("H5Dcreate fails.")
#define ERROR_INVALID_FILE_FORMAT _W("Invalid file format.")
#define ERROR_VALID_LOCATION_EXPECTED _W("Valid location expected.")
#define ERROR_VALID_ATTRIBUTE_NAME_EXPECTED _W("Valid attribute name expected.")
#define ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED                                   \
    _W("Specified HDF5 object location could not be opened.")
#define ERROR_ATTRIBUTE_NAME_NOT_FOUND _W("Attribute name not found.")
#define ERROR_ATTRIBUTE_HAVE_AN_INVALID_TYPE _W("Attribute have an invalid type.")
#define ERROR_IMPOSSIBLE_TO_READ_DIMENSIONS_AND_MAXIMUM_SIZE_OF_DATA_SET                           \
    _W("Impossible to read dimensions and maximum size of data set.")
#define ERROR_IMPOSSIBLE_TO_OPEN_HDF5_FILE _W("Impossible to open hdf5 file.")
#define ERROR_IMPOSSIBLE_TO_READ_DATA_SET _W("Impossible to read data set")
#define ERROR_CANNOT_REPLACE_FILE _W("Cannot replace file")
#define ERROR_INVALID_FILE_VERSION _W("Invalid file version.")
#define ERROR_CANNOT_OPEN_HDF5_FILE_EXPECTED _W("Cannot open HDF5 file expected.")
#define ERROR_CANNOT_CREATE_DATA_SET _W("Cannot create data set.")
#define ERROR_CANNOT_WRITE_DATA_SET _W("Cannot write data set.")
#define ERROR_DOUBLE_EXPECTED _W("double expected.")
#define ERROR_ROW_VECTOR_EXPECTED _W("row vector expected.")
#define ERROR_SCALAR_VALUE_EXPECTED _W("Scalar value expected.")
#define ERROR_UNSUPPORTED_VALUE_TYPE _W("Unsupported value type.")
#define ERROR_WRONG_TEXT_ENCODING_PARAMETER _W("Wrong text encoding parameter.")
#define ERROR_INVALID_NELSON_PARAMETER _W("Invalid parameter.")
#define ERROR_TEXTENCODING_EXPECTED _W("'TextEncoding' expected.")
#define ERROR_HANDLES_BEING_CATENATED_HAVE_INCOMPATIBLE_CLASSES                                    \
    _W("Handles being catenated have incompatible classes.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT _W("Wrong value for #1 argument.")
#define ERROR_LOOK_AND_FEEL_NOT_APPLIED _W("look and feel not applied.")
#define ERROR_CHAR_CELL_OR_STRING_EXPECTED _W("char, cell, or string expected.")
#define ERROR_THE_INPUT_ARGUMENT_MUST_BE_A_STRING _W("The input argument must be a string.")
#define ERROR_FIGURE_NUMBER_IS_OUT_OF_RANGE_IT_MUST_BE_BETWEEN_1_AND_2147483647                    \
    _W("figure number is out of range - it must be between 1 and 2147483647.")
#define ERROR_PARENT_DIRECTORY_DOES_NOT_EXIST _W("Parent directory does not exist: {0}")
#define ERROR_UNSUPPORTED_FORMAT _W("Unsupported format: {0}.")
#define ERROR_IMPOSSIBLE_TO_SAVE_IMAGE _W("Impossible to save image.")
#define ERROR_UNITS_NOT_MANAGED _W("Units not managed: {0}")
#define ERROR_CASE_NOT_YET_MANAGED _W("Case not yet managed.")
#define ERROR_EXPECTING_A_COLOR_SPEC_EITHER_A_COLOR_NAME_OR_A_3_VECTOR_OR_RGB_VALUES               \
    _W("Expecting a color spec: either a color name or a 3-vector or RGB values")
#define ERROR_EXPECT_AN_M_X_3_MATRIX_FOR_COLOR_ORDERS _W("Expect an m x 3 matrix for color orders.")
#define ERROR_COLOR_VECTOR_MUST_BE_BETWEEN_0_AND_1 _W("Color vector must be between 0 and 1.")
#define ERROR_PLEASE_CLOSE_SOME_GRAPHICAL_WINDOWS _W("Please close some graphical windows.")
#define ERROR_EXPECTING_HANDLE_FOR_PROPERTY _W("Expecting handle for property.")
#define ERROR_INVALID_FIGURE_ID _W("Invalid figure id.")
#define ERROR_THE_VALUE_MUST_BE_A_DELIMITED_CHARACTER_VECTOR_STRING_ARRAY_OR_CELL_ARRAY            \
    _W("The value must be a '|' delimited character vector, string array, or cell array")
#define ERROR_NX3_DIMENSIONAL_MATRIX_EXPECTED_FOR_VERTICES                                         \
    _W("Nx3 dimensional matrix expected for 'Vertices'.")
#define ERROR_INVALID_FACE_COLORSPEC _W("Invalid Face Colorspec.")
#define ERROR_INVALID_EDGECOLOR_PARAMETER _W("Invalid EdgeColor parameter.")
#define ERROR_INVALID_FACEVERTEXCDATA_PARAMETER_WITH_EGDECOLOR_TO_FLAT                             \
    _W("Invalid FaceVertexCData parameter with EgdeColor to 'flat'.")
#define ERROR_INVALID_FACEVERTEXCDATA_PARAMETER_WITH_EGDECOLOR_TO_INTERP                           \
    _W("Invalid FaceVertexCData parameter with EgdeColor to 'interp'.")
#define ERROR_VERTEX_INDEX_OUT_OF_BOUNDS _W("Vertex Index out of bounds.")
#define ERROR_EXPECTING_A_STRING_FOR_PROPERTY _W("Expecting a string for property.")
#define ERROR_ILLEGAL_SELECTION_FOR_PROPERTY _W("Illegal selection for property.")
#define ERROR_FLAT_EXPECTED _W("'flat' expected.")
#define ERROR_INVALID_IMAGE_FORMAT_EXPECTED_A_3D_ARRAY_WITH_3_COLOR_CHANNELS                       \
    _W("Invalid image format: expected a 3D array with 3 color channels.")
#define ERROR_INVALID_IMAGE_FORMAT_ARRAY_WITH_VALUES_IN_0_1_EXPECTED                               \
    _W("Invalid image format: array with values in [0. 1.] expected.")
#define ERROR_FINITE_VALUE_EXPECTED _W("Finite value expected.")
#define ERROR_RESTRICTED_STRING_SCALAR_DEFAULT_FAILED_LOOKUP_OF                                    \
    _W("Restricted string/scalar default failed lookup of: <{0}>.")
#define ERROR_RESTRICTED_STRING_COLOR_DEFAULT_FAILED_LOOKUP_OF                                     \
    _W("Restricted string/color default failed lookup of:")
#define ERROR_SET_RESTRICTED_STRING_DEFAULT_FAILED_LOOKUP_OF                                       \
    _W("Set restricted string default failed lookup of:")
#define ERROR_INVALID_GRAPHICS_OBJECT_TYPE_NAME _W("Invalid Graphics object type name: {0}")
#define ERROR_UICONTROL_GRAPHIC_OBJECT_EXPECTED _W("uicontrol graphic object expected.")
#define ERROR_FIGURE_OR_UICONTROL_GRAPHIC_OBJECT_EXPECTED                                          \
    _W("Figure or uicontrol graphic object expected.")
#define ERROR_FIGURE_EXPECTED _W("Figure expected.")
#define ERROR_PROPERTY_IS_READABLE_ONLY _W("Property is readable only: {0}")
#define ERROR_GOT_ERROR_FOR_PROPERTY _W("Got error for property:")
#define ERROR_SINGLE_ARGUMENT_TO_AXES_FUNCTION_MUST_BE_HANDLE_FOR_A_FIGURE                         \
    _W("Single argument to axes function must be handle for a figure.")
#define ERROR_FAILED_TO_CAST_GRAPHICSOBJECT_TO_GOFIGURE                                            \
    _W("Failed to cast GraphicsObject to GOFigure.")
#define ERROR_SINGLE_ARGUMENT_TO_AXES_FUNCTION_MUST_BE_HANDLE_FOR_AN_AXES                          \
    _W("Single argument to axes function must be handle for an axes.")
#define ERROR_FIGURE_GRAPHICS_OBJECT_EXPECTED _W("figure graphics object expected.")
#define ERROR_INVALID_FIGURE_HANDLE _W("Invalid Figure handle.")
#define ERROR_WRONG_NUMBER_OF_INPUT_PARAMETERS _W("Wrong number of input parameters.")
#define ERROR_HGGROUP_EXPECTED _W("hggroup expected.")
#define ERROR_PARENT_SHOULD_HAVE_ONLY_ONE_GRAPHICS_OBJECT                                          \
    _W("Parent should have only one graphics object.")
#define ERROR_EXPECTED_GRAPHICS_OBJECT_S _W("Expected graphics object(s).")
#define ERROR_INVALID_OBJECT _W("Invalid object.")
#define ERROR_AXES_GRAPHICS_OBJECT_EXPECTED _W("axes graphics object expected.")
#define ERROR_INVALID_2_ARGUMENT_ONE_MULTIPLE_EXPECTED                                             \
    _W("Invalid #2 argument: 'one', 'multiple' expected.")
#define ERROR_CANNOT_GET_CURRENT_FIGURE _W("Cannot get current figure.")
#define ERROR_EXPECTED_GRAPHICS_OBJECT _W("Expected graphics object.")
#define ERROR_VALID_GRAPHICS_OBJECT_EXPECTED _W("Valid graphics object expected.")
#define ERROR_AXES_GRAPHIC_OBJECT_EXPECTED _W("Axes graphic object expected.")
#define ERROR_WRONG_TYPE_FOR_2_ARGUMENT _W("wrong type for #2 argument.")
#define ERROR_SINGLE_SCALAR_ARGUMENT_MUST_BE_2_OR_3 _W("Single scalar argument must be 2 or 3.")
#define ERROR_VECTOR_AZ_EL_OR_X_Y_Z_EXPECTED _W("Vector [az, el] or [x, y, z] expected.")
#define ERROR_CASE_NOT_MANAGED_PLEASE_REPORT _W("Case not managed. Please report.")
#define ERROR_FUNCTION_MACRO_NAME_NOT_FOUND _W("function macro name not found.")
#define ERROR_NOT_AN_EXISTING_DIRECTORY _W("Not an existing directory: {0}")
#define ERROR_2_ARGUMENT_MUST_BE_ALL_OR_MODULE _W("#2 Argument must be '-all' or  '-module'.")
#define ERROR_S_EXPECTED _W("'s' expected.")
#define ERROR_CANNOT_APPLY_WISDOM _W("Cannot apply wisdom.")
#define ERROR_WRONG_VALUE_FOR_1_LOAD_OR_FREE_EXPECTED                                              \
    _W("Wrong value for #1: 'load' or 'free' expected.")
#define ERROR_WRONG_VALUE_FOR_1_LOAD_EXPECTED _W("Wrong value for #1: 'load' expected.")
#define ERROR_FIRST_INPUT_ARGUMENT_MUST_BE_A_VALID_MESSAGE_IDENTIFIER                              \
    _W("First input argument must be a valid message identifier.")
#define ERROR_WRONG_VALUE_FOR_2_INPUT_ARGUMENT_X_EXPECTED                                          \
    _W("Wrong value for #2 input argument: '{0}' expected.")
#define ERROR_RESET_OR_LAST_VALUE_EXPECTED _W("'reset' or 'last' value expected.")
#define ERROR_MEXCEPTION_EXPECTED _W("MException expected.")
#define ERROR_MEXCEPTION_SCALAR_EXPECTED _W("MException scalar expected.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_VALID_WARNING_STRUCT_EXPECTED                             \
    _W("Wrong value for #1 argument: valid warning struct expected.")
#define ERROR_WARNING_QUERY_DOES_NOT_REQUIRE_AN_SECOND_ARGUMENT                                    \
    _W("warning('query') does not require an second argument.")
#define ERROR_UNKNOWN_MODE _W("unknown mode.")
#define ERROR_UNDEFINED_FUNCTION_ISFINITE_FOR_INPUT_ARGUMENTS_OF_TYPE                              \
    _W("Undefined function 'isfinite' for input arguments of type '{0}'.")
#define ERROR_UNDEFINED_FUNCTION_ISINF_FOR_INPUT_ARGUMENTS_OF_TYPE                                 \
    _W("Undefined function 'isinf' for input arguments of type '{0}'.")
#define ERROR_UNDEFINED_FUNCTION_ISNAN_FOR_INPUT_ARGUMENTS_OF_TYPE                                 \
    _W("Undefined function 'isnan' for input arguments of type '{0}'.")
#define ERROR_UNDEFINED_FUNCTION_MOD_FOR_COMPLEX_INPUT_ARGUMENT                                    \
    _W("Undefined function 'mod' for complex input argument.")
#define ERROR_UNDEFINED_FUNCTION_MOD_FOR_SPARSE_INPUT_ARGUMENT                                     \
    _W("Undefined function 'mod' for sparse input argument.")
#define ERROR_INTEGERS_TYPE_NOT_MANAGED _W("Integers type not managed.")
#define ERROR_INTEGERS_MUST_BE_COMBINED_WITH_INTEGERS_OF_THE_SAME_CLASS                            \
    _W("Integers must be combined with integers of the same class.")
#define ERROR_FIRST_ARGUMENT_MUST_BE_REAL _W("First argument must be real.")
#define ERROR_UNDEFINED_FUNCTION_REM_FOR_COMPLEX_INPUT_ARGUMENT                                    \
    _W("Undefined function 'rem' for complex input argument.")
#define ERROR_UNDEFINED_FUNCTION_REM_FOR_SPARSE_INPUT_ARGUMENT                                     \
    _W("Undefined function 'rem' for sparse input argument.")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_FULLPATHEXT_OR_FULLPATH_EXPECTED                          \
    _W("Wrong value for #1 argument, \'fullpathext\' or  \'fullpath\' expected.")
// These messages were converted from direct Error(_W("...") ) calls
#define ERROR_UNHASHABLE_TYPE _W("Unhashable type {0}.")
#define ERROR_UNEXPECTED_FORMAT _W("unexpected format.")
#define ERROR_UNEXPECTED_NUMERIC_FORMAT _W("unexpected Numeric Format.")
#define ERROR_UNEXPECTED_LINE_SPACING _W("unexpected Line Spacing.")
#define ERROR_WRONG_SIZE_ARG1_SCALAR_EXPECTED _W("Wrong size for argument #1. scalar expected")
#define ERROR_WRONG_TYPE_ARG1_SCALAR_STRING_OR_ROW_CHAR_EXPECTED                                   \
    _W("Wrong type for argument #1. 'scalar string or row char vector expected")
#define ERROR_WRONG_TYPE_ARG2_SCALAR_STRING_OR_ROW_CHAR_EXPECTED                                   \
    _W("Wrong type for argument #2. 'scalar string or row char vector expected")
#define ERROR_CANNOT_CREATE_DESTINATION_FILE _W("Cannot create destination file.")
#define ERROR_CANNOT_INITIALIZE_WEBWRITE _W("Cannot initialize webwrite.")
#define ERROR_CANNOT_INITIALIZE_WEBSAVE _W("Cannot initialize websave.")
#define ERROR_INVALID_HEADERFIELDS_SIZE _W("Invalid HeaderFields size.")
#define ERROR_WEBOPTIONS_OBJECT_EXPECTED _W("weboptions object expected.")
#define ERROR_WRONG_VALUE_ARG2_DTD_EXPECTED                                                        \
    _W("Wrong value for argument #2: An existing .dtd file expected.")
#define ERROR_WRONG_VALUE_ARG1_XML_DOC_EXPECTED                                                    \
    _W("Wrong value for argument #1: An existing .xml documentation file expected.")
#define ERROR_INVALID_DATE_FORMAT_DUPLICATED_FIELD _W("Invalid date format (duplicated field): {0}")
#define ERROR_FAILED_TO_CONVERT_TEXT_TO_DATE_NUMBER _W("Failed to convert text to date number.")
#define ERROR_VECTOR_DOUBLE_CHAR_OR_STRING_ARRAY_EXPECTED                                          \
    _W("vector double, character vector or string array expected.")
#define ERROR_FIRST_INPUT_ARG_DATE_CHAR_OR_STRING                                                  \
    _W("First input argument must be a date character vector or a string.")
#define ERROR_SECOND_ARG_CHAR_STRING_OR_SCALAR_NUMERIC                                             \
    _W("Second argument must be a character vector, a string or scalar numerica value.")
#define ERROR_INVALID_VECTOR_SIZE_MUST_BE_COMPATIBLE _W("Invalid vector size must be compatible")
#define ERROR_EVALUATOR_NOT_DEFINED _W("evaluator is not defined.")
#define ERROR_CANNOT_CALL_TOC _W("Cannot call toc.")
#define ERROR_TIC_TOC_REQUIREMENT                                                                  \
    _W("You must call 'tic' without an output argument before calling 'toc' ")
#define ERROR_NONE_OF_THE_STANDARD_FORMATS_MATCH_THE_DATE_STRING                                   \
    _W("None of the standard formats match the DATE string.")
#define ERROR_TYPE_NOT_SUPPORTED_EXTENDED                                                          \
    _W("Type not supported. Only char, string, numeric or logical allowed.")
#define ERROR_FAILED_TO_CREATE_EVALUATOR _W("Failed to create evaluator: {0}")
#define ERROR_LAST_ARGUMENT_MUST_BE_POSITIVE_INTEGER_OR_ALLOWS_ALL_EMPTIES                         \
    _W("The last argument must be a positive integer or 'allows-all-empties'.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_ALLOWS_ALL_EMPTIES                                           \
    _W("Second argument must be a 'allows-all-empties'.")
#define ERROR_IJV_VECTORS_MUST_BE_SAME_SIZE_OR_SCALARS                                             \
    _W("in I, J, V format, all three vectors must be the same size or be scalars.")
#define ERROR_DENSE_TYPE_EXPECTED _W("dense type for all input arguments expected.")
#define ERROR_INPUTS_MUST_BE_REAL _W("Inputs arguments must be real.")
#define ERROR_X_AND_V_MUST_BE_SAME_LENGTH _W("X and V must be of the same length.")
#define ERROR_INPUT_MUST_BE_NONNEGATIVE _W("Input must be nonnegative.")
// stream_manager added messages
#define ERROR_INVALID_FSCANF_FORMAT _W("Invalid format.")
#define ERROR_UNSUPPORTED_FSCANF_FORMAT _W("Unsupported fscanf format.")
#define ERROR_SSCANF_INTERNAL_ERROR _W("sscanf internal error.")
#define ERROR_UNABLE_TO_OPEN_FILE _W("Unable to open file: {0}")
#define ERROR_A_VALID_FILENAME_EXPECTED _W("A valid filename expected.")
// Additional stream_manager messages used during migration
#define ERROR_VALID_FORMAT_EXPECTED _W("valid format expected.")
#define ERROR_ID_NOT_SUPPORTED _W("ID not supported.")
#define ERROR_WRONG_VALUE_FOR_MACHINE_FORMAT _W("Wrong value for machine format.")
#define ERROR_WRONG_VALUE_ARG3_NOT_SUPPORTED_PRECISION                                             \
    _W("Wrong value for #3 argument: not supported precision.")
#define ERROR_PROBLEM_TO_READ_DATA _W("Problem to read data.")
// Additional stream_manager messages
#define ERROR_INVALID_ORIGIN _W("Invalid origin.")
#define ERROR_REWIND_FAILED _W("Rewind failed.")
#define ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID_GENERIC                                         \
    _W("Not implemented for requested file identifier.")
#define ERROR_CANNOT_WRITE_REFERENCES_TYPE _W("Cannot write references type.")
#define ERROR_CANNOT_WRITE_SPARSE_TYPE _W("Cannot write sparse type.")
#define ERROR_ENDIAN_CONVERSION_NOT_SUPPORTED_FOR_FILEID                                           \
    _W("Endian conversion not supported for this file identifier.")
#define ERROR_ENCODING_CONVERSION_NOT_SUPPORTED_FOR_FILEID                                         \
    _W("encoding conversion not supported for this file identifier.")
// Additional stream_manager messages added during migration
#define ERROR_WRONG_VALUE_GE_0_EXPECTED _W("Wrong value >= 0 expected.")
#define ERROR_WRONG_SIZE_SCALAR_OR_A_B_EXPECTED _W("Wrong size. scalar or [a, b] expected.")
#define ERROR_WRONG_TYPE_DOUBLE_EXPECTED _W("Wrong type. double expected.")
#define ERROR_NOT_IMPLEMENTED_FOR_STDIO _W("Not implemented for 'stdout', 'stderr' or 'stdin'.")
// Additional stream_manager messages (migration additions)
#define ERROR_VALID_OPTION_EXPECTED _W("Valid option expected.")
#define ERROR_VALID_VARIABLE_NAME_EXPECTED _W("Valid variable name expected.")
#define ERROR_SAVE_FUNCTION_EXPECTED _W("save function expected.")
#define ERROR_LOAD_FUNCTION_EXPECTED _W("load function expected.")
#define ERROR_FILENAME_IS_EMPTY _W("Filename is empty")
#define ERROR_FIRST_ARGUMENT_MUST_BE_TEXT_SCALAR _W("First argument must be a text scalar.")
// Debugger module messages
#define ERROR_EVALUATOR_IS_NULL _W("Evaluator is null.")
#define ERROR_NO_ACTIVE_BREAKPOINT _W("No active breakpoint.")
#define ERROR_N_MUST_BE_POSITIVE_INTEGER _W("N must be a positive integer.")
#define ERROR_COMPLETENAMES_EXPECTED _W("'-completenames' expected.")
#define ERROR_COMPLETENAMES_OR_SCALAR_INT_EXPECTED                                                 \
    _W("'-completenames' expected or scalar integer value required.")
#define ERROR_INVALID_FILE_ID_EXPECTED _W("Wrong value for #1 argument: a valid file ID expected.")
#define ERROR_DEBUGGER_NOT_ACTIVE _W("Debugger is not active.")
#define ERROR_NO_STEP_BREAKPOINT_SET _W("No step breakpoint is set.")
#define ERROR_UNKNOWN_MODE_FOR_DBSTEP _W("Unknown mode for dbstep.")
#define ERROR_INVALID_STRING_ARGUMENT_FOR_DBSTEP                                                   \
    _W("Invalid string argument for dbstep. Expected 'in' or 'out'.")
#define ERROR_INVALID_ARG_TYPE_FOR_DBSTEP                                                          \
    _W("Invalid argument type for dbstep. Expected numeric or string.")
#define ERROR_BREAKPOINTS_ONLY_IN_MACRO_FUNCTIONS                                                  \
    _W("Breakpoints can only be set in macro functions.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_IN _W("Second argument must be 'in'.")
// dynamic_link module messages
#define ERROR_DLSYM_HANDLE_EXPECTED _W("dlsym handle expected.")
#define ERROR_DLLIB_VALID_HANDLE_EXPECTED _W("dllib valid handle expected.")
// dynamic_link: cannot get library symbols
#define ERROR_CANNOT_GET_LIBRARY_SYMBOLS _W("Cannot get library symbols: {0}")
// QML engine messages added during migration
#define ERROR_PROPERTY_TYPE_NOT_MANAGED _W("property type not managed.")
#define ERROR_VECTOR_EXPECTED _W("vector expected.")
#define ERROR_VECTOR_1x2_EXPECTED _W("vector 1x2 expected.")
#define ERROR_VECTOR_1x3_EXPECTED _W("vector 1x3 expected.")
#define ERROR_VECTOR_1x4_EXPECTED _W("vector 1x4 expected.")
#define ERROR_VECTOR_1x6_EXPECTED _W("vector 1x6 expected.")
#define ERROR_MATRIX_3x3_EXPECTED _W("matrix 3x3 expected.")
#define ERROR_MATRIX_4x4_EXPECTED _W("matrix 4x4 expected.")
#define ERROR_TYPE_CONVERSION_TO_QVARIANT_NOT_MANAGED _W("Type conversion to QVariant not managed.")
#define ERROR_EMPTY_MATRIX_NOT_MANAGED _W("Empty matrix not managed.")
#define ERROR_DLLIB_HANDLE_EXPECTED _W("dllib handle expected.")
#define ERROR_LIBPOINTER_HANDLE_EXPECTED _W("libpointer handle expected.")
#define ERROR_CANNOT_LOAD_LIBRARY _W("Cannot load library: {0}")
#define ERROR_UNABLE_TO_IMPORT_FUNCTION_FFI _W("Unable to import function through FFI.")
#define ERROR_INVALID_ARGUMENT_TYPE _W("Invalid argument type: {0}")
#define ERROR_WRONG_TYPE_ARG1_DLLIB_HANDLE_EXPECTED                                                \
    _W("Wrong type for argument #1: dllib scalar handle expected.")
#define ERROR_MULTIPLE_POSSIBLE_SYMBOL_NAME_FOUND _W("Multiple possible symbol name found: {0}")
#define ERROR_INVALID_SYMBOL_NAME _W("Invalid symbol name: {0}")
#define ERROR_VOID_NOT_ALLOWED_AS_INPUT_TYPE _W("'void' not allowed as input type.")
#define ERROR_INVALID_ARG2_TYPE_EXPECTED _W("Invalid #2 argument type expected: {0}")
#define ERROR_INVALID_ARG2_SCALAR_EXPECTED _W("Invalid #2 argument scalar expected.")
#define ERROR_DATATYPE_AND_SIZE_MUST_BE_DEFINED                                                    \
    _W("The datatype and size of the value must be defined.")
#define ERROR_OFFSET_MUST_NOT_BE_GREATER_THAN_SIZE                                                 \
    _W("Offset must not be greater than the size of the pointer.")
#define ERROR_NULL_POINTER_CANNOT_BE_INCREMENTED _W("null pointer cannot be incremented.")
#define ERROR_VOID_CANNOT_BE_INCREMENTED _W("void cannot be incremented.")
#define ERROR_VOIDPTR_CANNOT_BE_INCREMENTED _W("voidPtr cannot be incremented.")
#define ERROR_LIBPOINTER_CANNOT_BE_INCREMENTED _W("libpointer cannot be incremented.")
#define ERROR_CANNOT_BE_INCREMENTED _W("{0} cannot be incremented.")
#define ERROR_INCOMPATIBLE_TYPES _W("Incompatible types {0} --> {1}")
#define ERROR_IMPOSSIBLE_TO_GET_CURRENT_DIRECTORY _W("Impossible to get current directory.")
#define ERROR_ALL_INPUTS_MUST_BE_STRINGS_OR_CELL_ARRAYS                                            \
    _W("All inputs must be strings, character vectors, or cell arrays of character vectors.")
#define ERROR_ALL_STRING_CELL_INPUTS_MUST_BE_SAME_SIZE_OR_SCALARS                                  \
    _W("All string and cell array inputs must be the same size or scalars.")
// files_folders_functions additional messages
#define ERROR_ARGUMENT_2_MUST_CONTAIN_VALID_STRING_PATH_FILENAME_OR_EXTENSION                      \
    _W("Argument #2 must contain a valid string 'path', 'filename' or 'extension' expected.")
// stream_manager: fclose messages
#define ERROR_CANNOT_CLOSE_FILES _W("Cannot close files.")
#define ERROR_WRONG_VALUE_ARG1_ALL_EXPECTED _W("Wrong value for #1: 'all' expected.")
// stream_manager: additional messages
#define ERROR_FERROR_CLEAR_EXPECTED _W("'clear' expected as second argument.")
#define ERROR_NOT_IMPLEMENTED_FOR_REQUESTED_FILEID                                                 \
    _W("Not implemented for requested file identifier.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_GREATER_THAN_ZERO                                            \
    _W("Second argument must be greater than zero.")
// stream_manager encoding message
#define ERROR_CANNOT_USE_ENCODING _W("Cannot to use encoding: {0}")
#define ERROR_DIARY_ERROR_USING_DIARY _W("Error using diary.")
#define ERROR_DIARY_ARG1_GET_EXPECTED _W("#1 Argument 'get' expected.")
#define ERROR_FILE_SOURCE_DOES_NOT_EXIST _W("File source does not exist.")
#define ERROR_DIRECTORY_SOURCE_DOES_NOT_EXIST _W("Directory source does not exist.")
#define ERROR_DIRECTORY_DESTINATION_DOES_NOT_EXIST _W("Directory destination does not exist.")
#define ERROR_A_CELL_OF_EXISTING_FILENAMES_EXPECTED _W("A cell of existing filenames expected.")
#define ERROR_F_EXPECTED _W("'f' expected.")
// Change directory message
#define ERROR_CANNOT_CHANGE_DIRECTORY _W("Cannot change directory '{0}'.")
#define ERROR_UNDEFINED_FUNCTION_IFFT_FOR_INPUT_ARGS                                               \
    _W("Undefined function 'ifft' for input arguments of type '{0}'.")
#define ERROR_UNDEFINED_FUNCTION_FFT_FOR_INPUT_ARGS                                                \
    _W("Undefined function 'fft' for input arguments of type '{0}'.")
#define ERROR_MPI_COMM_HANDLE_EXPECTED _W("MPI_Comm handle expected.")
#define ERROR_MPI_COMM_VALID_HANDLE_EXPECTED _W("MPI_Comm valid handle expected.")
#define ERROR_INVALID_COMMUNICATOR _W("Invalid communicator")
#define ERROR_IMAGE_DATA_MUST_BE_MxN_OR_MxNx3 _W("Image data must be either MxN or MxNx3.")
#define ERROR_CANNOT_SAVE_IMAGE_FILE _W("Cannot save image file: {0}")
#define ERROR_NOT_SUPPORTED_IMAGE_FORMAT _W("Not supported format: {0}")
#define ERROR_CANNOT_WRITE_FILE _W("Cannot write {0}")
#define ERROR_IMPOSSIBLE_READ_IMAGE_FILE _W("Impossible read image file.")
#define ERROR_UNSUPPORTED_FILE_IMAGE_FORMAT _W("Unsupported file image format.")
#define ERROR_PARENT_CANNOT_BE_MODIFIED _W("'parent' can not modified.")
#define ERROR_CHILDREN_CANNOT_BE_MODIFIED _W("'children' can not modified.")
#define ERROR_CLASSNAME_CANNOT_BE_MODIFIED _W("'className' can not modified.")
#define ERROR_QOBJECT_PARENT_EQUALS_CHILD _W("QObject parent egals to the child.")
#define ERROR_PROPERTY_CANNOT_BE_MODIFIED _W("'{0}' can not modified.")
#define ERROR_DIARY_ARG2_DIARY_OR_DIARYFILE_EXPECTED                                               \
    _W("#2 Argument 'Diary' or 'DiaryFile' expected.")
#define ERROR_DIARY_ARG1_SET_EXPECTED _W("#1 Argument 'set' expected.")
#define ERROR_DIARY_ARG3_ON_OFF_EXPECTED _W("#3 Argument 'on' or 'off' expected.")
#define ERROR_DIARY_ARG3_STRING_EXPECTED _W("#3 Argument a string expected.")
// general IO messages
#define ERROR_IMPOSSIBLE_TO_OPEN_FILE _W("Impossible to open file.")
// spreadsheet messages
// dlmwrite messages
#define ERROR_DLMWRITE_APPEND_EXPECTED _W("'-append' expected.")
#define ERROR_DLMWRITE_VALID_DELIMITER_EXPECTED _W("a valid delimiter expected.")
#define ERROR_DLMWRITE_PC_OR_UNIX_EXPECTED _W("'pc' or 'unix' expected.")
#define ERROR_CELL2MAT_RETURNS_MORE_THAN_ONE_OUTPUT                                                \
    _W("cell2mat returns more than one output argument.")
#define ERROR_CELL2MAT_FUNCTION_NOT_FOUND _W("cell2mat function not found.")
// dlmread messages
#define ERROR_DLMREAD_WRONG_VALUE_ARG2_SINGLE_CHAR_EXPECTED                                        \
    _W("Wrong value for argument #2: single character expected.")
#define ERROR_DLMREAD_WRONG_SIZE_FOR_3_ROW_VECTOR_EXPECTED                                         \
    _W("Wrong size for #3 argument. row vector expected.")
#define ERROR_DLMREAD_WRONG_VALUE_ARG3_VALID_RANGE_EXPECTED                                        \
    _W("Wrong value for #3 argument. valid range expected.")
#define ERROR_DLMREAD_WRONG_TYPE_FOR_3_NUMERIC_VALUES_EXPECTED                                     \
    _W("Wrong type for #3 argument. numeric values expected.")
#define ERROR_DLMREAD_WRONG_TYPE_FOR_3_DENSE_VALUES_EXPECTED                                       \
    _W("Wrong type for #3 argument. dense values expected.")
#define ERROR_DLMREAD_WRONG_VALUE_ARG3_INTEGER_EXPECTED                                            \
    _W("Wrong value for argument #3: integer value expected.")
#define ERROR_DLMREAD_WRONG_VALUE_ARG4_INTEGER_EXPECTED                                            \
    _W("Wrong value for argument #4: integer value expected.")
#define ERROR_IMPORT_OPTIONS_OBJECT_EXPECTED _W("Import options object expected.")
#define ERROR_OUTPUTTYPE_NAME_EXPECTED _W("OutputType name expected.")
#define ERROR_UNSUPPORTED_NELSON_TYPE _W("Unsupported type.")
#define ERROR_UNRECOGNIZED_FILE_EXTENSION _W("Unrecognized file extension.")
#define ERROR_QUOTESTRINGS_NOT_ALLOWED_FOR_XML _W("QuoteStrings not allowed for xml file type.")
#define ERROR_DELIMITER_NOT_ALLOWED_FOR_XML _W("Delimiter not allowed for xml file type.")
#define ERROR_WRITEMODE_NOT_ALLOWED_FOR_XML _W("WriteMode not allowed for xml file type.")
#define ERROR_ATTRIBUTESUFFIX_NOT_ALLOWED_FOR_TEXT                                                 \
    _W("AttributeSuffix not allowed for text file type.")
#define ERROR_ROWNODE_ALLOWED_ONLY_FOR_XML _W("RowNodeName allowed only for xml file type.")
#define ERROR_TABLENODE_ALLOWED_ONLY_FOR_XML _W("TableNodeName allowed only for xml file type.")
#define ERROR_INVALID_PARAMETER_NAME                                                               \
    _W("Invalid parameter name. The parameter name must be a non-empty string or character "       \
       "array.")
#define ERROR_DELIMITER_NOT_SUPPORTED _W("delimiter not supported.")
#define ERROR_QUOTE_STRINGS_NOT_SUPPORTED _W("Quote strings not supported.")
#define ERROR_ONLY_TEXT_CURRENTLY_SUPPORTED _W("Only text currently supported.")
#define ERROR_APPEND_OR_OVERWRITE_EXPECTED _W("'append' or 'overwrite' expected.")
#define ERROR_PROPERTY_NAME_NOT_SUPPORTED _W("Property name not supported.")
#define ERROR_WRONG_TYPE_ARG1_TABLE_EXPECTED _W("Wrong type for argument #1. table expected.")
// statistics module messages
#define ERROR_INPUTS_MUST_BE_2D _W("Inputs must be 2-D.")
#define ERROR_NATIVE_ACCUMULATION_ON_CHAR_ARRAY_NOT_SUPPORTED                                      \
    _W("Native accumulation on char array is not supported.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_0 _W("Second argument must be 0.")
// julia_engine added messages
#define ERROR_JULIA_ENGINE_NOT_INITIALIZED _W("Julia engine not initialized.")
#define ERROR_MAIN_MODULE_NOT_FOUND _W("Main module not found.")
#define ERROR_CANNOT_CONVERT_NELSON_VARIABLE_TO_JULIA _W("Cannot convert Nelson variable to Julia.")
#define ERROR_FILENAME_NOT_FOUND _W("File not found: {0}")
// string module
#define ERROR_WRONG_FORMAT_STRING _W("Wrong format string.")
#define ERROR_INVALID_DELIMITER_DIMENSIONS _W("Invalid delimiter dimensions.")
#define ERROR_INVALID_DELIMITER_TYPE _W("Invalid delimiter type.")
// more string module messages
#define ERROR_A_2D_MATRIX_EXPECTED _W("A 2D matrix expected.")
#define ERROR_INPUT_MUST_BE_TEXT_SCALAR _W("Input must be a text scalar.")
#define ERROR_CHAR_VECTOR_OR_CELL_EXPECTED _W("char vector or cell of strings expected.")
#define ERROR_CELL_OF_STRINGS_EXPECTED _W("Cell of strings expected.")
#define ERROR_ALL_INPUTS_MUST_BE_STRINGS                                                           \
    _W("All inputs must be strings, character vectors, or cell arrays of character vectors.")
#define ERROR_NAN_INF_NOT_ALLOWED _W("NaN and Inf not allowed.")
#define ERROR_EXPECTED_AN_INTEGER_VALUE _W("Expected a integer value.")
#define ERROR_NUMBER_INPUT_ARGS_MUST_MATCH_OUTPUT                                                  \
    _W("Number of Input arguments must the same as output.")
#define ERROR_CONVERSION_TO_CHAR_FROM_COMPLEX_NOT_POSSIBLE                                         \
    _W("Conversion to char from complex is not possible.")
#define ERROR_FIRST_ARGUMENT_MUST_BE_CHARACTER_ARRAY_OR_STRING                                     \
    _W("First argument must be a character array or string.")
#define ERROR_LOGICAL_ARGUMENT_MUST_BE_SCALAR _W("Logical argument must be a scalar.")
#define ERROR_INTEGER_ARGUMENT_MUST_BE_SCALAR _W("Integer argument must be a scalar.")
#define ERROR_NUMERIC_ARGUMENT_MUST_BE_SCALAR _W("Numeric argument must be a scalar.")
#define ERROR_CHARACTER_ARGUMENT_MUST_BE_ROW_VECTOR                                                \
    _W("Character argument must be a row character vector.")
#define ERROR_STRING_ARGUMENT_MUST_BE_SCALAR _W("String argument must be a scalar string.")
#define ERROR_UNSUPPORTED_ARGUMENT_TYPE_FOR_FORMATSTRING                                           \
    _W("Unsupported argument type for formatString.")
#define ERROR_INVALID_FORMAT_STRING _W("Invalid format string.")
#define ERROR_ALL_STRING_AND_CELL_INPUTS_MUST_BE_SAME_SIZE_OR_SCALARS                              \
    _W("All string and cell array inputs must be the same size or scalars.")
#define ERROR_NOT_ENOUGH_ARGUMENTS_FOR_FORMAT_STRING _W("Not enough arguments for format string.")
#define ERROR_WRONG_FORMAT_ARGUMENT_INDEX _W("Wrong format argument index.")
#define ERROR_INVALID_INPUT_ARGUMENTS_CELL_OR_STRING_EXPECTED                                      \
    _W("Invalid input argument(s): cell or string expected.")
#define ERROR_FORCECELLOUTPUT_EXPECTED _W("'ForceCellOutput' expected as third input argument.")
#define ERROR_SECOND_ARGUMENT_A_SINGLE_STRING_EXPECTED                                             \
    _W("Second argument a single string expected.")
#define ERROR_FIRST_ARGUMENT_MUST_BE_CELL_OF_STRINGS_OR_STRING                                     \
    _W("First argument must be a cell of strings (or a string) and second argument a single "      \
       "string expected.")
#define ERROR_INPUT_STRINGS_MUST_HAVE_ONE_ROW _W("Input strings must have one row.")
#define ERROR_WRONG_TYPE_FOR_ARGUMENT_X_STRING_ARRAY_OR_CHARACTER_VECTOR_OR_CELL_ARRAY_EXPECTED    \
    _W("Wrong type for argument #{}: string array or character vector or cell array of character " \
       "vectors expected.")
#define ERROR_EMPTY_EXPECTED_MESSAGE_NOT_ALLOWED _W("empty string not allowed as expected message.")
#define ERROR_NO_ERROR_PRODUCED_WHILE_EVALUATING_COMMAND                                           \
    _W("No error was produced while evaluating command.")
// string module messages
#define ERROR_PRECISION_MUST_BE_SCALAR_INT _W("PRECISION must be a scalar integer >= 0.")
// assert_functions messages
#define ERROR_ISEQUALTO_RETURNS_MORE_THAN_ONE_OUTPUT_ARG                                           \
    _W("isequalto returns more than one output argument.")
#define ERROR_ISEQUALTO_MUST_RETURN_LOGICAL _W("isequalto must return an logical.")
#define ERROR_ISEQUALTO_RETURNS_UNEXPECTED_ERROR _W("isequalto returns an unexpected error.")
#define ERROR_ISEQUALTO_FUNCTION_NOT_FOUND _W("isequalto function not found.")
#define ERROR_ISAPPROX_RETURNS_MORE_THAN_ONE_OUTPUT_ARG                                            \
    _W("isapprox returns more than one output argument.")
#define ERROR_ISAPPROX_MUST_RETURN_LOGICAL _W("isapprox must return an logical.")
#define ERROR_ISAPPROX_FUNCTION_NOT_FOUND _W("isapprox function not found.")
#define ERROR_ASSERTION_VALUES_TOO_DIFFERENT                                                       \
    _W("Assertion failed: expected and computed values are too different.")
#define ERROR_INPUTS_MUST_BE_SAME_SIZE_OR_SCALAR                                                   \
    _W("Inputs must be the same size or either one can be a scalar.")
// added for mat2str and strcat
#define ERROR_CLASS_ARGUMENT_EXPECTED _W("'class' argument expected.")
#define ERROR_SECOND_INPUT_ARGUMENT_MUST_BE_REAL_POSITIVE_INTEGER                                  \
    _W("Second input argument must be a real positive integers.")
#define ERROR_NUMERIC_MATRIX_EXPECTED _W("An numeric matrix expected.")
// added for matchesBuiltin
#define ERROR_WRONG_VALUE_ARG3_IGNORECASE_EXPECTED _W("Wrong value for #3: 'IgnoreCase' expected.")
// string builtin: join
#define ERROR_WRONG_TYPE_ARG3_STRING_CHAR_OR_CELL_EXPECTED                                         \
    _W("Wrong type for argument #3: string, characters or cell of characters expected.")
// string builtin: strjust
#define ERROR_WRONG_VALUE_ARG2_LEFT_RIGHT_CENTER_EXPECTED                                          \
    _W("Wrong value for #2 argument: 'left', 'right', 'center' expected.")
// audio module messages
#define ERROR_AUDIOPLAYER_VALID_HANDLE_EXPECTED _W("audioplayer valid handle expected.")
#define ERROR_WRONG_ARGUMENT_X_VALUE_WITH_NAME _W("Wrong value for #{} argument. {0}")
// audiorecorder messages
#define ERROR_AUDIORECORDER_VALID_HANDLE_EXPECTED _W("audiorecorder valid handle expected.")
#define ERROR_INVALID_AUDIORECORDER_HANDLE _W("Invalid audiorecorder handle.")
#define ERROR_UNSUPPORTED_DATA_TYPE_FOR_AUDIORECORDER_GETAUDIODATA                                 \
    _W("Unsupported data type for audiorecorder getaudiodata.")
#define ERROR_DURATION_IN_SECONDS_EXPECTED _W("duration in seconds expected.")
#define ERROR_DURATION_GT_ZERO_EXPECTED _W("duration > 0 expected.")
#define ERROR_START_GE_1_EXPECTED _W("start >= 1 expected.")
#define ERROR_INDEX_GE_1_EXPECTED _W("Index >= 1 expected.")
#define ERROR_SCALAR_OR_START_END_VECTOR_EXPECTED _W("scalar or [start, end] vector expected.")
#define ERROR_INVALID_RANGE _W("Invalid range.")
#define ERROR_CANNOT_SET_READ_ONLY_PROPERTY _W("Cannot set a read only property.")
#define ERROR_CANNOT_CREATE_AUDIORECORDER_HANDLE _W("Cannot create audiorecorder handle.")
// com_engine module messages
#define ERROR_COM_HANDLE_EXPECTED _W("COM handle expected.")
#define ERROR_COM_VALID_HANDLE_EXPECTED _W("COM valid handle expected.")
#define ERROR_ERROR_CLSID_FROM_STRING _W("Error CLSIDFromString.")
#define ERROR_ERROR_CLSID_FROM_PROGID _W("Error CLSIDFromProgID.")
#define ERROR_ERROR_COCREATEINSTANCEEX _W("Error CoCreateInstanceEx.")
#define ERROR_INVALID_PROGID _W("Invalid PROGID.")
#define ERROR_SERVER_NOT_RUNNING _W("Server is not running on this system.")
#define ERROR_FAILS_TO_CONNECT_TO_SERVER _W("Fails to connect to server.")
#define ERROR_CANNOT_READ_REGISTRY _W("Cannot read registry.")
#define ERROR_ONLY_7_INPUT_PARAMETERS_EXPECTED _W("Only 7 input parameters expected.")
// console module messages
#define ERROR_WRONG_TYPE_ARG1 _W("Wrong type for #1 argument.")
#define ERROR_UNRECOGNIZED_OPTION_S_EXPECTED _W("Unrecognized option. \"s\" expected.")
// com_engine added messages
#define ERROR_MACHINE_VALUE_EXPECTED _W("'machine' value expected.")
#define ERROR_NOT_IMPLEMENTED_ON_PLATFORM _W("Not implemented on this platform.")
#define ERROR_COM_FIELDNAMES_UNRECOGNIZED_OPTION _W("Unrecognized option. \"-full\" expected.")
// core module added messages
#define ERROR_EVALIN_ARG1_SCOPE_EXPECTED _W("Argument #1 : 'base', 'local' or 'caller' expected.")
#define ERROR_EXECSTR_ARG2_ERRCATCH_NOCATCH_EXPECTED _W("#2 'errcatch' or 'nocatch' expected.")
#define ERROR_INPUT_FUNCTION_NOT_ALLOWED_FROM_EVALC _W("input function not allowed from evalc.")
#define ERROR_INVALID_USE_OF_STATEMENT_LIST _W("Invalid use of statement list.")
#define ERROR_SCOPE_LOCAL_CALLER_BASE_EXPECTED _W("'local', 'caller', 'base' scope expected.")
#define ERROR_INPUTNAME_NOT_IN_ACTIVE_FUNCTION                                                     \
    _W("Cannot return input name if not in an active function.")
#define ERROR_ARGUMENT_NUMBER_NOT_VALID _W("Argument number is not valid.")
#define ERROR_NARGIN_NOT_ALLOWED_IN_BASE_SCOPE _W("not allowed in base scope.")
#define ERROR_NARGOUT_NOT_ALLOWED_IN_BASE_SCOPE _W("not allowed in base scope.")
#define ERROR_NARGOUTCHK_NO_OUTPUT_ARGS_IF_TWO_INPUTS                                              \
    _W("No output arguments are allowed if only two input arguments.")
#define ERROR_NARGOUTCHK_ONLY_FROM_NELSON_FUNCTION                                                 \
    _W("You can only call 'nargoutchk' from within a Nelson function.")
#define ERROR_NARGOUTCHK_ARG4_STRUCT_OR_STRING_EXPECTED                                            \
    _W("#4 input must be either 'struct' or 'string'.")
#define ERROR_NARGINCHK_ONLY_FROM_NELSON_FUNCTION                                                  \
    _W("You can only call 'narginchk' from within a Nelson function.")
#define ERROR_WRONG_VALUE_ARG1 _W("Wrong value for #1 argument.")
#define ERROR_UNKNOWN_OPTION _W("Unknown option.")
#define ERROR_VALUE_BETWEEN_0_255_EXPECTED _W("Value between 0 and 255 expected.")
// constructors_functions module messages
#define ERROR_DIAG_SECOND_ARG_SCALAR_EXPECTED _W("Second argument must be a scalar.")
#define ERROR_DIAG_FIRST_ARG_2D_EXPECTED _W("First argument to 'diag' function must be 2D.")
// ArrayOf constructors messages
#define ERROR_INPUT_MUST_BE_2D _W("Input must be 2-D")
#define ERROR_SPARSE_MATRIX_NOT_MANAGED _W("Sparse matrix not managed.")
#define ERROR_ARGUMENT_TO_DIAGONAL_CONSTRUCTOR_MUST_BE_VECTOR                                      \
    _W("Argument to diagonal constructor must be a vector!")
// constructors_functions additional messages
#define ERROR_SUPPORTED_TYPE_EXPECTED_LAST_ARGUMENT                                                \
    _W("A supported type expected at last argument.")
// conversion / sparse support
#define ERROR_N_DIMENSIONAL_ARRAYS_NOT_SUPPORTED _W("N-dimensional arrays are not supported.")
#define ERROR_SIZE_VECTOR_ROW_REAL_ELEMENTS_EXPECTED                                               \
    _W("Size vector should be a row vector with real elements.")
#define ERROR_INPUT_FOLLOWING_LIKE_NOT_NUMERIC_ARRAY                                               \
    _W("Input following 'like' is not a numeric array.")
// sparse module messages
#define ERROR_UNSUPPORTED_TYPE_IN_EYE_SPARSE_MATRIX_CONSTRUCTOR                                    \
    _W("Unsupported type in EyeSparseMatrixConstructor.")
#define ERROR_UNSUPPORTED_TYPE_IN_MAKE_DENSE_ARRAY_OF _W("Unsupported type in MakeDenseArrayOf.")
#define ERROR_UNSUPPORTED_TYPE_IN_MAKE_SPARSE_ARRAY_OF _W("Unsupported type in MakeSparseArrayOf")
#define ERROR_UNSUPPORTED_TYPE_IN_COPYSPARSEMATRIX _W("Unsupported type in CopySparseMatrix.")
#define ERROR_INVALID_SPARSE _W("Invalid sparse.")
#define ERROR_UNSUPPORTED_TYPE_IN_COUNTNONZEROS _W("Unsupported type in CountNonzeros.")
#define ERROR_UNSUPPORTED_TYPE_IN_COUNTNONZERS_MAX _W("Unsupported type in CountNonzerosMax.")
#define ERROR_INDEX_EXCEEDS_VARIABLE_DIMENSIONS _W("Index exceeds variable dimensions.")
#define ERROR_UNSUPPORTED_TYPE_IN_GETSPARSENDIMSUBSETS                                             \
    _W("Unsupported type in GetSparseNDimSubsets.")
#define ERROR_REPEATED_INDICES_NOT_SUPPORTED_FOR_SPARSE_LOGICAL_MATRICES                           \
    _W("Repeated indices are not supported for sparse logical matrices.")
#define ERROR_UNSUPPORTED_TYPE_IN_SPARSETOIJV _W("Unsupported type in SparseToIJV.")
#define ERROR_UNSUPPORTED_TYPE_IN_SPARSE_MATRIX_CONSTRUCTOR                                        \
    _W("Unsupported type in SparseMatrixConstructor.")
#define ERROR_UNSUPPORTED_TYPE_IN_SETSPARSENDIMSUBSETS                                             \
    _W("Unsupported type in SetSparseNDimSubsets.")
#define ERROR_UNSUPPORTED_TYPE_IN_SETSARSEVECTOR_SUBSETS                                           \
    _W("Unsupported type in SetSparseVectorSubsets.")
#define ERROR_UNSUPPORTED_TYPE_IN_DELETE_SPARSE_MATRIX_COLS                                        \
    _W("Unsupported type in DeleteSparseMatrixCols.")
#define ERROR_UNSUPPORTED_TYPE_IN_DELETE_SPARSE_MATRIX_ROWS                                        \
    _W("Unsupported type in DeleteSparseMatrixRows.")
#define ERROR_UNSUPPORTED_TYPE_IN_DELETE_SPARSE_MATRIX_VECTOR_SUBSET                               \
    _W("Unsupported type in DeleteSparseMatrixVectorSubset.")
#define ERROR_UNSUPPORTED_TYPE_IN_TYPECONVERTSPARSE _W("Unsupported type in TypeConvertSparse.")
#define ERROR_UNSUPPORTED_TYPE_IN_TYPECONVERTSPARSE_COMPLEX_TO_LOGICAL                             \
    _W("Unsupported type in TypeConvertSparse (complex to logical).")
#define ERROR_UNSUPPORTED_TYPE_IN_TYPECONVERTSPARSE_COMPLEX_TO_DOUBLE                              \
    _W("Unsupported type in TypeConvertSparse (complex to double).")
#define ERROR_UNSUPPORTED_TYPE_IN_RESHAPESPARSEMATRIX _W("Unsupported type in ReshapeSparseMatrix.")
#define ERROR_CANNOT_DO_IMAG_WITH_CURRENT_TYPE _W("Cannot do imag with current type '{0}'.")
#define ERROR_CANNOT_DO_REAL_WITH_CURRENT_TYPE _W("Cannot do real with current type '{0}'.")
#define ERROR_CANNOT_DO_UMINUS_WITH_CURRENT_TYPE _W("Cannot do uminus with current type '{0}'.")
#define ERROR_INDEX_INTO_MATRIX_MUST_BE_POSITIVE _W("Index into matrix must be positive.")
#define ERROR_INDEX_EXCEEDS_MATRIX_DIMENSIONS _W("Index exceeds matrix dimensions.")
#define ERROR_INDEX_MUST_EITHER_BE_REAL_POSITIVE_INTEGERS_OR_LOGICALS                              \
    _W("index must either be real positive integers or logicals.")
#define ERROR_INDEX_EXCEEDS_DIMENSIONS _W("Index exceeds dimensions.")
#define ERROR_MULTIDIMENSIONAL_INDEXING_NOT_LEGAL_FOR_SPARSE                                       \
    _W("multidimensional indexing (more than 2 dimensions) not legal for sparse arrays")
// sparse creation messages
#define ERROR_CANNOT_MAKE_SPARSE _W("Cannot make sparse.")
#define ERROR_CANNOT_MAKE_STRINGS_OR_REFERENCE_TYPES_SPARSE                                        \
    _W("Cannot make strings or reference types sparse.")
#define ERROR_CANNOT_MAKE_ND_ARRAYS_SPARSE _W("Cannot make n-dimensional arrays sparse.")
// delete operation messages
#define ERROR_STATEMENT_A_CAN_ONLY_CONTAIN_ONE_NON_COLON_INDEX                                     \
    _W("Statement A(...) = [] can only contain one non-colon index.")
#define ERROR_SPARSE_MATRICES_DO_NOT_SUPPORT_DELETING_ND_PLANES                                    \
    _W("sparse matrices do not support deleting n-dimensional planes - Only 2-D")
// generic dimension messages
#define ERROR_INVALID_DIMENSIONS _W("Invalid dimensions.")
// types module auto-converted messages
#define ERROR_EMPTY_MATRIX_OF_TYPE_DOUBLE_EXPECTED _W("Empty matrix of type double expected.")
#define ERROR_SIZE_MISMATCH_ASSIGNMENT_A_I1_I2_IN_B                                                \
    _W("Size mismatch in assignment A(I1,I2,...,In) = B.")
#define ERROR_CANNOT_PROMOTE_TO_STRING_ARRAY _W("Cannot promote to string array.")
#define ERROR_CANNOT_PROMOTE_TO_FUNCTION_HANDLE_ARRAY _W("Cannot promote to function_handle array.")
#define ERROR_CANNOT_PROMOTE_TO_ANOTHER_CLASS_TYPE_ARRAY                                           \
    _W("Cannot promote to another class type array.")
#define ERROR_CANNOT_PROMOTE_TO_CLASS_ARRAY _W("Cannot promote to class array.")
#define ERROR_CANNOT_CONVERT_BASE_TYPES_TO_REFERENCE_TYPES                                         \
    _W("Cannot convert base types to reference types.")
// additional assignment messages
#define ERROR_ASSIGNMENT_A_COLON_B_REQUIRES_SAME_SIZE                                              \
    _W("Assignment A(:) = B requires A and B to be the same size")
#define ERROR_SIZE_MISMATCH_ASSIGNMENT_A_I_B _W("Size mismatch in assignment A(I) = B.")
// character / cell messages
#define ERROR_STRING_ARRAY_EXPECTED _W("String array expected.")
#define ERROR_CONVERSION_MISSING_TO_CHARACTER_VECTOR_NOT_SUPPORTED                                 \
    _W("Conversion <missing> to character vector is not supported.")
#define ERROR_UNABLE_TO_CONVERT_SUPPLIED_OBJECT_TO_A_STRING                                        \
    _W("Unable to convert supplied object to a string.")
#define ERROR_UNABLE_TO_CONVERT_SUPPLIED_OBJECT_TO_A_SINGLE_STRING                                 \
    _W("Unable to convert supplied object to a single string.")
#define ERROR_A_CELL_OR_STRING_ARRAY_EXPECTED _W("A cell or string array expected.")
#define ERROR_GETVECTORCONTENTS_NOT_SUPPORTED_FOR_SPARSE_ARRAYS                                    \
    _W("getVectorContents not supported for sparse arrays.")
// generic sparse expectation
#define ERROR_SPARSE_EXPECTED _W("Sparse expected.")
// handle type messages
#define ERROR_EXPECTED_HANDLE_SCALAR _W("Expected a handle scalar.")
#define ERROR_EXPECTED_VALID_HANDLE _W("Expected a valid handle.")
#define ERROR_EXPECTED_A_HANDLE _W("Expected a handle.")
// graphics object messages
#define ERROR_EXPECTED_AN_GRAPHICS_OBJECT _W("Expected an graphics object.")
#define ERROR_EXPECTED_AN_GRAPHICS_OBJECT_SCALAR _W("Expected an graphics object scalar.")
// function handle messages
#define ERROR_EXPECTED_A_FUNCTION_HANDLE _W("Expected a function_handle.")
// data_analysis (conv2, sum, etc.) messages
#define ERROR_CONV2_UNKNOWN_SHAPE_PARAMETER                                                        \
    _W("shape parameter must be 'full', 'same', or 'valid'.")
#define ERROR_CONV2_INVALID_DATA_TYPE_FIRST                                                        \
    _W("Invalid data type: First argument must be numeric or logical.")
#define ERROR_CONV2_INVALID_DATA_TYPE_SECOND                                                       \
    _W("Invalid data type: Second argument must be numeric or logical.")
#define ERROR_CONV2_INVALID_DATA_TYPE_THIRD                                                        \
    _W("Invalid data type: Third argument must be numeric or logical.")
#define ERROR_CONV2_ND_ARRAYS_NOT_SUPPORTED _W("N-D arrays are not supported.")
#define ERROR_CONV2_SPARSE_MATRICES_NOT_SUPPORTED _W("Sparse matrices are not supported.")
#define ERROR_FUNCTION_UNDEFINED _W("function {0} undefined.")
// unique cell input
#define ERROR_UNIQUE_INPUT_MUST_BE_CELL_OF_CHARACTER_VECTORS                                       \
    _W("The input must be a cell array containing character vectors.")
// sort module
#define ERROR_ONLY_ONE_INPUT_PARAMETER_SUPPORTED_FOR_CELL_ARRAYS                                   \
    _W("Only one input parameter is supported for cell arrays.")
// prod/sum/module generic
#define ERROR_INPUT_ARGUMENTS_MUST_HAVE_SAME_SIZE _W("Input Arguments must have same size.")
#define ERROR_DIMENSION_ARGUMENT_MUST_BE_POSITIVE_INTEGER_SCALAR                                   \
    _W("Dimension argument must be a positive integer scalar.")
// sort builtin messages
#define ERROR_SORT_DIRECTION_MUST_BE_ASCEND_OR_DESCEND                                             \
    _W("Sort direction must be either the string 'ascend' or 'descend'")
#define ERROR_SORT_COMPARISON_OR_MISSINGPLACEMENT_EXPECTED                                         \
    _W("'ComparisonMethod' or 'MissingPlacement' expected.")
#define ERROR_MISSINGPLACEMENT_EXPECTED_VALUES                                                     \
    _W("'auto', 'first', or 'last' for 'MissingPlacement' parameter.")
#define ERROR_COMPARISONMETHOD_EXPECTED_VALUES                                                     \
    _W("'auto', 'real', or 'abs' for 'ComparisonMethod' parameter.")
// core: function message for nargin/nargout support
#define ERROR_FUNCTION_DOES_NOT_KNOW_HOW_TO_ANSWER_NARGIN_NARGOUT                                  \
    _W("'{0}' does not know how to answer nargin/nargout.")
// unique builtin messages
#define ERROR_UNIQUE_SECOND_ARGUMENT_MUST_BE_ROWS _W("second argument to unique must be 'rows'")
#define ERROR_ROWS_MODE_ONLY_WORKS_FOR_2D_MATRIX _W("'rows' mode only works for 2D matrix.")
// random module messages
#define ERROR_NO_UNREAD_FUTURES _W("There are no unread Futures to fetch.")
#define ERROR_FUTURE_EXECUTION_ERROR _W("One or more futures resulted in an error.")
#define ERROR_FUTURE_CONCATENATE_OUTPUTS _W("Unable to concatenate outputs.")
#define ERROR_OUTPUT_DIMENSIONS_MUST_BE_POSITIVE _W("Output dimensions must be positive.")
#define ERROR_INDEXED_IMAGE_MUST_BE_NUMERIC _W("Indexed image must be numeric.")
#define ERROR_COLORMAP_MUST_BE_NX3_NUMERIC_ARRAY _W("Colormap must be an Nx3 numeric array.")
#define ERROR_UNSUPPORTED_INDEXED_IMAGE_TYPE                                                       \
    _W("Unsupported data type for indexed image. Must be uint8, uint16, uint32, int8, int16, or "  \
       "int32.")
#define ERROR_CANNOT_OPEN_MAT_FILE _W("Cannot open .mat file.")
// Special functions
#define ERROR_INPUTS_MUST_BE_THE_SAME_SIZE _W("Inputs must be the same size.")
#define ERROR_INPUTS_MUST_BE_REAL_INTEGERS _W("Inputs must be real integers.")
// matio module messages
#define ERROR_INVALID_VARIABLE_NAME _W("Invalid variable name: {0}")
#define ERROR_UNKNOWN_SAVE_FORMAT _W("Unknown save format.")
#define ERROR_CANNOT_APPEND_VARIABLE _W("Cannot append variable (-v7.3 required).")
#define ERROR_CANNOT_SAVE_FILE _W("Cannot save file.")
#define ERROR_CANNOT_SAVE_VARIABLE _W("Cannot save variable: {0}")
#define ERROR_VALID_MAT_FILE_EXPECTED _W("Valid .mat file expected.")
// qml_engine module messages
#define ERROR_NO_PARENT _W("No parent.")
#define ERROR_INVALID_PARAMETERS _W("Invalid parameters")
#define ERROR_QVARIANT_INVALID _W("QVariant invalid.")
#define ERROR_QML_ENGINE_NOT_INITIALIZED _W("QML engine not initialized.")
#define ERROR_CANNOT_CREATE_QQUICKVIEW _W("Cannot create QQuickView.")
#define ERROR_CANNOT_SET_PARENT _W("Cannot set parent.")
#define ERROR_ONLY_10_INPUT_PARAMETERS_EXPECTED _W("Only 10 input parameters expected.")
#define ERROR_RANDOM_ENGINE_NOT_MANAGED _W("random engine not managed.")
#define ERROR_A_VALID_GENERATOR_EXPECTED _W("A valid generator expected.")
#define ERROR_TYPE_OF_STATE_MUST_BE _W("type of state must be {0}")
#define ERROR_DIMENSIONS_OF_STATE_MUST_BE _W("dimensions of state must be {0}")
// data_structures module messages
#define ERROR_NOT_YET_IMPLEMENTED_WITH_DIM_GT_2 _W("Not yet implemented with dim > 2")
#define ERROR_NUMBER_OF_FIELD_NAMES_MUST_MATCH_NUMBER_OF_FIELDS_IN_NEW_STRUCT                      \
    _W("Number of field names must match number of fields in new structure.")
#define ERROR_SPARSE_INPUTS_NOT_SUPPORTED _W("Sparse inputs are not supported.")
#define ERROR_FUNCTION_RETURNED_FEWER_OUTPUTS_THAN_EXPECTED                                        \
    _W("function returned fewer outputs than expected")
#define ERROR_NON_SCALAR_IN_UNIFORM_OUTPUT _W("Non-scalar in Uniform output.")
#define ERROR_UNIFORMOUTPUT_MUST_BE_SCALAR_LOGICAL _W("UniformOutput must be a scalar logical.")
#define ERROR_ERRORHANDLER_MUST_BE_FUNCTION_HANDLE _W("ErrorHandler must be a function handle.")
#define ERROR_FUNCTION_HANDLE_AND_AT_LEAST_ONE_ARRAY_REQUIRED                                      \
    _W("Function handle and at least one array are required.")
#define ERROR_UNKNOWN_FUNCTION_NAME _W("Unknown function name.")
#define ERROR_INVALID_FUNCTION_SPECIFICATION _W("Invalid function specification.")
#define ERROR_FUNCTION_RETURNED_NON_SCALAR_RESULT _W("function returned non-scalar result")
#define ERROR_NUMBER_OF_OLD_FIELD_NAMES_MUST_MATCH_NUMBER_OF_NEW_FIELD_NAMES                       \
    _W("Number of old field names must match number of new field names.")
#define ERROR_INVALID_FIELD_NAME _W("Invalid field name: {0}")
#define ERROR_CANNOT_RENAME_TO_EXISTING_FIELD _W("Cannot rename to an existing field: {0}")
#define ERROR_FIELD_NAMES_MUST_BE_VALID _W("Field names must be valid.")
#define ERROR_DUPLICATED_FIELD_DETECTED _W("Duplicated field detected.")
#define ERROR_CONVERSION_STRUCT_TO_FUNCTION_HANDLE_NOT_POSSIBLE                                    \
    _W("Conversion to 'struct' to 'function_handle' is not possible.")
#define ERROR_STRUCT_EMPTY_EXPECTED _W("struct([]) expected.")
#define ERROR_REQUIRES_PAIRS_OF_FIELD_NAMES_AND_VALUES                                             \
    _W("requires pairs of field names and values.")
#define ERROR_REQUIRES_A_VALID_FIELDNAME _W("requires a valid fieldname.")
#define ERROR_WRONG_TYPE_ARG1_CLASS_EXPECTED _W("Wrong type for argument #1. class expected.")
#define ERROR_SECOND_ARGUMENT_MUST_BE_STRUCT_TYPE_SUBS                                             \
    _W("Second argument must be a structure with two fields whose names are 'type' and 'subs'.")
#define ERROR_ILLEGAL_INDEXING_STRUCTURE_ARGUMENT_TYPE_DOT_EXPECTED                                \
    _W("Illegal indexing structure argument: type '.' expected.")
#define ERROR_CELLFUN_WORKS_ONLY_ON_CELLS _W("cellfun works only on cells.")
#define ERROR_ERROR_WRONG_TYPE_EXPECTED _W("Error wrong type expected.")
#define ERROR_ERROR_ALREADY_DEFINED _W("Error already defined.")
// rng builtin messages
#define ERROR_DEFAULT_SHUFFLE_OR_ENGINELIST_EXPECTED                                               \
    _W("'default', 'shuffle' or 'enginelist' expected.")
#define ERROR_RNG_SHUFFLE_EXPECTED _W("'shuffle' expected.")
#define ERROR_LIKE_MUST_BE_FOLLOWED_BY_ARRAY _W("'like' must be followed by an array.")
#define ERROR_UNKNOWN_TYPENAME _W("Unknown typename: {0}")
#define ERROR_SINGLE_OR_DOUBLE_EXPECTED_AT_LAST_ARGUMENT                                           \
    _W("'single' or 'double' expected at last argument.")
// image_processing module messages
#define ERROR_INPUT_IMAGE_MUST_BE_NUMERIC_OR_LOGICAL _W("Input image must be numeric or logical.")
#define ERROR_INPUT_IMAGE_MUST_BE_2D _W("Input image must be 2-D.")
#define ERROR_ANGLE_MUST_BE_NUMERIC_SCALAR _W("Angle must be a numeric scalar.")
#define ERROR_ANGLE_MUST_BE_FINITE _W("Angle must be a finite value.")
#define ERROR_INTERPOLATION_METHOD_MUST_BE_NEAREST_BILINEAR_BICUBIC                                \
    _W("Interpolation method must be 'nearest', 'bilinear', or 'bicubic'.")
#define ERROR_BOUNDING_BOX_MUST_BE_LOOSE_OR_CROP _W("Bounding box must be 'loose' or 'crop'.")
// imresize messages
#define ERROR_FIRST_ARG_IMAGE_NUMERIC_OR_LOGICAL                                                   \
    _W("First argument must be a numeric or logical array representing an image.")
#define ERROR_SCALE_FACTOR_OR_OUTPUT_SIZE_MUST_BE_PROVIDED                                         \
    _W("Scale factor or output size must be provided.")
#define ERROR_PARAMETER_VALUE_PAIRS_MUST_BE_COMPLETE _W("Parameter-value pairs must be complete.")
#define ERROR_PARAMETER_NAMES_MUST_BE_STRINGS _W("Parameter names must be strings.")
#define ERROR_ANTIALIASING_PARAM_MUST_BE_LOGICAL_OR_STRING                                         \
    _W("Antialiasing parameter must be a logical value or string.")
#define ERROR_COLORMAP_PARAM_MUST_BE_STRING                                                        \
    _W("Colormap parameter must be a string ('optimized' or 'original').")
#define ERROR_COLORMAP_PARAM_MUST_BE_OPTIMIZED_OR_ORIGINAL                                         \
    _W("Colormap parameter must be 'optimized' or 'original'.")
#define ERROR_DITHER_PARAM_MUST_BE_LOGICAL _W("Dither parameter must be a logical value.")
#define ERROR_UNKNOWN_PARAMETER _W("Unknown parameter: {0}")
#define ERROR_SCALE_FACTOR_MUST_BE_POSITIVE _W("Scale factor must be positive.")
#define ERROR_AT_LEAST_ONE_OUTPUT_DIMENSION_SPECIFIED                                              \
    _W("At least one output dimension must be specified (not NaN).")
#define ERROR_SCALE_PARAMETER_MUST_BE_SCALAR_OR_2_ELEMENT_VECTOR                                   \
    _W("Scale parameter must be a scalar or 2-element vector.")
#define ERROR_SCALE_PARAMETER_MUST_BE_NUMERIC _W("Scale parameter must be numeric.")
#define ERROR_UNKNOWN_INTERPOLATION_METHOD _W("Unknown interpolation method: {0}")
#define ERROR_ANTIALIASING_MUST_BE_TRUE_FALSE_ON_OFF                                               \
    _W("Antialiasing must be 'true', 'false', 'on', or 'off'.")
// validators module: wrap runtime messages (validator-generated) using {0}
#define ERROR_INVALID_INPUT_POSITION _W("Invalid input argument position.")
#define ERROR_NO_BREAKPOINT_FOUND_AT_SPECIFIED_LOCATION                                            \
    _W("No breakpoint found at specified location.")
#define ERROR_THIRD_ARGUMENT_MUST_BE_AT _W("Third argument must be 'at'.")
#define ERROR_INVALID_POSITION_ARGUMENT _W("Invalid position argument.")
#define ERROR_NUMERIC_INPUT_DATA_MUST_BE_REAL _W("Numeric input data must be real.")
#define ERROR_UNKNOWN_DATE_FORMAT_NUMBER _W("Unknown date format number.")
#define ERROR_SECOND_ARG_MUST_BE_SCALAR_OR_CHARACTER_VECTOR                                        \
    _W("#2 argument must be a scalar or a character vector.")
#define ERROR_ARGUMENT_2_NS_OR_S_EXPECTED _W("Argument #2: 'ns' or 's' expected.")
#define ERROR_MODULE_NOT_FOUND _W("Module not found: {0}")
#define ERROR_MODULE_ALREADY_USED _W("Module already used: {0}")
#define ERROR_MAKEDIRECTORY_ERROR _W("Error creating directory: {0}")
#define ERROR_UNDEFINED_FUNCTION _W("Undefined function: {0}")
#define ERROR_BUILTIN_FUNCTION_EXPECTED _W("Built-in function expected: {0}")
#define ERROR_WRONG_VALUE_FOR_1_ARGUMENT_STYLESHEET_EXPECTED                                       \
    _W("Wrong value for #1 argument: 'stylesheet' expected.")
#define ERROR_CANNOT_CONVERT_DATA _W("Cannot convert data: {0}.")
#define ERROR_CANNOT_READ_VARIABLE _W("Cannot read variable: {0}.")
#define ERROR_UNDEFINED_FUNCTION_OR_VARIABLE _W("Undefined function or variable: {0}.")
#define ERROR_UNDEFINED_FUNCTION_OR_VARIABLE_NO_NAME _W("Undefined function or variable.")
#define ERROR_UNDEFINED_FUNCTION_NO_NAME _W("Undefined function.")
#define ERROR_BUILTIN_TYPE_NOT_MANAGED _W("Built-in type not managed.")
#define ERROR_SYSTEM_ERROR_DETECTED _W("System error detected: {0}.")
#define ERROR_UNDEFINED_METHOD _W("Undefined method: {0}.")
#define ERROR_UNDEFINED_METHOD_NO_NAME _W("Undefined method.")
#define ERROR_WRONG_NUMBERS_OUTPUT_ARGS_FOR_FUNCTION _W("Wrong number of output arguments for {0}.")
#define ERROR_MAXIMUM_NAME_LENGTH_EXCEEDED _W("Maximum name length exceeded.")
#define ERROR_CANNOT_OPEN_DESTINATION_FILE _W("Cannot open destination file.")
#define ERROR_CANNOT_OPEN_FILE _W("Cannot open file.")
#define ERROR_CANNOT_CREATE_INTERMEDIATE_DIRECTORY _W("Cannot create intermediate directory.")
#define ERROR_FUNCTION_ALREADY_DECLARED                                                            \
    _W("Function '{0}' has already been declared within this scope.")
#define ERROR_FILENAME_FUNCTION_NAME_MISMATCH                                                      \
    _W("Filename and function name are not same ({0} vs {1}).")
#define ERROR_OPERATOR_NOT_SUPPORTED                                                               \
    _W("Operator '{0}' is not supported for operands of type '{1}'.")
#define ERROR_NONSCALAR_ARRAYS_OF_FUNCTION_HANDLES_NOT_ALLOWED                                     \
    _W("Nonscalar arrays of function handles are not allowed; use cell arrays instead.")
#define ERROR_EXPECTING _W("Expecting {0}")
#define ERROR_EXPECTING_AT _W("Expecting {0}\n\tat line {1}, column {2} of file {3}")
#define ERROR_2_PARAMETER_INVALID_MINIMIZE_PARAMETER_EXPECTED                                      \
    _W("#2 parameter invalid: 'post', 'eval', 'put', 'get', 'minimize' or 'isvar' parameter "      \
       "expected.")
#define ERROR_CANNOT_DECODE_JSON _W("Cannot decode JSON: {0}")
#define ERROR_CANNOT_ENCODE_JSON _W("Cannot encode JSON: {0}")
#define ERROR_UNABLE_TO_SET_PROPERTY_JULIAENVIRONMENT_READONLY                                     \
    _W("Unable to set property of class 'JuliaEnvironment' it is read-only.")
#define ERROR_IN_JULIA _W("Error in Julia: \n{0}")
#define ERROR_UNDEFINED_FUNCTION_FOR_INPUT_ARGUMENTS                                               \
    _W("Undefined function '{0}' for input arguments of type '{1}'.")
#define ERROR_MPI_COMM_NOT_ALLOWED _W("{0} not allowed.")
#define ERROR_FUNCTION_NOT_AVAILABLE _W("function '{0}' not available.")
#define ERROR_CTRANSPOSE_ON_ND_ARRAY_UNDEFINED _W("ctranspose on N-D array is undefined.")
#define ERROR_TRANSPOSE_ON_ND_ARRAY_UNDEFINED _W("transpose on N-D array is undefined.")
#define ERROR_UNKNOWN_ERROR_LOADING_ENVIRONMENT _W("Unknown error while loading environment")
#define ERROR_EXCEPTION _W("Exception: {0}")
#define ERROR_UNDEFINED_FUNCTION_FOR_INPUT_ARGUMENTS_OF_TYPE                                       \
    _W("Undefined function '{0}' for input arguments of type '{1}'.")
#define ERROR_CANNOT_CREATE_DIRECTORY _W("Cannot create directory.")
#define ERROR_CANNOT_READ_FILE _W("Cannot read file: {0}.")
#define ERROR_CANNOT_READ_FILE_GLOBAL_INFO _W("Cannot read file global info.")
#define ERROR_CANNOT_READ_FILE_INFO _W("Cannot read file info.")
#define ERROR_CANNOT_READ_DATA _W("Cannot read data.")
#define ERROR_CANNOT_READ_NEXT_FILE _W("Cannot read next file.")
#define ERROR_INVALID_ROOT_PATH _W("Invalid root path.")
#define ERROR_NOTHING_TO_ZIP _W("Nothing to zip.")
#define ERROR_CANNOT_ADD_ENTRY _W("Cannot add entry: {0}.")
#define ERROR_CONVERSION_FROM_SPARSE_NOT_POSSIBLE                                                  \
    _W("Conversion to '{0}' from sparse matrix is not possible.")
#define ERROR_INVALID_CONVERSION_FROM_COMPLEX                                                      \
    _W("Invalid conversion from complex matrix to '{0}' matrix.")
#define ERROR_CONVERSION_FROM_GRAPHICS_OBJECT_NOT_POSSIBLE                                         \
    _W("Conversion to '{0}' from graphics object is not possible.")
#define ERROR_CONVERSION_FROM_HANDLE_NOT_POSSIBLE                                                  \
    _W("Conversion to '{0}' from handle is not possible.")
#define ERROR_CONVERSION_FROM_STRING_NOT_POSSIBLE                                                  \
    _W("Conversion to '{0}' from string is not possible.")
#define ERROR_CONVERSION_FROM_CELL_NOT_POSSIBLE _W("Conversion to '{0}' from cell is not possible.")
#define ERROR_CONVERSION_FROM_FUNCTION_HANDLE_NOT_POSSIBLE                                         \
    _W("Conversion to '{0}' from function_handle is not possible.")
#define ERROR_CONVERSION_FROM_STRUCT_NOT_POSSIBLE                                                  \
    _W("Conversion to '{0}' from struct is not possible.")
#define ERROR_CASE_NOT_SUPPORTED _W("Case not supported.")
#define ERROR_HISTORY_MANAGER_NOT_ENABLED _W("History manager not enabled.")
#define ERROR_BALANCE_UNKNOWN_SECOND_ARGUMENT _W("Second argument must be 'noperm'.")
#define ERROR_BALANCE_INPUT_MUST_BE_2D _W("Input must be 2-D.")
#define ERROR_BALANCE_NO_BALANCE_SPARSE _W("Use balance(full(S)).")
#define ERROR_BALANCE_INPUT_TYPE                                                                   \
    _W("Input type not supported. First argument must be single or double.")
#define ERROR_SIZE_MISMATCH_ON_OPERATOR _W("Size mismatch on arguments to arithmetic operator {0}.")
#define ERROR_VALID_TEXT_ENCODING_EXPECTED _W("Valid text encoding expected.")
#define ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_DOES_NOT_EXIST                                        \
    _W("Specified HDF5 object location does not exist.")
#define ERROR_SPECIFIED_HDF5_OBJECT_LOCATION_COULD_NOT_BE_OPENED                                   \
    _W("Specified HDF5 object location could not be opened.")
#define ERROR_COULD_NOT_DELETE_EXISTING_ATTRIBUTE _W("Could not delete existing attribute.")
#define ERROR_COULD_NOT_CHECK_IF_ATTRIBUTE_EXISTS _W("Could not check if attribute exists.")
#define ERROR_CANNOT_WRITE_ATTRIBUTE _W("Cannot write attribute.")
#define ERROR_VALID_FUNCTION_EXPECTED _W("A valid function expected.")
#define ERROR_VALUE_OUT_OF_RANGE _W("Value is out of range {0} <= value <= {1}.")
#define ERROR_INVALID_GRAPHICS_OBJECT _W("Invalid graphics object.")
#define ERROR_CALLBACK_VALUE_MUST_BE_CHARACTER_VECTOR_FUNCTION_HANDLE_OR_CELL_ARRAY                \
    _W("Callback value must be a character vector, a function handle, or a cell array containing " \
       "character vector or function handle.")
#define ERROR_INVALID_WRITEMODE _W("Wrong value for WriteMode: 'overwrite' or 'append' expected.")
#define ERROR_INVALID_DELAY_TIME _W("Wrong value for delay time: [0, 655] expected.")
#define ERROR_INVALID_QUALITY _W("Wrong value for quality: [0, 100] expected.")
#define ERROR_UNABLE_TO_DETERMINE_FILE_FORMAT                                                      \
    _W("Unable to determine the file format from the file name.")
#define ERROR_COLORMAP_THREE_COLUMNS _W("Colormap should have three columns.")
#define ERROR_WRONG_SIZE_ALPHAMAP _W("Wrong size for AlphaMap.")
#define ERROR_UNSUPPORTED_IMAGE_TYPE _W("Image must be double, single, logical or uint8 type.")
#define ERROR_PDF_CANNOT_GENERATE_IN_ENGINE_MODE _W("pdf cannot generated in this engine mode.")
#define ERROR_PDF_NOT_GENERATED _W("pdf file not generated.")
#define ERROR_CANNOT_OPEN_DESTINATION_FILE _W("Cannot open destination file.")
#define ERROR_INVALID_ANONYMOUS_FUNCTION _W("Invalid anonymous function.")
#define ERROR_INCOMPATIBLE_HANDLE_CLASSES _W("Handles being catenated have incompatible classes.")
#define ERROR_X_NOT_DEFINED _W("{0} not defined.")
#define ERROR_ONCLEANUP_HANDLE_EXPECTED _W("OnCleanup handle expected.")
#define ERROR_UNKNOWN_PROPERTY_NAME _W("Unknown property name: {0}.")
#define ERROR_ARG1_MAX_EXPECTED _W("Argument #1: 'max' expected.")
#define ERROR_ARG1_VALID_VALUE_EXPECTED _W("Argument #1: valid value expected.")
#define ERROR_MODULE_REGISTERED_WITHOUT_PATH                                                       \
    _W("{0}: This module is registered but it has no path.")
#define ERROR_MARKDOWN_GENERATION_FAILS _W("Markdown generation fails.")
#define ERROR_BUILTIN_HAVE_NO_COMMENTS _W("built-in have no comments.")
#define ERROR_FUNCTION_DOES_NOT_EXIST _W("function does not exist.")
#define ERROR_NO_COMMENTS_BUILTIN_MEX_ANONYMOUS                                                    \
    _W("built-in, mex, anonymous function have no comments.")
#define ERROR_IMPORT_TYPE_NOT_DEFINED _W("import type {0} not defined in FFI type table.")
#define ERROR_INVALID_TYPE_FOR_LIKE_INPUT_ARGUMENT                                                 \
    _W("Invalid type for #{0} input argument: {1} expected.")
#define ERROR_IMPORT_TYPE_NOT_DEFINED _W("import type {0} not defined in FFI type table.")
#define ERROR_INPUT_ARGUMENT_1_SCALAR_EXPECTED _W("Input argument #1: scalar expected.")
#define ERROR_INPUT_ARGUMENT_2_SCALAR_EXPECTED _W("Input argument #2: scalar expected.")
#define ERROR_INPUT_ARGUMENT_WRONG_SIZE _W("Input argument #{0}: wrong size. {1} expected.")
#define ERROR_UNABLE_TO_SET_PROPERTY_PYTHON_ENVIRONMENT                                            \
    _W("Unable to set property of class 'PythonEnvironment' it is read-only.")
#define ERROR_DETECTIMPORTOPTIONS_ERROR _W("Error in detectImportOptions: {0}")
#define ERROR_READMATRIX_ERROR _W("Error in readmatrix: {0}")
#define ERROR_READTABLE_ERROR _W("Error in readtable: {0}")
#define ERROR_READCELL_ERROR _W("Error in readcell: {0}")
#define ERROR_READMATRIX_ERROR _W("Error in readmatrix: {0}")
#define ERROR_FIGURE_OBJECT_EXPECTED _W("figure object expected.")
#define ERROR_SPECIFIED_WINDOW_DOES_NOT_EXIST _W("Specified window does not exist.")
#define ERROR_FAILED_TO_CREATE_PYTHON_METHOD_NAME_OBJECT                                           \
    _W("Failed to create Python method name object.")
#define ERROR_FAILED_TO_CREATE_PYTHON_ARGUMENT_TUPLE _W("Failed to create Python argument tuple.")
#define ERROR_TYPE_UNHASHABLE _W("TypeError: unhashable type: '{0}'")
#define ERROR_NAME_OF_INTEGER_CLASS_EXPECTED _W("Name of integer class expected.")
#define ERROR_GRAPHICS_OBJECT_EXPECTED _W("graphics_object expected.")
#define ERROR_RGB_TRIPLET_EXPECTED _W("RGB triplet expected.")
#define ERROR_ONE_COLOR_SPECIFICATION_OR_MULTIPLE_OPTION_EXPECTED                                  \
    _W("One color specification or use the 'multiple' option expected.")
#define ERROR_VALID_COLOR_EXPECTED _W("Valid color expected.")
#define ERROR_INVALID_PROPERTY _W("Invalid property.")
#define ERROR_ISEQUALN_NOT_FOUND _W("isequaln function not found.")
#define ERROR_ISEQUALN_LHS_ERROR _W("isequaln lhs error.")
#define ERROR_CANNOT_CONVERT_TO _W("Cannot convert to {0}.")
#define ERROR_METHOD_DOES_NOT_EXIST _W("Method does not exist.")
#define GRAPHICS_OBJECT_EXPECTED _W("Graphics object expected.")
#define ERROR_FILESYSTEM_ERROR _W("Filesystem error: {0}.")
#define ERROR_DIRECTORY_NOT_EXIST _W("Directory does not exist: {0}.")