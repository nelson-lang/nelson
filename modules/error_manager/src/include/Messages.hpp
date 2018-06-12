//=============================================================================
// Copyright (c) 2016-2018 Allan CORNET (Nelson)
//=============================================================================
// LICENCE_BLOCK_BEGIN
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
// LICENCE_BLOCK_END
//=============================================================================
#pragma once
//=============================================================================
#include "i18n.hpp"
//=============================================================================
#define ERROR_STACK_DEPTH_EXCEEDED _W("Allowable stack depth exceeded...")
#define ERROR_POP_GLOBAL_SCOPE _W("Attempt to pop global scope off of context stack!")
#define ERROR_AST_SYNTAX_ERROR _W("AST - syntax error!")
#define ERROR_END_ILLEGAL _W("END keyword illegal!")
#define ERROR_UNRECOGNIZED_NODE _W("Unrecognized reserved node in expression tree!")
#define ERROR_UNRECOGNIZED_EXPRESSION _W("Unrecognized expression!")
#define ERROR_EMPTY_EXPRESSION _W("Empty expression!")
#define ERROR_ILLEGAL_USE_COLON _W("Illegal use of the ':' keyword in indexing expression")
#define ERROR_SWITCH_STATEMENTS _W("Switch statements support scalar and string arguments only.")
#define ERROR_ENDFUNCTION_WRONG_USE _W("endfunction cannot used here.")
#define ERROR_UNRECOGNIZED_STATEMENT _W("Unrecognized statement type.")
#define ERROR_INDEX_EXPRESSION_EXPECTED _W("Expected indexing expression!")
#define ERROR_NEED_OVERLOAD _W("Need to Overload!")
#define ERROR_DYNAMIC_FIELD_STRING_EXPECTED                                                        \
    _W("dynamic field reference to structure requires a string argument")
#define ERROR_NEED_TO_IMPLEMENT_ASSIGN _W("NEED TO IMPLEMENT ASSIGNATION FOR OBJECT.")
#define ERROR_ASSIGN_TO_NON_STRUCT _W("Cannot apply A.field_name = B to non struct-array object A.")
#define ERROR_ILLEGAL_LEFT_MULTIFUNCTION_EXPRESSION                                                \
    _W("Illegal left hand side in multifunction expression")
#define ERROR_MULTIPLE_ROWS_NOT_ALLOWED                                                            \
    _W("Multiple rows not allowed in left hand side of multifunction expression")
#define ERROR_PARENTHETICAL_EXPRESSION                                                             \
    _W("Parenthetical expression in the left hand side of a function call must resolve to a "      \
       "single "                                                                                   \
       "element.")
#define ERROR_CANNOT_USE_ARGS _W("Cannot use arguments in a call to a script.")
#define ERROR_ASSIGN_OUTPUTS _W("Cannot assign outputs in a call to a script.")
#define ERROR_ILLEGAL_EXPRESSION_IN_FUNCTION _W("Illegal expression in function expression")
#define ERROR_MUST_HAVE_LVALUE _W("Must have lvalue in argument passed by reference")
#define ERROR_INVALID_FILEID _W("Invalid file id.")
#define ERROR_MEMORY_ALLOCATION _W("Memory allocation error... You may have run out of memory!")
#define ERROR_WRONG_NUMBERS_OUTPUT_ARGS _W("Wrong numbers of output arguments.")
#define ERROR_WRONG_NUMBERS_INPUT_ARGS _W("Wrong number of input arguments.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_STRING_EXPECTED                                                \
    _W("Wrong type for argument #1: string expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_STRING_EXPECTED                                                \
    _W("Wrong type for argument #2: string expected.")
#define ERROR_WRONG_ARGUMENT_3_TYPE_STRING_EXPECTED                                                \
    _W("Wrong type for argument #3: string expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_EXPECTED                                                \
    _W("Wrong type for argument #%d: string expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_CELL_OF_STRINGS_EXPECTED                                       \
    _W("Wrong type for argument #1: cell of strings expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_CELL_OF_STRINGS_EXPECTED                                       \
    _W("Wrong type for argument #2: cell of strings expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_CELL_OF_STRINGS_EXPECTED                                       \
    _W("Wrong type for argument #%d: cell of strings expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_LOGICAL_EXPECTED                                               \
    _W("Wrong type for argument #1: logical expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_LOGICAL_EXPECTED                                               \
    _W("Wrong type for argument #2: logical expected.")
#define ERROR_WRONG_ARGUMENT_3_TYPE_LOGICAL_EXPECTED                                               \
    _W("Wrong type for argument #3: logical expected.")
#define ERROR_WRONG_ARGUMENT_4_TYPE_LOGICAL_EXPECTED                                               \
    _W("Wrong type for argument #4: logical expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_LOGICAL_EXPECTED                                               \
    _W("Wrong size for argument #1. logical matrix expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_LOGICAL_EXPECTED                                               \
    _W("Wrong size for argument #2. logical matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_LOGICAL_EXPECTED                                        \
    _W("Wrong type for argument #1: sparse logical expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_LOGICAL_EXPECTED                                        \
    _W("Wrong type for argument #2: sparse logical expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #1: double expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #2: double expected.")
#define ERROR_WRONG_ARGUMENT_3_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #3: double expected.")
#define ERROR_WRONG_ARGUMENT_4_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #4: double expected.")
#define ERROR_WRONG_ARGUMENT_5_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #5: double expected.")
#define ERROR_WRONG_ARGUMENT_6_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #6: double expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_DOUBLE_EXPECTED                                                \
    _W("Wrong type for argument #%d: double expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_DOUBLE_EXPECTED                                                \
    _W("Wrong size for argument #1. double matrix expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_DOUBLE_EXPECTED                                                \
    _W("Wrong size for argument #2. double matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_DOUBLE_EXPECTED                                         \
    _W("Wrong type for argument #1: sparse double expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_DOUBLE_EXPECTED                                         \
    _W("Wrong type for argument #2: sparse double expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_SPARSE_EXPECTED                                                \
    _W("Wrong type for argument #1: sparse expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_SPARSE_EXPECTED                                                \
    _W("Wrong type for argument #2: sparse expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_SINGLE_EXPECTED                                                \
    _W("Wrong type for argument #1: single expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_SINGLE_EXPECTED                                                \
    _W("Wrong type for argument #2: single expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_SINGLE_EXPECTED                                                \
    _W("Wrong size for argument #1. single matrix expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_SINGLE_EXPECTED                                                \
    _W("Wrong size for argument #2. single matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_INTEGER_EXPECTED                                               \
    _W("Wrong type for argument #1. integer expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_INTEGER_EXPECTED                                               \
    _W("Wrong type for argument #2. integer expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_INTEGER_EXPECTED                                               \
    _W("Wrong size for argument #1. integer matrix expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_INTEGER_EXPECTED                                               \
    _W("Wrong size for argument #2. integer matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_NDARRAY_INTEGER_EXPECTED                                       \
    _W("Wrong size for argument #1. integer n-d matrix expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_NDARRAY_INTEGER_EXPECTED                                       \
    _W("Wrong size for argument #2. integer n-d matrix expected.")

#define ERROR_SAME_INTEGER_TYPE_EXPECTED _W("Same integer type expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_INT8_EXPECTED _W("Wrong type for argument #1. int8 expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_UINT8_EXPECTED _W("Wrong type for argument #1. uint8 expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_INT16_EXPECTED _W("Wrong type for argument #1. int16 expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_UINT16_EXPECTED                                                \
    _W("Wrong type for argument #1. uint16 expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_INT32_EXPECTED _W("Wrong type for argument #1. int32 expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_UINT32_EXPECTED                                                \
    _W("Wrong type for argument #1. uint32 expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_INT64_EXPECTED _W("Wrong type for argument #1. int64 expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_UINT64_EXPECTED                                                \
    _W("Wrong type for argument #1. uint64 expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_FUNCTION_HANDLE_EXPECTED                                       \
    _W("Wrong type for argument #1: function handle expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_FUNCTION_HANDLE_EXPECTED                                       \
    _W("Wrong type for argument #%d: function handle expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED                             \
    _W("Wrong type for argument #1: string or function handle expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_DOUBLE_EXPECTED                                      \
    _W("Wrong type for argument #1: string or double expected.")
#define ERROR_WRONG_ARGUMENT_1_TYPE_STRING_OR_CELL_EXPECTED                                        \
    _W("Wrong type for argument #1: string or cell expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_STRING_OR_CELL_EXPECTED                                        \
    _W("Wrong type for argument #2: string or cell expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_NUMERIC_EXPECTED                                               \
    _W("Wrong type for #1 argument: numeric value expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_NUMERIC_EXPECTED                                               \
    _W("Wrong type for #1 argument: numeric value expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_CELL_EXPECTED _W("Wrong type for argument #1. cell expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_CELL_EXPECTED _W("Wrong type for argument #2. cell expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_STRUCT_EXPECTED                                                \
    _W("Wrong type for argument #1. struct expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_STRUCT_EXPECTED                                                \
    _W("Wrong type for argument #2. struct expected.")

#define ERROR_WRONG_ARGUMENTS_TYPE _W("Wrong types for inputs arguments.")
#define ERROR_WRONG_ARGUMENT_1_TYPE _W("Wrong type: #1 argument.")
#define ERROR_WRONG_ARGUMENT_2_TYPE _W("Wrong type: #2 argument.")
#define ERROR_WRONG_ARGUMENTS_TYPE_DOUBLE_EXPECTED                                                 \
    _W("Wrong type for input arguments: double expected.")
#define ERROR_WRONG_ARGUMENTS_TYPE_SINGLE_EXPECTED                                                 \
    _W("Wrong type for input arguments: single expected.")

#define ERROR_TYPE_LOGICAL_EXPECTED _W("logical expected.")
#define ERROR_TYPE_STRUCT_EXPECTED _W("struct expected.")
#define ERROR_TYPE_CELL_OF_STRINGS_EXPECTED _W("cell of strings expected.")
#define ERROR_TYPE_NOT_SUPPORTED _W("type not supported.")
#define ERROR_TYPE_ALREADY_RESERVED _W("type reserved.")

//=============================================================================
#define ERROR_WRONG_ARGUMENTS_SIZE_FULL_MATRIX_EXPECTED                                            \
    _W("Wrong size for input arguments: full matrix expected.")
#define ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED                                              \
    _W("Wrong size for input arguments: 2D matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_FULL_MATRIX_EXPECTED                                           \
    _W("Wrong size for argument #1: full matrix expected.")
#define ERROR_WRONG_ARGUMENT_1_SIZE_2D_MATRIX_EXPECTED                                             \
    _W("Wrong size for argument #1: 2D matrix expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_EXPECTED                                                \
    _W("Wrong size for argument #1: scalar expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_SCALAR_EXPECTED                                                \
    _W("Wrong size for argument #2: scalar expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_EXPECTED                                                \
    _W("Wrong size for argument #%d: scalar expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_SCALAR_OR_ROW_VECTOR_EXPECTED                                  \
    _W("Wrong size for #1 argument. a scalar or a row vector expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_A_B_VECTOR_EXPECTED                                            \
    _W("Wrong size for #1 argument. [a, b] expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_A_B_VECTOR_EXPECTED                                            \
    _W("Wrong size for #2 argument. [a, b] expected.")

#define ERROR_WRONG_ARGUMENT_1_SIZE_ROW_VECTOR_EXPECTED                                            \
    _W("Wrong size for #1 argument. row vector expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_ROW_VECTOR_EXPECTED                                            \
    _W("Wrong size for #2 argument. row vector expected.")

#define ERROR_SAME_SIZE_EXPECTED _W("Same size expected.")
#define ERROR_SIZE_SCALAR_EXPECTED _W("A scalar expected.")
//=============================================================================
#define ERROR_WRONG_ARGUMENT_1_SIZE_A_B_VECTOR_EXPECTED                                            \
    _W("Wrong size for #1 argument. [a, b] expected.")
#define ERROR_WRONG_ARGUMENT_2_SIZE_A_B_VECTOR_EXPECTED                                            \
    _W("Wrong size for #2 argument. [a, b] expected.")

//=============================================================================
#define ERROR_WRONG_ARGUMENT_1_VALUE _W("Wrong value for #1 argument.")
#define ERROR_WRONG_ARGUMENT_2_VALUE _W("Wrong value for #2 argument.")
#define ERROR_WRONG_ARGUMENT_X_VALUE _W("Wrong value for #%d argument.")

#define ERROR_WRONG_ARGUMENT_1_SCALAR_INTEGER_VALUE_EXPECTED                                       \
    _W("Wrong value for argument #1. Scalar integer value expected.")
#define ERROR_WRONG_ARGUMENT_2_SCALAR_INTEGER_VALUE_EXPECTED                                       \
    _W("Wrong value for argument #2. Scalar integer value expected.")
#define ERROR_WRONG_ARGUMENT_X_SCALAR_INTEGER_VALUE_EXPECTED                                       \
    _W("Wrong value for argument #%d. Scalar integer value expected.")

#define ERROR_WRONG_ARGUMENT_1_FINITE_SCALAR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #1. A finite scalar integer value expected.")
#define ERROR_WRONG_ARGUMENT_2_FINITE_SCALAR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #2. A finite scalar integer value expected.")
#define ERROR_WRONG_ARGUMENT_X_FINITE_SCALAR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #%d. A finite scalar integer value expected.")

#define ERROR_WRONG_ARGUMENT_1_FINITE_VECTOR_INTEGER_VALUE_EXPECTED                                \
    _W("Wrong value for argument #1. A finite vector of integer values expected.")

#define ERROR_WRONG_ARGUMENT_1_INVALID_VECTOR_SIZE _W("Wrong value for #1 :Invalid size.")
#define ERROR_WRONG_ARGUMENT_2_INVALID_VECTOR_SIZE _W("Wrong value for #2 :Invalid size.")
#define ERROR_WRONG_ARGUMENT_X_INVALID_VECTOR_SIZE _W("Wrong value for #%d :Invalid size.")

#define ERROR_WRONG_ARGUMENT_1_POSITIVE_VALUE_EXPECTED                                             \
    _W("Wrong value for #1 argument. positive value expected.")
#define ERROR_WRONG_ARGUMENT_2_POSITIVE_VALUE_EXPECTED                                             \
    _W("Wrong value for #2 argument. positive value expected.")
#define ERROR_WRONG_ARGUMENT_X_POSITIVE_VALUE_EXPECTED                                             \
    _W("Wrong value for #%d argument. positive value expected.")

#define ERROR_WRONG_ARGUMENT_1_A_MUST_BE_HIGHER_THAN_B                                             \
    _W("Wrong value for #1 argument. a must be higher than b ([a,b]).")
#define ERROR_WRONG_ARGUMENT_2_A_MUST_BE_HIGHER_THAN_B                                             \
    _W("Wrong value for #2 argument. a must be higher than b ([a,b]).")
#define ERROR_WRONG_ARGUMENT_X_A_MUST_BE_HIGHER_THAN_B                                             \
    _W("Wrong value for #2 argument. a must be higher than b ([a,b]).")
//=============================================================================
#define MSG_CTRL_C_DETECTED _W("Interrupt (ctrl-c) encountered.")
//=============================================================================
#define WARNING_OUTPUTS_NOT_ASSIGNED _W("Warning! one or more outputs not assigned in call.")
//=============================================================================
#define ERROR_DIMENSIONS_NOT_CONSISTENT _W("Dimensions concatenated are not consistent.")

#define ERROR_FIELDNAMES_MUST_MATCH _W("Fieldnames in structs must match.")

#define ERROR_SCALAR_EXPECTED _W("A scalar expected.")

#define ERROR_WRONG_ARGUMENT_1_TYPE_HANDLE_EXPECTED                                                \
    _W("Wrong type for argument #1: handle expected.")
#define ERROR_WRONG_ARGUMENT_2_TYPE_HANDLE_EXPECTED                                                \
    _W("Wrong type for argument #2: handle expected.")
#define ERROR_WRONG_ARGUMENT_3_TYPE_HANDLE_EXPECTED                                                \
    _W("Wrong type for argument #3: handle expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_HANDLE_EXPECTED                                                \
    _W("Wrong type for argument #%d: handle expected.")
