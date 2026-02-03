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
#define ERROR_SAME_SIZE_EXPECTED _W("Same size expected.")
#define ERROR_SIZE_SCALAR_EXPECTED _W("A scalar expected.")
#define ERROR_DIMENSIONS_NOT_CONSISTENT _W("Dimensions concatenated are not consistent.")

#define ERROR_WRONG_ARGUMENTS_SIZE_2D_MATRIX_EXPECTED                                              \
    _W("Wrong size for input arguments: 2D matrix expected.")
#define ERROR_WRONG_ARGUMENT_X_SIZE_SCALAR_EXPECTED                                                \
    _W("Wrong size for argument #{}: scalar expected.")
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
#define ERROR_TYPE_STRUCT_EXPECTED _W("struct expected.")
#define ERROR_TYPE_CLASS_EXPECTED _W("class expected.")
#define ERROR_TYPE_NOT_SUPPORTED _W("type not supported.")
#define ERROR_TYPE_ALREADY_RESERVED _W("type reserved.")
#define ERROR_SAME_INTEGER_TYPE_EXPECTED _W("Same integer type expected.")
#define ERROR_WRONG_ARGUMENTS_TYPE _W("Wrong types for inputs arguments.")
#define ERROR_TYPE_CELL_OF_STRINGS _W("cell of strings")
#define ERROR_TYPE_X_EXPECTED _W("{} expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE _W("Wrong type: #{} argument.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_Y_EXPECTED _W("Wrong type for argument #{}: {} expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_FUNCTION_HANDLE_EXPECTED                             \
    _W("Wrong type for argument #{}: string or function handle expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_DOUBLE_EXPECTED                                      \
    _W("Wrong type for argument #{}: string or double expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_STRING_OR_CELL_EXPECTED                                        \
    _W("Wrong type for argument #{}: string or cell expected.")
#define ERROR_WRONG_ARGUMENT_X_TYPE_NUMERIC_EXPECTED                                               \
    _W("Wrong type for #{} argument: numeric value expected.")
//=============================================================================
// VALUE ERRORS
//=============================================================================
#define ERROR_WRONG_ARGUMENT_X_VALUE _W("Wrong value for #{} argument.")
#define ERROR_WRONG_ARGUMENT_X_SCALAR_INTEGER_VALUE_EXPECTED                                       \
    _W("Wrong value for argument #{}. Scalar integer value expected.")
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
#define ERROR_WRONG_NUMBERS_OUTPUT_ARGS _W("Wrong number of output arguments.")
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
#define ERROR_ILLEGAL_USE_COLON _W("Illegal use of the ':' keyword in indexing expression")
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
#define ERROR_CANNOT_USE_ARGS _W("Cannot use arguments in a call to a script.")
#define ERROR_ASSIGN_OUTPUTS _W("Cannot assign outputs in a call to a script.")
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
#define ERROR_NEED_TO_IMPLEMENT_ASSIGN _W("NEED TO IMPLEMENT ASSIGNATION FOR OBJECT.")
#define ERROR_ASSIGN_TO_NON_STRUCT _W("Cannot apply A.field_name = B to non struct-array object A.")
#define ERROR_FIELDNAMES_MUST_MATCH _W("Fieldnames in structs must match.")
#define ERROR_NEED_OVERLOAD _W("Need to Overload!")
//=============================================================================
// SWITCH STATEMENT ERRORS
//=============================================================================
#define ERROR_SWITCH_STATEMENTS _W("Switch statements support scalar and string arguments only.")
//=============================================================================
// SYSTEM & RESOURCE ERRORS
//=============================================================================
#define ERROR_INVALID_FILEID _W("Invalid file id.")
#define ERROR_MEMORY_ALLOCATION _W("Memory allocation error... You may have run out of memory!")
#define MSG_CTRL_C_DETECTED _W("Interrupt (ctrl-c) encountered.")
//=============================================================================
