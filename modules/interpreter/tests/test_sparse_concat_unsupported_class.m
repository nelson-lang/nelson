%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% An integer, a char, a struct or a cell has no sparse form. The first operand
% decides the error (for a struct, after the leading non-sparse 2-D empties):
% one that cannot become the result class names that conversion, then one
% still N-D once converted cannot become sparse, otherwise the result would be
% a sparse array of that class.
%=============================================================================
idU = 'Nelson:UnableToConvert';
fmtU = _('The following error occurred converting from %s to %s:\n%s');
unimplemented = _('Attempt to convert to unimplemented sparse type');
idI = 'Nelson:unimplementedSparseType';
idD = 'Nelson:catenate:dimensionMismatch';
msgD = _('Concatenation involves conversion to sparse of an array with more than 2 dimensions.');
%=============================================================================
% integers
msg = sprintf(fmtU, 'double', 'int8', unimplemented);
assert_checkerror('[sparse(1), int8(1)]', msg, idU);
assert_checkerror('[sparse(1); int8(1)]', msg, idU);
assert_checkerror('[sparse([]), int8(1)]', msg, idU);
assert_checkerror('[sparse(1i), int8(1)]', msg, idU);
assert_checkerror('[sparse(1), int8(zeros(1, 1, 2))]', msg, idU);
assert_checkerror('[sparse(1), int64(1)]', sprintf(fmtU, 'double', 'int64', unimplemented), idU);
assert_checkerror('[sparse(true), uint8(1)]', sprintf(fmtU, 'logical', 'uint8', unimplemented), idU);
assert_checkerror('[int8(1), sparse(1)]', unimplemented, idI);
assert_checkerror('[int8(1); sparse(1)]', unimplemented, idI);
assert_checkerror('[int8([]), sparse([])]', unimplemented, idI);
assert_checkerror('[1, sparse(1), int8(1)]', unimplemented, idI);
assert_checkerror('[[], sparse(1), int8(1)]', unimplemented, idI);
assert_checkerror('[zeros(0, 1), sparse(1), int8(1)]', unimplemented, idI);
assert_checkerror('[1, int8(zeros(1, 1, 2)), sparse(1)]', unimplemented, idI);
assert_checkerror('[int8(zeros(1, 1, 2)); sparse(1)]', msgD, idD);
assert_checkerror('[zeros(1, 1, 2), sparse(1), int8(1)]', msgD, idD);
%=============================================================================
% char
assert_checkerror('[sparse(1), ''a'']', sprintf(fmtU, 'double', 'char', unimplemented), idU);
assert_checkerror('[sparse(true), ''a'']', sprintf(fmtU, 'logical', 'char', _('Conversion to char from logical is not possible.')), idU);
assert_checkerror('[sparse(1i), ''a'']', sprintf(fmtU, 'double', 'char', _('Complex values cannot be converted to chars')), idU);
assert_checkerror('[''a'', sparse(1)]', unimplemented, idI);
assert_checkerror('[''a'', sparse(true)]', unimplemented, idI);
assert_checkerror('[[], sparse(1), ''a'']', unimplemented, idI);
assert_checkerror('[repmat(''a'', [1 1 2]), sparse(1)]', msgD, idD);
assert_checkerror('[zeros(1, 1, 2), sparse(1), ''a'']', msgD, idD);
%=============================================================================
% struct
fmtS = _('Conversion to struct from %s is not possible.');
msgDouble = sprintf(fmtU, 'double', 'struct', sprintf(fmtS, 'double'));
msgLogical = sprintf(fmtU, 'logical', 'struct', sprintf(fmtS, 'logical'));
assert_checkerror('[sparse(1), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[sparse(1i), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[sparse(true), struct(''a'', 1)]', msgLogical, idU);
assert_checkerror('[1, sparse(1), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[true, sparse(1), struct(''a'', 1)]', msgLogical, idU);
assert_checkerror('[''a'', sparse(1), struct(''a'', 1)]', sprintf(fmtU, 'char', 'struct', sprintf(fmtS, 'char')), idU);
assert_checkerror('[zeros(1, 1, 2), sparse(1), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[[], sparse(1), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[single([]), sparse(1), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[sparse(1), zeros(1, 1, 2), struct(''a'', 1)]', msgDouble, idU);
assert_checkerror('[struct(''a'', 1), sparse(1)]', unimplemented, idI);
assert_checkerror('[struct([]), sparse(1)]', unimplemented, idI);
assert_checkerror('[sparse([]), struct(''a'', 1)]', unimplemented, idI);
assert_checkerror('[zeros(0, 1), sparse([]), struct(''a'', 1)]', unimplemented, idI);
assert_checkerror('[zeros(0, 1), struct([]), sparse(1)]', unimplemented, idI);
assert_checkerror('[struct(''a'', cell(1, 1, 2)), sparse([])]', msgD, idD);
assert_checkerror('[zeros(0, 0, 2), sparse(1), struct(''a'', 1)]', msgD, idD);
assert_checkerror('[[], zeros(0, 0, 2), sparse(1), struct(''a'', 1)]', msgD, idD);
assert_checkerror('[int8(zeros(0, 0, 2)), sparse(1), struct(''a'', 1)]', msgD, idD);
%=============================================================================
% cell
assert_checkerror('[sparse(1), {1}]', unimplemented, idI);
assert_checkerror('[{1}, sparse(1)]', unimplemented, idI);
assert_checkerror('[{1}; sparse(1)]', unimplemented, idI);
assert_checkerror('[{}, sparse(1)]', unimplemented, idI);
assert_checkerror('[sparse(1), cell(1, 1, 2)]', unimplemented, idI);
assert_checkerror('[zeros(1, 1, 2), sparse(1), {1}]', unimplemented, idI);
assert_checkerror('[cell(1, 1, 2), sparse(1)]', msgD, idD);
%=============================================================================
