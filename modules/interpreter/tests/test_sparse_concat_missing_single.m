%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A sparse concatenation converts a missing operand to the class of the first
% operand that is not missing (only a double, a single or a string holds it),
% a single next to a sparse operand gives a sparse single, and an N-D operand
% cannot become sparse (a missing one next to a leading sparse operand would
% be expanded as an N-D sparse array).
%=============================================================================
R = [single(1), sparse(1)];
assert_isequal(class(R), 'single');
assert_istrue(issparse(R));
assert_isequal(full(R), single([1 1]));
R = [sparse(2); single(3)];
assert_isequal(class(R), 'single');
assert_istrue(issparse(R));
assert_isequal(full(R), single([2; 3]));
R = [single(1 + 2i), sparse(1)];
assert_isequal(class(R), 'single');
assert_istrue(issparse(R));
assert_isequal(full(R), single([1 + 2i, 1]));
R = [single([]), sparse(1)];
assert_isequal(class(R), 'single');
assert_istrue(issparse(R));
%=============================================================================
R = [missing, [], sparse(true)];
assert_isequal(class(R), 'double');
assert_istrue(issparse(R));
assert_istrue(isequaln(full(R), [NaN 1]));
R = [[], sparse(true), missing];
assert_istrue(issparse(R));
assert_istrue(isequaln(full(R), [1 NaN]));
R = [single([]), sparse(true), missing];
assert_isequal(class(R), 'single');
assert_istrue(issparse(R));
assert_istrue(isequaln(full(R), single([1 NaN])));
R = [missing, sparse(1)];
assert_istrue(issparse(R));
assert_istrue(isequaln(full(R), [NaN 1]));
%=============================================================================
id = 'Nelson:invalidConversion';
fmt = _('Conversion to %s from missing is not possible.');
msg = sprintf(fmt, 'logical');
assert_checkerror('[missing, sparse(true)]', msg, id);
assert_checkerror('[missing; sparse(true)]', msg, id);
assert_checkerror('[sparse(false), missing]', msg, id);
assert_checkerror('[true, sparse(1), missing]', msg, id);
assert_checkerror('[missing, true, sparse(1)]', msg, id);
assert_checkerror('[sparse(true), [], missing]', msg, id);
assert_checkerror('[sparse(true), sparse(1), missing]', msg, id);
assert_checkerror('[repmat(missing, 1, 1, 2), sparse(true)]', msg, id);
assert_checkerror('[int8(1), sparse(1), missing]', sprintf(fmt, 'int8'), id);
assert_checkerror('[missing, int8(1), sparse(1)]', sprintf(fmt, 'int8'), id);
assert_checkerror('[uint16(1), sparse(1), missing]', sprintf(fmt, 'uint16'), id);
assert_checkerror('[''a'', sparse(1), missing]', sprintf(fmt, 'char'), id);
assert_checkerror('[{1}, sparse(1), missing]', sprintf(fmt, 'cell'), id);
assert_checkerror('[struct(''a'', 1), sparse(1), missing]', sprintf(fmt, 'struct'), id);
assert_checkerror('[missing, @sin, sparse(1)]', sprintf(fmt, 'function_handle'), id);
%=============================================================================
id = 'Nelson:catenate:dimensionMismatch';
msg = _('Concatenation involves conversion to sparse of an array with more than 2 dimensions.');
assert_checkerror('[single(zeros(1, 1, 2)), sparse(1)]', msg, id);
assert_checkerror('[sparse(1), single(zeros(1, 1, 2))]', msg, id);
assert_checkerror('[sparse(1); single(zeros(1, 1, 2))]', msg, id);
assert_checkerror('[zeros(1, 1, 2), sparse(1)]', msg, id);
assert_checkerror('[sparse(1), true(1, 1, 2)]', msg, id);
assert_checkerror('[missing, zeros(1, 1, 2), sparse(1)]', msg, id);
assert_checkerror('[sparse(1), missing, zeros(1, 1, 2)]', msg, id);
assert_checkerror('[[], repmat(missing, 1, 1, 2), sparse(1)]', msg, id);
assert_checkerror('[single(zeros(1, 1, 2)), sparse(1), repmat(missing, 1, 1, 2)]', msg, id);
%=============================================================================
id = 'Nelson:repmat:NdSparseOutput';
msg = _('N-D sparse output arrays are not supported.');
assert_checkerror('[repmat(missing, 1, 1, 2), sparse(1)]', msg, id);
assert_checkerror('[sparse(1), repmat(missing, 1, 1, 2)]', msg, id);
assert_checkerror('[sparse(1); repmat(missing, 1, 1, 2)]', msg, id);
assert_checkerror('[sparse(1), [], repmat(missing, 1, 1, 2)]', msg, id);
assert_checkerror('[sparse(1), repmat(missing, 1, 1, 2), single(zeros(1, 1, 2))]', msg, id);
%=============================================================================
