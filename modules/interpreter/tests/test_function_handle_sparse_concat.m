%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function handle has no sparse form. The first operand decides the error:
% any value but a handle or a non-sparse empty would first have to become a
% handle, then an N-D one cannot become sparse, otherwise a sparse handle.
%=============================================================================
id = 'Nelson:unimplementedSparseType';
msg = _('Attempt to convert to unimplemented sparse type');
assert_checkerror('[function_handle.empty(0, 1), sparse([])]', msg, id);
assert_checkerror('[function_handle.empty(0, 1); sparse([])]', msg, id);
assert_checkerror('[function_handle.empty, sparse([])]', msg, id);
assert_checkerror('[@sin, sparse(1)]', msg, id);
assert_checkerror('[@sin; sparse(1)]', msg, id);
assert_checkerror('[@sin, sparse(true)]', msg, id);
assert_checkerror('[@sin, [], sparse(1)]', msg, id);
assert_checkerror('[@sin, sparse(1), function_handle.empty(0, 0, 3)]', msg, id);
assert_checkerror('[[], sparse(1), @sin]', msg, id);
assert_checkerror('[zeros(0, 1), sparse(1), @sin]', msg, id);
assert_checkerror('[{}, sparse(1), @sin]', msg, id);
assert_checkerror('horzcat(@sin, sparse(1))', msg, id);
assert_checkerror('cat(1, @sin, sparse(1))', msg, id);
%=============================================================================
id = 'Nelson:UnableToConvert';
fmt = _('The following error occurred converting from %s to function_handle:\nToo many output arguments.');
msgDouble = sprintf(fmt, 'double');
assert_checkerror('[sparse([]), function_handle.empty(0, 1)]', msgDouble, id);
assert_checkerror('[sparse([]); function_handle.empty(0, 1)]', msgDouble, id);
assert_checkerror('[sparse(1), @sin]', msgDouble, id);
assert_checkerror('[sparse(1i), @sin]', msgDouble, id);
assert_checkerror('[sparse(1), [], @sin]', msgDouble, id);
assert_checkerror('[sparse(1), function_handle.empty(0, 0, 3)]', msgDouble, id);
assert_checkerror('[1, @sin, sparse(1)]', msgDouble, id);
assert_checkerror('vertcat(sparse(1), @sin)', msgDouble, id);
assert_checkerror('[sparse(true), @sin]', sprintf(fmt, 'logical'), id);
assert_checkerror('[zeros(1, 1, 2), sparse(1), @sin]', msgDouble, id);
%=============================================================================
id = 'Nelson:catenate:dimensionMismatch';
msg = _('Concatenation involves conversion to sparse of an array with more than 2 dimensions.');
assert_checkerror('[function_handle.empty(0, 0, 3), sparse([])]', msg, id);
assert_checkerror('[function_handle.empty(0, 0, 3); sparse(1)]', msg, id);
assert_checkerror('[function_handle.empty(0, 1, 0), sparse([])]', msg, id);
assert_checkerror('[zeros(0, 0, 2), sparse(1), @sin]', msg, id);
%=============================================================================
