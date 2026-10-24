%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 1-->
%=============================================================================
% cat along dim > 2 with function handles: 0x0 operands are dropped, the
% others must agree, the result is a function handle not exceeding 1 along any
% dimension.
%=============================================================================
R = cat(3, @sin, []);
assert_isequal(class(R), 'function_handle');
assert_isequal(func2str(R), 'sin');
R = cat(3, [], [], @sin);
assert_isequal(func2str(R), 'sin');
R = cat(4, @sin, [], []);
assert_isequal(func2str(R), 'sin');
R = cat(3, {}, @sin);
assert_isequal(func2str(R), 'sin');
R = cat(3, @sin, struct([]));
assert_isequal(func2str(R), 'sin');
R = cat(3, @sin, function_handle.empty);
assert_isequal(func2str(R), 'sin');
%=============================================================================
% an empty operand of size 1x1x0 agrees with a scalar handle
R = cat(3, @sin, zeros(1, 1, 0));
assert_isequal(func2str(R), 'sin');
R = cat(3, @sin, zeros(1, 1, 0), zeros(1, 1, 0));
assert_isequal(func2str(R), 'sin');
R = cat(3, @sin, function_handle.empty(1, 1, 0));
assert_isequal(func2str(R), 'sin');
%=============================================================================
% empty operands only: the result is an empty function handle
R = cat(3, function_handle.empty, zeros(0, 1));
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, zeros(0, 1), function_handle.empty);
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, function_handle.empty(0, 1), struct([]));
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, function_handle.empty(0, 1), [], []);
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, zeros(0, 1), struct([]), function_handle.empty);
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, function_handle.empty(0, 1), zeros(0, 1, 0));
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, function_handle.empty(0, 1, 0), zeros(0, 1));
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = cat(3, function_handle.empty(1, 1, 0), function_handle.empty(1, 1, 0));
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [1 1 0]);
%=============================================================================
msg = _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.');
id = 'Nelson:err_non_scalar_function_handles';
assert_checkerror('cat(3, @sin, @cos)', msg, id);
assert_checkerror('cat(3, @sin, @cos, [])', msg, id);
assert_checkerror('cat(3, function_handle.empty, [])', msg, id);
assert_checkerror('cat(3, function_handle.empty, function_handle.empty)', msg, id);
assert_checkerror('cat(3, function_handle.empty(0, 1), zeros(0, 1))', msg, id);
assert_checkerror('cat(3, function_handle.empty(0, 1), function_handle.empty(0, 1))', msg, id);
assert_checkerror('cat(3, function_handle.empty(1, 0), zeros(1, 0))', msg, id);
assert_checkerror('cat(4, function_handle.empty(0, 1), zeros(0, 1))', msg, id);
%=============================================================================
msg = _('Dimensions of arrays being concatenated are not consistent.');
id = 'Nelson:catenate:dimensionMismatch';
assert_checkerror('cat(3, @sin, zeros(0, 2))', msg, id);
assert_checkerror('cat(3, zeros(0, 2), @sin)', msg, id);
assert_checkerror('cat(3, @sin, zeros(1, 0))', msg, id);
assert_checkerror('cat(3, @sin, zeros(1, 0, 0))', msg, id);
assert_checkerror('cat(3, @sin, zeros(0, 0, 2))', msg, id);
assert_checkerror('cat(3, @sin, function_handle.empty(0, 1))', msg, id);
assert_checkerror('cat(3, function_handle.empty(0, 1), zeros(0, 2))', msg, id);
assert_checkerror('cat(3, function_handle.empty(1, 0), zeros(0, 1))', msg, id);
%=============================================================================
% a non-empty operand of another class cannot become a function handle: the
% first one in argument order names the error, before any size rule
id = 'Nelson:UnableToConvertToFunctionHandle';
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'double');
assert_checkerror('cat(3, @sin, 5)', msg, id);
assert_checkerror('cat(3, 5, @sin)', msg, id);
assert_checkerror('cat(4, @sin, ones(2, 2))', msg, id);
assert_checkerror('cat(3, @sin, @cos, 5)', msg, id);
assert_checkerror('cat(3, [], @sin, 1)', msg, id);
assert_checkerror('cat(3, @sin, zeros(0, 2), 5)', msg, id);
assert_checkerror('cat(3, zeros(0, 2), @sin, 5)', msg, id);
assert_checkerror('cat(3, @sin, zeros(1, 1, 0), 5)', msg, id);
assert_checkerror('cat(3, ones(1, 1, 2), @sin, 1)', msg, id);
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'cell');
assert_checkerror('cat(3, @sin, {1})', msg, id);
assert_checkerror('cat(3, @sin, @cos, {1}, 1)', msg, id);
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'logical');
assert_checkerror('cat(3, @sin, true, {1})', msg, id);
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'int8');
assert_checkerror('cat(4, int8(1), @sin)', msg, id);
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'char');
assert_checkerror('cat(3, @sin, ''a'')', msg, id);
msg = sprintf(_('Conversion to function_handle from %s is not possible.'), 'struct');
assert_checkerror('cat(3, @sin, struct(''a'', 1))', msg, id);
%=============================================================================
% a string, sparse or missing operand follows the conversion rules of the
% operators
msg = _('Conversion to string from function_handle is not possible.');
id = 'Nelson:invalidConversion';
assert_checkerror('cat(3, @sin, "a")', msg, id);
assert_checkerror('cat(3, 1, @sin, "a")', msg, id);
assert_checkerror('cat(3, "a", @sin)', msg, id);
msg = _('Attempt to convert to unimplemented sparse type');
id = 'Nelson:unimplementedSparseType';
assert_checkerror('cat(3, @sin, sparse(1))', msg, id);
assert_checkerror('cat(3, @sin, 1, sparse(1))', msg, id);
assert_checkerror('cat(3, [], sparse(1), @sin)', msg, id);
id = 'Nelson:UnableToConvert';
fmt = _('The following error occurred converting from %s to function_handle:\nToo many output arguments.');
msg = sprintf(fmt, 'double');
assert_checkerror('cat(3, sparse(1), @sin)', msg, id);
assert_checkerror('cat(3, sparse([]), @sin)', msg, id);
assert_checkerror('cat(3, ones(1, 1, 2), sparse(1), @sin)', msg, id);
assert_checkerror('cat(4, ones(1, 1, 2), sparse(1), @sin)', msg, id);
assert_checkerror('cat(3, ones(1, 1, 2), @sin, sparse(1))', msg, id);
assert_checkerror('cat(3, {1}, sparse(1), @sin)', sprintf(fmt, 'cell'), id);
assert_checkerror('cat(3, int8(1), sparse(1), @sin)', sprintf(fmt, 'int8'), id);
assert_checkerror('cat(3, sparse(true), @sin)', sprintf(fmt, 'logical'), id);
msg = _('Concatenation involves conversion to sparse of an array with more than 2 dimensions.');
id = 'Nelson:catenate:dimensionMismatch';
assert_checkerror('cat(3, zeros(1, 1, 0), sparse(1), @sin)', msg, id);
assert_checkerror('cat(3, function_handle.empty(1, 1, 0), sparse(1), @sin)', msg, id);
assert_checkerror('cat(3, @sin, sparse(1), {1})', _('Attempt to convert to unimplemented sparse type'), 'Nelson:unimplementedSparseType');
try
  [missing, @sin];
catch ME
end
assert_checkerror('cat(3, missing, @sin)', ME.message, ME.identifier);
assert_checkerror('cat(3, @sin, missing)', ME.message, ME.identifier);
%=============================================================================
