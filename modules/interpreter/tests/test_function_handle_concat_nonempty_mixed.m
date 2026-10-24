%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A nonempty operand of another class next to function handles cannot be
% converted: the first such operand names the error, before the nonscalar
% function handle rule.
%=============================================================================
msgFmt = _('Conversion to function_handle from %s is not possible.');
id = 'Nelson:UnableToConvertToFunctionHandle';
%=============================================================================
msg = sprintf(msgFmt, 'double');
assert_checkerror('[@sin, 1]', msg, id);
assert_checkerror('[1, @sin]', msg, id);
assert_checkerror('[@sin; 1]', msg, id);
assert_checkerror('[1; @sin]', msg, id);
assert_checkerror('horzcat(@sin, 1)', msg, id);
assert_checkerror('vertcat(1, @sin)', msg, id);
assert_checkerror('[@sin, [1 2]]', msg, id);
assert_checkerror('[@sin, 1+2i]', msg, id);
assert_checkerror('[@sin, [], 1]', msg, id);
assert_checkerror('[@sin, zeros(1, 0), 1]', msg, id);
assert_checkerror('[1, function_handle.empty(0, 1)]', msg, id);
assert_checkerror('[function_handle.empty(0, 1), 1]', msg, id);
assert_checkerror('[@sin, @cos, 1]', msg, id);
assert_checkerror('[1, @sin, @cos]', msg, id);
assert_checkerror('[@sin, 1, {1}]', msg, id);
%=============================================================================
assert_checkerror('[@sin, true]', sprintf(msgFmt, 'logical'), id);
assert_checkerror('[@sin, ''a'']', sprintf(msgFmt, 'char'), id);
assert_checkerror('[@sin, int8(1)]', sprintf(msgFmt, 'int8'), id);
assert_checkerror('[@sin; uint16(3)]', sprintf(msgFmt, 'uint16'), id);
assert_checkerror('[@sin, single(1)]', sprintf(msgFmt, 'single'), id);
assert_checkerror('[int8(1), 1, @sin]', sprintf(msgFmt, 'int8'), id);
assert_checkerror('[zeros(1, 0), @sin, true]', sprintf(msgFmt, 'logical'), id);
assert_checkerror('[@sin, cell(1, 0), true]', sprintf(msgFmt, 'logical'), id);
assert_checkerror('[@sin, true, int8(2)]', sprintf(msgFmt, 'logical'), id);
%=============================================================================
msg = sprintf(msgFmt, 'cell');
assert_checkerror('[@sin, {1}]', msg, id);
assert_checkerror('[{1}, @sin]', msg, id);
assert_checkerror('[{1}; @sin]', msg, id);
assert_checkerror('[@sin, {1}, 1]', msg, id);
assert_checkerror('[function_handle.empty(0, 1), {1}]', msg, id);
%=============================================================================
assert_checkerror('[@sin, struct(''a'', 1)]', sprintf(msgFmt, 'struct'), id);
%=============================================================================
R = [@sin, struct([])];
assert_isequal(class(R), 'function_handle');
assert_isequal(func2str(R), 'sin');
%=============================================================================
msg = _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.');
assert_checkerror('[@sin, @cos]', msg, 'Nelson:err_non_scalar_function_handles');
%=============================================================================
