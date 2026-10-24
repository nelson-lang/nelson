%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Empty operands of any class next to function handles: the result keeps the
% size of the plain empty concatenation, a dimension above 1 is an error.
%=============================================================================
f01 = function_handle.empty(0, 1);
R = [f01, struct.empty];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [struct.empty; f01];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [f01, [], struct.empty];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [f01, {}];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [{}; f01];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [f01, ''];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [f01; true(0, 0); int8([]); single([])];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
%=============================================================================
R = [f01; f01];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [f01; zeros(0, 1)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
R = [function_handle.empty, zeros(1, 0)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [1 0]);
R = [f01, zeros(1, 0)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
R = [f01, zeros(0, 0, 3)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
R = [function_handle.empty(0, 2), zeros(2, 0)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
R = [function_handle.empty(0, 0, 3); cell(0, 1)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
%=============================================================================
msg = _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.');
assert_checkerror('[function_handle.empty(0, 1), function_handle.empty(0, 1)]', msg);
assert_checkerror('[function_handle.empty(0, 1), zeros(0, 1)]', msg);
assert_checkerror('[function_handle.empty(1, 0); zeros(1, 0)]', msg);
assert_checkerror('[function_handle.empty(0, 1), struct.empty(0, 1)]', msg);
assert_checkerror('[function_handle.empty(0, 0, 3), []]', msg);
assert_checkerror('[function_handle.empty(0, 0, 3); {}]', msg);
assert_checkerror('[@sin, [], @cos]', msg);
assert_checkerror('[function_handle.empty(0, 1); function_handle.empty(0, 2)]', msg);
assert_checkerror('[function_handle.empty(0, 1), function_handle.empty(2, 0)]', msg);
assert_checkerror('[function_handle.empty(0, 1); zeros(0, 3)]', msg);
assert_checkerror('[function_handle.empty(1, 0), zeros(0, 2)]', msg);
assert_checkerror('[function_handle.empty(0, 2); cell(0, 1)]', msg);
%=============================================================================
R = [@sin, {}];
assert_isequal(func2str(R), 'sin');
R = [{}; @sin];
assert_isequal(func2str(R), 'sin');
R = [@sin, cell(0, 2)];
assert_isequal(func2str(R), 'sin');
R = [@sin, struct('a', {})];
assert_isequal(func2str(R), 'sin');
R = [@sin, ''];
assert_isequal(func2str(R), 'sin');
R = [@sin; zeros(2, 0)];
assert_isequal(func2str(R), 'sin');
R = [@sin, zeros(0, 2)];
assert_isequal(func2str(R), 'sin');
R = [@sin, function_handle.empty(2, 0)];
assert_isequal(func2str(R), 'sin');
R = [@sin; true(0, 0); int8([]); single([])];
assert_isequal(func2str(R), 'sin');
% a non-empty operand that is not a function handle cannot be converted
failed = false;
try
  R = [@sin, {1}];
catch
  failed = true;
end
assert_istrue(failed);
%=============================================================================
msg = _('Dimensions of arrays being concatenated are not consistent.');
assert_checkerror('[@sin, zeros(0, 0, 3)]', msg);
assert_checkerror('[function_handle.empty(0, 0, 3); @sin]', msg);
assert_checkerror('[@sin, zeros(0, 1, 2)]', msg);
%=============================================================================
% an empty function handle cannot be converted to a string
msg = sprintf(_('Conversion to string from %s is not possible.'), 'function_handle');
assert_checkerror('[function_handle.empty, string.empty]', msg);
assert_checkerror('[function_handle.empty, string.empty]', msg, 'Nelson:invalidConversion');
assert_checkerror('[strings(0, 1); function_handle.empty(0, 1)]', msg);
assert_checkerror('[strings(0, 1); function_handle.empty(0, 1)]', msg, 'Nelson:invalidConversion');
%=============================================================================
