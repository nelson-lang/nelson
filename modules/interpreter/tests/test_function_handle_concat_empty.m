%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Function handles cannot form nonscalar arrays: empty operands are dropped.
%=============================================================================
f = function_handle.empty(0, 3);
R = [f, @sin];
assert_isequal(class(R), 'function_handle');
assert_isequal(func2str(R), 'sin');
%=============================================================================
R = [@cos; f];
assert_isequal(func2str(R), 'cos');
%=============================================================================
R = [[], @sin];
assert_isequal(func2str(R), 'sin');
%=============================================================================
R = [@sin; []];
assert_isequal(func2str(R), 'sin');
%=============================================================================
R = horzcat(f, @cos);
assert_isequal(func2str(R), 'cos');
%=============================================================================
R = [function_handle.empty(0, 1); function_handle.empty(0, 1)];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 1]);
%=============================================================================
R = [function_handle.empty, function_handle.empty];
assert_isequal(class(R), 'function_handle');
assert_isequal(size(R), [0 0]);
%=============================================================================
msg = _('Nonscalar arrays of function handles are not allowed; use cell arrays instead.');
assert_checkerror('[@sin, @cos]', msg);
assert_checkerror('[@sin; @cos]', msg);
assert_checkerror('[function_handle.empty(0, 1), function_handle.empty(0, 2)]', msg);
assert_checkerror('[function_handle.empty(0, 2), []]', msg);
%=============================================================================
