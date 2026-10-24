%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
R = evalc('f = function_handle.empty(0, 3)');
assert_istrue(contains(R, ['0', char(215), '3 empty function_handle array']));
assert_isfalse(contains(R, 'with value'));
%=============================================================================
R = evalc('disp(function_handle.empty)');
assert_isfalse(contains(R, 'with value'));
%=============================================================================
R = evalc('c = {function_handle.empty(0, 2)}');
assert_istrue(contains(R, ['{0', char(215), '2 function_handle}']));
%=============================================================================
