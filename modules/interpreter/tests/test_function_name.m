%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/']);
%=============================================================================
msg = sprintf(_('Filename and function name are not same (%s vs %s).'), 'toto', 'tutu');
assert_checkerror('tutu()', msg);
%=============================================================================
R = toto();
assert_isequal(R, 'hello');
%=============================================================================
msg = sprintf(_('Function ''%s'' has already been declared within this scope.'), 'tata');
assert_checkerror('tata()', msg);
%=============================================================================
