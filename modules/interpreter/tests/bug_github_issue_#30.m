%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/30
% <-- Short Description -->
% search path order for functions was wrong.
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/issue_#30_1']);
r = func1()
assert_isequal(r, 1);
clear('func1')
%=============================================================================
addpath([nelsonroot(), '/modules/interpreter/tests/issue_#30_2']);
r = func1()
assert_isequal(r, 2);
clear('func1')
%=============================================================================
rmpath([nelsonroot(), '/modules/interpreter/tests/issue_#30_2']);
r = func1()
assert_isequal(r, 1);
%=============================================================================
