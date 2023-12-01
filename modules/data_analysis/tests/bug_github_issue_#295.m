%=============================================================================
% Copyright (c) 2017 Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Issue URL -->
% https://github.com/nelson-lang/nelson/issues/295
% <-- Short Description -->
% sort did not return an wrong error message for struct
%=============================================================================
% <--ENGLISH IMPOSED-->
tests_dir = [nelsonroot(), '/modules/mex'];
nonreg_tests = dir([tests_dir, 'test_*.m'])
msg = sprintf(_('Check for incorrect argument data type or missing argument in call to function ''%s''.'), 'sort');

assert_checkerror('sort(nonreg_tests)', msg);
%=============================================================================
assert_checkerror('sort(cell(1,1))', msg);
%=============================================================================
