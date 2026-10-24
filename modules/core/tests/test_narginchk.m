%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
addpath([nelsonroot(), '/modules/core/tests/']);
%=============================================================================
asserts.checkerror('fun_narginchk()', _('Not enough input arguments.'), 'Nelson:narginchk:notEnoughInputs');
%=============================================================================
fun_narginchk(1)
%=============================================================================
fun_narginchk(1, 2)
%=============================================================================
asserts.checkerror('fun_narginchk(1, 2, 3)', _('Too many input arguments.'), 'Nelson:narginchk:tooManyInputs');
%=============================================================================
addpath([modulepath('functions_manager'), '/tests']);
asserts.isequal(runtime_narginchk_fixture(1), 1);
asserts.isequal(runtime_narginchk_fixture(1, 2), 2);
asserts.checkerror('runtime_narginchk_fixture()', _('Not enough input arguments.'), 'Nelson:narginchk:notEnoughInputs');
asserts.checkerror('runtime_narginchk_fixture(1, 2, 3)', _('Too many input arguments.'), 'Nelson:narginchk:tooManyInputs');
%=============================================================================
