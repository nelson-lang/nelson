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
assert_checkerror('fun_narginchk()', _('Wrong number of input arguments.'), 'Nelson:narginchk:notEnoughInputs');
%=============================================================================
fun_narginchk(1)
%=============================================================================
fun_narginchk(1,2)
%=============================================================================
assert_checkerror('fun_narginchk(1, 2, 3)', _('Wrong number of input arguments.'), 'Nelson:narginchk:tooManyInputs');
%=============================================================================
