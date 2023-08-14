%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
if strcmp(overloadmode(), 'all')
    addpath([modulepath('display_format', 'tests'), '/overload'])
    M = eye(3, 3);
    R = formattedDisplayText(M);
    REF = "Formatted disp as [1 0 0; 0 1 0; 0 0 1]"
    assert_isequal(R, REF);
end
%=============================================================================
