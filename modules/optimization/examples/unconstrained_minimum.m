%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Minimize a smooth two-variable objective without constraints.
objective = @(x)(x(1) - 2) .^ 2 + (x(2) + 3) .^ 2;
[solution, value] = fminsearch(objective, [0; 0], optimset('Display', 'off'));
disp('Minimum location:');
disp(solution);
disp(['Objective value: ', mat2str(value)]);
