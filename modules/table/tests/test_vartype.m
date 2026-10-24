%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = [1; 2];
B = {'x'; 'y'};
C = [true; false];
T = table(A, B, C);
R = varfun(@mean, T, 'InputVariables', vartype('double'));
asserts.isequal(R.Properties.VariableNames, {'mean_A'});
asserts.isequal(R.mean_A, 1.5);
%=============================================================================
