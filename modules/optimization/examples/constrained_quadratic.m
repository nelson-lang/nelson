%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Minimize a quadratic objective under bounds and a linear inequality.
H = 2 * eye(2);
f = [-2; -4];
A = [1, 1];
b = 2;
[solution, value] = quadprog(H, f, A, b, [], [], [0; 0], []);
disp('Constrained solution:');
disp(solution);
disp(['Objective value: ', mat2str(value)]);
