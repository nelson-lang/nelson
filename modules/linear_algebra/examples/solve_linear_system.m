%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Solve an electrical-network style linear system and report the residual.
A = [10, -2, 0; -2, 8, -1; 0, -1, 5];
b = [12; 15; 7];
x = A \ b;
disp('Solution:');
disp(x);
disp(['Residual norm: ', mat2str(norm(A * x - b))]);
