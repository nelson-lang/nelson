%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = optimvar('x');
prob = optimproblem('Objective', (x - 2) .^ 2);
problem = prob2struct(prob, struct('x', 0));
asserts.istrue(isstruct(problem));
asserts.isequal(problem.solver, 'quadprog');
asserts.isequal(problem.x0, 0);
%=============================================================================
