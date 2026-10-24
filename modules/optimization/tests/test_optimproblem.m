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
txt = evalc('show(prob)');
asserts.istrue(contains(txt, 'OptimizationProblem'));
asserts.istrue(contains(txt, 'minimize'));
%=============================================================================
