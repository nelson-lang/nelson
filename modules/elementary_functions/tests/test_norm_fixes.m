%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(norm([3 4],'inf'), 4);
asserts.isequal(norm([3 4],'Inf'), 4);
asserts.isequal(norm([3 4],'INF'), 4);
asserts.isequal(norm([3 4],'f'), 5);
asserts.isequal(norm([3 4],'fr'), 5);
asserts.isequal(norm([3 4],'i'), 4);
asserts.isequal(norm([3 4],'in'), 4);
asserts.isequal(norm([3 4],'Fro'), 5);
asserts.isequal(norm([3 4],"fro"), 5);
asserts.isequal(norm([3 4],"inf"), 4);
asserts.checkerror('norm([3 4],true)', _('Wrong value for #2 argument.'));
asserts.isequal(norm([3 4],sparse(1)), 7);
asserts.isequal(norm([1e200 1e200]), 1.414213562373095e+200);
asserts.isequal(norm([1e-200 1e-200]), 1.414213562373095e-200);
asserts.isequal(norm([1e200 1e200],3), 1.2599210498948731e+200);
asserts.isequal(norm([1e-200 1e-200],3), 1.2599210498948731e-200);
asserts.isequal(norm([1e200 1e200;1e200 1e200],'fro'), 1.9999999999999999e+200);
asserts.isequal(norm([1e-200 1e-200;1e-200 1e-200],'fro'), 2e-200);
asserts.isequal(norm([NaN 1;2 3],1), NaN);
asserts.isequal(norm([1e200+1e200i 1]), 1.414213562373095e+200);
asserts.isequal(norm(single([1e30 1e30])), single(1.41421351e+30));
asserts.isequal(norm(single([1e-30 1e-30])), single(1.41421356e-30));
asserts.isequal(norm(single([1e-30 1e-30]),3), single(1.25992108e-30));
asserts.isequal(norm([3 4;1 2],'inf'), 7);
asserts.isequal(norm([3 4;1 2],'f'), 5.4772255750516612);
asserts.isequal(norm([1e200 0],3), 9.9999999999999997e+199);
asserts.isequal(norm([0 1e-200],3), 9.9999999999999998e-201);
asserts.isequal(norm([1 2 3],-1.5), 0.74792782156719784);
asserts.isequal(norm([1e160 1e160]), 1.4142135623730951e+160);
asserts.isequal(norm(single([1e-20 1e-20])), single(1.41421357e-20));
asserts.isequal(norm(single([1e20 1e20])), single(1.41421358e+20));
asserts.isequal(norm(single([1e19 1e19]),3), single(1.25992103e+19));
asserts.isequal(norm(single([1e-19 1e-19]),3), single(1.25992105e-19));
asserts.isequal(norm(realmin*[1 1]), 3.1467296279827175e-308);
asserts.isequal(norm(5e-324*[1 1]), 4.9406564584124654e-324);
asserts.isequal(norm(5e-324*[1 1],3), 4.9406564584124654e-324);
asserts.isequal(norm(zeros(1,0),NaN), NaN);
asserts.isequal(norm([1 Inf;2 NaN],1), NaN);
asserts.isequal(norm([1 NaN;2 3],1), NaN);
%=============================================================================
