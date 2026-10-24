%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--REUSE PROCESS-->
%=============================================================================
asserts.isequal(nargin('runstest'), -2);
asserts.isequal(nargout('runstest'), 3);
%=============================================================================
[h, p, stats] = runstest([1 2 3 4 5 0 -1 -2]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.228571428571428, 1e-12);
asserts.isequal(stats.nruns, 3);
asserts.isequal(stats.n1, 4);
asserts.isequal(stats.n0, 4);
asserts.isapprox(stats.z, -1.527525231651947, 1e-12);
%=============================================================================
[h2, p2, stats2] = runstest([1 3 2 4 3 5 4], 'ud');
asserts.isequal(h2, false);
asserts.isapprox(p2, 0.067889154861829, 1e-12);
asserts.isequal(stats2.nruns, 6);
asserts.isequal(stats2.n1, 3);
asserts.isequal(stats2.n0, 3);
asserts.isapprox(stats2.z, 1.825741858350554, 1e-12);
%=============================================================================
[h3, p3, stats3] = runstest([1 1 2 2 3 3 4 4], 2.5, 'Tail', 'left');
asserts.isequal(h3, true);
asserts.isapprox(p3, 0.028571428571429, 1e-12);
asserts.isequal(stats3.nruns, 2);
asserts.isequal(stats3.n1, 4);
asserts.isequal(stats3.n0, 4);
asserts.isapprox(stats3.z, -2.291287847477920, 1e-12);
%=============================================================================
[h4, p4, stats4] = runstest([1 3 1 3 1 3], 2, ...
  'Tail', 'right', 'Method', 'approximate', 'Alpha', 0.1);
asserts.isequal(h4, true);
asserts.isapprox(p4, 0.033944577430914, 1e-12);
asserts.isequal(stats4.nruns, 6);
asserts.isequal(stats4.n1, 3);
asserts.isequal(stats4.n0, 3);
asserts.isapprox(stats4.z, 1.825741858350554, 1e-12);
%=============================================================================
[h5, p5, stats5] = runstest(single([1 NaN 2 0 -1 -2]), 0);
asserts.isequal(h5, false);
asserts.isequal(class(p5), 'double');
asserts.isapprox(p5, 0.666666666666667, 1e-12);
asserts.isequal(stats5.nruns, 2);
asserts.isequal(stats5.n1, 2);
asserts.isequal(stats5.n0, 2);
asserts.isapprox(stats5.z, -1.224744871391589, 1e-12);
%=============================================================================
asserts.checkerror('runstest({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('runstest([NaN NaN])', _('X must contain at least one non-NaN value.'));
asserts.checkerror('runstest([1 1 1], 1)', _('X must contain at least one value in each run category.'));
asserts.checkerror('runstest([1 2 3], {1})', _('Reference value must be a real numeric scalar or ''ud''.'));
asserts.checkerror('runstest([1 2 3], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('runstest([1 2 3], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('runstest([1 2 3], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('runstest([1 2 3], ''Method'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'METHOD'));
asserts.checkerror('runstest([1 2 3], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
%=============================================================================
