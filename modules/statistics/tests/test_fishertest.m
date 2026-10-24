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
asserts.isequal(nargin('fishertest'), -2);
asserts.isequal(nargout('fishertest'), 3);
%=============================================================================
[h, p, stats] = fishertest([3 6; 1 7]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.576470588235295, 1e-12);
asserts.isapprox(stats.OddsRatio, 3.500000000000000, 1e-12);
asserts.isapprox(stats.ConfidenceInterval, [0.283823971587192 43.160554520803501], 1e-12);
%=============================================================================
[h2, p2, stats2] = fishertest([3 6; 1 7], 'Tail', 'right', 'Alpha', 0.01);
asserts.isequal(h2, false);
asserts.isapprox(p2, 0.335294117647059, 1e-12);
asserts.isapprox(stats2.OddsRatio, 3.500000000000000, 1e-12);
asserts.isapprox(stats2.ConfidenceInterval, [0.128892030632144 95.040786772622951], 1e-12);
%=============================================================================
[h3, p3, stats3] = fishertest([0 4; 5 1], 'Tail', 'left');
asserts.isequal(h3, true);
asserts.isapprox(p3, 0.023809523809524, 1e-12);
asserts.isequal(stats3.OddsRatio, 0);
asserts.istrue(isinf(stats3.ConfidenceInterval(1)) && stats3.ConfidenceInterval(1) < 0);
asserts.istrue(isinf(stats3.ConfidenceInterval(2)) && stats3.ConfidenceInterval(2) > 0);
%=============================================================================
[h4, p4, stats4] = fishertest([40 13; 26 21]);
asserts.isequal(h4, true);
asserts.isapprox(p4, 0.037514796645510, 1e-12);
asserts.isapprox(stats4.OddsRatio, 2.485207100591716, 1e-12);
asserts.isapprox(stats4.ConfidenceInterval, [1.062396902870157 5.813509354315508], 1e-12);
%=============================================================================
T = table([3; 1], [6; 7], 'VariableNames', {'Flu', 'NoFlu'});
[h5, p5, stats5] = fishertest(T);
asserts.isequal(h5, h);
asserts.isapprox(p5, p, 1e-12);
asserts.isapprox(stats5.OddsRatio, stats.OddsRatio, 1e-12);
%=============================================================================
asserts.checkerror('fishertest()', _('Not enough input arguments.'));
asserts.checkerror('fishertest([1 2 3])', _('X must be a 2-by-2 numeric matrix or table.'));
asserts.checkerror('fishertest({1})', _('X must be a 2-by-2 numeric matrix or table.'));
asserts.checkerror('fishertest([1 -1; 2 3])', _('X must contain nonnegative integer counts.'));
asserts.checkerror('fishertest([1 1.5; 2 3])', _('X must contain nonnegative integer counts.'));
asserts.checkerror('fishertest([0 0; 0 0])', _('X must contain at least one observation.'));
asserts.checkerror('fishertest([1 2; 3 4], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('fishertest([1 2; 3 4], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('fishertest([1 2; 3 4], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('fishertest([1 2; 3 4], ''Tail'', ''bad'')', sprintf(_('%s has an unsupported value.'), 'TAIL'));
%=============================================================================
