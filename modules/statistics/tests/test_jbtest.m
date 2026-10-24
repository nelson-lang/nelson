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
asserts.isequal(nargin('jbtest'), -2);
asserts.isequal(nargout('jbtest'), 4);
%=============================================================================
[h, p, jbstat, critval] = jbtest([1 2 3 4 5]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.838583041649059, 1e-12);
asserts.isapprox(jbstat, 0.352083333333333, 1e-12);
asserts.isapprox(critval, 5.99146454710810, 1e-12);
%=============================================================================
[h2, p2, jbstat2, critval2] = jbtest([1 1 1 1]);
asserts.isequal(h2, false);
asserts.isequal(p2, 1);
asserts.isequal(jbstat2, 0);
asserts.isapprox(critval2, 5.99146454710810, 1e-12);
%=============================================================================
[h3, p3, jbstat3, critval3] = jbtest([1 2 3 10 20], 0.1);
asserts.isequal(h3, false);
asserts.isapprox(p3, 0.666521353388826, 1e-12);
asserts.isapprox(jbstat3, 0.811366203567641, 1e-12);
asserts.isapprox(critval3, 4.60517018598811, 1e-12);
%=============================================================================
[h4, p4, jbstat4, critval4] = jbtest([1 NaN 2 3 4 5], [], 0.01);
asserts.isequal(h4, false);
asserts.isapprox(p4, p, 1e-12);
asserts.isapprox(jbstat4, jbstat, 1e-12);
asserts.isapprox(critval4, critval, 1e-12);
%=============================================================================
[h5, p5, jbstat5, critval5] = jbtest(single([1 2 3 4 5]));
asserts.isequal(h5, false);
asserts.isequal(class(p5), 'double');
asserts.isapprox(p5, p, 1e-12);
asserts.isapprox(jbstat5, jbstat, 1e-12);
asserts.isapprox(critval5, critval, 1e-12);
%=============================================================================
[h6, p6, jbstat6, critval6] = jbtest([zeros(1, 20) 100]);
asserts.isequal(h6, true);
asserts.istrue(p6 > 0 && p6 < 1e-50);
asserts.isapprox(jbstat6, 288.577187500000, 1e-12);
asserts.isapprox(critval6, 5.99146454710810, 1e-12);
%=============================================================================
[h7, p7, jbstat7, critval7] = jbtest([1 2]);
asserts.isequal(h7, false);
asserts.istrue(isnan(p7));
asserts.istrue(isnan(jbstat7) || isfinite(jbstat7));
asserts.istrue(isnan(critval7));
%=============================================================================
asserts.checkerror('jbtest()', _('Not enough input arguments.'));
asserts.checkerror('jbtest([1], 0.05, 0.01, 1)', _('Too many input arguments.'));
asserts.checkerror('jbtest({1})', sprintf(_('%s must be a real numeric vector.'), 'X'));
asserts.checkerror('jbtest([NaN])', sprintf(_('%s must contain at least one non-NaN observation.'), 'X'));
asserts.checkerror('jbtest([1 2 3], 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('jbtest([1 2 3], 0.05, -1)', sprintf(_('%s must be a nonnegative scalar.'), 'MCTOL'));
%=============================================================================
