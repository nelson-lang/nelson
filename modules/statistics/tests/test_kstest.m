%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--REUSE PROCESS-->
asserts.isequal(nargin('kstest'), -1);
asserts.isequal(nargout('kstest'), 4);
%=============================================================================
x = [-1.2 -0.4 0.1 0.3 0.8];
[h, p, ksstat, cv] = kstest(x);
asserts.isequal(h, false);
asserts.isapprox(p, 0.957493541127802, 1e-12);
asserts.isapprox(ksstat, 0.211855398583397, 1e-12);
asserts.isapprox(cv, 0.607361461908305, 1e-12);
%=============================================================================
cdf = [-2 0; -1 0.2; 0 0.5; 1 0.8; 2 1];
[h, p, ksstat, cv] = kstest(x, 'CDF', cdf);
asserts.isequal(h, false);
asserts.isapprox(p, 0.829019218303356, 1e-12);
asserts.isapprox(ksstat, 0.26, 1e-12);
asserts.isapprox(cv, 0.607361461908305, 1e-12);
%=============================================================================
xobj = [0 1 2];
cdfFunction = @(z) 0.2 + 0.3 .* z;
[h, p, ksstat, cv] = kstest(xobj, 'CDF', cdfFunction);
asserts.isequal(h, false);
asserts.isapprox(p, 0.998536663772354, 1e-12);
asserts.isapprox(ksstat, 0.2, 1e-12);
asserts.isapprox(cv, 0.784100275699685, 1e-12);
%=============================================================================
addpath([modulepath('statistics', 'tests'), '/classdef']);
cdfObject = NelsonKSTestCDF();
[h, p, ksstat, cv] = kstest(xobj, 'CDF', cdfObject);
asserts.isequal(h, false);
asserts.isapprox(p, 0.998536663772354, 1e-12);
asserts.isapprox(ksstat, 0.2, 1e-12);
asserts.isapprox(cv, 0.784100275699685, 1e-12);
%=============================================================================
[h, p, ksstat] = kstest(x, 'Tail', 'larger');
asserts.isequal(h, false);
asserts.isapprox(p, 0.638376465024447, 1e-12);
asserts.isapprox(ksstat, 0.211855398583397, 1e-12);
%=============================================================================
[h, p, ksstat] = kstest(x, 'Tail', 'smaller');
asserts.isequal(h, false);
asserts.isapprox(p, 0.811371926224080, 1e-12);
asserts.isapprox(ksstat, 0.144578258389676, 1e-12);
%=============================================================================
[h, p, ksstat] = kstest(x, 'C', cdf, 'Ta', 'smaller');
asserts.isequal(h, false);
asserts.isapprox(p, 0.723250242379842, 1e-12);
asserts.isapprox(ksstat, 0.18, 1e-12);
%=============================================================================
[h, p, ksstat] = kstest([x NaN]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.957493541127802, 1e-12);
asserts.isapprox(ksstat, 0.211855398583397, 1e-12);
%=============================================================================
asserts.checkerror('kstest([1 2; 3 4])', sprintf(_('%s must be a vector.'), 'X'));
asserts.checkerror('kstest([NaN NaN])', _('X must contain at least one non-NaN value.'));
asserts.checkerror('kstest(x, ''CDF'', [0 0.5 1])', _('CDF must be a two-column matrix.'));
asserts.checkerror('kstest(x, ''CDF'', [0 0; 1 1])', _('CDF must cover all sample values.'));
asserts.checkerror('kstest(xobj, ''CDF'', @(z) 0.5)', sprintf(_('CDF function must return %s.'), 'one value per sample'));
asserts.checkerror('kstest(xobj, ''CDF'', @(z) z + 2)', sprintf(_('%s must be in the range [0,1].'), 'CDF values'));
asserts.checkerror('kstest(x, ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('kstest(x, ''Tail'', ''bad'')', _('TAIL must be ''unequal'', ''larger'', or ''smaller''.'));
%=============================================================================
