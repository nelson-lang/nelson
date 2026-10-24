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
%=============================================================================
asserts.isequal(nargin('geofit'), 2);
asserts.isequal(nargout('geofit'), 2);
%=============================================================================
x = [0 1 2 3 5 8];
[P, PCI] = geofit(x);
asserts.isapprox(P, 0.24, 1e-12);
asserts.isapprox(PCI, [0.1119956371576484; 0.4415577784197786], 1e-12);
%=============================================================================
[P, PCI] = geofit(x, 0.01);
asserts.isapprox(P, 0.24, 1e-12);
asserts.isapprox(PCI, [0.08635920434147905; 0.5133875692649073], 1e-12);
%=============================================================================
xm = [0 2; 1 3; 2 4; 3 6; 5 8];
[PM, PCIM] = geofit(xm);
asserts.isequal(size(PM), [1 2]);
asserts.isapprox(PM, [0.3125 0.1785714285714286], 1e-12);
asserts.isapprox(PCIM, [0.1363923003711153 0.07633827514844192;
                       0.5667660187316496 0.3637924916891246], 1e-12);
%=============================================================================
[P, PCI] = geofit(single(x));
asserts.isequal(class(P), 'double');
asserts.isequal(class(PCI), 'double');
asserts.isapprox(P, 0.24, 1e-12);
%=============================================================================
[P, PCI] = geofit([1 NaN 3]);
asserts.istrue(isnan(P));
asserts.istrue(all(isnan(PCI(:))));
[P, PCI] = geofit([0 0 0]);
asserts.isequal(P, 1);
asserts.isequal(PCI, [1; 1]);
%=============================================================================
asserts.checkerror('geofit()', _('Not enough input arguments.'));
asserts.checkerror('geofit([])', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('geofit(ones(1, 1, 2))', _('X must be a nonempty vector or matrix.'));
asserts.checkerror('geofit({1 2 3})', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('geofit([-1 1 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('geofit([1.5 2 3])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('geofit([1 Inf 2])', sprintf(_('%s must contain nonnegative integer finite values.'), 'X'));
asserts.checkerror('geofit([1 2 3], {0.05})', sprintf(_('%s must be numeric.'), 'ALPHA'));
asserts.checkerror('geofit([1 2 3], -0.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
asserts.checkerror('geofit([1 2 3], 1.1)', sprintf(_('%s must be a scalar in the range [0,1].'), 'ALPHA'));
%=============================================================================