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
asserts.isequal(nargin('binostat'), 2);
asserts.isequal(nargout('binostat'), 2);
%=============================================================================
[m, v] = binostat([10 20], [0.25 0.5]);
asserts.isapprox(m, [2.5 10], 1e-15);
asserts.isapprox(v, [1.875 5], 1e-15);
%=============================================================================
[m, v] = binostat(single([10 20]), single([0.25 0.5]));
asserts.isequal(class(m), 'single');
asserts.isequal(class(v), 'single');
asserts.isapprox(double(m), [2.5 10], 1e-6);
asserts.isapprox(double(v), [1.875 5], 1e-6);
%=============================================================================
[m, v] = binostat([10 1.5 -1 NaN], [0.5 0.5 0.5 0.5]);
asserts.isapprox(m(1), 5, 1e-15);
asserts.isapprox(v(1), 2.5, 1e-15);
asserts.istrue(all(isnan(m(2:4))));
asserts.istrue(all(isnan(v(2:4))));
%=============================================================================
[m, v] = binostat([10 10], [-0.1 1.1]);
asserts.istrue(all(isnan(m)));
asserts.istrue(all(isnan(v)));
%=============================================================================
asserts.checkerror('binostat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('binostat({1}, 0.5)', _('N must be a nonnegative integer numeric array.'));
asserts.checkerror('binostat(1, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
[binoMean, binoVariance] = binostat(4, 0.5);
asserts.isequal(binoMean, 2);
asserts.isequal(binoVariance, 1);
%=============================================================================
