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
asserts.isequal(nargin('kstest2'), -1);
asserts.isequal(nargout('kstest2'), 3);
%=============================================================================
x1 = [1 2 3 4 5];
x2 = [2 3 4 6 8 10];
[h, p, ks2stat] = kstest2(x1, x2);
asserts.isequal(h, false);
asserts.isapprox(p, 0.367001385090225, 1e-12);
asserts.isapprox(ks2stat, 0.5, 1e-12);
%=============================================================================
[h, p, ks2stat] = kstest2(x1, x2, 'Tail', 'larger');
asserts.isequal(h, false);
asserts.isapprox(p, 0.255729159913101, 1e-12);
asserts.isapprox(ks2stat, 0.5, 1e-12);
%=============================================================================
[h, p, ks2stat] = kstest2(x1, x2, 'Tail', 'smaller');
asserts.isequal(h, false);
asserts.isequal(p, 1);
asserts.isequal(ks2stat, 0);
%=============================================================================
[h, p, ks2stat] = kstest2([1 1 2 2 3 NaN], [1 2 2 4]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.994330585835702, 1e-12);
asserts.isapprox(ks2stat, 0.25, 1e-12);
%=============================================================================
[h, p, ks2stat] = kstest2(single([10 11 12 13]), single([1 2 3]), 'Alpha', 0.05);
asserts.isequal(h, true);
asserts.isapprox(p, 0.020503981704794, 1e-12);
asserts.isequal(ks2stat, 1);
%=============================================================================
[h, p, ks2stat] = kstest2([Inf 1 2], [1 2 3]);
asserts.isequal(h, false);
asserts.isapprox(p, 0.976212648864478, 1e-12);
asserts.isapprox(ks2stat, 0.333333333333333, 1e-12);
%=============================================================================
asserts.checkerror('kstest2([1 2; 3 4], [1 2])', sprintf(_('%s must be a real numeric vector.'), 'X1'));
asserts.checkerror('kstest2([1 2], [1 2; 3 4])', sprintf(_('%s must be a real numeric vector.'), 'X2'));
asserts.checkerror('kstest2([NaN NaN], [1 2])', sprintf(_('%s must be a real numeric vector.'), 'X1'));
asserts.checkerror('kstest2([1 2], [NaN])', sprintf(_('%s must be a real numeric vector.'), 'X2'));
asserts.checkerror('kstest2([1 2], [1 2], ''Alpha'')', _('Name-value arguments must be paired.'));
asserts.checkerror('kstest2([1 2], [1 2], ''Unknown'', 1)', _('Invalid name-value argument name.'));
asserts.checkerror('kstest2([1 2], [1 2], ''Alpha'', 2)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
asserts.checkerror('kstest2([1 2], [1 2], ''Tail'', ''bad'')', _('TAIL must be ''unequal'', ''larger'', or ''smaller''.'));
%=============================================================================
