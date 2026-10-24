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
asserts.isequal(nargin('fcdf'), 4);
asserts.isequal(nargout('fcdf'), 1);
%=============================================================================
x = [0 0.5 1 2 Inf];
v1 = [1 2 5 10 4];
v2 = [5 10 20 30 6];
R = fcdf(x, v1, v2);
REF = [0 0.379078676940845 0.556974815315123 ...
       0.930386291923616 1];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = fcdf(x, v1, v2, 'upper');
REF = [1 0.620921323059155 0.443025184684877 ...
       0.0696137080763843 0];
asserts.isapprox(R, REF, 1e-12);
%=============================================================================
R = fcdf([0.5 1 2], [Inf 5 Inf], [5 Inf Inf]);
asserts.isapprox(R, [0.0752352461465122 0.584119813004492 1], 1e-12);
%=============================================================================
R = fcdf(single(x), single(v1), single(v2));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([0 0.379078676940845 0.556974815315123 ...
                           0.930386291923616 1]), 1e-6);
%=============================================================================
R = fcdf([-1 0 Inf NaN 1], [2 2 2 2 -1], [5 5 5 5 5]);
asserts.isequal(R(1), 0);
asserts.isequal(R(2), 0);
asserts.isequal(R(3), 1);
asserts.istrue(isnan(R(4)));
asserts.istrue(isnan(R(5)));
%=============================================================================
R = fcdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('fcdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('fcdf(1, 2, 3, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'F'));
asserts.checkerror('fcdf({1}, 2, 3)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('fcdf(1, {2}, 3)', sprintf(_('%s must be real numeric.'), 'V1'));
asserts.checkerror('fcdf(1, 2, {3})', sprintf(_('%s must be real numeric.'), 'V2'));
%=============================================================================
