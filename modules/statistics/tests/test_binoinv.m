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
asserts.isequal(nargin('binoinv'), 3);
asserts.isequal(nargout('binoinv'), 1);
%=============================================================================
y = [0 0.025 0.5 0.975 1];
n = [10 10 10 10 10];
p = [0.2 0.2 0.5 0.5 0.8];
R = binoinv(y, n, p);
asserts.isequal(R, [0 0 5 8 10]);
%=============================================================================
R = binoinv([0.1 0.9 0.5], 10, [0 0 1]);
asserts.isequal(R, [0 0 10]);
%=============================================================================
R = binoinv(single(y), single(n), single(p));
asserts.isequal(class(R), 'single');
asserts.isequal(double(R), [0 0 5 8 10]);
%=============================================================================
R = binoinv([-0.1 1.1 NaN 0 1], 10, 0.5);
asserts.istrue(all(isnan(R(1:3))));
asserts.isequal(R(4), 0);
asserts.isequal(R(5), 10);
%=============================================================================
R = binoinv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('binoinv(ones(2), ones(3), 0.5)', _('Size mismatch on input arguments'));
asserts.checkerror('binoinv({0.5}, 10, 0.5)', sprintf(_('%s must be real numeric.'), 'Y'));
asserts.checkerror('binoinv(0.5, {10}, 0.5)', _('N must be a nonnegative integer numeric array.'));
asserts.checkerror('binoinv(0.5, 10, {0.5})', _('P must be a numeric array in the range [0,1].'));
%=============================================================================
asserts.isequal(binoinv(0.6875, 4, 0.5), 2);
%=============================================================================
