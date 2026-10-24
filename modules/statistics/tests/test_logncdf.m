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
asserts.isequal(nargin('logncdf'), 6);
asserts.isequal(nargout('logncdf'), 3);
%=============================================================================
x = [0 1 exp(1)];
R = logncdf(x);
E = [0 0.5 0.8413447460685429];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = logncdf(exp([0 1 2]), 1, 2);
E = [0.3085375387259869 0.5 0.6914624612740131];
asserts.isapprox(R, E, 1e-15);
%=============================================================================
R = logncdf([0 1 exp(1)], 'upper');
E = [1 0.5 0.15865525393145707];
asserts.isapprox(R, E, 1e-15);
R = logncdf(exp(10), 'upper');
asserts.istrue(abs(R - 7.619853024160593e-24) < 1e-35);
%=============================================================================
[p, pLo, pUp] = logncdf(exp(1), 0, 1, [0.01 0; 0 0.04]);
asserts.isapprox(p, 0.8413447460685429, 1e-15);
asserts.isapprox(pLo, 0.7128529786997453, 1e-14);
asserts.isapprox(pUp, 0.9248200317736012, 1e-14);
%=============================================================================
R = logncdf(single([1 exp(1)]), single(0), single(1));
asserts.isequal(class(R), 'single');
asserts.isapprox(double(R), [0.5 0.8413447460685429], 1e-7);
%=============================================================================
R = logncdf([1 NaN 1 1], [0 0 NaN 0], [1 1 1 0]);
asserts.isapprox(R(1), 0.5, 1e-15);
asserts.istrue(all(isnan(R(2:4))));
%=============================================================================
R = logncdf([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
asserts.checkerror('logncdf(ones(2), ones(3), 1)', _('Size mismatch on input arguments'));
asserts.checkerror('logncdf(1, 0, 1, ''bad'')', sprintf(_('Invalid %s distribution option.'), 'lognormal'));
asserts.checkerror('logncdf({1}, 0, 1)', sprintf(_('%s must be real numeric.'), 'X'));
asserts.checkerror('logncdf(1, {0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('logncdf(1, 0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
