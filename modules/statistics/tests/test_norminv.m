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
asserts.isequal(nargin('norminv'), 5);
asserts.isequal(nargout('norminv'), 3);
%=============================================================================
p = [0.001, 0.025, 0.5, 0.975, 0.999];
R = norminv(p);
REF = [-3.09023230616781, -1.95996398454005, 0, ...
        1.95996398454005, 3.09023230616781];
asserts.isapprox(R, REF, 1e-11);
%=============================================================================
R = norminv([0.5 0.841344746068543], 2, 3);
asserts.isapprox(R, [2 5], 1e-11);
%=============================================================================
R = norminv(single([0.025 0.5 0.975]));
asserts.isequal(class(R), 'single');
asserts.isapprox(R, single([-1.95996398454005 0 1.95996398454005]), 1e-5);
%=============================================================================
R = norminv([], [], []);
asserts.isequal(size(R), [0 0]);
%=============================================================================
R = norminv(0.5, [], []);
asserts.isequal(R, 0);
%=============================================================================
R = norminv([0 1 -0.1 1.1 NaN], 0, 1);
asserts.istrue(isinf(R(1)) && R(1) < 0);
asserts.istrue(isinf(R(2)) && R(2) > 0);
asserts.istrue(all(isnan(R(3:5))));
%=============================================================================
R = norminv(0.5, 0, 0);
asserts.istrue(isnan(R));
%=============================================================================
[x, xlo, xup] = norminv(0.5, 0, 1, [0.04 0; 0 0.01], 0.05);
asserts.isapprox(x, 0, 1e-12);
asserts.isapprox(xlo, -0.391992796908011, 1e-12);
asserts.isapprox(xup, 0.391992796908011, 1e-12);
%=============================================================================
prob = single([0.2 0.5; 0.8 0.975]);
mu = single([0 1; 2 3]);
sigma = single([1 2; 0.5 1.5]);
pCov = single([0.04 0.01; 0.01 0.09]);
[x, xlo, xup] = norminv(prob, mu, sigma, pCov, single(0.1));
asserts.isequal(class(x), 'single');
asserts.isequal(class(xlo), 'single');
asserts.isequal(class(xup), 'single');
asserts.isequal(size(x), [2 2]);
asserts.isapprox(x, single([-0.8416212 1; 2.4208107 5.9399467]), 1e-5);
asserts.isapprox(xlo, single([-1.3265517 0.6710293; 1.8496370 4.8677206]), 1e-5);
asserts.isapprox(xup, single([-0.3566907 1.3289707; 2.9919844 7.0121727]), 1e-5);
%=============================================================================
[x, xlo, xup] = norminv(prob, mu, single(1), pCov, single(0.1));
asserts.isequal(class(x), 'single');
asserts.isequal(size(x), [2 2]);
asserts.isapprox(x, single([-0.8416212 1; 2.8416212 4.9599638]), 1e-5);
asserts.isapprox(xlo, single([-1.3265517 0.6710293; 2.2704475 3.8877382]), 1e-5);
asserts.isapprox(xup, single([-0.3566907 1.3289707; 3.4127948 6.0321903]), 1e-5);
%=============================================================================
asserts.checkerror('norminv(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('norminv({0.5})', sprintf(_('%s must be real numeric.'), 'P'));
asserts.checkerror('norminv(0.5, 0, 1, [1 0 0])', _('PCOV must be a 2-by-2 numeric matrix.'));
asserts.checkerror('norminv(0.5, 0, 1, [1 0; 0 1], 0)', sprintf(_('%s must be a scalar in the range (0,1).'), 'ALPHA'));
%=============================================================================
asserts.isapprox(norminv(0.5), 0, 1e-12);
%=============================================================================
