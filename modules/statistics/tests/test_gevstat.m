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
asserts.isequal(nargin('gevstat'), 3);
asserts.isequal(nargout('gevstat'), 2);
%=============================================================================
[M, V] = gevstat([0 0.2 0.75 1.2], [1 1 1 1], [0 0 0 0]);
asserts.isapprox(M(1:3), [0.5772156649015329 0.8211485686265163 3.500813210962544], 1e-15);
asserts.istrue(isnan(M(4)));
asserts.isapprox(V(1:2), [1.644934066848226 3.344035562297886], 1e-14);
asserts.istrue(isnan(V(3)));
asserts.istrue(isnan(V(4)));
%=============================================================================
[M, V] = gevstat(single([0 0.2]), single([1 1]), single([0 0]));
asserts.isequal(class(M), 'single');
asserts.isequal(class(V), 'single');
asserts.isapprox(double(M), [0.5772156649015329 0.8211485686265163], 1e-6);
asserts.isapprox(double(V), [1.644934066848226 3.344035562297886], 1e-6);
%=============================================================================
[M, V] = gevstat([0 NaN 0 0], [1 1 NaN -1], [0 0 0 NaN]);
asserts.isapprox(M(1), 0.5772156649015329, 1e-15);
asserts.isapprox(V(1), 1.644934066848226, 1e-15);
asserts.istrue(all(isnan(M(2:4))));
asserts.istrue(all(isnan(V(2:4))));
[M, V] = gevstat([], [], []);
asserts.isequal(size(M), [0 0]);
asserts.isequal(size(V), [0 0]);
%=============================================================================
asserts.checkerror('gevstat(0, 1)', _('Not enough input arguments.'));
asserts.checkerror('gevstat(ones(2), ones(3), 0)', _('Size mismatch on input arguments'));
asserts.checkerror('gevstat({0}, 1, 0)', sprintf(_('%s must be real numeric.'), 'K'));
asserts.checkerror('gevstat(0, {1}, 0)', sprintf(_('%s must be real numeric.'), 'SIGMA'));
asserts.checkerror('gevstat(0, 1, {0})', sprintf(_('%s must be real numeric.'), 'MU'));
%=============================================================================
