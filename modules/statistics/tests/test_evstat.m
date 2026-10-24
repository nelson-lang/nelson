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
asserts.isequal(nargin('evstat'), 2);
asserts.isequal(nargout('evstat'), 2);
%=============================================================================
[M, V] = evstat([0 2], [1 3]);
asserts.isapprox(M, [-0.5772156649015329 0.2683530052954013], 1e-15);
asserts.isapprox(V, [1.644934066848226 14.80440660163404], 1e-14);
%=============================================================================
[M, V] = evstat(single([0 2]), single([1 3]));
asserts.isequal(class(M), 'single');
asserts.isequal(class(V), 'single');
asserts.isapprox(double(M), [-0.5772156649015329 0.2683530052954013], 1e-6);
asserts.isapprox(double(V), [1.644934066848226 14.80440660163404], 1e-6);
%=============================================================================
[M, V] = evstat([0 NaN 0 0], [1 1 NaN -1]);
asserts.isapprox(M(1), -0.5772156649015329, 1e-15);
asserts.isapprox(V(1), 1.644934066848226, 1e-15);
asserts.istrue(all(isnan(M(2:4))));
asserts.istrue(all(isnan(V(2:4))));
%=============================================================================
[M, V] = evstat([], []);
asserts.isequal(size(M), [0 0]);
asserts.isequal(size(V), [0 0]);
%=============================================================================
asserts.checkerror('evstat(ones(2), ones(3))', _('Size mismatch on input arguments'));
asserts.checkerror('evstat({0}, 1)', sprintf(_('%s must be real numeric.'), 'MU'));
asserts.checkerror('evstat(0, {1})', sprintf(_('%s must be real numeric.'), 'SIGMA'));
%=============================================================================
