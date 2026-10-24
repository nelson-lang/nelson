%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 8-->
% <--REUSE PROCESS-->
%=============================================================================
X = randn(2000, 8);
tic();
[B, T] = rotatefactors(X, 'Method', 'varimax');
t = toc();
asserts.isequal(size(B), size(X));
asserts.isequal(size(T), [8 8]);
asserts.istrue(all(isfinite(B(:))));
asserts.istrue(all(isfinite(T(:))));
asserts.istrue(t < 10);
%=============================================================================
