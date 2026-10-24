%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('pageinv'), 1);
asserts.isequal(nargout('pageinv'), 1);
%=============================================================================
M = cat(3, [2 0; 0 4], [1 2; 3 4], [4 7; 2 6]);
Y = pageinv(M);
asserts.isequal(size(Y), [2 2 3]);
for k = 1:3
  asserts.isapprox(Y(:, :, k), inv(M(:, :, k)), 1e-12);
end
%=============================================================================
% pageinv(M) times M is identity on each page
I = pagemtimes(pageinv(M), M);
for k = 1:3
  asserts.isapprox(I(:, :, k), eye(2), 1e-10);
end
%=============================================================================
% 2-D input matches inv
asserts.isapprox(pageinv([1 2; 3 4]), inv([1 2; 3 4]), 1e-12);
%=============================================================================
% complex pages
Zc = cat(3, [1 + 1i 0; 0 2], [2 1i; -1i 2]);
Yc = pageinv(Zc);
asserts.isapprox(Yc(:, :, 2), inv(Zc(:, :, 2)), 1e-12);
%=============================================================================
% single input keeps single class
asserts.isequal(class(pageinv(single([1 2; 3 4]))), 'single');
%=============================================================================
% singular page yields non-finite entries (no error)
S = pageinv([1 1; 1 1]);
asserts.istrue(all(~isfinite(S(:))));
%=============================================================================
asserts.checkerror('pageinv(reshape(1:12, 2, 3, 2))', _('Each page of the input array must be a square matrix.'));
%=============================================================================
