%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargout('pagemtimes'), 1);
%=============================================================================
A = reshape(1:24, 2, 3, 4);
B = reshape(1:24, 3, 2, 4);
C = pagemtimes(A, B);
asserts.isequal(size(C), [2 2 4]);
for k = 1:4
  asserts.isequal(C(:, :, k), A(:, :, k) * B(:, :, k));
end
%=============================================================================
% transpose options
D = pagemtimes(A, 'transpose', A, 'none');
asserts.isequal(size(D), [3 3 4]);
asserts.isequal(D(:, :, 1), A(:, :, 1)' * A(:, :, 1));
%=============================================================================
% ctranspose on complex data
Z = reshape((1:8) + 1i * (8:-1:1), 2, 2, 2);
E = pagemtimes(Z, 'ctranspose', Z, 'none');
asserts.isequal(E(:, :, 1), Z(:, :, 1)' * Z(:, :, 1));
%=============================================================================
% page broadcasting (B has a single page)
Bs = [1 0; 0 1; 1 1];
F = pagemtimes(A, Bs);
asserts.isequal(size(F), [2 2 4]);
asserts.isequal(F(:, :, 3), A(:, :, 3) * Bs);
%=============================================================================
% 2-D inputs behave like mtimes
asserts.isequal(pagemtimes([1 2; 3 4], [5; 6]), [1 2; 3 4] * [5; 6]);
%=============================================================================
% single input keeps single class
asserts.isequal(class(pagemtimes(single(A), single(B))), 'single');
%=============================================================================
asserts.checkerror('pagemtimes(reshape(1:24,2,3,4), reshape(1:24,2,3,4))', _('Incorrect dimensions for matrix multiplication of the pages.'));
%=============================================================================
