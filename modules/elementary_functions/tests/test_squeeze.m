%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
A = zeros(2, 1, 2);
A(:, :, 1) = [1 2]';
A(:, :, 2) = [3 4]';
R = squeeze(A);
REF = [1 3; 2 4];
asserts.isequal(R, REF)
%=============================================================================
A = zeros(1, 1, 3);
A(:, :, 1:3) = [1 2 3];
R = squeeze(A);
REF = [1; 2; 3];
asserts.isequal(R, REF)
%=============================================================================
% reshape drops the trailing singleton dimension, so the array is already
% 1x3 and squeeze leaves it unchanged (matching expected behavior).
asserts.isequal(squeeze(reshape(1:3, [1 3 1])), [1 2 3]);
asserts.isequal(ndims(reshape(1:6, 1, 6, 1)), 2);
asserts.isequal(size(reshape(1:6, 1, 6, 1)), [1 6]);
asserts.isequal(size(reshape(1:6, 2, 3, 1)), [2 3]);
% a genuine interior singleton is kept
asserts.isequal(size(squeeze(reshape(1:6, 1, 1, 6))), [6 1]);
%=============================================================================
