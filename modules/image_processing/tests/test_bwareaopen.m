%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
BW = false(32, 32);
BW(10:22, 10:22) = true;
BW(3, 3) = true;
J = bwareaopen(BW, 10);
asserts.isequal(sum(J(:)), 169);
asserts.isfalse(J(3, 3));
BW(1:4, 20:24) = true;
K = imclearborder(BW);
asserts.isfalse(K(1, 20));
asserts.istrue(K(16, 16));
%=============================================================================
asserts.checkerror('bwareaopen(single([NaN 0]), 1)', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('bwareaopen(logical([1 0]), -1)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Minimum object size'));
%=============================================================================
asserts.checkerror('bwareaopen(logical([1 0]), Inf)', sprintf(_('%s must be a nonnegative integer scalar.'), 'Minimum object size'));
%=============================================================================
asserts.checkerror('bwareaopen([1 2], 1)', sprintf(_('%s must contain binary values.'), 'Image'));
