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
P = bwperim(BW);
asserts.isequal(sum(P(:)), 48);
asserts.istrue(P(10, 10));
asserts.isfalse(P(16, 16));
%=============================================================================
asserts.checkerror('bwperim({1})', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('bwperim(sparse([1 0]))', sprintf(_('%s are not supported.'), 'Sparse images'));
%=============================================================================
asserts.checkerror('bwperim([1 + i 0])', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('bwperim(zeros(2, 2, 2))', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('bwperim([1 NaN])', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('bwperim([1 2])', sprintf(_('%s must contain binary values.'), 'Image'));
%=============================================================================
asserts.checkerror('bwperim(logical([1 0]), 6)', _('Unsupported connectivity.'));
