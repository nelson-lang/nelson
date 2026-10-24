%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('grayconnected({1}, 1, 1)', sprintf(_('%s must be numeric or logical.'), 'Image'));
%=============================================================================
asserts.checkerror('grayconnected([1 + i], 1, 1)', sprintf(_('%s must be real.'), 'Image'));
%=============================================================================
asserts.checkerror('grayconnected([1 Inf], 1, 1)', sprintf(_('%s values must be finite.'), 'Image'));
%=============================================================================
asserts.checkerror('grayconnected(zeros(2, 2, 2), 1, 1)', sprintf(_('%s must be a 2-D array.'), 'Image'));
%=============================================================================
asserts.checkerror('grayconnected([1 2], Inf, 1)', sprintf(_('%s must be a finite numeric scalar.'), 'Row subscript'));
%=============================================================================
asserts.checkerror('grayconnected([1 2], 1, 3)', _('Column subscript is outside image bounds.'));
%=============================================================================
asserts.checkerror('grayconnected([1 2], 1, 1, -1)', sprintf(_('%s must be a nonnegative finite scalar.'), 'Tolerance'));
%=============================================================================
asserts.checkerror('grayconnected([1 2], 1, 1, 0, eye(3))', _('Unsupported connectivity.'));
