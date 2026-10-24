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
t = datetime(2024, 2, 1);
lo = datetime(2024, 1, 1);
hi = datetime(2024, 3, 1);
asserts.isequal(isbetween(t, lo, hi), true);
asserts.isequal(isbetween(lo, lo, hi, 'open'), false);
asserts.isequal(isbetween(lo, lo, hi, 'openright'), true);
asserts.isequal(isbetween([lo t hi], lo, hi, 'open'), [false true false]);
%=============================================================================
