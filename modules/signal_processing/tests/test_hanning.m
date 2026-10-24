%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(hanning(0), zeros(0, 1));
asserts.isequal(hanning(1), 1);
asserts.isapprox(hanning(6, 'periodic').', [0 0.25 0.75 1 0.75 0.25], 1e-12);
asserts.isapprox(hanning(6), hann(6), 1e-12);
%=============================================================================
