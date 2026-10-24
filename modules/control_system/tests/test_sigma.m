%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
sys = tf(2, [1 1]);
w = [1 2 4];
[sv, wout] = sigma(sys, w);
ref = abs(squeeze(freqresp(sys, w))).';
asserts.isapprox(sv, ref, 1e-12);
asserts.isequal(wout, w);
%=============================================================================
