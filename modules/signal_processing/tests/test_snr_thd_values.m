%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[r, noisePower] = snr([1 1 1], [0.1 0.1 0.1]);
asserts.isapprox(r, 20, 1e-12);
asserts.isapprox(noisePower, 0.03, 1e-12);
%=============================================================================
fs = 1000;
t = (0:999)' / fs;
x = sin(2 * pi * 50 * t) + 0.01 * sin(2 * pi * 123 * t);
[r, noisePower] = snr(x, fs);
asserts.isapprox(r, 40.0000000000004, 1e-11);
asserts.isapprox(noisePower, -43.0102999566402, 1e-11);
%=============================================================================
x = sin(2 * pi * 50 * t) + 0.1 * sin(2 * pi * 100 * t);
asserts.isapprox(thd(x, fs), -20, 1e-12);
%=============================================================================
asserts.checkerror('snr([1 2], [1 2 3])', _('Signal and noise must have the same dimensions.'));
asserts.checkerror('snr(1:8, 0)', _('Sample rate must be a positive scalar.'));
asserts.checkerror('snr(1:8, 1, 0)', _('Number of harmonics must be a positive scalar.'));
%=============================================================================
asserts.istrue(isnan(thd(ones(8, 1))));
%=============================================================================
