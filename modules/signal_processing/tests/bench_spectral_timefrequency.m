%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = sin((0:8191)' * 0.02) + 0.2 * sin((0:8191)' * 0.2);
w = hamming(256);
tic();
for k = 1:10
  [pxx, f] = pwelch(x, w, 128, 512, 1000);
  [s, sf, st] = spectrogram(x, w, 128, 512, 1000);
  [z, zf, zt] = stft(x, 1000, 'Window', w, 'OverlapLength', 128, 'FFTLength', 512);
end
toc();
asserts.isequal(length(pxx), length(f));
asserts.isequal(length(sf), size(s, 1));
asserts.isequal(length(zt), size(z, 2));
asserts.isequal(length(zf), size(z, 1));
%=============================================================================
