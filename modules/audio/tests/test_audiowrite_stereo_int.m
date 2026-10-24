%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <-- Short Description -->
% audiowrite: interleaving for int16/uint8 stereo data used k*(channels+ch)
% instead of k*channels+ch, garbling the channels (2026-07 audit fix).
%=============================================================================
dest_path = tempname();
mkdir(dest_path);
%=============================================================================
% two distinct channels so a wrong interleave shows up
n = 64;
t = (0:n - 1)';
L = int16(round(10000 * sin(2 * pi * 5 * t / n)));
R = int16(round(-8000 * cos(2 * pi * 3 * t / n)));
Y = [L, R];
f = [dest_path, '/stereo_int16.wav'];
audiowrite(f, Y, 8000);
[y, fs] = audioread(f, 'native');
asserts.isequal(size(y), [n, 2]);
asserts.isequal(y(:, 1), L);
asserts.isequal(y(:, 2), R);
%=============================================================================
% uint8 stereo path (same interleave code branch). 8-bit PCM is lossy on
% round-trip, so we only check that the channels stay SEPARATED (a wrong
% interleave would garble them wholesale, giving a huge cross-channel error),
% not that values match exactly.
Lu = uint8(mod(t * 3, 256));
Ru = uint8(mod(t * 7 + 5, 256));
fu = [dest_path, '/stereo_uint8.wav'];
audiowrite(fu, [Lu, Ru], 8000, 'BitsPerSample', 8);
[yu, fsu] = audioread(fu, 'native');
asserts.isequal(size(yu), [n, 2]);
% channel 1 must resemble Lu far more than Ru (and vice versa)
asserts.istrue(max(abs(double(yu(:, 1)) - double(Lu))) < 8);
asserts.istrue(max(abs(double(yu(:, 2)) - double(Ru))) < 8);
%=============================================================================
rmdir(dest_path, 's');
%=============================================================================
