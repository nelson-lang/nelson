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
% audiowrite: multichannel interleave used one monotonic index into a
% column-major source, so signals longer than the 8192-frame chunk read
% wrong offsets and scrambled channels. audioread: stereo 24/32-bit PCM
% used sf_readf_int(BUFFER_FRAMES) whose count never matched the
% frames*channels validation, so every stereo 24/32-bit read failed
% (2026-08 audit fix).
%=============================================================================
dest_path = tempname();
mkdir(dest_path);
%=============================================================================
% More than one 8192-frame chunk so a monotonic interleave index is exposed.
n = 20000;
t = (0:n - 1)';
L = int16(round(10000 * sin(2 * pi * 5 * t / n)));
R = int16(round(-8000 * cos(2 * pi * 3 * t / n)));
Y = [L, R];
f = [dest_path, '/stereo_int16_long.wav'];
audiowrite(f, Y, 8000);
[y, fs] = audioread(f, 'native');
asserts.isequal(size(y), [n, 2]);
asserts.isequal(y(:, 1), L);
asserts.isequal(y(:, 2), R);
%=============================================================================
% Stereo 24-bit PCM 'native' read path: before the fix, sf_readf_int with a
% fixed frame count never matched the frames*channels validation, so every
% stereo 24-bit read errored ("Cannot read audio file"). Just calling
% audioread(..., 'native') and getting an [n, 2] result exercises the fix.
% 24-bit 'native' returns int32; the double read path (default) is unaffected
% and gives an exact channel check below.
Ld = 0.7 * sin(2 * pi * 5 * t / n);
Rd = -0.5 * cos(2 * pi * 3 * t / n);
Yd = [Ld, Rd];
f24 = [dest_path, '/stereo_24bit_long.wav'];
audiowrite(f24, Yd, 8000, 'BitsPerSample', 24);
[y24n, fs24n] = audioread(f24, 'native');
asserts.isequal(size(y24n), [n, 2]);
asserts.isequal(class(y24n), 'int32');
% channels must stay separated: their 24-bit sign must follow the source sign
asserts.istrue(sum(sign(double(y24n(:, 1))) == sign(Ld)) > 0.99 * n);
asserts.istrue(sum(sign(double(y24n(:, 2))) == sign(Rd)) > 0.99 * n);
% the double read path (unchanged) still round-trips the channels accurately
[y24, fs24] = audioread(f24);
asserts.isequal(size(y24), [n, 2]);
asserts.istrue(max(abs(y24(:, 1) - Ld)) < 1e-3);
asserts.istrue(max(abs(y24(:, 2) - Rd)) < 1e-3);
%=============================================================================
rmdir(dest_path, 's');
%=============================================================================
