%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
filename = [tempname(), '.wav'];
fs_ref = 8000;
t = (0:99)' / fs_ref;
y = sin(2 * pi * 440 * t);
%=============================================================================
audiowrite(filename, y, fs_ref);
[z, fs] = audioread(filename);
info = audioinfo(filename);
%=============================================================================
asserts.isequal(fs, fs_ref);
asserts.isequal(info.SampleRate, fs_ref);
asserts.isequal(size(z), size(y));
asserts.isapprox(z, y, 1e-4);
%=============================================================================
rmfile(filename);
%=============================================================================
