%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--RELEASE ONLY-->
wav_file = [modulepath('audio', 'tests'), '/MacBoing.wav'];
[y_wav, fs_wav] = audioread(wav_file);
info_wav = audioinfo(wav_file);
asserts.isequal(size(y_wav), [7673 1]);
asserts.isequal(class(y_wav), 'double');
asserts.isequal(fs_wav, 11025);
asserts.isequal(info_wav.NumChannels, 1);
asserts.isequal(info_wav.SampleRate, 11025);
asserts.isequal(info_wav.TotalSamples, 7673);
%=============================================================================
[y_wav_native, fs_wav_native] = audioread(wav_file, 'native');
asserts.isequal(size(y_wav_native), [7673 1]);
asserts.isequal(class(y_wav_native), 'uint8');
asserts.isequal(fs_wav_native, fs_wav);
%=============================================================================
