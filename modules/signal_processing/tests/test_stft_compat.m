%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.checkerror('stft(1:16, ''Window'', hamming(8), ''OverlapLength'', 8)', _('Overlap length must be less than window length.'));
asserts.checkerror('stft(1:16, ''Window'', hamming(8), ''OverlapLength'', 4, ''FFTLength'', 4)', _('FFT length must be greater than or equal to window length.'));
asserts.checkerror('stft(1:16, ''BadOption'', 1)', _('Unsupported option.'));
asserts.checkerror('stft(1:16, ''Window'')', _('Name-value options must be paired.'));
asserts.checkerror('stft(1:16, 10, 1, 2)', _('Option name must be a character vector or string scalar.'));
asserts.checkerror('stft(1:16, ''FrequencyRange'', ''bad'')', _('Unsupported frequency range.'));
asserts.checkerror('stft(1:16, ''FrequencyRange'', 1)', _('Frequency range must be a character vector or string scalar.'));
%=============================================================================
[s, f, t] = stft(1:16, 'Window', hamming(8), 'OverlapLength', 4, 'FFTLength', 8, 'FrequencyRange', 'twosided');
asserts.isequal(size(s, 1), 8);
asserts.isequal(length(f), 8);
asserts.isequal(length(t), size(s, 2));
%=============================================================================
