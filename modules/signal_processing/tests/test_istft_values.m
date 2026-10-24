%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
x = sin((0:31)' * 0.2);
[s1, ~] = stft(x, 10, 'Window', hamming(8), 'OverlapLength', 4, 'FFTLength', 16, 'FrequencyRange', 'onesided');
x1 = istft(s1, 10, 'Window', hamming(8), 'OverlapLength', 4, 'FFTLength', 16, 'FrequencyRange', 'onesided');
asserts.isapprox(x1(1:12).', [0 0.198669330795061 0.389418342308651 0.564642473395036 0.717356090899523 0.841470984807896 0.932039085967226 0.98544972998846 0.999573603041505 0.973847630878195 0.909297426825682 0.80849640381959], 1e-12);
%=============================================================================
asserts.checkerror('istft(s1, ''BadOption'', 1)', _('Unsupported option.'));
asserts.checkerror('istft(s1, ''Window'')', _('Name-value options must be paired.'));
asserts.checkerror('istft(s1, 10, 1, 2)', _('Option name must be a character vector or string scalar.'));
asserts.checkerror('istft(s1, ''Window'', hamming(8), ''OverlapLength'', 4, ''FFTLength'', 16, ''FrequencyRange'', ''bad'')', _('Unsupported frequency range.'));
asserts.checkerror('istft(s1, ''Window'', hamming(8), ''OverlapLength'', 4, ''FFTLength'', 16, ''FrequencyRange'', 1)', _('Frequency range must be a character vector or string scalar.'));
%=============================================================================
[s, ~] = stft(sin((0:127)' * 0.1), 'Window', hamming(32), 'OverlapLength', 16, 'FFTLength', 64);
xr = istft(s, 'Window', hamming(32), 'OverlapLength', 16, 'FFTLength', 64);
asserts.isapprox(xr(1:8).', [0 0.099833416646828 0.198669330795061 0.29552020666134 0.389418342308651 0.479425538604203 0.564642473395035 0.644217687237691], 1e-12);
%=============================================================================
