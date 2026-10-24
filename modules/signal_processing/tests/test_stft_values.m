%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
[s, sf, st] = stft(sin((0:127)' * 0.1), 'Window', hamming(32), 'OverlapLength', 16, 'FFTLength', 64);
asserts.isequal(length(sf), size(s, 1));
asserts.isequal(length(st), size(s, 2));
%=============================================================================
x = sin((0:31)' * 0.2);
[s1, f1, t1] = stft(x, 10, 'Window', hamming(8), 'OverlapLength', 4, 'FFTLength', 16, 'FrequencyRange', 'onesided');
asserts.isequal(size(s1), [9 7]);
asserts.isapprox(f1.', [0 0.625 1.25 1.875 2.5 3.125 3.75 4.375 5], 1e-12);
asserts.isapprox(t1(1:4).', [0.4 0.8 1.2 1.6], 1e-12);
asserts.isapprox(real(s1(1:4, 1)).', [2.37939694950247 -0.0169645726242999 -1.3566477561934 -0.019447452025915], 1e-12);
asserts.isapprox(imag(s1(1:4, 1)).', [0 -2.07512452740221 0.00715319320564324 0.634707537781338], 1e-12);
%=============================================================================
