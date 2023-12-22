%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
A = [-1.5,-2;1,0];
B = [0.5;0];
C = [0,1];
D = 0;
sys = ss(A, B, C, D);
w = [1 10 100];
[H, wout] = freqresp(sys, w)

assert_isequal(size(H), [1 1 3]);
assert_isapprox(H(1), 0.1538 - 0.2308i, 1e-3)
assert_isapprox(H(2), -0.0050 - 0.0008i, 1e-1)
assert_istrue(H(3) - eps < eps);
assert_isequal(wout, w);
%=============================================================================
G = tf(1,[1 1]);
h1 = freqresp(G, 3);
assert_isapprox(h1,  0.1000 - 0.3000i, 1e-2);
%=============================================================================
num = [1 2];
den = [1 3 2];
sys = tf(num,den);
w = linspace(0, 100, 60);
[resp,freq] = freqresp(sys, w);

f = figure();
subplot(2,1,1);
plot(freq,20*log10(abs(squeeze(resp))));
ylabel('Amplitude (dB)');
subplot(2,1,2);
plot(freq,angle(squeeze(resp))*180/pi);
ylabel('Phase (degrees)');
xlabel('Frequency (Hz)');
%=============================================================================
