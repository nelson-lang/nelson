%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
p = bandpower(sin((0:127)' * 0.1), 10, [0 5]);
asserts.isapprox(p, 0.499829331603769, 1e-12);
[fmean, pmean] = meanfreq(sin((0:127)' * 0.1), 10);
asserts.isapprox(fmean, 0.15774828798515, 1e-12);
asserts.isapprox(pmean, 0.491022314775036, 1e-12);
[fmed, pmed] = medfreq(sin((0:127)' * 0.1), 10);
asserts.isapprox(fmed, 0.156392790389587, 1e-12);
asserts.isapprox(pmed, 0.491022314775036, 1e-12);
[pxx, f] = periodogram(sin((0:127)' * 0.1), [], 128, 10);
asserts.isapprox(bandpower(pxx, f, 'psd'), 0.491022314775036, 1e-12);
asserts.isapprox(meanfreq(pxx, f), 0.15774828798515, 1e-12);
asserts.isapprox(medfreq(pxx, f), 0.156392790389587, 1e-12);
%=============================================================================
