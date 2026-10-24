%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
rng('default');
x = randn(1, 648000);
tic(); [pks, locs] = findpeaks(x); toc();
tic(); [pks, locs, w, p] = findpeaks(x, 'MinPeakProminence', 1, 'MinPeakWidth', 1.2); toc();
tic(); [pks, locs] = findpeaks(x, 'MinPeakDistance', 10); toc();
tic(); [pks, locs, w] = findpeaks(1 + 0.1 * x, 'WidthReference', 'halfheight'); toc();
%=============================================================================
