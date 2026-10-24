%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Fill missing samples and compute a moving average.
measurements = [18; NaN; 21; 25; NaN; 24];
[cleaned, replaced] = fillmissing(measurements, 'linear');
smoothed = movmean(cleaned, 3);
disp(table(measurements, cleaned, replaced, smoothed));
