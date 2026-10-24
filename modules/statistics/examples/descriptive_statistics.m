%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Summarize a small measurement series with robust statistics.
measurements = [10.2, 9.8, 10.5, 10.1, 15.4, 9.9, 10.0];
disp(['Mean: ', mat2str(mean(measurements))]);
disp(['Median: ', mat2str(median(measurements))]);
disp(['Standard deviation: ', mat2str(std(measurements))]);
disp(['Interquartile range: ', mat2str(iqr(measurements))]);
