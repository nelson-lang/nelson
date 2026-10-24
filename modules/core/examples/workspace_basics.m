%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Create, transform and summarize values in the current workspace.
temperatures = [18.4, 19.1, 21.3, 22.0, 20.7];
celsiusMean = mean(temperatures);
fahrenheitMean = 1.8 * celsiusMean + 32;
disp(['Mean temperature: ', mat2str(celsiusMean), ' C / ', mat2str(fahrenheitMean), ' F']);
