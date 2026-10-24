%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Generate reproducible pseudo-random samples.
rng(42);
uniformSamples = rand(1, 5);
normalSamples = randn(1, 5);
disp('Uniform samples:');
disp(uniformSamples);
disp('Normal samples:');
disp(normalSamples);
