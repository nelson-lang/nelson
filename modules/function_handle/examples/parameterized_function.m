%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Capture model coefficients in a function handle.
decayRate = 0.75;
offset = 2;
decayModel = @(time) offset + exp(-decayRate * time);
sampleTimes = 0:0.5:3;
modelValues = arrayfun(decayModel, sampleTimes);

disp('Parameterized model values:');
disp(modelValues);
