%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Import an FMI 3.0 Co-Simulation FMU and run it from Nelson.
%
% Uses the Van der Pol oscillator from the bundled Reference FMUs. The same
% workflow applies to any FMI 3.0 Co-Simulation FMU: inspect it with fmiInfo,
% then run it with fmiCoSimulate.
%=============================================================================
fmu = [fileparts(mfilename('fullpathext')), '/VanDerPol.fmu'];
%=============================================================================
% Inspect the model description.
info = fmiInfo(fmu);
disp(['model identifier : ', info.modelIdentifier]);
disp(['FMI version      : ', info.fmiVersion]);
disp(['Co-Simulation    : ', mat2str(info.coSimulation)]);
outputs = info.names(strcmp(info.causalities, 'output'));
disp(['outputs          : ', strjoin(outputs, ', ')]);
%=============================================================================
% Run a Co-Simulation over 20 seconds with a 0.01 s communication step.
r = fmiCoSimulate(fmu, 20, 0.01);
%=============================================================================
% Plot the recorded outputs.
f = figure();
plot(r.time, r.outputs);
legend(r.outputNames);
xlabel('time (s)');
ylabel('output');
title('Van der Pol oscillator (FMI 3.0 Co-Simulation)');
grid('on');
%=============================================================================
