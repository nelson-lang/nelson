%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
clear global ODE_OUTPUT_RECORDER_LOG;
global ODE_OUTPUT_RECORDER_LOG;
ODE_OUTPUT_RECORDER_LOG = {};
options = odeset('OutputFcn', @odeOutputRecorder, 'MaxStep', 0.1);
[t, y] = ode45(@(t, y) y, [0 0.5], 1, options);
asserts.istrue(length(ODE_OUTPUT_RECORDER_LOG) >= 3);
asserts.isequal(ODE_OUTPUT_RECORDER_LOG{1}.flag, 'init');
asserts.isequal(ODE_OUTPUT_RECORDER_LOG{1}.t, [0 0.5]);
asserts.isequal(ODE_OUTPUT_RECORDER_LOG{1}.y, 1);
lastIndex = length(ODE_OUTPUT_RECORDER_LOG);
asserts.isequal(ODE_OUTPUT_RECORDER_LOG{lastIndex}.flag, 'done');
asserts.istrue(isempty(ODE_OUTPUT_RECORDER_LOG{lastIndex}.t));
asserts.istrue(isempty(ODE_OUTPUT_RECORDER_LOG{lastIndex}.y));
asserts.istrue(abs(t(length(t)) - 0.5) < 1e-12);
asserts.istrue(abs(y(length(y)) - exp(0.5)) < 1e-4);

clear global ODE_OUTPUT_STOP_LOG;
global ODE_OUTPUT_STOP_LOG;
ODE_OUTPUT_STOP_LOG = {};
options = odeset('OutputFcn', @odeOutputStopAtThree, 'MaxStep', 0.1);
[t, y] = ode45(@(t, y) y, [0 1], 1, options);
asserts.istrue(t(length(t)) < 1);
asserts.istrue(t(length(t)) >= 0.3);
stopTimes = [];
for k = 1:length(ODE_OUTPUT_STOP_LOG)
  if isempty(ODE_OUTPUT_STOP_LOG{k}.flag) && ODE_OUTPUT_STOP_LOG{k}.t >= 0.3
    stopTimes(end + 1) = ODE_OUTPUT_STOP_LOG{k}.t;
  end
end
asserts.istrue(~isempty(stopTimes));
asserts.istrue(abs(t(length(t)) - stopTimes(1)) < 1e-12);
%=============================================================================
clear global ODE_OUTPUT_RECORDER_LOG;
global ODE_OUTPUT_RECORDER_LOG;
ODE_OUTPUT_RECORDER_LOG = {};
options = odeset('OutputFcn', @odeOutputRecorder, 'Refine', 4, ...
  'InitialStep', 0.25, 'MaxStep', 0.25);
[t, y] = ode45(@(t, y) 1, [0 0.5], 0, options);
outputTimes = [];
for k = 1:length(ODE_OUTPUT_RECORDER_LOG)
  if isempty(ODE_OUTPUT_RECORDER_LOG{k}.flag)
    outputTimes(end + 1) = ODE_OUTPUT_RECORDER_LOG{k}.t;
  end
end
asserts.isequal(length(outputTimes), 8);
asserts.istrue(max(abs(outputTimes - (0.0625:0.0625:0.5))) < 1e-12);
%=============================================================================
clear global ODE_OUTPUT_RECORDER_LOG;
global ODE_OUTPUT_RECORDER_LOG;
ODE_OUTPUT_RECORDER_LOG = {};
options = odeset('OutputFcn', @odeOutputRecorder, 'InitialStep', 0.25, 'MaxStep', 0.25);
[t, y] = ode45(@(t, y) 1, [0 0.2 0.5], 0, options);
outputTimes = [];
for k = 1:length(ODE_OUTPUT_RECORDER_LOG)
  if isempty(ODE_OUTPUT_RECORDER_LOG{k}.flag)
    outputTimes(end + 1) = ODE_OUTPUT_RECORDER_LOG{k}.t;
  end
end
asserts.isequal(length(outputTimes), 2);
asserts.istrue(max(abs(outputTimes - [0.2 0.5])) < 1e-12);
%=============================================================================
clear global ODE_OUTPUT_STOP_LOG;
global ODE_OUTPUT_STOP_LOG;
ODE_OUTPUT_STOP_LOG = {};
options = odeset('OutputFcn', @odeOutputStopAtThree, 'InitialStep', 0.25, 'MaxStep', 0.25);
[t, y] = ode45(@(t, y) 1, [0 0.3 0.5], 0, options);
asserts.istrue(max(abs(t - [0; 0.3])) < 1e-12);
asserts.istrue(max(abs(y - [0; 0.3])) < 1e-12);
%=============================================================================
clear global ODE_OUTPUT_SEL_LOG;
global ODE_OUTPUT_SEL_LOG;
ODE_OUTPUT_SEL_LOG = {};
options = odeset('OutputFcn', @odeOutputSelRecorder, 'OutputSel', 2, ...
  'InitialStep', 0.25, 'MaxStep', 0.25);
ode45(@(t, y)[y(1); y(2)], [0 0.5], [1; 2], options);
asserts.isequal(ODE_OUTPUT_SEL_LOG{1}.flag, 'init');
asserts.isequal(ODE_OUTPUT_SEL_LOG{1}.y, 2);
%=============================================================================
clear global ODE_OUTPUT_PARAMETER_LOG;
global ODE_OUTPUT_PARAMETER_LOG;
ODE_OUTPUT_PARAMETER_LOG = {};
parameterOptions = odeset('OutputFcn', @odeOutputParameterizedRecorder, ...
  'InitialStep', 0.25, 'MaxStep', 0.25);
parameterOptions.Parameters = {7, 11};
ode45(@(t, y, scale, offset) y, [0 0.5], 1, parameterOptions);
asserts.istrue(length(ODE_OUTPUT_PARAMETER_LOG) >= 3);
asserts.isequal(ODE_OUTPUT_PARAMETER_LOG{1}.scale, 7);
asserts.isequal(ODE_OUTPUT_PARAMETER_LOG{1}.offset, 11);
asserts.isequal(ODE_OUTPUT_PARAMETER_LOG{length(ODE_OUTPUT_PARAMETER_LOG)}.scale, 7);
asserts.isequal(ODE_OUTPUT_PARAMETER_LOG{length(ODE_OUTPUT_PARAMETER_LOG)}.offset, 11);
%=============================================================================
badStopOptions = odeset('OutputFcn', @odeOutputBadStop, ...
  'InitialStep', 0.25, 'MaxStep', 0.25);
asserts.checkerror('ode45(@(t, y) y, [0 1], 1, badStopOptions)', ...
  sprintf(_('%s stop flag must be a scalar numeric or logical value.'), 'OutputFcn'));
%=============================================================================
