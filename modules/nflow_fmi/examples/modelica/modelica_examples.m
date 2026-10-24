%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% nflow Modelica bridge -- worked examples.
%
% Each example places a single `modelica` block that points at a .mo file and
% simulates it. nflow compiles the Modelica model to an FMU with OpenModelica
% (transparently, cached) and runs it through its FMI path -- no hand-written
% equations, no directed signal wiring inside the physical model.
%
% Requires a working OpenModelica (omc). Check/point it with modelicaInfo /
% modelicaConfigure. If OpenModelica is missing, sim() blocks with a clear error.
%=============================================================================
if iswasm()
  error('Nelson:nflow_fmi:desktopRequired', 'These examples require desktop Nelson and an external OpenModelica installation.');
end
info = modelicaInfo();
if ~info.capable
  fprintf('OpenModelica is not usable: %s\n', info.reason);
  fprintf('Configure it with modelicaConfigure(path), then re-run.\n');
  return
end
fprintf('Using %s\n\n', info.version);

here = fileparts(mfilename('fullpath'));

% --- Example 1: first-order RC charging, vC(t) = V (1 - e^{-t/RC}) ------------
[t1, vC1] = run_modelica_example([here, '/RC.mo'], 'RC', 6.0, 0.02, 'vC');
fprintf('RC   : vC(end) = %.4f  (steady V = 1.0)\n', vC1(end));

% --- Example 2: series RLC, damped oscillation of vC toward V -----------------
[t2, vC2] = run_modelica_example([here, '/RLC.mo'], 'RLC', 12.0, 0.02, 'vC');
fprintf('RLC  : vC(end) = %.4f  (steady V = 1.0), peak = %.4f\n', vC2(end), max(vC2));

% --- Example 3: mass-spring-damper, decaying oscillation of position ----------
[t3, x3] = run_modelica_example([here, '/MassSpringDamper.mo'], 'MassSpringDamper', 20.0, 0.02, 'x');
fprintf('MSD  : x(end) = %.4f  (rest 0.0), |x| max = %.4f\n', x3(end), max(abs(x3)));

% --- Example 4: lumped thermal mass cooling toward ambient --------------------
[t4, T4] = run_modelica_example([here, '/Thermal.mo'], 'Thermal', 30.0, 0.1, 'T');
fprintf('Therm: T(end) = %.2f  (ambient 20.0)\n', T4(end));

% --- Example 5: DC motor spinning up to steady speed -------------------------
[t5, w5] = run_modelica_example([here, '/DCMotor.mo'], 'DCMotor', 5.0, 0.02, 'w');
fprintf('Motor: w(end) = %.4f  (steady ~0.909 rad/s)\n', w5(end));

% --- Example 6: bouncing ball (state events: reinit on impact) ---------------
[t6, h6] = run_modelica_example([here, '/BouncingBall.mo'], 'BouncingBall', 3.0, 0.005, 'h');
fprintf('Ball : bounces, h(end) = %.3f  (min height %.3f)\n', h6(end), min(h6));

% --- Plot ---------------------------------------------------------------------
try
  figure('Name', 'nflow Modelica bridge -- examples');
  subplot(3, 2, 1); plot(t1, vC1, 'LineWidth', 1.5); grid on; title('RC charging'); ylabel('v_C [V]');
  subplot(3, 2, 2); plot(t2, vC2, 'LineWidth', 1.5); grid on; title('Series RLC'); ylabel('v_C [V]');
  subplot(3, 2, 3); plot(t3, x3, 'LineWidth', 1.5); grid on; title('Mass-spring-damper'); ylabel('x [m]');
  subplot(3, 2, 4); plot(t4, T4, 'LineWidth', 1.5); grid on; title('Thermal cooling'); ylabel('T [degC]');
  subplot(3, 2, 5); plot(t5, w5, 'LineWidth', 1.5); grid on; title('DC motor'); ylabel('w [rad/s]'); xlabel('time [s]');
  subplot(3, 2, 6); plot(t6, h6, 'LineWidth', 1.5); grid on; title('Bouncing ball'); ylabel('h [m]'); xlabel('time [s]');
catch
  % headless / no graphics -- the printed values above are the result
end
%=============================================================================
function [t, y] = run_modelica_example(moFile, modelName, stopTime, dt, outVar)
  % Build a one-block model: a `modelica` block (pointing at moFile) feeding a
  % To Workspace, then simulate. This is all the wiring a physical Modelica
  % model needs on the nflow side.
  m = struct('id', 'phys', 'type', 'modelica', 'inputs', 0, 'outputs', 1, ...
              'params', struct('file', moFile, 'modelName', modelName));
  tw = struct('id', 'out', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
              'params', struct('VariableName', outVar));
  conn = struct('from', 'phys', 'to', 'out', 'fromIndex', 0, 'toIndex', 0);
  doc = struct('blocks', {{m, tw}}, 'connections', {{conn}}, ...
               'runtime', stopTime, 'sampleTime', dt);
  p = [tempdir(), '/nflow_modelica_example_', modelName, '.nflow'];
  fid = fopen(p, 'w'); fwrite(fid, jsonencode(doc)); fclose(fid);
  out = sim(p);
  t = out.tout;
  y = out.logsout(1).values;
end
%=============================================================================
