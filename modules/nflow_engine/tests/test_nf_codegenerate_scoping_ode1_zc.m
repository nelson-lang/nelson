%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% ode1: the simulator's fixed-step solver loop localizes zero-crossing
% surfaces even at order 1; the fused Euler emission cannot, and must say so
% (a solverDowngrade warning naming the block) instead of staying silent.
dz = struct();
dz.blocks = {...
  struct('id', 'k', 'type', 'constant', 'inputs', 0, 'outputs', 1, 'params', struct('Value', 1)), ...
  struct('id', 'ig', 'type', 'integrator', 'inputs', 1, 'outputs', 1, ...
    'params', struct('InitialCondition', 0, 'LowerSaturationLimit', 0, ...
      'UpperSaturationLimit', 1.35)), ...
  struct('id', 'y', 'type', 'labelSink', 'inputs', 1, 'outputs', 1, ...
    'params', struct('GotoTag', 'y', 'isExternalPort', true))};
dz.connections = {...
  struct('from', 'k', 'to', 'ig', 'fromIndex', 0, 'toIndex', 0), ...
  struct('from', 'ig', 'to', 'y', 'fromIndex', 0, 'toIndex', 0)};
dz.sampleTime = 0.1;
dz.duration = 2.0;
dz.solver = 'ode1';
dz.variables = struct();
fz = [tempname(), '.nflow'];
fid = fopen(fz, 'wt');
fwrite(fid, jsonencode(dz));
fclose(fid);
ok = false;
try
  nflow_codegenerate(fz, tempdir(), false, 'C', struct('failOnWarning', true));
catch e
  ok = ~isempty(strfind(e.message, 'zero-crossing'));
end
assert_istrue(ok);
rmfile(fz);
%=============================================================================
