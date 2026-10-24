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
% SSP import of an FMI 2.0 component (F5): the biggest collateral win of FMI 2.0
% support is that the huge existing corpus of FMI 2.0 FMUs (most real-world SSPs)
% imports and simulates. Built at run time from the bundled 2.0 Reference FMU
% Dahlquist (dx/dt = -k*x), so the run is deterministic and self-contained.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
skip_testsuite(nflow_fmi_skip_bundled_fmi2_runtime(), ...
  'bundled FMI 2.0 reference FMUs only ship x86-64 binaries');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
staging = [work, filesep, 'stage'];
mkdir(staging);
mkdir([staging, filesep, 'resources']);
% A distinct file-name stem keeps the extraction staging independent of any
% other test that imports a Dahlquist FMU in the same process.
copyfile([modulepath('nflow_fmi', 'root'), '/examples/Dahlquist_fmi2.fmu'], ...
  [staging, filesep, 'resources', filesep, 'Dahlquist2cs.fmu']);
%=============================================================================
ssd = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssd:SystemStructureDescription version="1.0" name="dq2"', ...
  ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
  ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssd:System name="dq2">', char(10), '    <ssd:Elements>', char(10), ...
  '      <ssd:Component name="D" source="resources/Dahlquist2cs.fmu" type="application/x-fmu-sharedlibrary"/>', char(10), ...
  '    </ssd:Elements>', char(10), '  </ssd:System>', char(10), ...
  '</ssd:SystemStructureDescription>', char(10)];
fid = fopen([staging, filesep, 'SystemStructure.ssd'], 'wt');
fwrite(fid, ssd);
fclose(fid);
sspFile = [work, filesep, 'dq2.ssp'];
cur = pwd();
cd(staging);
zip(sspFile, {'SystemStructure.ssd', 'resources/Dahlquist2cs.fmu'});
cd(cur);
%=============================================================================
% sspInfo reads the FMI 2.0 component without rejecting it.
info = NFlow.sspInfo(sspFile);
assert_isequal(numel(info.components), 1);
%=============================================================================
% Import must accept the FMI 2.0 component (no version rejection) and map it to
% an fmu block.
nflowPath = NFlow.sspImport(sspFile, work);
d = jsondecode(fileread(nflowPath));
bl = d.blocks;
if ~iscell(bl)
  bl = num2cell(bl);
end
assert_isequal(bl{1}.type, 'fmu');
%=============================================================================
% Simulate: the FMI 2.0 state x decays from 1.
bl{end + 1} = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
  'params', struct('VariableName', 'ssp2_x', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
cs = d.connections;
if ~iscell(cs)
  cs = num2cell(cs);
end
cs{end + 1} = struct('from', 'D', 'fromIndex', 0, 'to', 'tw', 'toIndex', 0);
d.blocks = bl;
d.connections = cs;
d.duration = 1.0;
d.sampleTime = 0.05;
d.solver = 'discrete';
r = jsondecode(__nflow_simulate__(jsonencode(d)));
assert_isequal(r.status, 'done');
x = evalin('base', 'ssp2_x');
x = x(:);
assert_istrue(x(1) > x(end)); % decays
assert_istrue(x(end) > 0);
evalin('base', 'clear(''ssp2_x'')');
%=============================================================================
