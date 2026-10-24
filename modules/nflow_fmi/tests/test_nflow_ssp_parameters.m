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
% SSP parameterization (SSV): a component's ParameterBinding references a .ssv
% file whose values must be applied to the imported FMU block. Built around the
% bundled Dahlquist FMU (dx/dt = -k*x, output x, parameter k): setting k = 3 via
% an .ssv drives the output into the fast-decay regime, well below the k = 1
% default. The SSP is assembled at run time from the bundled FMU (no extra
% binary committed).
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
staging = [work, filesep, 'stage'];
mkdir(staging);
mkdir([staging, filesep, 'resources']);
copyfile([modulepath('nflow_fmi', 'root'), '/examples/Dahlquist.fmu'], ...
  [staging, filesep, 'resources', filesep, 'Dahlquist.fmu']);
%=============================================================================
ssd = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssd:SystemStructureDescription version="1.0" name="dq"', ...
  ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
  ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssd:System name="dq">', char(10), '    <ssd:Elements>', char(10), ...
  '      <ssd:Component name="D" source="resources/Dahlquist.fmu" type="application/x-fmu-sharedlibrary">', char(10), ...
  '        <ssd:ParameterBindings><ssd:ParameterBinding source="resources/D.ssv"/></ssd:ParameterBindings>', char(10), ...
  '      </ssd:Component>', char(10), '    </ssd:Elements>', char(10), ...
  '  </ssd:System>', char(10), '</ssd:SystemStructureDescription>', char(10)];
fid = fopen([staging, filesep, 'SystemStructure.ssd'], 'wt');
fwrite(fid, ssd);
fclose(fid);
%=============================================================================
ssv = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssv:ParameterSet version="1.0" name="p"', ...
  ' xmlns:ssv="http://ssp-standard.org/SSP1/SystemStructureParameterValues"', ...
  ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssv:Parameters><ssv:Parameter name="k"><ssv:Real value="3"/></ssv:Parameter></ssv:Parameters>', char(10), ...
  '</ssv:ParameterSet>', char(10)];
fid = fopen([staging, filesep, 'resources', filesep, 'D.ssv'], 'wt');
fwrite(fid, ssv);
fclose(fid);
%=============================================================================
sspFile = [work, filesep, 'dq.ssp'];
cur = pwd();
cd(staging);
zip(sspFile, {'SystemStructure.ssd', 'resources/Dahlquist.fmu', 'resources/D.ssv'});
cd(cur);
%=============================================================================
% sspInfo exposes the component's parameter source.
info = NFlow.sspInfo(sspFile);
assert_isequal(numel(info.components), 1);
assert_isequal(info.components(1).parameterSources, {'resources/D.ssv'});
%=============================================================================
% The .ssv reader returns the parameter value.
pv = NFlow.readSsv([staging, filesep, 'resources', filesep, 'D.ssv']);
assert_isequal(pv.k, 3);
%=============================================================================
% Import must carry the value onto the FMU block.
nflowPath = NFlow.sspImport(sspFile, work);
d = jsondecode(fileread(nflowPath));
bl = d.blocks;
if ~iscell(bl)
  bl = num2cell(bl);
end
fmu = bl{1};
assert_isequal(fmu.type, 'fmu');
assert_istrue(isfield(fmu.params, 'parameterValues'));
assert_isequal(fmu.params.parameterValues.k, 3);
%=============================================================================
% Functional check: with k = 3 the state decays much faster than the k = 1
% default (x(1) would be exp(-1) = 0.368), so x(1) must be well below that.
bl{end + 1} = struct('id', 'tw', 'type', 'toWorkspace', 'inputs', 1, 'outputs', 0, ...
  'params', struct('VariableName', 'ssv_x', 'SaveFormat', 'Array', 'Decimation', 1, 'MaxDataPoints', 'inf'));
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
x = evalin('base', 'ssv_x');
x = x(:);
assert_istrue(x(end) < 0.15);
evalin('base', 'clear(''ssv_x'')');
%=============================================================================
% Inline parameters: the same k = 3 written straight into the .ssd (a
% ParameterBinding with an embedded ParameterSet, no external .ssv) must be
% picked up the same way.
staging2 = [work, filesep, 'stage_inline'];
mkdir(staging2);
mkdir([staging2, filesep, 'resources']);
copyfile([modulepath('nflow_fmi', 'root'), '/examples/Dahlquist.fmu'], ...
  [staging2, filesep, 'resources', filesep, 'Dahlquist.fmu']);
ssd2 = ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssd:SystemStructureDescription version="1.0" name="dq"', ...
  ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
  ' xmlns:ssv="http://ssp-standard.org/SSP1/SystemStructureParameterValues"', ...
  ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssd:System name="dq">', char(10), '    <ssd:Elements>', char(10), ...
  '      <ssd:Component name="D" source="resources/Dahlquist.fmu" type="application/x-fmu-sharedlibrary">', char(10), ...
  '        <ssd:ParameterBindings><ssd:ParameterBinding><ssd:ParameterValues>', char(10), ...
  '          <ssv:ParameterSet version="1.0" name="p"><ssv:Parameters>', char(10), ...
  '            <ssv:Parameter name="k"><ssv:Real value="3"/></ssv:Parameter>', char(10), ...
  '          </ssv:Parameters></ssv:ParameterSet>', char(10), ...
  '        </ssd:ParameterValues></ssd:ParameterBinding></ssd:ParameterBindings>', char(10), ...
  '      </ssd:Component>', char(10), '    </ssd:Elements>', char(10), ...
  '  </ssd:System>', char(10), '</ssd:SystemStructureDescription>', char(10)];
fid = fopen([staging2, filesep, 'SystemStructure.ssd'], 'wt');
fwrite(fid, ssd2);
fclose(fid);
sspInline = [work, filesep, 'dq_inline.ssp'];
cur = pwd();
cd(staging2);
zip(sspInline, {'SystemStructure.ssd', 'resources/Dahlquist.fmu'});
cd(cur);
info2 = NFlow.sspInfo(sspInline);
% No external .ssv, but the inline value is parsed.
assert_istrue(isempty(info2.components(1).parameterSources));
assert_isequal(info2.components(1).inlineParameters.k, 3);
nflow2 = NFlow.sspImport(sspInline, work);
d2 = jsondecode(fileread(nflow2));
b2 = d2.blocks;
if ~iscell(b2)
  b2 = num2cell(b2);
end
assert_istrue(isfield(b2{1}.params, 'parameterValues'));
assert_isequal(b2{1}.params.parameterValues.k, 3);
%=============================================================================
