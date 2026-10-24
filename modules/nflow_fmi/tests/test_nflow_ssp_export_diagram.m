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
% NFlow.sspExportDiagram is the inverse of NFlow.sspImport: a diagram of FMU
% blocks is packaged back into an SSP that references the same component FMUs.
% Round-trip the bundled ControlledDrivetrain.ssp (FMI 2.0, binary-less: only
% the modelDescription.xml is read, so no C toolchain is needed) through
% sspImport then sspExportDiagram and check the composition survives:
% both components, every component-to-component link, and the system output.
% (A system input has no driver block after import, so it is intentionally not
% reproduced -- the round-trip is lossy on system inputs by design.)
%=============================================================================
ssp = [modulepath('nflow_fmi', 'root'), '/examples/ControlledDrivetrain.ssp'];
assert_istrue(isfile(ssp));
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% Original composition: 2 components, 4 connections (1 system input, 2 links,
% 1 system output).
orig = NFlow.sspInfo(ssp);
assert_isequal(numel(orig.components), 2);
% The component-to-component links (both start and end elements non-empty).
c2c = orig.connections(~cellfun(@isempty, {orig.connections.startElement}) & ...
  ~ cellfun(@isempty, {orig.connections.endElement}));
%=============================================================================
% Import to a diagram, then export that diagram back to a fresh SSP.
nflowPath = NFlow.sspImport(ssp, work);
outSsp = [work, filesep, 'RoundTrip.ssp'];
r = NFlow.sspExportDiagram(nflowPath, outSsp);
assert_isequal(r, outSsp);
assert_istrue(isfile(outSsp));
%=============================================================================
% The re-exported SSP describes the same two components.
back = NFlow.sspInfo(outSsp);
backNames = sort({back.components.name});
assert_isequal(backNames, sort({orig.components.name}));
%=============================================================================
% Every component-to-component link is preserved (same element/connector pairs).
for i = 1:numel(c2c)
  match = false;
  for j = 1:numel(back.connections)
    bc = back.connections(j);
    if strcmp(bc.startElement, c2c(i).startElement) && ...
       strcmp(bc.startConnector, c2c(i).startConnector) && ...
       strcmp(bc.endElement, c2c(i).endElement) && ...
       strcmp(bc.endConnector, c2c(i).endConnector)
      match = true;
      break;
    end
  end
  assert_istrue(match);
end
%=============================================================================
% The system output (drivetrain.w -> w) survives as a connection with an empty
% end element and is declared as a system output connector.
sysOut = back.connections(cellfun(@isempty, {back.connections.endElement}));
assert_isequal(numel(sysOut), 1);
assert_isequal(sysOut.startElement, 'drivetrain');
assert_isequal(sysOut.endConnector, 'w');
scNames = {back.systemConnectors.name};
assert_istrue(any(strcmp(scNames, 'w')));
%=============================================================================
% NFlow.writeSsv is the inverse of NFlow.readSsv: values round-trip by type.
ssvFile = [work, filesep, 'rt.ssv'];
pv = struct('k', 3.5, 'n', int32(7), 'flag', true);
NFlow.writeSsv(ssvFile, pv);
back2 = NFlow.readSsv(ssvFile);
assert_isequal(back2.k, 3.5);
assert_isequal(back2.n, 7); % Integer -> double on read
assert_istrue(islogical(back2.flag) && back2.flag);
%=============================================================================
% SSV round-trip through the diagram: an FMU block carrying parameterValues
% re-exports them as a bound .ssv, and a re-import reads the values back.
% (Use a binary-less component FMU extracted from the bundled SSP: only its
% modelDescription.xml is inspected, so no C toolchain is needed.)
ex = [work, filesep, 'ex'];
mkdir(ex);
unzip(ssp, ex);
ctrlFmu = [ex, filesep, 'resources', filesep, 'Controller.fmu'];
assert_istrue(isfile(ctrlFmu));
dg = struct();
dg.blocks = {struct('id', 'controller', 'type', 'fmu', 'inputs', 2, 'outputs', 1, ...
  'params', struct('path', ctrlFmu, 'parameterValues', struct('Kp', 42.0, 'Ki', 1.5)))};
dg.connections = {};
dg.sampleTime = 0.01;
dg.duration = 1.0;
dg.variables = struct();
dgFile = [work, filesep, 'params.nflow'];
fid = fopen(dgFile, 'wt');
fwrite(fid, jsonencode(dg));
fclose(fid);
paramSsp = [work, filesep, 'Params.ssp'];
NFlow.sspExportDiagram(dgFile, paramSsp);
pinfo = NFlow.sspInfo(paramSsp);
assert_isequal(numel(pinfo.components), 1);
% The component now binds a .ssv parameter source.
assert_istrue(isfield(pinfo.components, 'parameterSources'));
assert_isequal(numel(pinfo.components(1).parameterSources), 1);
% And that .ssv holds the exported values.
pex = [work, filesep, 'pex'];
mkdir(pex);
unzip(paramSsp, pex);
ssvBack = NFlow.readSsv([pex, filesep, strrep(pinfo.components(1).parameterSources{1}, '/', filesep)]);
assert_isequal(ssvBack.Kp, 42.0);
assert_isequal(ssvBack.Ki, 1.5);
%=============================================================================
