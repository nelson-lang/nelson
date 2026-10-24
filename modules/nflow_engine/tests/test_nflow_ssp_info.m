%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% NFlow.sspInfo reads the SystemStructure.ssd of an SSP (System Structure and
% Parameterization) archive and reports the system name, its components (each
% referencing an FMU) and the connections between them.
%=============================================================================
ssd = [...
  '<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
  '<ssd:SystemStructureDescription version="1.0" name="mini"', ...
  ' xmlns:ssd="http://ssp-standard.org/SSP1/SystemStructureDescription"', ...
  ' xmlns:ssc="http://ssp-standard.org/SSP1/SystemStructureCommon">', char(10), ...
  '  <ssd:System name="mini">', char(10), ...
  '    <ssd:Elements>', char(10), ...
  '      <ssd:Component name="A" source="resources/a.fmu" type="application/x-fmu-sharedlibrary" implementation="CoSimulation"/>', char(10), ...
  '      <ssd:Component name="B" source="resources/b.fmu" type="application/x-fmu-sharedlibrary" implementation="CoSimulation"/>', char(10), ...
  '    </ssd:Elements>', char(10), ...
  '    <ssd:Connections>', char(10), ...
  '      <ssd:Connection startElement="A" startConnector="y" endElement="B" endConnector="u"/>', char(10), ...
  '    </ssd:Connections>', char(10), ...
  '  </ssd:System>', char(10), ...
  '</ssd:SystemStructureDescription>', char(10)];

staging = tempname();
mkdir(staging);
fid = fopen([staging, filesep, 'SystemStructure.ssd'], 'wt');
fwrite(fid, ssd);
fclose(fid);
sspFile = [tempname(), '.ssp'];
zip(sspFile, fullfile(staging, 'SystemStructure.ssd'), [staging, filesep]);

info = NFlow.sspInfo(sspFile);
asserts.isequal(info.name, 'mini');
asserts.isequal(numel(info.components), 2);
asserts.isequal(info.components(1).name, 'A');
asserts.isequal(info.components(1).source, 'resources/a.fmu');
asserts.isequal(info.components(1).implementation, 'CoSimulation');
asserts.isequal(info.components(2).name, 'B');
asserts.isequal(numel(info.connections), 1);
asserts.isequal(info.connections(1).startElement, 'A');
asserts.isequal(info.connections(1).startConnector, 'y');
asserts.isequal(info.connections(1).endElement, 'B');
asserts.isequal(info.connections(1).endConnector, 'u');

% A missing file is an error.
asserts.checkerror('NFlow.sspInfo([tempdir(), ''no_such.ssp''])', [_('SSP file not found: '), tempdir(), 'no_such.ssp']);
rmdir(staging, 's');
rmfile(sspFile);
%=============================================================================
