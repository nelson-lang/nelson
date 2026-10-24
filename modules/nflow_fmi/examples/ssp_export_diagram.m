%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Round-trip an SSP composition: SSP -> nflow diagram -> SSP.
%
% NFlow.sspExportDiagram is the inverse of NFlow.sspImport. Import turns an SSP
% into a diagram of FMU blocks; sspExportDiagram packages such a diagram back
% into an SSP that references the same component FMUs. The composition's
% components, its component-to-component links and its system outputs survive
% the round-trip (a system input has no driver after import, so it is dropped).
%
% ControlledDrivetrain.ssp (structure from the FMPy project, BSD-2-Clause) is
% binary-less, so only the modelDescription.xml is read -- no C toolchain needed.
%=============================================================================
ssp = [fileparts(mfilename('fullpathext')), '/ControlledDrivetrain.ssp'];
work = [tempdir(), 'ssp_export_diagram_example'];
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
%=============================================================================
% Original composition.
orig = NFlow.sspInfo(ssp);
disp(['original   : ', orig.name, ' (', num2str(numel(orig.components)), ...
  ' components, ', num2str(numel(orig.connections)), ' connections)']);
%=============================================================================
% Import to a diagram of FMU blocks, then export the diagram back to an SSP.
nflowFile = NFlow.sspImport(ssp, work);
outSsp = NFlow.sspExportDiagram(nflowFile, [work, filesep, 'RoundTrip.ssp']);
%=============================================================================
% The re-exported SSP describes the same components and preserves the links.
back = NFlow.sspInfo(outSsp);
disp(['round-trip : ', back.name, ' (', num2str(numel(back.components)), ...
  ' components, ', num2str(numel(back.connections)), ' connections)']);
for i = 1:numel(back.connections)
  cn = back.connections(i);
  to = cn.endElement; if isempty(to); to = '(system)'; end
  disp(['  ', cn.startElement, '.', cn.startConnector, ' -> ', to, '.', cn.endConnector]);
end
%=============================================================================
