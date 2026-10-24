%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Inspect a System Structure and Parameterization (SSP) archive.
%
% ControlledDrivetrain.ssp is the classic controlled-drivetrain composition
% (a Controller FMU driving a Drivetrain FMU, with a speed feedback loop). Its
% structure is taken from the FMPy project's ControlledDrivetrain example
% (https://github.com/CATIA-Systems/FMPy, BSD-2-Clause). The component binaries
% are stripped here: the two FMUs it references are FMI 2.0, which the FMI 3.0
% runtime does not load, so only their modelDescription.xml (interface + version)
% is kept. The archive still demonstrates the full SSP topology and the clean
% version diagnostic on import.
%=============================================================================
ssp = [fileparts(mfilename('fullpathext')), '/ControlledDrivetrain.ssp'];
%=============================================================================
% Read the system structure: components and how their connectors are wired.
info = NFlow.sspInfo(ssp);
disp(['system     : ', info.name]);
disp(['components : ', num2str(numel(info.components))]);
for i = 1:numel(info.components)
  c = info.components(i);
  disp(['  ', c.name, ' <- ', c.source]);
end
disp(['connections: ', num2str(numel(info.connections))]);
for i = 1:numel(info.connections)
  cn = info.connections(i);
  % An empty startElement is a system input; an empty endElement a system output.
  from = cn.startElement; if isempty(from); from = '(system)'; end
  to = cn.endElement; if isempty(to); to = '(system)'; end
  disp(['  ', from, '.', cn.startConnector, ' -> ', to, '.', cn.endConnector]);
end
%=============================================================================
% Import wires each FMI 3.0 Co-Simulation component into an nflow diagram. Here
% the components are FMI 2.0, so import reports the version instead of guessing.
try
  NFlow.sspImport(ssp, tempdir());
catch e
  disp(['import: ', e.message]);
end
%=============================================================================
