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
% The bundled ControlledDrivetrain.ssp (structure from the FMPy project,
% BSD-2-Clause) exercises every SSP connection shape: a system input
% (w_ref -> controller.u_s), two component-to-component links, and a system
% output (drivetrain.w -> w). Its components are FMI 2.0, so import must report
% the version cleanly rather than fail obscurely.
%=============================================================================
ssp = [modulepath('nflow_fmi', 'root'), '/examples/ControlledDrivetrain.ssp'];
assert_istrue(isfile(ssp));
%=============================================================================
info = NFlow.sspInfo(ssp);
assert_isequal(info.name, 'ControlledDrivetrain');
assert_isequal(numel(info.components), 2);
names = {info.components.name};
assert_istrue(any(strcmp(names, 'controller')));
assert_istrue(any(strcmp(names, 'drivetrain')));
assert_isequal(numel(info.connections), 4);
%=============================================================================
% A system input has an empty startElement; a system output an empty endElement.
starts = {info.connections.startElement};
ends = {info.connections.endElement};
assert_istrue(any(cellfun(@isempty, starts))); % w_ref -> controller.u_s
assert_istrue(any(cellfun(@isempty, ends))); % drivetrain.w -> w
% The system output carries the connector name 'w'.
sysOut = info.connections(cellfun(@isempty, ends));
assert_isequal(sysOut.startElement, 'drivetrain');
assert_isequal(sysOut.endConnector, 'w');
%=============================================================================
% The parent System declares its own connectors: input w_ref and output w.
sc = info.systemConnectors;
assert_isequal(numel(sc), 2);
scNames = {sc.name};
assert_isequal(sc(strcmp(scNames, 'w_ref')).kind, 'input');
assert_isequal(sc(strcmp(scNames, 'w')).kind, 'output');
%=============================================================================
% Version helper reads the declared FMI version straight from the FMU archive.
work = tempname();
if isdir(work)
  rmdir(work, 's');
end
mkdir(work);
unzip(ssp, work);
assert_isequal(NFlow.fmuFmiVersion([work, '/resources/Controller.fmu']), '2.0');
%=============================================================================
% FMI 2.0 components are supported, so import maps both to fmu blocks. (These
% example FMUs ship without binaries, so the composition is not simulated here;
% test_nflow_ssp_import_fmi2 covers the end-to-end 2.0 import + simulation.)
nflowPath = NFlow.sspImport(ssp, work);
d = jsondecode(fileread(nflowPath));
bl = d.blocks;
if ~iscell(bl)
  bl = num2cell(bl);
end
types = cellfun(@(b) b.type, bl, 'UniformOutput', false);
assert_isequal(sum(strcmp(types, 'fmu')), 2);
%=============================================================================
