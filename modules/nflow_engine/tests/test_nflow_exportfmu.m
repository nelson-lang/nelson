%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 16-->
% <--ENGLISH IMPOSED-->
%=============================================================================
% FMU export: NFlow.exportfmu packages a model as an FMI 3.0 Co-Simulation
% source FMU (modelDescription.xml at the archive root + C sources under
% sources/), with the external label ports becoming FMI input/output variables.
%=============================================================================
d = nflow_exportfmu_gain_model();

wd = tempname();
mkdir(wd);
cleaner = onCleanup(@() rmdir(wd, 's')); %#ok<NASGU>
modelFile = [wd, '/fmu_export_test.nflow'];
fid = fopen(modelFile, 'wt'); fwrite(fid, jsonencode(d)); fclose(fid);

fmu = NFlow.exportfmu(modelFile, wd);
asserts.istrue(isfile(fmu));
asserts.istrue(~isempty(strfind(fmu, 'fmu_export_test.fmu')));

% The archive must hold modelDescription.xml at the root and the C sources.
co = [wd, '/fmu_export_test_check'];
names = unzip(fmu, co);
rel = cellfun(@(s) strrep(strrep(s, co, ''), '\', '/'), names, 'UniformOutput', false);
asserts.istrue(any(strcmp(rel, '/modelDescription.xml')));
asserts.istrue(any(strcmp(rel, '/sources/model.c')));
asserts.istrue(any(strcmp(rel, '/sources/fmi3Functions.c')));
asserts.istrue(any(strcmp(rel, '/sources/buildDescription.xml')));

% The modelDescription must be FMI 3.0 CS with the external ports as variables.
md = fileread([co, filesep, 'modelDescription.xml']);
asserts.istrue(~isempty(strfind(md, 'fmiVersion="3.0"')));
asserts.istrue(~isempty(strfind(md, '<CoSimulation')));
asserts.istrue(~isempty(strfind(md, 'name="u" valueReference="1" causality="input"')));
asserts.istrue(~isempty(strfind(md, 'name="y" valueReference="2" causality="output"')));

% The generated wrapper wires the FMI value references to the model fields.
wrapper = fileread([co, filesep, 'sources', filesep, 'fmi3Functions.c']);
asserts.istrue(~isempty(strfind(wrapper, 'fmi3DoStep')));
asserts.istrue(~isempty(strfind(wrapper, 'c->input.u = values[i]')));
asserts.istrue(~isempty(strfind(wrapper, 'values[i] = c->output.y')));

% When a C toolchain and the FMI headers are available, the sources are also
% compiled to a platform binary under binaries/<platform>/; if present, it must
% be named after the model identifier.
hasBinaries = any(cellfun(@(s) ~ isempty(strfind(s, '/binaries/')) ...
  || ~isempty(strfind(s, [filesep, 'binaries', filesep])), names));
if hasBinaries
  binaryName = ['fmu_export_test', getdynlibext()];
  isBinary = cellfun(@(s) endsWith(s, binaryName), names);
  asserts.istrue(any(isBinary));
end
%=============================================================================
