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
% modelicaToFmu path handling: a model given as a .mo file path must be
% resolved as a file (and reported clearly when missing), never silently
% mistaken for inline Modelica source. Covers install paths that contain
% spaces and parentheses, e.g. the default Windows location
% "Program Files\Nelson-x.y.z.w (64 bits)".
%=============================================================================
% A missing .mo path fails fast with a typed, readable error -- NOT a cryptic
% OpenModelica parser error from writing the path out as Modelica source. This
% does not need OpenModelica, so it runs everywhere.
missing = [tempdir(), '/this file (does not) exist.mo'];
assert_isfalse(isfile(missing));
id = '';
msg = '';
try
  modelicaToFmu(missing, 'Whatever');
catch e
  id = e.identifier;
  msg = e.message;
end
assert_isequal(id, 'Nelson:nflow_fmi:modelicaToFmuFileNotFound');
assert_istrue(~isempty(strfind(msg, 'does not exist')));
%=============================================================================
% Inline Modelica source is still accepted and must never be read as a path.
info = modelicaInfo();
if ~info.capable
  return
end
%=============================================================================
src = [modulepath('nflow_fmi'), '/examples/modelica/RLC.mo'];
%=============================================================================
% A .mo file under a directory whose name has spaces AND parentheses compiles
% to an FMU exactly like any other path.
workDir = [tempname(), ' (64 bits) v2.0.0.0'];
mkdir(workDir);
dst = [workDir, '/RLC.mo'];
copyfile(src, dst);
assert_istrue(isfile(dst));

fmu = modelicaToFmu(dst, 'RLC');
assert_istrue(isfile(fmu));
assert_istrue(endsWith(fmu, 'RLC.fmu'));

fi = fmiInfo(fmu);
assert_isequal(fi.fmiVersion, '2.0');
assert_istrue(fi.coSimulation);
%=============================================================================
