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
% FMI 2.0 loader (F1): the version-neutral loader reads FMI 2.0 metadata just
% like FMI 3.0. Uses the Modelica Association Reference FMUs (BSD-2), the 2.0
% variants of the models already bundled in 3.0.
%=============================================================================
skip_testsuite(~__nflow_fmi_available__(), 'nflow_fmi runtime not available');
exdir = [modulepath('nflow_fmi', 'root'), '/examples'];
%=============================================================================
% Dahlquist 2.0: one state output 'x' and one parameter 'k'.
info = fmiInfo([exdir, '/Dahlquist_fmi2.fmu']);
assert_isequal(info.fmiVersion, '2.0');
assert_istrue(info.coSimulation);
assert_istrue(any(strcmp(info.names, 'x')));
assert_istrue(any(strcmp(info.names, 'k')));
assert_isequal(info.causalities{strcmp(info.names, 'x')}, 'output');
assert_isequal(info.causalities{strcmp(info.names, 'k')}, 'parameter');
%=============================================================================
% VanDerPol 2.0: two state outputs x0/x1 and a parameter mu.
info = fmiInfo([exdir, '/VanDerPol_fmi2.fmu']);
assert_isequal(info.fmiVersion, '2.0');
outs = info.names(strcmp(info.causalities, 'output'));
assert_istrue(any(strcmp(outs, 'x0')));
assert_istrue(any(strcmp(outs, 'x1')));
%=============================================================================
% The JSON inspector (consumed by SSP import) reports FMI 2.0 with success.
j = jsondecode(__nflow_fmi_inspect_json__([exdir, '/BouncingBall_fmi2.fmu']));
assert_istrue(j.success);
assert_isequal(j.fmiVersion, '2.0');
%=============================================================================
% FMI 2.0 has only scalar variables: no array/dimension metadata is reported.
vs = j.variables;
if ~iscell(vs)
  vs = num2cell(vs);
end
for k = 1:numel(vs)
  if isfield(vs{k}, 'arraySize')
    assert_isequal(double(vs{k}.arraySize), 1);
  end
end
%=============================================================================
