%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% Every bundled FMU demo diagram must simulate to completion.
%=============================================================================
exdir = [modulepath('nflow_fmi', 'root'), '/examples'];
demos = {'bouncingball', 'vanderpol', 'dahlquist', 'statespace', 'feedthrough'};
%=============================================================================
for k = 1:numel(demos)
  nflow = [exdir, '/', demos{k}, '_fmu.nflow'];
  assert_istrue(isfile(nflow));
  d = jsondecode(fileread(nflow));
  % resolve-params equivalents for a headless run.
  d.duration = str2double(d.runtime);
  d.baseDir = exdir;
  d.solver = 'discrete';
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  assert_istrue(numel(res.scopes) >= 1);
end
%=============================================================================
% Model Exchange demo diagrams: Nelson's own solver integrates the FMU states,
% so keep each diagram's own solver (ode4) instead of forcing 'discrete'.
meDemos = {'vanderpol_me', 'bouncingball_me'};
for k = 1:numel(meDemos)
  nflow = [exdir, '/', meDemos{k}, '_fmu.nflow'];
  assert_istrue(isfile(nflow));
  d = jsondecode(fileread(nflow));
  d.duration = str2double(d.runtime);
  d.baseDir = exdir;
  res = jsondecode(__nflow_simulate__(jsonencode(d)));
  assert_isequal(res.status, 'done');
  assert_istrue(numel(res.scopes) >= 1);
end
%=============================================================================
