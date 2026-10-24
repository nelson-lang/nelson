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
% The causal example diagrams shipped for the niche blocks (Coulomb & Viscous
% Friction, the signal utilities, and Bus Assignment) must each be well-formed
% and simulate to completion.
%=============================================================================
exdir = [modulepath('nflow_blocks'), '/examples/causal/'];
names = {'Coulomb_Viscous_Friction_Demo', 'Signal_Utilities_Demo', 'Bus_Assignment_Demo'};
for k = 1:numel(names)
  p = [exdir, names{k}, '.nflow'];
  assert_istrue(isfile(p));
  txt = fileread(p);
  d = jsondecode(txt); % well-formed JSON
  assert_istrue(isfield(d, 'blocks'));
  assert_istrue(isfield(d, 'connections'));
  r = jsondecode(__nflow_simulate__(txt));
  assert_isequal(r.status, 'done');
end
%=============================================================================
