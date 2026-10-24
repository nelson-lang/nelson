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
% B1: structured simulation output: sim() aggregates every logged signal (To
% Workspace variables and scopes) into a queryable out.logsout, each entry
% carrying name / time / values, so results can be explored programmatically.
%=============================================================================
examples_dir = [modulepath('nflow_blocks'), '/examples'];

out = sim([examples_dir, '/workspace/To_Workspace_Demo.nflow']);

asserts.istrue(isfield(out, 'logsout'));
asserts.istrue(isstruct(out.logsout));
asserts.istrue(numel(out.logsout) >= 1);
% Each entry is queryable by name and carries time + values.
names = {out.logsout.name};
asserts.istrue(any(strcmp(names, 'simout')));
idx = find(strcmp(names, 'simout'), 1);
asserts.istrue(~isempty(idx));
asserts.istrue(~isempty(out.logsout(idx).values));
asserts.istrue(~isempty(out.logsout(idx).time));
% The logged signal is also still exposed directly as out.<VariableName>.
asserts.istrue(isfield(out, 'simout'));
asserts.istrue(isfield(out, 'tout'));
%=============================================================================
