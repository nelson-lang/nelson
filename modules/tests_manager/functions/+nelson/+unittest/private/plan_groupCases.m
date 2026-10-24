%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function groups = plan_groupCases(cases) %#ok<NLS0026>
  groups = struct('parallel', {{}}, 'sequential', {{}}, 'gui', {{}}, 'mpi', {{}}, 'bench', {{}});
  for k = 1:length(cases)
    c = cases(k);
    if strcmp(c.kind, 'bench')
      groups.bench = [groups.bench; c.id];
    elseif strcmp(c.mode, 'gui')
      groups.gui = [groups.gui; c.id];
    elseif plan_hasResource(c, 'mpi_mode')
      groups.mpi = [groups.mpi; c.id];
    elseif plan_hasTag(c, 'sequential_test_required')
      groups.sequential = [groups.sequential; c.id];
    else
      groups.parallel = [groups.parallel; c.id];
    end
  end
end
