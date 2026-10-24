%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function kind = discover_caseKind(name)
  if startsWith(name, 'bench_')
    kind = 'bench';
  elseif startsWith(name, 'bug_')
    kind = 'bug';
  else
    kind = 'test';
  end
end
