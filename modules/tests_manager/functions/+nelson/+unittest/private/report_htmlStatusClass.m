%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function className = report_htmlStatusClass(outcome)
  className = 'failed';
  if any(strcmp(outcome, {'passed', 'skipped', 'bench', 'filtered', 'timeout'}))
    className = outcome;
  end
end
%=============================================================================
