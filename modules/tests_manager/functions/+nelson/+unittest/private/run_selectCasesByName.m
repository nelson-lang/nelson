%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
function selected = run_selectCasesByName(cases, names)
  selected = struct([]);
  for k = 1:length(cases)
    if any(strcmp(names, cases(k).name))
      selected = [selected; cases(k)];
    end
  end
end
