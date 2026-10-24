%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function TF = isreal(T)
  narginchk(1, 1);
  names = T.Properties.VariableNames;
  TF = true;
  for k = 1:length(names)
    if ~isreal(T.(names{k}))
      TF = false;
      return
    end
  end
end
%=============================================================================
