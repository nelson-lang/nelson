%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = ip_has_fast_arc(values)
  doubled = [values values];
  runLength = 0;
  tf = false;
  for k = 1:numel(doubled)
    if doubled(k)
      runLength = runLength + 1;
      if runLength >= 9
        tf = true;
        return
      end
    else
      runLength = 0;
    end
  end
end
%=============================================================================
