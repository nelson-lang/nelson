%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = cell_isequaln(a, b)
  if iscell(b)
    s1 = size(a);
    s2 = size(b);
    if isequal(s1, s2)
      r = true;
      k = 1;
      for e = a(:)'
        if ~isequaln(e{1}, b{k})
          r = false;
          break;
        else
          k = k + 1;
        end
      end
    else
      r = false;
    end
  else
    r = false;
  end
end
