%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = missing_input_second(mode, x)
  switch mode
    case 1
      r = x;
    case 2
      r = x(1);
    case 3
      r = x{1};
    case 4
      r = x.field;
    case 5
      r = mode + x;
    case 6
      r = [mode, x];
    otherwise
      r = mode;
  end
end
%=============================================================================
