%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function ms = milliseconds(x)
  narginchk(1, 1);
  if isa(x, 'duration')
    ms = double(x) .* 1000;
  else
    ms = duration(0, 0, 0, double(x));
    ms.Format = 's';
  end
end
%=============================================================================
