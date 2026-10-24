%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function y = year(t)
  narginchk(1, 1);
  if isa(t, 'datetime')
    y = t.Year;
  else
    v = datevec(t);
    y = reshape(v(:, 1), size(t));
  end
end
%=============================================================================
