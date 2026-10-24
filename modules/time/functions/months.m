%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function m = months(t1, t2)
  narginchk(2, 2);
  v1 = datevec(datenum(t1));
  v2 = datevec(datenum(t2));
  m = (v2(:, 1) - v1(:, 1)) .* 12 + (v2(:, 2) - v1(:, 2));
  adjust = v2(:, 3) < v1(:, 3);
  m(adjust) = m(adjust) - 1;
  m = reshape(m, size(datenum(t1)));
end
%=============================================================================
