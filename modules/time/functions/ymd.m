%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [y, m, d] = ymd(t)
  narginchk(1, 1);
  nargoutchk(0, 3);
  v = datevec(datenum(t));
  y = reshape(v(:, 1), size(datenum(t)));
  m = reshape(v(:, 2), size(datenum(t)));
  d = reshape(v(:, 3), size(datenum(t)));
end
%=============================================================================
