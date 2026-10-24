%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = eomday(y, m)
  narginchk(2, 2);
  nargoutchk(0, 1);
  mustBeNumeric(y, 1);
  mustBeInteger(y, 1);
  mustBeNumeric(m, 2);
  mustBeInteger(m, 2);
  mustBeInRange(m, 1, 12, 2);
  d = datenum(y, m + 1, 1) - datenum(y, m, 1);
end
