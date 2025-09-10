%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function qval = percentile(td, p)
  mustBeGreaterThanOrEqual(p, 0);
  mustBeLessThanOrEqual(p, 100);
  q = p / 100;
  qval = td.quantile(q);
end
%=============================================================================
