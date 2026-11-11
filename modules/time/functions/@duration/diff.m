%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = diff(x, varargin)
  % Differences between consecutive durations, in the format of x (see diff).
  narginchk(1, 3);
  d = duration.fromSeconds(diff(duration.rawSeconds(x), varargin{:}), x.FormatValue);
end
%=============================================================================
