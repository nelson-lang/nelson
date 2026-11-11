%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function d = diff(t, varargin)
  % Elapsed time between consecutive elements, as a duration (see diff).
  narginchk(1, 3);
  [dayNumber, millis, offset] = datetime.instantParts(t);
  % seconds from the first finite day keep the time of day precise
  reference = min(dayNumber(isfinite(dayNumber)));
  if isempty(reference)
    reference = 0;
  end
  secondsValue = (dayNumber - reference) .* 86400 + millis ./ 1000 - offset;
  d = duration.fromSeconds(diff(secondsValue, varargin{:}));
end
%=============================================================================
