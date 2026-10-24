%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function timeseriesValidate(obj)
  dataLength = timeseriesLengthFromData(obj.Data_, obj.IsTimeFirst_);
  timeLength = length(obj.Time_);
  if dataLength ~= timeLength
    error('Nelson:time:dataTimeLengthsMatch', '%s and time lengths must match.', 'Data');
  end
  if ~isempty(obj.Quality_) && numel(obj.Quality_) ~= timeLength
    error('Nelson:time:qualityTimeLengthsMatch', '%s and time lengths must match.', 'Quality');
  end
end
%=============================================================================
