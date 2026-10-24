%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = getsampleusingtime(obj, startTime, endTime)
  if nargin < 3
    endTime = startTime;
  end
  rows = find(obj.Time >= startTime & obj.Time <= endTime);
  out = obj;
  out.Time_ = obj.Time(rows);
  names = fieldnames(out.Members_);
  for k = 1:length(names)
    out.Members_.(names{k}) = getsamples(out.Members_.(names{k}), rows);
  end
  out = tscollectionRefreshMetadata(out);
end
%=============================================================================
