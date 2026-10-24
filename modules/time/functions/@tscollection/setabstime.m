%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = setabstime(obj, startDate)
  info = obj.TimeInfo;
  info.StartDate = startDate;
  obj.TimeInfo = info;
  names = fieldnames(obj.Members_);
  for k = 1:length(names)
    obj.Members_.(names{k}) = setabstime(obj.Members_.(names{k}), startDate);
  end
end
%=============================================================================
