%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function desc = getqualitydesc(obj, code)
  desc = {};
  if isempty(obj.QualityInfo.Code)
    return
  end
  for k = 1:numel(code)
    idx = find(obj.QualityInfo.Code == code(k), 1);
    if isempty(idx) || idx > numel(obj.QualityInfo.Description)
      desc{k, 1} = '';
    else
      desc{k, 1} = obj.QualityInfo.Description{idx};
    end
  end
end
%=============================================================================
