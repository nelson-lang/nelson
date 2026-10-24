%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function obj = delsamplefromcollection(obj, varargin)
  names = fieldnames(obj.Members_);
  for k = 1:length(names)
    obj.Members_.(names{k}) = delsample(obj.Members_.(names{k}), varargin{:});
  end
  if ~isempty(names)
    obj.Time_ = obj.Members_.(names{1}).Time;
  end
  obj = tscollectionRefreshMetadata(obj);
end
%=============================================================================
