%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = vertcat(varargin)
  out = varargin{1};
  for k = 2:nargin
    names = fieldnames(out.Members_);
    for j = 1:length(names)
      if isfield(varargin{k}.Members_, names{j})
        out.Members_.(names{j}) = append(out.Members_.(names{j}), varargin{k}.Members_.(names{j}));
      end
    end
    out.Time_ = [out.Time_; varargin{k}.Time(:)];
  end
  out = tscollectionRefreshMetadata(out);
end
%=============================================================================
