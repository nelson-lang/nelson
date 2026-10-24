%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = horzcat(varargin)
  out = varargin{1};
  for k = 2:nargin
    names = fieldnames(varargin{k}.Members_);
    for j = 1:length(names)
      out = addts(out, varargin{k}.Members_.(names{j}), names{j});
    end
  end
end
%=============================================================================
