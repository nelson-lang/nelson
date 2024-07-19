%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = uiinspect(varargin)
  disp('uiinspect')
  if nargin > 0
    class(varargin{1})
  end
  if nargin > 1
    varargin{2}
  end
  if nargin > 3
    varargin{3}
  end
  varargout={};
end
