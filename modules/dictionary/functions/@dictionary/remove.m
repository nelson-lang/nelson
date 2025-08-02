%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = remove(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  obj = varargin{1};
  if ~isConfigured(obj)
    varargout{1} = obj;
    return
  end
  key = varargin{2};
  obj(key) = []; 
  varargout{1} = obj;
end
%=============================================================================
