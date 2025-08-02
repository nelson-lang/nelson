%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function result = errorfun(S, varargin)
  disp(nargin())
  disp(S)
  disp(class(varargin))
  disp(size(varargin))
  disp(varargin{1})
  disp(varargin{2})
  result = false;
end

