%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = unique(A, varargin)
  [C, ia, ic] = tableUnique(A, varargin{:});
  values = {C, ia, ic};
  varargout = values(1:nargout);
end
%=============================================================================
