%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = deal(varargin)
  narginchk(1, 1000)
  if (nargin == 1 || nargin == nargout)
    if nargin == 1
      varargout = varargin(ones(1, nargout));
    else
      varargout = varargin;
    end
  else
    error(_('The number of outputs should match the number of inputs.'));
  end
end
%=============================================================================
