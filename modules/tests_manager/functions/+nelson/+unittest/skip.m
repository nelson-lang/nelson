%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = skip(varargin)
  if nargout == 0
    skip_impl(varargin{:});
    varargout = {};
  else
    [varargout{1:nargout}] = skip_impl(varargin{:}); %#ok<NLS0004>
  end
end
%=============================================================================
