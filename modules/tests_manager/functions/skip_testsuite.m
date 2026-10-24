%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = skip_testsuite(varargin)
  if nargout == 0
    nelson.unittest.skip(varargin{:}); %#ok<NLS0005>
    varargout = {};
  else
    [varargout{1:nargout}] = nelson.unittest.skip(varargin{:});
  end
end
%=============================================================================
