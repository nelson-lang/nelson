%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pow2db(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if (any(varargin{1} < 0))
    error('Nelson:validators:mustBeNonnegative', '%s must be nonnegative.', 'Input argument value');
  end
  varargout{1} = 10 .* log10(varargin{1});
end
%=============================================================================
