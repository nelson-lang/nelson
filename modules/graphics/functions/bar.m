%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = bar(varargin)
  narginchk(1, 100);
  nargoutchk(0, 1);
  if nargout == 1
    varargout{1} = barBase('vertical', varargin{:});
  else
    barBase('vertical', varargin{:});
  end
end
%=============================================================================
