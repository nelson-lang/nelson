%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = flag(varargin)
  narginchk(0, 1);
  nargoutchk(0, 1);
  colorCount = requestedColorCount(varargin{:});
  pattern = [1 0 0;
             1 1 1;
             0 0 1;
             0 0 0];
  rowIndex = rem(0:colorCount - 1, size(pattern, 1)) + 1;
  varargout{1} = pattern(rowIndex, :);
end
