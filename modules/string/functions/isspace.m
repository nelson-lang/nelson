%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = isspace(varargin)
  narginchk(1, 1);
  nargoutchk(0, 1);
  A = varargin{1};
  if isstring(A)
    mustBeTextScalar(A, 1);
    A = char(A);
  end
  if ~ischar(A)
    % numbers, logicals and cells are never whitespace
    varargout{1} = false(size(A));
    return
  end
  % ASCII controls 9-13 and 28-31, the space, and the Unicode space
  % separators that are not no-break spaces
  D = double(A);
  varargout{1} = (D == 32) | (D >= 9 & D <= 13) | (D >= 28 & D <= 31) | (D == 5760) ...
    | (D >= 8192 & D <= 8202 & D ~= 8199) | (D == 8232) | (D == 8233) | (D == 8287) | (D == 12288);
end
%=============================================================================
