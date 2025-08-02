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
  mustBeA(A, ["char", "string"], 1);
  if isstring(A)
    mustBeTextScalar(A, 1);
  end
  A = convertStringsToChars(A);
  varargout{1} = (A == ' ');
end
%=============================================================================
