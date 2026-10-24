%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [r, q] = iqr(A, varargin)
  if nargout > 1
    [r, q] = iqr(A.SecondsValue, varargin{:});
    q = duration.fromSeconds(q, A.FormatValue);
  else
    r = iqr(A.SecondsValue, varargin{:});
  end
  r = duration.fromSeconds(r, A.FormatValue);
end
%=============================================================================
