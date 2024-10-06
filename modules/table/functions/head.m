%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = head(varargin)
    narginchk(1, 2);
    nargoutchk(0, 1);
    if nargin < 2
        k = 8;
    else
        k = varargin{2};
    end
    A = varargin{1};
    k = min(k, size(A, 1));
    mustBeInteger(k, 2);
    mustBeGreaterThanOrEqual(k, 0, 2);
    out = A(1:k, :);

    if nargout > 0
        varargout{1} = out;
    else
        disp(out);
    end    
end
%=============================================================================
  