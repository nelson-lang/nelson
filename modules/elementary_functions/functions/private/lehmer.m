%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function lehmerMatrix = lehmer(varargin)
    narginchk(1, 2);
    n = varargin{1};
    if nargin < 2
        className = 'double';
    else
        className = varargin{2};
    end
    rowIndices = cast(1:n, className);
    lowerTriangle = rowIndices ./ rowIndices';
    lehmerMatrix = tril(lowerTriangle) + tril(lowerTriangle, -1)';
end
%=============================================================================
