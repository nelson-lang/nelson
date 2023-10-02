%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: MIT OR LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = size(varargin)
    narginchk(1, 2);
    nargoutchk(0, 1);
    if (nargin == 1)
        res = size(varargin{1}.Numerator);
    else
        res = size(varargin{1}.Numerator, varargin{2});
    end
    if (nargout == 0 && nargin == 1)
        disp(sprintf(_('Transfer function with %d outputs and %d inputs.'), res(1), res(2)));
    else
        varargout{1} = res;
    end
end
%=============================================================================

