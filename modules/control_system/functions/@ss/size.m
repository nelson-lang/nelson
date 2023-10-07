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
    nargoutchk(0, 2);
 
    sys = varargin{1};
    p = 0; % Number of outputs
    m = 0; % Number of inputs
    n = 0; % Number of states
    if isempty(sys.A) && isempty(sys.B) && isempty(sys.C) && ~isempty(sys.D)
        p = size(sys.D, 1);
        m = size(sys.D, 2);
        n = size(sys.A, 1);
    else
        p = size(sys.C, 1);
        m = size(sys.B, 2);
        n = size(sys.A, 1);
    end

    if (nargout == 0 && nargin == 1)
        disp(sprintf(_('State-space model with %d outputs, %d inputs, and %d states.'), p, m, n));
    else
        if (nargin == 2)
            pos = varargin{2};
            if (pos < 1)
                error(_('Second argument must be a positive integer.'));
            end
            switch pos
                case  1
                    varargout{1} = p;
                case 2
                    varargout{1} = m;
                case 3
                    varargout{1} = n;
                otherwise
                    varargout{1} = 1;
                end
        else
            varargout{1} = p;
            if nargout > 1
                varargout{2} = m;
            end
            if nargout > 2
                varargout{3} = n;
            end
        end
    end
end
%=============================================================================
