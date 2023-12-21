%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = parallel(varargin)
    % Generates the new transfer function or new state space model of parallel connection
    % sys = parallel(sys1, sys2)
    narginchk(2, 2);
    nargoutchk(0, 1);

    sys1 = varargin{1};
    sys2 = varargin{2};
    if ~islti(sys1) || ~islti(sys2)
      error(_('LTI model expected.'));
    end
    Ts1 = sys1.Ts;
    Ts2 = sys2.Ts;
    if (Ts1 ~= Ts2)
      error(_('Sampling times must agree.'));
    end

    if isa(sys1, 'ss') || isa(sys2, 'ss')
        sys1 = ss(sys1);
        sys2 = ss(sys2);
    end

    varargout{1} = sys1 + sys2;
end
