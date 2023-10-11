%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = dcgain(varargin)
  % Generates the low frequency gain of a state space model or a transfer function
  % dc = dcgain(sys)
  
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  
  if isa(sys, 'tf')
    for i = 1:size(sys, 1)
      for j = 1:size(sys,2)
        % Get necessary info
        % Get the static gain
        numerators = sys.Numerator;
        denominators = sys.Denominator;
        numerator = numerators{i, j};
        denominator = denominators{i, j};
        dc(i, j) = numerator(length(numerator)) / denominator(length(denominator));
        % If divided by zero - Is not a number
        if isnan(dc(i, j))
          disp(sprintf('Divided my zero - dcgain (%i, %i) set to 0', i, j))
          dc(i, j) = 0;
        end
      end
    end
  elseif isa(sys, 'ss')
    A = sys.A;
    B = sys.B;
    C = sys.C;
    D = sys.D;
    dc = C * inv(-A) * B + D;
  elseif isa(sys, 'zpk')
    dc = dcgain(zpk2tf(sys));
  else
    error(_('LTI model expected.'));
  end
  varargout{1} = dc;
end
