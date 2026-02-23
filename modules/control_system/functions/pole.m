%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = pole(varargin)
  % Generates poles from transfer functions or state space models
  % p = pole(sys)
  
  narginchk(1, 1);
  nargoutchk(0, 1);
  
  sys = varargin{1};
  if ~islti(sys)
    error(message('nelson:control_system:LTIModelExpected'));
  end
  
  if isa(sys, 'ss')
    p = eig(sys.A); % Eigenvalues
  elseif isa(sys, 'tf')
    if ~issiso(sys)
      error(message('nelson:control_system:SISOLTIModelExpected'));
    end
    denominators = sys.Denominator;
    denominator = denominators{1};
    p = roots(denominator);
  elseif isa(sys, 'zpk')
    p = pole(zpk2tf(sys));
  else
    error(message('nelson:control_system:LTIModelExpected'));
  end
  varargout{1} = p;
end