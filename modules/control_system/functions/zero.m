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
function varargout = zero(varargin)
  % Generates zeros from SISO transfer function or SISO state space model
  % [z, gain] = zero(sys)
  
  narginchk(1, 1);
  nargoutchk(0, 2);
  
  sys = varargin{1};
  if ~islti(sys)
    error(_('LTI model expected.'));
  end
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  
  if isa(sys, 'ss')
    A = sys.A;
    sizeOrginalA = size(A, 2);
    B = sys.B;
    C = sys.C;
    D = sys.D;
    z = eig([A, B; C, D], [eye(size(A,1)), B*0; C*0, D*0]); % Get zeros
    z = z(isfinite(z)); % Remove the inf and zero values from z
    % check the lengt of z so we can remove some 0 0 0 from z
    if(length(z) - sizeOrginalA > 0)
      % Ok! We have more zeros that orginal size of A
      % Remove all 0 0 0
      z = z(z ~= 0);
      % Then add new one!
      % if sizeOrginalA - length(z) <= 0, then it will be no change
      z = [z zeros(1, sizeOrginalA - length(z))]; % add 0 if needed
    end
    p = pole(varargin{1});
    dc = dcgain(varargin{1});
    gain = dc * prod(-p) / prod(-z);
  elseif isa(sys, 'tf')
    numerators = sys.Numerator;
    denominators = sys.Denominator;
    numerator = numerators{1};
    denominator = denominators{1};
    z = roots(numerator);
    p = roots(denominator);
    dc = dcgain(sys);
    gain = dc * prod(-p) / prod(-z);
  elseif isa(sys, 'zpk')
    [z, gain] = zero(zpk2tf(sys));
  else
    error(_('LTI model expected.'));
  end
  varargout{1} = z;
  if nargout > 1
    varargout{2} = gain;
  end
end
%=============================================================================
