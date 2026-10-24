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
function varargout = d2c(varargin)
  % sysc = d2c(sysd)
  % sysc = d2c(sysd, method)
  % sysc = d2c(sysd, 'prewarp', w0)

  narginchk(1, 3);
  nargoutchk(0, 1);

  sys = varargin{1};
  if ~isdt(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'Discrete model');
  end
  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  variable = 's';
  sys = ss(sys);

  w0 = 0;
  method = 'Z';

  if nargin > 1
    method = toupper(convertStringsToChars(varargin{2}));
    if ~contains(method(1), {'Z', 'P', 'T'})
      error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 3, 'valid method');
    end
    method = method(1);
  end
  if nargin > 2
    if ~strcmp(method, 'P')
      error('Nelson:control_system:prewarpMethodExpected', 'Wrong value for #%d argument: ''prewarp'' method expected.', 2);
    end
    w0 = varargin{3};
    mustBeNumeric(w0, 3);
    if ~isscalar(w0)
      error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 3, 'scalar');
    end
    mustBePositive(w0, 3);
    mustBeFinite(w0, 3);
  end

  if nargin == 3
    sysa = d2c(sys, method, w0);
  else
    sysa = d2c(sys, method);
  end

  sysa = tf(sysa);
  sysa.Variable = variable;

  varargout{1} = sysa;
end
