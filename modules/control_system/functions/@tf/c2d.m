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
function varargout = c2d(varargin)
  % Convert model from continuous to discrete time
  % sysd = c2d(sysc, Ts)
  % sysd = c2d(sysc, Ts, method)
  % sysd = c2d(sysc, Ts, 'prewarp', W)

  narginchk(2, 4);
  nargoutchk(0, 1);
  sys = varargin{1};

  if ~isct(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'Continuous model');
  end

  if ~issiso(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  variable = 'z';
  sys = ss(sys);

  Ts = varargin{2};
  mustBeNumeric(Ts, 2);
  if ~isscalar(Ts);
    error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 2, 'scalar');
  end
  if (Ts < 0)
    error('Nelson:control_system:invalidSampleTime', 'Ts must be finite positive scalar.');
  end

  w0 = 0;
  method = 'Z';

  if nargin > 2
    method = toupper(convertStringsToChars(varargin{3}));
    if ~contains(method(1), {'Z', 'P', 'T'})
      error('Nelson:control_system:wrongValue', 'Wrong value for argument #%d: %s expected.', 3, 'valid method');
    end
    method = method(1);
  end

  if nargin > 3
    if ~strcmp(method, 'P')
      error('Nelson:control_system:prewarpMethodExpected', 'Wrong value for #%d argument: ''prewarp'' method expected.', 3);
    end
    w0 = varargin{4};
    mustBeNumeric(w0, 4);
    if ~isscalar(w0)
      error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 4, 'scalar');
    end
    mustBePositive(w0, 4);
    mustBeFinite(w0, 4);
  end

  if nargin == 4
    sysd = c2d(sys, Ts, method, w0);
  else
    sysd = c2d(sys, Ts, method);
  end

  sysd = tf(sysd);
  sysd.Variable = variable;
  varargout{1} = sysd;
end
