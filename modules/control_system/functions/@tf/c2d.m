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
function varargout = c2d(varargin)
  % Convert model from continuous to discrete time
  % sysd = c2d(sysc, Ts)
  % sysd = c2d(sysc, Ts, method)
  % sysd = c2d(sysc, Ts, 'prewarp', W)
  
  narginchk(2, 4);
  nargoutchk(0, 1);
  sys = varargin{1};
 
  if ~isct(sys)
    error(_('Continuous model expected.'));
  end
 
  if ~issiso(sys)
    error(_('SISO LTI model expected.'));
  end
  
  sys = ss(sys);
  
  Ts = varargin{2};
  mustBeNumeric(Ts, 2);
  if ~isscalar(Ts);
    error(_('Wrong size for input argument #2: scalar expected.'));
  end
  if (Ts < 0)
    error(_('Ts must be finite positive scalar.'));
  end
  
  w0 = 0;
  method = 'Z';
  
  if nargin > 2
    method = toupper(convertStringsToChars(varargin{3}));
    if ~contains(method(1), {'Z', 'P', 'T'})
      error(_('Wrong value for #3 input argument: valid method expected.'));
    end
    method = method(1);
  end
  
  if nargin > 3 
    if ~strcmp(method, 'P')
      error(_("Wrong value for #3 argument: 'prewarp' method expected."));
      end
      w0 = varargin{4};
      mustBeNumeric(w0, 4);
      if ~isscalar(w0)
        error(_('Wrong size for input argument #4: scalar expected.'));
      end
      mustBePositive(w0, 4);
      mustBeFinite(w0, 4);
    end
  end    

  if nargin == 4
    sysd = c2d(sys, Ts, method, w0);
  else
    sysd = c2d(sys, Ts, method);
  end
  
  sysd = tf(sysd);
  sysd.Variable = sys.Variable;
end
