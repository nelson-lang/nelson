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
function varargout = bode(varargin)
  % Plot the bode diagram of a state space model or a
  % transfer function
  % currently only one SISO system is allowed
  
  % bode()
  % bode(H)
  % bode(H, w)
  % bode(H, w, lineSpec)
  % [magnitude, phase, w] = bode(H)
  % [magnitude, phase, w] = bode(H, wIn)
  
  narginchk(0, 3);
  nargoutchk(0, 3);
  
  w = [];
  lineSpec = '';
  switch nargin
    case 0
      if (nargout != 0)
        error(_('bode without input argument does not return output arguments.'));
      end
      numerator = -1;
      denominator = [1   12   58  139  174  102   19];
      TF = tf(numerator, denominator);
      display(TF);
      disp(_('Call bode using the following command:'))
      disp('bode(tf(num, den));')
      disp(' ')
      bode(TF);
      return
    case 1
      sys = varargin{1};
    case 2
      sys = varargin{1};
      if ischar(varargin{2})
        lineSpec = varargin{2};
      else
        w = varargin{2};
      end
    case 3
      sys = varargin{1};
      if ischar(varargin{2})
        lineSpec = varargin{2};
      else
        w = varargin{2};
      end
      if ischar(varargin{3})
        lineSpec = varargin{3};
      else
        w = varargin{3};
      end
    otherwise
      errror(_('Wrong number of input arguments.'));
    end
    
    if isempty(lineSpec)
      lineSpec = '-';
    end
    [res, lineSpec] = checkLineSpec(lineSpec);
    if ~res
      error(_('invalid line specification.'));
    end
    
    if ~islti(sys)
      error(_('LTI model expected.'));
    end
    if ~issiso(sys)
      error(_('SISO LTI model expected.'));
    end
    
    L = 1000; % Number of frequency elements
    if isempty(w)
      w = [0.01 100];
      w = log10(w);
      w = logspace(w(1), w(2), L);
    else
      w = checkW(w, L);
    end
    
    phase = {};
    magnitude = {};
    
    a = sys.Numerator;
    b = sys.Denominator;
    a = a{1};
    b = b{1};
    H = zeros(1, length(w));
    h = sys.Ts;
    delay = 0;
    N = length(b);
    
    H = [];
    if (sys.Ts > 0)
      for k = 1 : L
        cc = fliplr((exp(1i * w(k) * h)) .^ (0 : N-1)).';
        X = (a * cc);
        Y = (b * cc);
        Z = exp(-delay * exp(1i * w(k) * h));
        H(k) = X / Y * Z;
      end
    else
      for k = 1 : L
        cc = fliplr((1i*w(k)) .^ (0 : N-1)).';
        X = (a* cc);
        Y = (b* cc);
        Z = exp(-delay*1i*w(k));
        H(k) = X / Y * Z;
        
      end
    end
    phase = angle(H) * 180/pi;
    magnitude = 20 * log10(abs(H));
    
    if (nargout == 0)
      f = figure('Visible', 'off');
      subplot(2,1,1);
      title(_('Bode Diagram'));
      semilogx(w, magnitude, lineSpec);
      ylabel(_('Magnitude [dB]'));
      subplot(2,1,2);
      
      semilogx(w, phase, lineSpec);
      ylabel(_('Phase [deg]'));
      xlabel(_('Frequency [rad/s]'));
      f.Visible = 'on';
    end
    
    if (nargout > 0)
      varargout{1} = reshape(magnitude, 1, 1, []);
    end
    if (nargout > 1)
      varargout{2} = reshape(phase, 1, 1, []);
    end    
    if (nargout > 2)
      varargout{3} = w;
    end    
    
  end
  %=============================================================================
function w = checkW(w_maybe, N)
  if (iscell(w_maybe) && (length(w_maybe) == 2))
    w_min = log10(w_maybe{1});
    w_max = log10(w_maybe{2});
    if (w_min > w_max)
      error(_(' frequency interval must be specified as {wmin, wmax} real.'))
    end
    w = logspace (w_min, w_max, N);
  elseif (isnumeric(w_maybe) && isvector(w_maybe))
    if ~isreal(w_maybe)
      error(_('real expected.'));
    else
      w = sort(w_maybe);
    end
  else
    error(_(' frequency interval must be specified as {wmin, wmax} real.'))
  end
end
%=============================================================================
function [res, lineSpec] = checkLineSpec(var)
  res = false;
  lineSpec = [];
  if ischar(var)
    if ismodule('graphics')
      [l, c, m, msg] = colstyle(var);
      if isempty(msg)
        res = true;
        lineSpec = var;
      end
    else
      res = true;
      lineSpec = var;
    end
  end
end
%=============================================================================
function w = computesW(sys, N)
  numerator = sys.Numerator{1}{1};
  denominator = sys.Denominator{1}{1};
  zp = [roots(numerator(:)); roots(denominator(:))];
  
  if ~isdt(sys)
    zp = zp(find (abs(zp) > norm(zp) * eps));
    w_min = min(abs(zp));
    w_min = floor(log10(w_min));
    w_max = max(abs(zp));
    w_max = ceil(log10(w_max));
  else
    zp = zp(find(abs(zp - 1.0) > norm(zp) * eps & abs(zp) > norm(zp) * eps));
    zp = min(abs(log(zp)/(2*pi*sys.Ts)));
    w_min = floor(log10(zp));
    w_max = log10(1/(2*sys.Ts));
  end
  
  if (abs(denominator(end)) >= norm(denominator) * eps)
    w_min = w_min - 2;
    if isct(sys)
      w_max = w_max + 1;
    end
  else
    w_min = w_min - 1;
    if isct(sys)
      w_max = w_max + 1;
    end
  end
  w = logspace(w_min, w_max, N)';
end
%=============================================================================
