%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = mrdivide(varargin)
  narginchk(2, 2);
  nargoutchk(0, 1);
  sysA = tf(varargin{1});
  sysB = tf(varargin{2});
  if ~issiso(sysA) && ~issiso(sysB)
    error(_('SISO LTI model expected.'));
  end
  Ts = mrdivide_timesample(sysA.Ts, sysB.Ts);
  
  numeratorA = sysA.Numerator{1};
  denominatorA = sysA.Denominator{1};
  numeratorB = sysB.Numerator{1};
  denominatorB = sysB.Denominator{1};
  
  if isempty(numeratorA) || isempty(numeratorB) || isempty(denominatorA) || isempty(denominatorB)
    error(_('Matrix dimensions must agree.'));
  end
  
  if isstatic(sysA) && isstatic(sysA)
    numerator = numeratorA / numeratorB;
    sys = tf(numerator, 1, Ts);
  else
    numerator = conv(numeratorA, denominatorB);
    denominator = conv(denominatorA, numeratorB);
    sys = tf(numerator, denominator, Ts);
  end 
  
  if isa(sysA, 'tf') && strcmp(sysA.Variable,'z^-1')
    sys.Variable = 'z^-1';
  elseif isa(sysB, 'tf') && strcmp(sysB.Variable,'z^-1')
    sys.Variable = 'z^-1';
  end
  
  UserData = [];
  if ~isempty(sysA.UserData) && ~isempty(sysB.UserData)
    UserData = sysB.UserData;
  else
    if ~isempty(sysA.UserData)
      UserData = sysA.UserData;
    end
    if ~isempty(sysA.UserData)
      UserData = sysA.UserData;
    end
  end
  if ~isempty(UserData)
    sys.UserData = UserData;
  end
  varargout{1} = sys;
end
%=============================================================================
function Ts = mrdivide_timesample(TsA, TsB)
  Ts = TsA;
  if Ts ~= TsB
    if ((Ts > 0) && (TsB > 0))
      error(_('Sampling times must agree.'));
    end
    if (Ts < 0)
      Ts = TsB;
    end
  end
end    
%=============================================================================
