%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = mtimes(sys1, sys2)
  sysA = tf(sys1);
  sysB = tf(sys2);
  
  if ~issiso(sysA) && ~issiso(sysB)
    error(_('SISO LTI model expected.'));
  end
  
  numeratorA = sysA.Numerator{1};
  denominatorA = sysA.Denominator{1};
  numeratorB = sysB.Numerator{1};
  denominatorB = sysB.Denominator{1};
  
  numerator = conv(numeratorA, numeratorB);
  denominator = conv(denominatorA, denominatorB);
  
  Ts = mtimes_timesample(sysA.Ts, sysB.Ts);
  sys = tf(numerator, denominator, Ts);
  
  if isa(sys1, 'tf') && strcmp(sys1.Variable,'z^-1')
    sys.Variable = 'z^-1';
  elseif isa(sys2, 'tf') && strcmp(sys2.Variable,'z^-1')
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
end
%=============================================================================
function Ts = mtimes_timesample(TsA, TsB);
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
