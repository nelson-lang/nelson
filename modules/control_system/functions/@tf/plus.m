%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = plus(sysA, sysB)
  sys_tfA = tf(sysA);
  sys_tfB = tf(sysB);
  if ~issiso(sysA) || ~issiso(sysA)
    error(_('SISO lti expected.'));
  end
  
  sys = plus_siso(sysA, sysB);
  
  if (strcmp (sysA.Variable, sysB.Variable))
    sys.Variable = sysB.Variable;
  else
    if isa(sysA, 'tf') && strcmp(sysA.Variable, 'z^-1')
      sys.Variable = 'z^-1';
    elseif isa(sysB, 'tf') && strcmp(sysB.Variable, 'z^-1')
      sys.Variable = 'z^-1';
    end
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
function sys = plus_siso(sysA, sysB)
  
  Ts = plus_timesample(sysA.Ts, sysB.Ts);
  numerator = [];
  denominator = [];
  for idx = 1:size(sysA.Numerator{1}, 1)
    numeratorA = sysA.Numerator{1}{1};
    numeratorB = sysB.Numerator{1}{1};
    denominatorA = sysA.Denominator{1}{1};
    denominatorB = sysB.Denominator{1}{1};
    
    numerator1 = conv(numeratorA(idx, :), denominatorB);
    numerator2 = conv(numeratorB(idx, :), denominatorA);
    len = length(numerator1) - length(numerator2);
    
    if (len > 0)
      numerator2 = [zeros(1, len), numerator2];
    elseif (len < 0)
      numerator1 = [zeros(1, -len), numerator1];
    end
    if isempty(numerator)
      numerator = numerator1 + numerator2;
    else
      numerator(idx, :) = numerator1 + numerator2;
    end
    denominator = conv(denominatorA, denominatorB);
  end
  
  sys = tf(numerator, denominator, Ts);
end
%=============================================================================
function Ts = plus_timesample(TsA, TsB)
  Ts = -1;
  if (TsA == -1) && (TsB ~= -1)
    Ts = TsB;
    return
  end
  if (TsB == -1) && (TsA ~= -1)
    Ts = TsA;
    return
  end
  if (TsB == TsA)
    Ts = TsA;
    return
  end
  if (TsA == -2)
    Ts = TsB;
    return
  end
  if (TsB == -2)
    Ts = TsA;
    return
  end
  error(_('Sampling times must agree.'));
end
%=============================================================================
