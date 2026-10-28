%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = minus(sysA, sysB)
  sysA = tf(sysA);
  sysB = tf(sysB);
  if ~issiso(sysA) || ~issiso(sysB)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO lti');
  end

  sys = minus_siso(sysA, sysB);

  variable = mergeTfVariable(sysA, sysB);
  if ~isempty(variable)
    sys.Variable = variable;
  end
  sys = applyCommonMetadata(sys, sysA, sysB);
  userData = mergeUserData(sysA, sysB);
  if ~isempty(userData)
    sys.UserData = userData;
  end
end
%=============================================================================
function sys = minus_siso(sysA, sysB)

  Ts = lti.resolveSampleTime(sysA, sysB);
  numerator = [];
  denominator = [];
  for idx = 1:size(sysA.Numerator{1}, 1)
    numeratorA = sysA.Numerator{1};
    numeratorB = sysB.Numerator{1};
    denominatorA = sysA.Denominator{1};
    denominatorB = sysB.Denominator{1};

    numerator1 = conv(numeratorA(idx, :), denominatorB);
    numerator2 = conv(numeratorB(idx, :), denominatorA);
    len = length(numerator1) - length(numerator2);

    if (len > 0)
      numerator2 = [zeros(1, len), numerator2];
    elseif (len < 0)
      numerator1 = [zeros(1, -len), numerator1];
    end
    if isempty(numerator)
      numerator = numerator1 - numerator2;
    else
      numerator(idx, :) = numerator1 - numerator2;
    end
    denominator = conv(denominatorA, denominatorB);
  end
  sys = tf(numerator, denominator, Ts);
end
