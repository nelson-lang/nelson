%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = mtimes(sys1, sys2)
  sysA = tf(sys1);
  sysB = tf(sys2);

  if ~issiso(sysA) && ~issiso(sysB)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end

  numeratorA = sysA.Numerator{1};
  denominatorA = sysA.Denominator{1};
  numeratorB = sysB.Numerator{1};
  denominatorB = sysB.Denominator{1};

  numerator = conv(numeratorA, numeratorB);
  denominator = conv(denominatorA, denominatorB);

  Ts = lti.resolveSampleTime(sysA, sysB);
  sys = tf(numerator, denominator, Ts);

  variable = mergeTfVariable(sysA, sysB);
  if ~isempty(variable)
    sys.Variable = variable;
  end
  sys = applySeriesMetadata(sys, sysA, sysB);

  userData = mergeUserData(sysA, sysB);
  if ~isempty(userData)
    sys.UserData = userData;
  end
end
