%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end
  Ts = lti.resolveSampleTime(sysA, sysB);

  numeratorA = sysA.Numerator{1};
  denominatorA = sysA.Denominator{1};
  numeratorB = sysB.Numerator{1};
  denominatorB = sysB.Denominator{1};

  if isempty(numeratorA) || isempty(numeratorB) || isempty(denominatorA) || isempty(denominatorB)
    error('Nelson:control_system:dimagree:matrixDimensionsAgree', 'Matrix dimensions must agree.');
  end

  if isstatic(sysA) && isstatic(sysB)
    numerator = numeratorA / numeratorB;
    sys = tf(numerator, 1, Ts);
  else
    numerator = conv(numeratorA, denominatorB);
    denominator = conv(denominatorA, numeratorB);
    sys = tf(numerator, denominator, Ts);
  end

  variable = mergeTfVariable(sysA, sysB);
  if ~isempty(variable)
    sys.Variable = variable;
  end
  sys = applyCommonMetadata(sys, sysA, sysB);

  userData = mergeUserData(sysA, sysB);
  if ~isempty(userData)
    sys.UserData = userData;
  end
  varargout{1} = sys;
end
