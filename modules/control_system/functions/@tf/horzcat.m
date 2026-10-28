%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sysOut = horzcat(varargin)
  % [A, B]
  sys = tf(varargin{1});
  for k = 2:1:nargin
    sysA = sys;
    sysB = tf(varargin{k});

    sys = tf();

    [pA, mA] = size(sysA.Numerator);
    [pB, mB] = size(sysB.Numerator);

    if (pA ~= pB)
      error('Nelson:control_system:systemsOutputIncompatible', 'dimensions of systems output is incompatible.')
    end

    sys.Numerator = [sysA.Numerator, sysB.Numerator];
    sys.Denominator = [sysA.Denominator, sysB.Denominator];

    sys.Ts = lti.resolveSampleTime(sysA, sysB);
    sys.Variable = mergeTfVariable(sysA, sysB, 'left');
    sys.TimeUnit = mergeTimeUnit(sysA, sysB);
    sys.InputName = lti.mergeCellMetadata(sysA.InputName, mA, sysB.InputName, mB, 'concat');
    sys.InputUnit = lti.mergeCellMetadata(sysA.InputUnit, mA, sysB.InputUnit, mB, 'concat');
    sys.OutputName = lti.mergeCellMetadata(sysA.OutputName, pA, sysB.OutputName, pB, 'match');
    sys.OutputUnit = lti.mergeCellMetadata(sysA.OutputUnit, pA, sysB.OutputUnit, pB, 'match');
    userData = mergeUserData(sysA, sysB);
    if ~isempty(userData)
      sys.UserData = userData;
    end
  end
  sysOut = sys;
end
%=============================================================================
