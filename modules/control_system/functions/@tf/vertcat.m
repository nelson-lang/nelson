%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sysOut = vertcat(varargin)
  % [A; B]
  sys = tf(varargin{1});
  for k = 2:1:nargin
    sysA = sys;
    sysB = tf(varargin{k});

    sys = tf();

    [pA, mA] = size(sysA.Numerator);
    [pB, mB] = size(sysB.Numerator);

    if (mA ~= mB)
      error('Nelson:control_system:systemsInputIncompatible', 'dimensions of systems input is incompatible.')
    end

    sys.Numerator = [sysA.Numerator; sysB.Numerator];
    sys.Denominator = [sysA.Denominator; sysB.Denominator];

    sys.Ts = ltiResolveSampleTime(sysA, sysB);
    sys.Variable = ltiMergeTfVariable(sysA, sysB, 'left');
    sys.TimeUnit = ltiMergeTimeUnit(sysA, sysB);
    sys.InputName = ltiMergeCellMetadata(sysA.InputName, mA, sysB.InputName, mB, 'match');
    sys.InputUnit = ltiMergeCellMetadata(sysA.InputUnit, mA, sysB.InputUnit, mB, 'match');
    sys.OutputName = ltiMergeCellMetadata(sysA.OutputName, pA, sysB.OutputName, pB, 'concat');
    sys.OutputUnit = ltiMergeCellMetadata(sysA.OutputUnit, pA, sysB.OutputUnit, pB, 'concat');
    userData = ltiMergeUserData(sysA, sysB);
    if ~isempty(userData)
      sys.UserData = userData;
    end
  end
  sysOut = sys;
end
%=============================================================================
