%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = zpk2tf(zpkObj)
  % Converts a zero-pole-gain model into a transfer function model.
  % sys = zpk2tf(zpkObj)
  narginchk(1, 1);
  nargoutchk(0, 1);
  if ~isa(zpkObj, 'zpk')
    error('Nelson:control_system:invalidType', '%s expected.', 'zpk model');
  end
  numerator = zpkObj.Numerator;
  denominator = zpkObj.Denominator;
  if isempty(numerator)
    sys = tf();
    return
  end
  sys = tf(numerator, denominator, zpkObj.Ts);
  sys.Variable = zpkObj.Variable;
  sys = lti.copyModelMetadata(sys, zpkObj);
end
%=============================================================================
