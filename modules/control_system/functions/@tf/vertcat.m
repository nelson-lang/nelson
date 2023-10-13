%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
    
    [pA, mA] = size (sysA.Numerator);
    [pB, mB] = size (sysB.Numerator);
    
    if (mA ~= mB)
      msg = _('dimensions of systems input is incompatible.');
      error(msg)
    end
    
    sys.Numerator = [sysA.Numerator; sysB.Numerator];
    sys.Denominator = [sysA.Denominator; sysB.Denominator];
    
    if (strcmp (sysA.Variable, sysB.Variable))
      sys.Variable = sysB.Variable;
    else
      sys.Variable = sysA.Variable;
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
  sysOut = sys;
end
%=============================================================================
