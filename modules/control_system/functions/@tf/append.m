%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function sys = append(varargin)
  for i = 1:length(varargin)
    if ~isa(varargin{i}, 'tf')
      sys = tf(varargin{i});
    else
      sys = varargin{i};
    end
    if(i == 1)
      sysAppend = sys;
    else
      sysAppend = append_two_lti(sysAppend, sys);
    end
  end
  sys = sysAppend;
end
%=============================================================================
function sys = append_two_lti(sys1, sys2)
  
  if (sys1.Ts == sys2.Ts)
    sampleTime = sys1.Ts;
  elseif (sys1.Ts == -2 || sys2.Ts == -2)
    if sys1.Ts == -2
      sampleTime = sys2.Ts;
    else
      sampleTime = sys1.Ts;
    end
  else
    error(_('Sampling times must agree.'));
  end
  
  sys = tf();
  [ny1, nu1] = size(sys1.Numerator);
  [ny2, nu2] = size(sys2.Numerator);
  pad12 = cell(ny1, nu2);
  pad21 = cell(ny2, nu1);
  pad12(:) = {0};
  pad21(:) = {0};
  sys.Numerator = [sys1.Numerator, pad12;pad21, sys2.Numerator];
  pad12(:) = {1};
  pad21(:) = {1};
  sys.Denominator = [sys1.Denominator, pad12;pad21, sys2.Denominator];
  
  if (strcmp (sys1.Variable, sys1.Variable))
    sys.Variable = sys1.Variable;
  else
    sys.Variable = sys1.Variable;
  end
  UserData = [];
  if ~isempty(sys1.UserData) && ~isempty(sys1.UserData)
    UserData = sys1.UserData;
  else
    if ~isempty(sys1.UserData)
      UserData = sys1.UserData;
    end
    if ~isempty(sys1.UserData)
      UserData = sys1.UserData;
    end
  end
  if ~isempty(UserData)
    sys.UserData = UserData;
  end
  sys.Ts = sampleTime;
end
%=============================================================================
