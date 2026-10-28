%=============================================================================
% Copyright (c) 2017 October Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
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
    if (i == 1)
      sysAppend = sys;
    else
      sysAppend = append_two_lti(sysAppend, sys);
    end
  end
  sys = sysAppend;
end
%=============================================================================
function sys = append_two_lti(sys1, sys2)

  sampleTime = lti.resolveSampleTime(sys1, sys2);

  sys = tf();
  [ny1, nu1] = size(sys1.Numerator);
  [ny2, nu2] = size(sys2.Numerator);
  pad12 = cell(ny1, nu2);
  pad21 = cell(ny2, nu1);
  pad12(:) = {0};
  pad21(:) = {0};
  sys.Numerator = [sys1.Numerator, pad12; pad21, sys2.Numerator];
  pad12(:) = {1};
  pad21(:) = {1};
  sys.Denominator = [sys1.Denominator, pad12; pad21, sys2.Denominator];

  sys.Ts = sampleTime;
  sys.Variable = mergeTfVariable(sys1, sys2, 'left');
  sys.TimeUnit = mergeTimeUnit(sys1, sys2);
  sys.InputName = lti.mergeCellMetadata(sys1.InputName, nu1, sys2.InputName, nu2, 'concat');
  sys.InputUnit = lti.mergeCellMetadata(sys1.InputUnit, nu1, sys2.InputUnit, nu2, 'concat');
  sys.OutputName = lti.mergeCellMetadata(sys1.OutputName, ny1, sys2.OutputName, ny2, 'concat');
  sys.OutputUnit = lti.mergeCellMetadata(sys1.OutputUnit, ny1, sys2.OutputUnit, ny2, 'concat');
  userData = mergeUserData(sys1, sys2);
  if ~isempty(userData)
    sys.UserData = userData;
  end
end
%=============================================================================
