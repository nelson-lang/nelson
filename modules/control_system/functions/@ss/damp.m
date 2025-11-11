%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = damp(varargin)
  narginchk(1, 1);
  nargoutchk(0, 4);
  sys = varargin{1};
  sampleTime = sys.Ts;
  if isct(sys)
    Poles = pole(sys);
    Frequency = abs(Poles);
    Damping = -cos(angle(Poles));
    TimeConstant = (1 ./ (Frequency .* Damping));
  else
    Poles = pole(sys);
    Frequency = abs(log(Poles) / sampleTime);
    Damping = -cos(angle(log(Poles)));
    TimeConstant = (1 ./ (Frequency .* Damping));
  end
  % sort
  sPoles = size(Poles);
  for k = 1:prod(sPoles(3:end))
    [~, idx] = sort(Damping(:, k));
    Damping(:, k) = Damping(idx, k);
    Poles(:, k) = Poles(idx, k);
    Frequency(:, k) = Frequency(idx, k);
    TimeConstant(:, k) = TimeConstant(idx, k);
  end
  
  varargout{1} = Frequency;
  if nargout > 1
    varargout{2} = Damping;
  end
  if nargout > 2
    varargout{3} = Poles;  
  end
  if nargout > 3
    varargout{4} = TimeConstant;  
  end
end
