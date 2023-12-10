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
function model = feedback(varargin)
  % Computes the new transfer function or state space model of serial connection
  % model = feedback(sys1, sys2)
  % model = feedback(sys1, sys2, sign)
  narginchk(2, 3);
  nargoutchk(0, 1);
  
  sys1 = varargin{1};
  sys2 = varargin{2};
  if isnumeric(sys1) && islti(sys2)
    if isa(sys2, 'ss')
      sys1 = ss(sys1);
    else
      sys1 = tf(sys1);
    end
  elseif isnumeric(sys2) && islti(sys1)
    if isa(sys1, 'ss')
      sys2 = ss(sys2);
    else
      sys2 = tf(sys2);
    end
  elseif ~islti(sys1) || ~islti(sys2)
    error(_('LTI model expected.'));
  end
  
  % Check feedback sign
  if(nargin == 3)
    sgn = varargin{3};
    if ~(sgn == 1 || sgn == -1)
      error('The feedback sign must be set to +1 or -1.');
    end
  else
    sgn = -1;
  end
  
  % Check if there is a TF or SS model
  if(isa(sys1, 'ss'))
    sys2 = ss(sys2);
    if(sys1.Ts == sys2.Ts)
      [ny1,nu1] = size(sys1.D);
      [ny2,nu2] = size(sys2.D);
      inputs1 = [1:nu1] * sgn;
      outputs1 = [1:ny1];
      inputs2 = [1:nu2] + nu1;
      outputs2 = [1:ny2] + ny1;
      sys = append(sys1, sys2);
      sys = cloop(sys,[outputs1, outputs2],[inputs2, inputs1]);
      model = ssselect(sys, [1:nu1], [1:ny1]);
    else
      error(_('Sampling times must agree.'));
    end
  elseif isa(sys1, 'tf')
    sys2 = tf(sys2);
    if(sys1.Ts == sys2.Ts)
      num = conv(sys1.Numerator{1}, sys2.Denominator{1});
      den = conv(sys1.Denominator{1}, sys2.Denominator{1}) - sgn*conv(sys1.Numerator{1}, sys2.Numerator{1});
      model = tf(num, den, sys1.Ts);
    else
      error(_('Sampling times must agree.'));
    end
  end
end
