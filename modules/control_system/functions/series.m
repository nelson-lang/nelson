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
function model = series(varargin)
  narginchk(2, 4);
  nargoutchk(0, 1);
  sys1 = varargin{1};
  sys2 = varargin{2};
  if ~islti(sys1) || ~islti(sys2)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end
  if (nargin == 3)
    error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.')
  end
  if (nargin == 2)
    if isa(sys1, 'ss')
      sys2 = ss(sys2);
      lti.checkSampleTimeCompatibility(sys1, sys2);
      model = sys2 * sys1;
    elseif isa(sys1, 'tf')
      sys2 = tf(sys2);
      lti.checkSampleTimeCompatibility(sys1, sys2);
      model = sys2 * sys1;
    end
    return
  end
  outputs1 = varargin{3};
  if ~isvector(outputs1)
    error('Nelson:control_system:wrongSize', 'Wrong size for argument #%d: %s expected.', 2, 'a vector');
  end
  inputs2 = varargin{4};
  if ~isnumeric(inputs2)
    error('Nelson:control_system:wrongType', 'Wrong type for argument #%d: %s expected.', 2, 'real');
  end
  if isa(sys1, 'ss')
    sys2 = ss(sys2);
  elseif isa(sys1, 'tf')
    sys2 = tf(sys2);
  end
  [p1, m1] = size(sys1);
  [p2, m2] = size(sys2);
  lenOut1 = length(outputs1);
  lenIn2 = length(inputs2);

  if lenIn2 ~= lenOut1
    error('Nelson:control_system:dimagree:oUTPUTS1INPUTS2VectorsLength', 'OUTPUTS1 and INPUTS2 must be vectors of the same length.');
  elseif lenIn2 > m2
    error('Nelson:control_system:invalidValue:lengthINPUTS2CannotExceedNumberInputs', 'length of INPUTS2 cannot exceed the number of inputs in SYS2.');
  elseif lenOut1 > p1
    error('Nelson:control_system:invalidValue:lengthOUTPUTS1CannotExceedNumberOutputs', 'length of OUTPUTS1 cannot exceed the number of outputs in SYS1.');
  elseif any(inputs2 <= 0) || any(inputs2 > m2)
    error('Nelson:control_system:indexOutOfRange', 'some index in %s is out of range.', 'INPUTS2');
  elseif any(outputs1 <= 0) || any(outputs1 > p1)
    error('Nelson:control_system:indexOutOfRange', 'some index in %s is out of range.', 'OUTPUTS1');
  end

  outModel = zeros(lenOut1, p1);
  inModel = zeros(m2, lenIn2);
  outModel([1:lenOut1], outputs1) = 1;
  inModel(inputs2, [1:lenOut1]) = 1;

  for k = 1:lenOut1
    outModel(k, outputs1(k)) = 1;
    inModel(inputs2(k), k) = 1;
  end
  model = sys2 * ((inModel * outModel) * sys1);
end
