%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
%#ok<NLS0026>
function model = cloop(varargin)
  narginchk(1, 3);
  nargoutchk(0, 1);

  sys = varargin{1};

  if nargin == 1
    sgn = -1;
  elseif nargin == 2
    sgn = sign(varargin{2});
  else
    inputs = varargin{2};
    outputs = varargin{3};
    sgn = sign(varargin{3});
  end
  if ~islti(sys)
    error('Nelson:control_system:invalidType', '%s expected.', 'LTI model');
  end

  if isa(sys, 'ss')
    a = sys.A;
    b = sys.B;
    c = sys.C;
    d = sys.D;
    outputCount = size(d, 1);
    inputCount = size(d, 2);
    if (nargin == 1)
      outputs = 1:outputCount;
      inputs = 1:inputCount;
      sgn = -ones(1, length(inputs));
    end
    if (nargin == 2)
      outputs = 1:outputCount;
      inputs = 1:inputCount;
      sgn = sgn * ones(1, length(inputs));
    end

    feedbackInputCount = length(inputs);
    feedbackOutputCount = length(outputs);
    if feedbackInputCount ~= feedbackOutputCount
      error('Nelson:control_system:dimagree:numberFeedbackInputsOutputsAreNot', 'The number of feedback inputs and outputs are not equal.');
    end
    stateCount = size(a, 1);

    S = [a, b; c, d];
    Bu = S(:, stateCount + inputs);
    Cy = S(stateCount + outputs, :);
    if ~isempty(Cy)
      Cy(sgn == -1, :) = -Cy(sgn == -1, :);
      E = eye(feedbackOutputCount) - Cy(:, stateCount + inputs);
      Cy = E \ Cy;
      Sc = S + Bu * Cy;

      ac = Sc(1:stateCount, 1:stateCount);
      bc = Sc(1:stateCount, stateCount + 1:stateCount + inputCount);
      cc = Sc(stateCount + 1:stateCount + outputCount, 1:stateCount);
      dc = Sc(stateCount + 1:stateCount + outputCount, stateCount + 1:stateCount + inputCount);
      model = ss(ac, bc, cc, dc, sys.Ts);
    else
      model = sys;
    end
  else % isa(sys, 'tf')
    numerator = sys.Numerator{1};
    denominator = sys.Denominator{1} - sgn * sys.Numerator{1};
    model = tf(numerator, denominator, sys.Ts);
    model.Variable = sys.Variable;
  end
end
