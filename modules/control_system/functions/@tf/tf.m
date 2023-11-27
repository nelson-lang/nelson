%=============================================================================
% Copyright (c) 2017 September Daniel Mårtensson (Swedish Embedded Control Systems Toolbox)
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = tf(varargin)
  narginchk(0, 3);
  nargoutchk(0, 1);
  sys = [];
  if (nargin == 0)
    sys = tf_no_rhs();
  end
  if (nargin == 1)
    sys = tf_one_rhs(varargin{1});
  end
  if (nargin == 2)
    m = varargin{1};
    if (ischar(m) || isStringScalar(m))
      Ts = varargin{2};
      sys = tf_one_rhs_char(m);
      sys.Ts = Ts;
    else 
      Ts = 0;
      [numeratorerator, denominator, variableName] = adjustTransferFunctionParameters(m, varargin{2}, Ts);
      sys = tf_constructor(numeratorerator, denominator, Ts, variableName);
    end
  end
  if (nargin == 3)
    Ts = varargin{3};
    [numeratorerator, denominator, variableName] = adjustTransferFunctionParameters(varargin{1}, varargin{2}, Ts);
    sys = tf_constructor(numeratorerator, denominator, Ts, variableName);
  end
  if (~isa(sys, 'tf'))
    error(_('Wrong number of input arguments.'));
  end
  varargout{1} = sys;
end
%=============================================================================
function sys = tf_no_rhs()
  tf = {};
  tf.Numerator = {};
  tf.Denominator= {};
  tf.Variable = 's';
  tf.Ts = 0;
  tf.TimeUnit = 'seconds';
  tf.UserData = [];
  tf.Internal = [];
  tf.Internal.Version = 1;
  sys = class(tf, 'tf');
end
%=============================================================================
function sys = tf_one_rhs(m)
  if islti(m)
    if isa(m, 'tf')
      sys = m;
    elseif isa(m, 'ss')
      error(_('Supported LTI model expected.'))
    else
      error(_('Supported LTI model expected.'))
    end
    return,
  elseif (ischar(m) || isStringScalar(m))
    sys = tf_one_rhs_char(m);
    return
  elseif ~isnumeric(m)
    error('numeric value expected.');
  end
  sys = tf_no_rhs();
  sys.Denominator = num2cell(ones(size(m)));
  sys.Numerator = num2cell(m);
  sys.Ts = -2;
end
%=============================================================================
function sys = tf_one_rhs_char(m)
  m = convertStringsToChars(m);
  isInvalid = true;
  if strcmp(m, 's')
    sys = tf([1 0], [0 1]);
    sys.Variable = 's';
    isInvalid = false;
  end
  if strcmp(m, 'z')
    sys = tf([1 0], [0 1]);
    sys.Variable = 'z';
    isInvalid = false;
  end
  if strcmp(m, 'p')
    sys = tf([1 0], [0 1]);
    sys.Variable = 'p';
    isInvalid = false;
  end
  if strcmp(m, 'q')
    sys = tf([1 0], [0 1]);
    sys.Variable = 'q';
    isInvalid = false;
  end
  if isInvalid
    error(_('Invalid syntax: ''s'', ''z'', ''p'', ''q'' expected.'));
  end
end
%=============================================================================
function modifiedPart = checkFractionPart(part)
  if iscell(part)
    if isvector(part)
      part = part(:)';
    else
      error(_('Invalid syntax.'));
    end
  elseif isnumeric(part)
    if isvector(part) || isempty(part)
      part = part(:)';
      part = {part};
    else
      error(_('Invalid syntax.'));
    end
  else
    error(_('Invalid syntax.'));
  end
  for j = 1:length(part)
    for i = 1:length(part{j})
      if(or(part{j}(i) > 0, part{j}(i) < 0))
        part{j} = part{j}(1, i:length(part{j}));
        break;
      end
    end
  end
  modifiedPart = part;
end
%=============================================================================
function [v1, v2] = arrangeVectors(v1, v2)
  v1 = v1(:)';
  v2 = v2(:)';
  lenV1 = length(v1);
  lenV2 = length(v2);
  if lenV2 ~= lenV1
    if (lenV2 > lenV1)
      lenDiff = lenV2 - lenV1;
      v1 = [zeros(1, lenDiff) , v1];
    end
    if (lenV2 < lenV1)
      lenDiff = lenV1 - lenV2;
      v2 = [zeros(1, lenDiff) , v2];
    end
  end
end
%=============================================================================
function [numerator, denominator, variableName] = adjustTransferFunctionParameters(numerator, denominator, Ts)
  variableName = '';
  isStaticGain = false;
  if (isempty(denominator))
    if (isempty(numerator))
      numerator = {}; 
      denominator = {};
      isStaticGain = true;
    elseif (isnumeric(numerator) && ismatrix(numerator) && isreal(numerator))
      numerator = num2cell(numerator);
      denominator = num2cell(ones(size(numerator)));
      isStaticGain = true;
    end
  end
  
  if (~iscell (numerator))
    numerator = {numerator};
  end
  if (~iscell (denominator))
    denominator = {denominator};
  end
  checkisAllZerosFunction = @(x) all(x == 0.);
  isAllZeroNumerator = all(cellfun(checkisAllZerosFunction, numerator));
  isAllZeroDenominator = all(cellfun(checkisAllZerosFunction, denominator));
  
  checkisScalarFunction = @(x) (find (x ~= 0, 1) == length (x)) || (length (find (x ~= 0, 1)) == 0);
  isAllScalarNumerator = ~isAllZeroNumerator && all(cellfun(checkisScalarFunction, numerator));
  isAllScalarDenominator = ~isAllZeroDenominator && all(cellfun(checkisScalarFunction, denominator));
  if (all (isAllScalarNumerator) && all(isAllScalarDenominator))
    isStaticGain = true;
  end 
  if (isStaticGain || (Ts == 0))
    variableName = 's';
  else
    variableName = 'z';
  end
end
%=============================================================================
function sys = tf_constructor(numeratorerator, denominator, Ts, variableName)
  numeratorerator = checkFractionPart(numeratorerator);
  denominator = checkFractionPart(denominator);
  if (length(numeratorerator) == 1)
    for k = 2:length(denominator)
      numeratorerator{k} = numeratorerator{1};
    end
  end
  if (length(denominator) == 1)
    for k = 2:length(numeratorerator)
      denominator{k} = denominator{1};
    end
  end
  if (length(numeratorerator) ~= length(denominator))
    error(_('Numerator and Denominator must have compatible sizes.'));
  end
  for k = 1:length(numeratorerator)
    [N, D] = arrangeVectors(numeratorerator{k}, denominator{k});
    numeratorerator{k} = N;
    denominator{k} = D;
  end
  sys = tf_no_rhs();
  sys.Denominator = denominator;
  sys.Numerator = numeratorerator;
  sys.Variable = variableName;
  sys.Ts = Ts;
end
%=============================================================================
