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
classdef tf < lti
  properties
    IODelay = 0
    InputDelay = 0
    OutputDelay = 0
    InputName = {}
    InputUnit = {}
    InputGroup = []
    OutputName = {}
    OutputUnit = {}
    OutputGroup = []
    Notes = []
    UserData = []
    SamplingGrid = []
  end

  properties (Dependent)
    Numerator
    Denominator
    Variable
    Name
    Ts
    TimeUnit
  end

  properties (Hidden)
    NumeratorValue = {}
    DenominatorValue = {}
    VariableValue = 's'
    NameValue = ''
    TsValue = 0
    TimeUnitValue = 'seconds'
    Internal = []
  end

  methods
    function obj = tf(varargin)
      nargoutchk(0, 1);
      obj.Internal.Version = 1;
      obj.Internal.Ts = 0;
      obj.Internal.IsStatic = false;
      if nargin == 0
        return
      end
      [dataArgs, propertyArgs] = splitConstructorArguments(varargin);
      obj = constructTransferFunction(obj, dataArgs);
      obj = applyPropertyPairs(obj, propertyArgs);
    end

    function value = get.Numerator(obj)
      value = obj.NumeratorValue;
    end

    function obj = set.Numerator(obj, value)
      if ~iscell(value)
        value = {value};
      end
      obj.NumeratorValue = value;
    end

    function value = get.Denominator(obj)
      value = obj.DenominatorValue;
    end

    function obj = set.Denominator(obj, value)
      if ~iscell(value)
        value = {value};
      end
      obj.DenominatorValue = value;
    end

    function value = get.Variable(obj)
      value = obj.VariableValue;
    end

    function obj = set.Variable(obj, value)
      value = lti.validateTextScalar('Variable', value);
      supported = {'s', 'p', 'z', 'q', 'z^-1', 'q^-1'};
      if ~any(strcmp(value, supported))
        error('Nelson:control_system:invalidVariable', '%s expected.', "'s', 'p', 'z', 'q', 'z^-1', or 'q^-1'");
      end
      if obj.Ts == 0 && any(strcmp(value, {'z', 'q', 'z^-1', 'q^-1'})) && ~isstatic(obj)
        error('Nelson:control_system:invalidVariable:discreteTimeVariableRequiresADiscrete', ...
          'Discrete-time variable requires a discrete-time model.');
      end
      if (obj.Ts > 0 || obj.Ts == -1) && any(strcmp(value, {'s', 'p'}))
        error('Nelson:control_system:invalidVariable:continuousTimeVariableRequiresAContinuous', ...
          'Continuous-time variable requires a continuous-time model.');
      end
      obj.VariableValue = value;
    end

    function value = get.Ts(obj)
      value = obj.TsValue;
    end

    function obj = set.Ts(obj, value)
      value = lti.validateSampleTime(value);
      obj.TsValue = value;
      obj.Internal.Ts = value;
      obj.Internal.IsStatic = false;
    end

    function value = get.TimeUnit(obj)
      value = obj.TimeUnitValue;
    end

    function obj = set.TimeUnit(obj, value)
      value = lti.validateTimeUnit(value);
      obj.TimeUnitValue = value;
    end

    function value = get.Name(obj)
      value = obj.NameValue;
    end

    function obj = set.Name(obj, value)
      obj.NameValue = lti.validateTextScalar('Name', value);
    end

    function st = struct(obj)
      st = builtin('struct');
      names = lti.propertyNames('tf');
      for k = 1:length(names)
        name = names{k};
        st.(name) = obj.(name);
      end
      st.Internal = obj.Internal;
    end
  end
end
%=============================================================================
function [dataArgs, propertyArgs] = splitConstructorArguments(args)
  propertyStart = length(args) + 1;
  if isempty(args)
    dataArgs = {};
    propertyArgs = {};
    return
  end
  if isVariableName(args{1})
    if length(args) >= 2 && isnumeric(args{2})
      propertyStart = 3;
    else
      propertyStart = 2;
    end
  else
    for k = 2:length(args)
      if isTextScalar(args{k})
        propertyStart = k;
        break
      end
    end
  end
  dataArgs = args(1:propertyStart - 1);
  propertyArgs = args(propertyStart:end);
end
%=============================================================================
function obj = constructTransferFunction(obj, args)
  switch length(args)
    case 0
      return
    case 1
      obj = tfOneArgument(obj, args{1});
    case 2
      obj = tfTwoArguments(obj, args{1}, args{2});
    case 3
      obj = tfNumeratorDenominator(obj, args{1}, args{2}, args{3});
    otherwise
      error('Nelson:control_system:wrongNumberOfArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function obj = tfOneArgument(obj, value)
  if islti(value)
    if isa(value, 'tf')
      obj = value;
    elseif isa(value, 'ss')
      obj = ssToTf(value);
    elseif isa(value, 'zpk')
      obj = zpk2tf(value);
    else
      error('Nelson:control_system:invalidType', '%s expected.', 'Supported LTI model')
    end
    return
  end
  if isVariableName(value)
    obj = tfVariable(obj, value);
    return
  end
  if ~isnumeric(value)
    error('Nelson:control_system:invalidType', '%s expected.', 'numeric value');
  end
  obj = tfStaticGain(obj, value);
end
%=============================================================================
function obj = tfTwoArguments(obj, first, second)
  if isVariableName(first)
    obj = tfVariable(obj, first, second);
  else
    obj = tfNumeratorDenominator(obj, first, second, 0);
  end
end
%=============================================================================
function obj = tfNumeratorDenominator(obj, numerator, denominator, Ts)
  lti.validateSampleTime(Ts);
  [num, den, variableName, isStatic] = adjustTransferFunctionParameters(numerator, denominator, Ts);
  obj = tfConstructor(obj, num, den, Ts, variableName, isStatic);
end
%=============================================================================
function obj = tfStaticGain(obj, value)
  obj.Denominator = num2cell(ones(size(value)));
  obj.Numerator = num2cell(value);
  obj.Variable = 's';
  obj.Ts = 0;
  obj.Internal.Ts = -2;
  obj.Internal.IsStatic = true;
end
%=============================================================================
function obj = tfVariable(obj, value, Ts)
  value = lti.validateTextScalar('Variable', value);
  if nargin < 3
    Ts = defaultSampleTimeForVariable(value);
  end
  lti.validateSampleTime(Ts);
  obj = tfConstructor(obj, [1 0], [1], Ts, value, false);
end
%=============================================================================
function Ts = defaultSampleTimeForVariable(value)
  if any(strcmp(value, {'z', 'q'}))
    Ts = -1;
  else
    Ts = 0;
  end
end
%=============================================================================
function obj = ssToTf(sysIn)
  if isstatic(sysIn)
    obj = tf(sysIn.D);
    obj = lti.copyModelMetadata(obj, sysIn);
    return
  end
  [A, B, C, D, Ts] = ssdata(sysIn);
  [ny, nu] = size(D);
  numerators = cell(ny, nu);
  denominators = cell(ny, nu);
  pattern = (A ~= 0);
  for i = 1:nu
    for j = 1:ny
      keep = structuralStates(pattern, B(:, i), C(j, :));
      [num, den] = ss2tf(A(keep, keep), B(keep, i), C(j, keep), D(j, i));
      [numerators{j, i}, denominators{j, i}] = arrangeVectors(num, den);
    end
  end
  obj = tf(numerators, denominators, Ts);
  obj = lti.copyModelMetadata(obj, sysIn);
end
%=============================================================================
function keep = structuralStates(pattern, b, c)
  % Keeps states both driven by the input and seen by the output, from the nonzero pattern of A.
  driven = (b ~= 0);
  seen = (c(:) ~= 0);
  changed = true;
  while changed
    nextDriven = driven | any(pattern(:, driven), 2);
    nextSeen = seen | any(pattern(seen, :), 1)';
    changed = ~isequal(nextDriven, driven) || ~isequal(nextSeen, seen);
    driven = nextDriven;
    seen = nextSeen;
  end
  keep = find(driven & seen);
end
%=============================================================================
function [numerator, denominator, variableName, isStaticGain] = adjustTransferFunctionParameters(numerator, denominator, Ts)
  isStaticGain = false;
  if isempty(denominator)
    if isempty(numerator)
      numerator = {};
      denominator = {};
      isStaticGain = true;
    elseif isnumeric(numerator) && ismatrix(numerator) && isreal(numerator)
      numerator = num2cell(numerator);
      denominator = num2cell(ones(size(numerator)));
      isStaticGain = true;
    end
  end
  if ~iscell(numerator)
    numerator = {numerator};
  end
  if ~iscell(denominator)
    denominator = {denominator};
  end
  checkAllZeros = @(x) all(x == 0.);
  zeroDenominator = cellfun(checkAllZeros, denominator);
  isAllZeroDenominator = all(zeroDenominator(:));
  checkScalar = @(x) isempty(find(x ~= 0, 1)) || (find(x ~= 0, 1) == length(x));
  scalarNumerator = cellfun(checkScalar, numerator);
  scalarDenominator = cellfun(checkScalar, denominator);
  isAllScalarNumerator = all(scalarNumerator(:));
  isAllScalarDenominator = ~isAllZeroDenominator && all(scalarDenominator(:));
  if isAllScalarNumerator && isAllScalarDenominator
    isStaticGain = true;
  end
  if Ts == 0
    variableName = 's';
  else
    variableName = 'z';
  end
end
%=============================================================================
function obj = tfConstructor(obj, numerator, denominator, Ts, variableName, isStaticGain)
  lti.validateSampleTime(Ts);
  numerator = checkFractionPart(numerator);
  denominator = checkFractionPart(denominator);
  if isscalar(numerator)
    numerator = repmat(numerator, size(denominator));
  end
  if isscalar(denominator)
    denominator = repmat(denominator, size(numerator));
  end
  if ~isequal(size(numerator), size(denominator))
    error('Nelson:control_system:dimagree:numeratorDenominatorCompatibleSizes', ...
      'Numerator and Denominator must have compatible sizes.');
  end
  for k = 1:numel(numerator)
    [N, D] = arrangeVectors(numerator{k}, denominator{k});
    numerator{k} = N;
    denominator{k} = D;
  end
  obj.Denominator = denominator;
  obj.Numerator = numerator;
  obj.Ts = Ts;
  obj.Variable = variableName;
  obj.Internal.IsStatic = isStaticGain;
  if isStaticGain && Ts == 0
    obj.Internal.Ts = -2;
  end
end
%=============================================================================
function modifiedPart = checkFractionPart(part)
  if iscell(part)
    part = part;
  elseif isnumeric(part)
    if isvector(part) || isempty(part)
      part = part(:)';
      part = {part};
    else
      error('Nelson:control_system:invalidSyntax', 'Invalid syntax.');
    end
  else
    error('Nelson:control_system:invalidSyntax', 'Invalid syntax.');
  end
  for j = 1:numel(part)
    part{j} = part{j}(:)';
    for i = 1:length(part{j})
      if part{j}(i) > 0 || part{j}(i) < 0
        part{j} = part{j}(1, i:length(part{j}));
        break
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
    if lenV2 > lenV1
      v1 = [zeros(1, lenV2 - lenV1), v1];
    end
    if lenV2 < lenV1
      v2 = [zeros(1, lenV1 - lenV2), v2];
    end
  end
end
%=============================================================================
function obj = applyPropertyPairs(obj, args)
  if isempty(args)
    return
  end
  if mod(length(args), 2) ~= 0
    error('Nelson:control_system:nameValuePair', '%s expected.', 'Name-value arguments');
  end
  for k = 1:2:length(args)
    name = resolvePropertyName(args{k}, lti.propertyNames('tf'));
    obj.(name) = args{k + 1};
  end
end
function tf = isVariableName(value)
  tf = false;
  if isTextScalar(value)
    value = char(value);
    tf = any(strcmp(value, {'s', 'p', 'z', 'q'}));
  end
end
%=============================================================================
function tf = isTextScalar(value)
  tf = ischar(value) || isStringScalar(value);
end
function name = resolvePropertyName(value, supported)
  value = lti.validateTextScalar('Name', value);
  matches = startsWith(upper(supported), upper(value));
  if sum(matches) == 0
    error('Nelson:control_system:unknownParameter', '"%s" is not a recognized parameter.', value);
  end
  if sum(matches) > 1
    error('Nelson:control_system:ambiguousParameter', '"%s" matches multiple parameters.', value);
  end
  name = supported{find(matches, 1)};
end
%=============================================================================
