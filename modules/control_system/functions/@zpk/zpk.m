%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef zpk
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
    Z
    P
    K
    Numerator
    Denominator
    Variable
    Name
    Ts
    TimeUnit
  end

  properties (Hidden)
    ZValue = {[]}
    PValue = {[]}
    KValue = []
    VariableValue = 's'
    NameValue = ''
    TsValue = 0
    TimeUnitValue = 'seconds'
    Internal = []
  end

  methods
    function obj = zpk(varargin)
      nargoutchk(0, 1);
      obj.Internal.Version = 1;
      obj.Internal.Ts = 0;
      obj.Internal.IsStatic = false;
      if nargin == 0
        return
      end
      [dataArgs, propertyArgs] = splitConstructorArguments(varargin);
      obj = constructZeroPoleGain(obj, dataArgs);
      obj = applyPropertyPairs(obj, propertyArgs);
    end

    function value = get.Z(obj)
      value = obj.ZValue;
    end

    function obj = set.Z(obj, value)
      obj.ZValue = normalizeRootCell(value);
    end

    function value = get.P(obj)
      value = obj.PValue;
    end

    function obj = set.P(obj, value)
      obj.PValue = normalizeRootCell(value);
    end

    function value = get.K(obj)
      value = obj.KValue;
    end

    function obj = set.K(obj, value)
      if ~isnumeric(value)
        error('Nelson:control_system:invalidType', '%s expected.', 'numeric gain');
      end
      obj.KValue = value;
    end

    function value = get.Numerator(obj)
      [num, ~] = zpkExpandNumeratorDenominator(obj);
      value = num;
    end

    function value = get.Denominator(obj)
      [~, den] = zpkExpandNumeratorDenominator(obj);
      value = den;
    end

    function value = get.Variable(obj)
      value = obj.VariableValue;
    end

    function obj = set.Variable(obj, value)
      value = ltiValidateTextScalar('Variable', value);
      supported = {'s', 'p', 'z', 'q', 'z^-1', 'q^-1'};
      if ~any(strcmp(value, supported))
        error('Nelson:control_system:invalidVariable', '%s expected.', "'s', 'p', 'z', 'q', 'z^-1', or 'q^-1'");
      end
      obj.VariableValue = value;
    end

    function value = get.Ts(obj)
      value = obj.TsValue;
    end

    function obj = set.Ts(obj, value)
      value = ltiValidateSampleTime(value);
      obj.TsValue = value;
      obj.Internal.Ts = value;
      obj.Internal.IsStatic = false;
    end

    function value = get.TimeUnit(obj)
      value = obj.TimeUnitValue;
    end

    function obj = set.TimeUnit(obj, value)
      value = ltiValidateTimeUnit(value);
      obj.TimeUnitValue = value;
    end

    function value = get.Name(obj)
      value = obj.NameValue;
    end

    function obj = set.Name(obj, value)
      obj.NameValue = ltiValidateTextScalar('Name', value);
    end

    function st = struct(obj)
      st = builtin('struct');
      names = ltiPropertyNames('zpk');
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
function obj = constructZeroPoleGain(obj, args)
  switch length(args)
    case 0
      return
    case 1
      obj = zpkOneArgument(obj, args{1});
    case 3
      obj = zpkFromZPK(obj, args{1}, args{2}, args{3}, 0);
    case 4
      obj = zpkFromZPK(obj, args{1}, args{2}, args{3}, args{4});
    otherwise
      error('Nelson:control_system:wrongNumberOfArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function obj = zpkOneArgument(obj, value)
  if islti(value)
    if isa(value, 'zpk')
      obj = value;
    elseif isa(value, 'tf') || isa(value, 'ss')
      obj = ltiToZpk(obj, value);
    else
      error('Nelson:control_system:invalidType', '%s expected.', 'Supported LTI model')
    end
    return
  end
  if isVariableName(value)
    obj = zpkVariable(obj, value);
    return
  end
  if ~isnumeric(value)
    error('Nelson:control_system:invalidType', '%s expected.', 'numeric value');
  end
  obj = zpkStaticGain(obj, value);
end
%=============================================================================
function obj = zpkVariable(obj, value, Ts)
  value = ltiValidateTextScalar('Variable', value);
  if nargin < 3
    if any(strcmp(value, {'z', 'q'}))
      Ts = -1;
    else
      Ts = 0;
    end
  end
  ltiValidateSampleTime(Ts);
  obj.Z = {0};
  obj.P = {zeros(0, 1)};
  obj.K = 1;
  obj.Ts = Ts;
  obj.Variable = value;
  obj.Internal.IsStatic = false;
end
%=============================================================================
function obj = zpkStaticGain(obj, value)
  if ~isscalar(value)
    error('Nelson:control_system:invalidType', '%s expected.', 'scalar gain');
  end
  obj.Z = {zeros(0, 1)};
  obj.P = {zeros(0, 1)};
  obj.K = value;
  obj.Variable = 's';
  obj.Ts = 0;
  obj.Internal.Ts = -2;
  obj.Internal.IsStatic = true;
end
%=============================================================================
function obj = zpkFromZPK(obj, z, p, k, Ts)
  ltiValidateSampleTime(Ts);
  if ~isnumeric(k) || ~isscalar(k)
    error('Nelson:control_system:invalidType', '%s expected.', 'scalar gain');
  end
  obj.Z = z;
  obj.P = p;
  obj.K = k;
  obj.Ts = Ts;
  isStatic = isempty(obj.ZValue{1}) && isempty(obj.PValue{1});
  obj.Internal.IsStatic = isStatic;
  if isStatic && Ts == 0
    obj.Internal.Ts = -2;
    obj.Variable = 's';
  elseif Ts == 0
    obj.Variable = 's';
  else
    obj.Variable = 'z';
  end
end
%=============================================================================
function obj = ltiToZpk(obj, sysIn)
  if ~issiso(sysIn)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end
  tfSys = tf(sysIn);
  num = tfSys.Numerator{1};
  den = tfSys.Denominator{1};
  z = roots(num);
  p = roots(den);
  firstNum = num(find(num ~= 0, 1));
  firstDen = den(find(den ~= 0, 1));
  if isempty(firstNum)
    k = 0;
  else
    k = firstNum / firstDen;
  end
  k = cleanupNumeric(k);
  obj.Z = {cleanupNumeric(z(:))};
  obj.P = {cleanupNumeric(p(:))};
  obj.K = k;
  obj.Ts = tfSys.Ts;
  obj.Variable = tfSys.Variable;
  obj.Internal.IsStatic = isstatic(tfSys);
  if isstatic(tfSys)
    obj.Internal.Ts = -2;
  end
  obj = ltiCopyModelMetadata(obj, tfSys);
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
    name = resolvePropertyName(args{k}, ltiPropertyNames('zpk'));
    obj.(name) = args{k + 1};
  end
end
%=============================================================================
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
%=============================================================================
function name = resolvePropertyName(value, supported)
  value = ltiValidateTextScalar('Name', value);
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
function c = normalizeRootCell(value)
  if isnumeric(value)
    c = {value(:)};
  elseif iscell(value)
    c = cell(size(value));
    for i = 1:numel(value)
      v = value{i};
      if ~isnumeric(v)
        error('Nelson:control_system:invalidType', '%s expected.', 'numeric roots');
      end
      c{i} = v(:);
    end
  else
    error('Nelson:control_system:invalidType', '%s expected.', 'numeric or cell array of roots');
  end
end
%=============================================================================
function [num, den] = zpkExpandNumeratorDenominator(obj)
  z = obj.ZValue;
  p = obj.PValue;
  k = obj.KValue;
  if isempty(k)
    num = {};
    den = {};
    return
  end
  num = cell(size(z));
  den = cell(size(z));
  for i = 1:numel(z)
    gain = k(min(i, numel(k)));
    numerator = cleanupNumeric(gain * poly(z{i}(:).'));
    denominator = cleanupNumeric(poly(p{i}(:).'));
    [numerator, denominator] = arrangeVectors(numerator, denominator);
    num{i} = numerator;
    den{i} = denominator;
  end
end
%=============================================================================
function [v1, v2] = arrangeVectors(v1, v2)
  v1 = v1(:)';
  v2 = v2(:)';
  lenV1 = length(v1);
  lenV2 = length(v2);
  if lenV2 > lenV1
    v1 = [zeros(1, lenV2 - lenV1), v1];
  elseif lenV2 < lenV1
    v2 = [zeros(1, lenV1 - lenV2), v2];
  end
end
%=============================================================================
function value = cleanupNumeric(value)
  if ~isreal(value)
    scale = max([1, abs(value(:)).']);
    if all(abs(imag(value(:))) <= 1e-10 * scale)
      value = real(value);
    end
  end
end
%=============================================================================
