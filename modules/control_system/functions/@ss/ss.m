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
classdef ss < lti
  properties
    A = []
    B = []
    C = []
    D = []
    E = []
    Offsets = []
    Scaled = false
    StateName = {}
    StatePath = {}
    StateUnit = {}
    InternalDelay = []
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
    Name
    Ts
    TimeUnit
  end

  properties (Hidden)
    NameValue = ''
    TsValue = 0
    TimeUnitValue = 'seconds'
    Internal = []
  end

  methods
    function obj = ss(varargin)
      nargoutchk(0, 1);
      obj.Internal.Version = 1;
      obj.Internal.Ts = 0;
      obj.Internal.IsStatic = false;
      if nargin == 0
        return
      end
      [dataArgs, propertyArgs] = splitConstructorArguments(varargin);
      obj = constructStateSpace(obj, dataArgs);
      obj = applyPropertyPairs(obj, propertyArgs);
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
      names = lti.propertyNames('ss');
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
  for k = 1:length(args)
    if isTextScalar(args{k}) && ~(k == 2 && islti(args{1}))
      propertyStart = k;
      break
    end
  end
  dataArgs = args(1:propertyStart - 1);
  propertyArgs = args(propertyStart:end);
end
%=============================================================================
function obj = constructStateSpace(obj, args)
  switch length(args)
    case 0
      return
    case 1
      obj = ssOneArgument(obj, args{1});
    case 4
      obj = ssFromABCD(obj, args{1}, args{2}, args{3}, args{4}, 0);
    case 5
      obj = ssFromABCD(obj, args{1}, args{2}, args{3}, args{4}, args{5});
    otherwise
      error('Nelson:control_system:wrongNumberOfArguments', 'Wrong number of input arguments.');
  end
end
%=============================================================================
function obj = ssOneArgument(obj, value)
  if islti(value)
    if isa(value, 'ss')
      obj = value;
    elseif isa(value, 'tf')
      obj = tfToSs(value);
    elseif isa(value, 'zpk')
      obj = tfToSs(tf(value));
    else
      error('Nelson:control_system:invalidType', '%s expected.', 'Supported LTI model')
    end
    return
  end
  obj = ssStaticGain(obj, value);
end
%=============================================================================
function obj = ssFromABCD(obj, A, B, C, D, Ts)
  lti.validateSampleTime(Ts);
  obj = ssABCD(obj, A, B, C, D, Ts);
end
%=============================================================================
function obj = ssStaticGain(obj, value)
  obj.B = zeros(0, size(value, 2));
  obj.C = zeros(size(value, 1), 0);
  obj.D = value;
  obj.Ts = 0;
  obj.Internal.Ts = -2;
  obj.Internal.IsStatic = true;
end
%=============================================================================
function obj = tfToSs(sysIn)
  if ~issiso(sysIn)
    error('Nelson:control_system:invalidType', '%s expected.', 'SISO LTI model');
  end
  Ts = sysIn.Ts;
  numerator = sysIn.Numerator{1};
  denominator = sysIn.Denominator{1};
  isemptytf = isempty(numerator) && isempty(denominator);
  A = [];
  B = [];
  C = [];
  D = [];
  if ~isemptytf
    [A, B, C, D] = compreal(numerator, denominator);
  end
  obj = ss(A, B, C, D, Ts);
  obj = lti.copyModelMetadata(obj, sysIn);
end
%=============================================================================
function obj = ssABCD(obj, A, B, C, D, Ts)
  [A, B, C, D, Ts] = adaptABCD(A, B, C, D, Ts);
  msg = checkABCDELocal(A, B, C, D);
  if ~isempty(msg)
    error(msg.identifier, msg.message);
  end
  obj.A = A;
  obj.B = B;
  obj.C = C;
  obj.D = D;
  if isequal(Ts, -2)
    obj.Ts = 0;
    obj.Internal.Ts = -2;
    obj.Internal.IsStatic = true;
  else
    obj.Ts = Ts;
    obj.Internal.Ts = Ts;
    obj.Internal.IsStatic = false;
  end
end
%=============================================================================
function [A, B, C, D, Ts] = adaptABCD(A, B, C, D, Ts)
  isDZero = isscalar(D) && D(1) == 0;
  if isempty(A) && isempty(B) && isempty(C)
    [m, n] = size(D);
    A = [];
    B = zeros(0, n);
    C = zeros(m, 0);
    Ts = -2;
    return
  end
  newRowCount = max(size(C, 1), size(D, 1));
  if isempty(C)
    C = eye(newRowCount, size(A, 1));
  end
  if isempty(D) || isDZero
    D = zeros(newRowCount, size(B, 2));
  end
end
%=============================================================================
function msg = checkABCDELocal(varargin)
  if nargin == 5
    E = varargin{5};
  else
    E = [];
  end
  A = varargin{1};
  B = varargin{2};
  C = varargin{3};
  D = varargin{4};
  [mA, nA] = size(A);
  [mB, nB] = size(B);
  [mC, nC] = size(C);
  [mD, nD] = size(D);
  [mE, nE] = size(E);
  msg.message = '';
  msg.identifier = '';
  msg = msg(zeros(0, 1));
  if mA ~= nA
    msg.message = sprintf(_('%s must be square.'), 'Matrix A');
    msg.identifier = 'Nelson:expectedSquare';
    return
  end
  if mB ~= mA
    msg.message = sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'A', 'B');
    msg.identifier = 'Nelson:control_system:AAndBNumRowsMismatch';
    return
  end
  if nC ~= nA
    msg.message = sprintf(_('Matrices %s and %s should have an identical number of columns.'), 'A', 'C');
    msg.identifier = 'Nelson:control_system:AAndCNumColumnsMismatch';
    return
  end
  if nB ~= nD
    msg.message = sprintf(_('Matrices %s and %s should have an identical number of columns.'), 'B', 'D');
    msg.identifier = 'Nelson:control_system:BAndDNumColumnsMismatch';
    return
  end
  if mC ~= mD
    msg.message = sprintf(_('The number of rows in matrices %s and %s must be equal.'), 'C', 'D');
    msg.identifier = 'Nelson:control_system:CAndDNumRowsMismatch';
    return
  end
  if ~isempty(E) && (mA ~= mE || nA ~= nE)
    msg.message = sprintf(_('Matrices %s and %s should have an identical size.'), 'A', 'E');
    msg.identifier = 'Nelson:control_system:AAndESizeMismatch';
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
    name = resolvePropertyName(args{k}, lti.propertyNames('ss'));
    obj.(name) = args{k + 1};
  end
end
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
