%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsasgn(varargin)
  objIn = varargin{1};
  S = varargin{2};
  value = varargin{3};
  name = S.subs;
  if strcmp(S.type, '.') == false
    error(message('nelson:runtime:illegalIndexingStructure', "'.'"));
  end
  if ~isprop(objIn, name)
    error(message('nelson:control_system:noPropertyMatchesIdentifier', 'tf', name));
  end
  switch name
    case 'Variable' 
      objOut = updateVariable(objIn, value);
    case 'TimeUnit'
      objOut = updateTimeUnit(objIn, value);
    case 'Numerator'
      objOut = updateNumerator(objIn, value);
    case 'Denominator'
      objOut = updateDenominator(objIn, value);
    case {'Ts', 'UserData'}
      st = struct(objIn);
      st.(name) = value;
      objOut = class(st, 'tf');
    otherwise
      error(message('nelson:control_system:unsupportedParameter', name));
    end
    varargout{1} = objOut;
  end
  %=============================================================================
function objOut = updateNumerator(objIn, value)
  st = struct(objIn);
  if ~iscell(value)
    st.Numerator = {value};
  else
    st.Numerator = value;
  end
  objOut = class(st, 'tf');
end
%=============================================================================
function objOut = updateDenominator(objIn, value)
  st = struct(objIn);
  if ~iscell(value)
    st.Denominator = {value};
  else
    st.Denominator = value;
  end
  objOut = class(st, 'tf');
end
%=============================================================================
function objOut = updateUserData(objIn, value)
  st = struct(objIn);
  st.Denomitor = value;
  objOut = class(st, 'tf');
end
%=============================================================================
function objOut = updateVariable(objIn, value)
  supportedVariables = {'s', 'z', 'p', 'q', 'z^-1', 'q^-1'};
  isSupported = any(strcmp(supportedVariables, value));
  if ~isSupported
    error(message('nelson:control_system:invalidVariable'));
  else
    st = struct(objIn);
    st.Variable = value;
    objOut = class(st, 'tf');
  end
end
%=============================================================================
function objOut = updateTimeUnit(objIn, value)
  supportedTimeUnits = {'nanoseconds', 'microseconds', ...
  'milliseconds', 'seconds', ...
  'minutes', 'hours', ...
  'days', 'weeks', ...
  'months', 'years'};
  isSupported = any(strcmp(supportedTimeUnits, value));
  if ~isSupported
    error(message('nelson:control_system:invalidTimeUnit'));
  else
    st = struct(objIn);
    st.TimeUnit = value;
    objOut = class(st, 'tf');
  end
end
%=============================================================================
