%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
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
    error(_('Illegal indexing structure argument: type ''.'' expected.'));
  end
  if ~isprop(objIn, name)
    msg = _('No property of the class ''%s'' matches the identifier ''%s''.');
    error(sprintf(msg, 'tf', name));
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
      error(_('Unsupported parameter: ''%s''.'), name)
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
    error(_('''s'', ''p'', ''z'', ''q'', ''z^-1'', or ''q^-1'' expected.'));
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
    msg = _('TimeUnit property must be a valid time unit.');
    error(msg);
  else
    st = struct(objIn);
    st.TimeUnit = value;
    objOut = class(st, 'tf');
  end
end
%=============================================================================
