%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef odeDelay
  properties
    History = []
    ValueDelay = []
    SlopeDelay = []
  end

  methods
    function obj = odeDelay(varargin)
      if mod(nargin, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = 1:2:nargin
        name = varargin{k};
        if isstring(name)
          name = char(name);
        end
        obj = odeAssignDelayProperty(obj, name, varargin{k + 1});
      end
    end

    function obj = set.History(obj, value)
      obj.History = odeNormalizeDelayValue(value, 'History', [1 2]);
    end

    function obj = set.ValueDelay(obj, value)
      obj.ValueDelay = odeNormalizeDelayValue(value, 'ValueDelay', [2 3]);
    end

    function obj = set.SlopeDelay(obj, value)
      obj.SlopeDelay = odeNormalizeDelayValue(value, 'SlopeDelay', [2 3]);
    end
  end
end

function obj = odeAssignDelayProperty(obj, name, value)
  switch lower(name)
    case 'history'
      obj.History = odeNormalizeDelayValue(value, 'History', [1 2]);
    case 'valuedelay'
      obj.ValueDelay = odeNormalizeDelayValue(value, 'ValueDelay', [2 3]);
    case 'slopedelay'
      obj.SlopeDelay = odeNormalizeDelayValue(value, 'SlopeDelay', [2 3]);
    otherwise
      error('Nelson:ode_solvers:unknownDelayProperty', 'Unknown %s property.', 'delay');
  end
end

function value = odeNormalizeDelayValue(value, name, validInputs)
  if isa(value, 'function_handle')
    odeValidateDelayFcn(value, name, validInputs);
    return
  end
  if strcmp(name, 'History') && isstruct(value)
    return
  end
  if ~isempty(value) && (~isnumeric(value) || any(~isfinite(value(:))))
    odeDelayValueError(name);
  end
  value = value(:);
end

function odeValidateDelayFcn(fcn, name, validInputs)
  ninputs = nargin(fcn);
  if ninputs ~= -1 && ~any(ninputs == validInputs)
    odeDelaySignatureError(name);
  end
end

function odeDelayValueError(name)
  if strcmp(name, 'History')
    error('Nelson:ode_solvers:historyNumericFunctionHandleSolutionStructure', 'History must be numeric, a function handle, or a solution structure.');
  elseif strcmp(name, 'ValueDelay')
    error('Nelson:ode_solvers:valuedelayNumericFunctionHandle', '%s must be numeric or a function handle.', 'ValueDelay');
  else
    error('Nelson:ode_solvers:slopedelayNumericFunctionHandle', '%s must be numeric or a function handle.', 'SlopeDelay');
  end
end

function odeDelaySignatureError(name)
  if strcmp(name, 'History')
    error('Nelson:ode_solvers:historyFunctionInvalidSignature', '%s function has an invalid signature.', 'History');
  elseif strcmp(name, 'ValueDelay')
    error('Nelson:ode_solvers:valuedelayFunctionInvalidSignature', '%s function has an invalid signature.', 'ValueDelay');
  else
    error('Nelson:ode_solvers:slopedelayFunctionInvalidSignature', '%s function has an invalid signature.', 'SlopeDelay');
  end
end
%=============================================================================
