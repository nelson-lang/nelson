%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef odeJacobian
  properties
    Jacobian = []
    SparsityPattern = []
  end

  properties (Hidden)
    Constant = []
  end

  methods
    function obj = odeJacobian(varargin)
      startIndex = 1;
      if nargin > 0 && ~ischar(varargin{1}) && ~isstring(varargin{1})
        value = varargin{1};
        if iscell(value)
          odeValidateJacobianCell(value);
        elseif ~(isempty(value) || isnumeric(value) || isa(value, 'function_handle'))
          error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', 'Jacobian');
        end
        obj.Jacobian = value;
        startIndex = 2;
      end
      if mod(nargin - startIndex + 1, 2) ~= 0
        error('Nelson:ode_solvers:nameValueArgumentsExpected', 'Name-value arguments expected.');
      end
      for k = startIndex:2:nargin
        name = char(varargin{k});
        obj = odeAssignJacobianProperty(obj, name, varargin{k + 1});
      end
    end

    function obj = set.Jacobian(obj, value)
      if iscell(value)
        odeValidateJacobianCell(value);
      elseif ~(isempty(value) || isnumeric(value) || isa(value, 'function_handle'))
        error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', 'Jacobian');
      end
      obj.Jacobian = value;
    end

    function obj = set.SparsityPattern(obj, value)
      odeValidateJacobianPattern(value);
      obj.SparsityPattern = value;
    end

    function obj = set.Constant(obj, value)
      odeValidateJacobianConstant(value);
      obj.Constant = value;
    end
  end
end
%=============================================================================
function obj = odeAssignJacobianProperty(obj, name, value)
  switch lower(name)
    case 'jacobian'
      obj.Jacobian = value;
    case {'sparsitypattern', 'pattern'}
      odeValidateJacobianPattern(value);
      obj.SparsityPattern = value;
    case 'constant'
      obj.Constant = odeValidateJacobianConstant(value);
    otherwise
      error('Nelson:ode_solvers:unknownJacobianProperty', 'Unknown %s property.', 'Jacobian');
  end
end
%=============================================================================
function odeValidateJacobianPattern(value)
  if iscell(value)
    odeValidatePatternCell(value);
  elseif ~(isempty(value) || isnumeric(value) || islogical(value))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Jacobian pattern');
  end
end
%=============================================================================
function odeValidateJacobianCell(value)
  if numel(value) ~= 2 || ~isnumeric(value{1}) || ~isnumeric(value{2})
    error('Nelson:ode_solvers:invalidType:valueNumericAFunctionHandle', '%s must be numeric or a function handle.', 'Jacobian');
  end
end
%=============================================================================
function odeValidatePatternCell(value)
  if numel(value) ~= 2 ...
      || ~(isnumeric(value{1}) || islogical(value{1})) ...
      || ~(isnumeric(value{2}) || islogical(value{2}))
    error('Nelson:validators:mustBeNumericOrLogical', '%s must be numeric or logical.', 'Jacobian pattern');
  end
end
%=============================================================================
function value = odeValidateJacobianConstant(value)
  if isempty(value)
    value = [];
    return
  end
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value) || ~(strcmpi(value, 'on') || strcmpi(value, 'off'))
    error('Nelson:ode_solvers:jacobianConstantFlagOff', 'Jacobian constant flag must be ''on'' or ''off''.');
  end
  if strcmpi(value, 'on')
    value = 'on';
  else
    value = 'off';
  end
end
%=============================================================================
