%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef interpolation
  properties
    Method
    Fhandle
  end

  methods
    function obj = interpolation(method)
      obj.Method = 'linear';
      obj.Fhandle = [];
      if nargin == 0
        return
      end
      if isa(method, 'tsdata.interpolation')
        obj = method;
        return
      end
      obj.Method = tsdata.interpolation.validateMethod(method);
    end

    function tf = eq(a, b)
      if ~isa(a, 'tsdata.interpolation') || ~isa(b, 'tsdata.interpolation')
        tf = false;
        return
      end
      tf = strcmpi(a.Method, b.Method);
    end

    function tf = isequal(a, varargin)
      tf = true;
      for k = 1:length(varargin)
        tf = tf && isa(varargin{k}, 'tsdata.interpolation') && strcmpi(a.Method, varargin{k}.Method);
      end
    end

    function value = get(obj, varargin)
      names = properties(obj);
      if nargin == 1
        value = cell2struct(cell(size(names)), names);
        for k = 1:length(names)
          value.(names{k}) = obj.(names{k});
        end
        return
      end
      if nargin < 2
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if nargin > 2
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      name = varargin{1};
      if isstring(name)
        name = char(name);
      end
      match = strcmpi(name, names);
      if ~any(match)
        error('Nelson:time:unknownInterpolationProperty', 'Unknown %s property.', 'interpolation');
      end
      value = obj.(names{find(match, 1)});
    end
  end

  methods (Static)
    function method = validateMethod(value)
      if isstring(value)
        value = char(value);
      end
      if ~ischar(value)
        error('Nelson:time:mustBeText', '%s must be text.', 'Interpolation method');
      end
      method = lower(value);
      supported = {'linear', 'zoh', 'nearest'};
      if ~any(strcmp(method, supported))
        error('Nelson:time:unsupportedInterpolationMethod', 'Unsupported %s.', 'interpolation method');
      end
    end
  end
end
%=============================================================================
