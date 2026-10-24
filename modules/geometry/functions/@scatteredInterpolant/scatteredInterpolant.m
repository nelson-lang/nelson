%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef scatteredInterpolant
  properties
    Points = []
    Values = []
    Method = 'linear'
    ExtrapolationMethod = 'none'
  end

  properties (Hidden)
    Triangulation = []
  end

  methods
    function obj = scatteredInterpolant(varargin)
      if nargin == 0
        return
      end
      [points, values, method, extrap] = parseInputs(varargin{:});
      obj.Points = double(points);
      obj.Values = values(:);
      obj.Method = method;
      obj.ExtrapolationMethod = extrap;
      if ~strcmp(obj.Method, 'nearest')
        try
          obj.Triangulation = delaunayn(obj.Points);
        catch
          warning('Nelson:scatteredInterpolant:emptyTriangulation', 'The underlying triangulation is empty - the points may be collinear.');
          obj.Triangulation = [];
        end
      end
    end

    vq = evaluate(obj, varargin)
    varargout = subsref(obj, s)
  end
end
%=============================================================================
function [points, values, method, extrap] = parseInputs(varargin)
  method = 'linear';
  extrap = 'none';
  if nargin >= 3 && isText(varargin{end - 1})
    method = lower(char(varargin{end - 1}));
    extrap = lower(char(varargin{end}));
    varargin = varargin(1:end - 2);
  elseif nargin >= 2 && isText(varargin{end})
    method = lower(char(varargin{end}));
    varargin = varargin(1:end - 1);
  end
  if length(varargin) == 2
    points = varargin{1};
    values = varargin{2};
  elseif length(varargin) == 3
    points = [varargin{1}(:), varargin{2}(:)];
    values = varargin{3};
  elseif length(varargin) == 4
    points = [varargin{1}(:), varargin{2}(:), varargin{3}(:)];
    values = varargin{4};
  else
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
  if strcmp(method, 'natural') || strcmp(method, 'cubic')
    method = 'linear';
  elseif strcmp(method, 'v4')
    method = 'nearest';
  elseif ~any(strcmp(method, {'linear', 'nearest'}))
    error('Nelson:geometry:unsupportedInterpolationMethod', 'Unsupported "%s" option.', 'interpolation method');
  end
end
%=============================================================================
function tf = isText(value)
  tf = ischar(value) || (isa(value, 'string') && isscalar(value));
end
%=============================================================================
