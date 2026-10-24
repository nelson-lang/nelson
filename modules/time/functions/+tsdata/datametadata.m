%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef datametadata
  properties
    Units
    Interpolation
    UserData
    InterpretSingleRowDataAs3D
  end

  methods
    function obj = datametadata(varargin)
      obj.Units = '';
      obj.Interpolation = tsdata.interpolation('linear');
      obj.UserData = [];
      obj.InterpretSingleRowDataAs3D = false;
      if nargin > 0
        obj = obj.applyNameValues(varargin{:});
      end
    end

    function obj = applyNameValues(obj, varargin)
      if rem(nargin - 1, 2) ~= 0
        error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(varargin)
        name = localText(varargin{k}, 'Property name');
        switch lower(name)
          case 'units'
            obj.Units = varargin{k + 1};
          case 'interpolation'
            if isa(varargin{k + 1}, 'tsdata.interpolation')
              obj.Interpolation = varargin{k + 1};
            else
              obj.Interpolation = tsdata.interpolation(varargin{k + 1});
            end
          case 'userdata'
            obj.UserData = varargin{k + 1};
          case 'interpretsinglerowdataas3d'
            obj.InterpretSingleRowDataAs3D = logical(varargin{k + 1});
          otherwise
            error('Nelson:time:unsupportedDataMetadataProperty', 'Unsupported %s.', 'data metadata property');
        end
      end
    end

    function value = get(obj, varargin)
      value = localMetadataGet(obj, varargin{:});
    end

    function obj = set(obj, varargin)
      obj = obj.applyNameValues(varargin{:});
    end

    function display(obj)
      disp('tsdata.datametadata');
      disp(' ');
      disp(obj);
    end

    function obj = plus(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = minus(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = times(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = mtimes(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = rdivide(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = mrdivide(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = ldivide(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = mldivide(a, b)
      obj = localCommonDataMetadata(a, b);
    end

    function obj = cat(a, b, varargin)
      obj = localCommonDataMetadata(a, b);
    end
  end
end
%=============================================================================
function value = localMetadataGet(obj, varargin)
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
  name = localText(varargin{1}, 'Property name');
  match = strcmpi(name, names);
  if ~any(match)
    error('Nelson:time:unknownDataMetadataProperty', 'Unknown %s property.', 'data metadata');
  end
  value = obj.(names{find(match, 1)});
end
%=============================================================================
function obj = localCommonDataMetadata(a, b)
  if isa(a, 'tsdata.datametadata')
    obj = a;
  elseif isa(b, 'tsdata.datametadata')
    obj = b;
  else
    obj = tsdata.datametadata();
    return
  end
  if isa(a, 'tsdata.datametadata') && isa(b, 'tsdata.datametadata')
    if ~strcmp(a.Units, b.Units)
      obj.Units = '';
    end
    if ~isequal(a.Interpolation, b.Interpolation)
      obj.Interpolation = tsdata.interpolation('linear');
    end
  end
end
%=============================================================================
function value = localText(value, name)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:time:mustBeText', '%s must be text.', name);
  end
end
%=============================================================================
