%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef qualmetadata
  properties
    Code
    Description
    UserData
  end

  methods
    function obj = qualmetadata(varargin)
      obj.Code = [];
      obj.Description = {};
      obj.UserData = [];
      if nargin > 0
        obj = obj.applyNameValues(varargin{:});
      end
    end

    function obj = applyNameValues(obj, varargin)
      if rem(nargin - 1, 2) ~= 0
        error('Nelson:time:nameValueArgumentsAppearPairs', 'Name-value arguments must be paired.');
      end
      for k = 1:2:length(varargin)
        name = localQualText(varargin{k}, 'Property name');
        switch lower(name)
          case 'code'
            obj.Code = varargin{k + 1};
          case 'description'
            value = varargin{k + 1};
            if ischar(value) || isstring(value)
              value = cellstr(value);
            end
            if ~iscell(value)
              error('Nelson:time:qualityDescriptionsCellArrayText', '%s must be a cell array of text.', 'Quality descriptions');
            end
            obj.Description = value(:);
          case 'userdata'
            obj.UserData = varargin{k + 1};
          otherwise
            error('Nelson:time:unsupportedQualityMetadataProperty', 'Unsupported %s.', 'quality metadata property');
        end
      end
    end

    function value = get(obj, varargin)
      value = localQualMetadataGet(obj, varargin{:});
    end

    function obj = set(obj, varargin)
      obj = obj.applyNameValues(varargin{:});
    end

    function display(obj)
      disp('tsdata.qualmetadata');
      disp(' ');
      disp(obj);
    end
  end
end
%=============================================================================
function value = localQualMetadataGet(obj, varargin)
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
  name = localQualText(varargin{1}, 'Property name');
  match = strcmpi(name, names);
  if ~any(match)
    error('Nelson:time:unknownQualityMetadataProperty', 'Unknown %s property.', 'quality metadata');
  end
  value = obj.(names{find(match, 1)});
end
%=============================================================================
function value = localQualText(value, name)
  if isstring(value)
    value = char(value);
  end
  if ~ischar(value)
    error('Nelson:time:mustBeText', '%s must be text.', name);
  end
end
%=============================================================================
