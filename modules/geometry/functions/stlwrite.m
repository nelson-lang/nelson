%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stlwrite(TR, filename, varargin)
  if nargin < 2
    error('Nelson:geometry:wrongNumberInputArguments', 'Wrong number of input arguments.');
  end
  [connectivity, points] = localTriangulationData(TR);
  [fileformat, attributes, solidID] = localParseOptions(size(connectivity, 1), varargin{:});
  __stlwrite__(connectivity, points, filename, fileformat, attributes, solidID);
end
%=============================================================================
function [connectivity, points] = localTriangulationData(TR)
  if ~isa(TR, 'triangulation')
    error('Nelson:geometry:triangulationObjectExpected', 'Triangulation object expected.');
  end
  connectivity = TR.ConnectivityList;
  points = TR.Points;
  if size(connectivity, 2) ~= 3
    error('Nelson:geometry:connectivityListThreeColumns', '%s must have three columns.', 'Connectivity list');
  end
  if size(points, 2) ~= 2 && size(points, 2) ~= 3
    error('Nelson:geometry:pointsTwoThreeColumns', '%s must have two or three columns.', 'Points');
  end
end
%=============================================================================
function [fileformat, attributes, solidID] = localParseOptions(ntri, varargin)
  fileformat = 'binary';
  attributes = [];
  solidID = [];
  if ~isempty(varargin) && localIsFileFormat(varargin{1})
    fileformat = lower(char(varargin{1}));
    varargin = varargin(2:end);
  elseif ~isempty(varargin) && localIsTextScalar(varargin{1}) ...
      && ~localIsName(varargin{1})
    error('Nelson:geometry:fileFormatBinaryText', 'File format must be ''binary'' or ''text''.');
  end
  if mod(length(varargin), 2) ~= 0
    error('Nelson:geometry:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  for k = 1:2:length(varargin)
    if ~localIsTextScalar(varargin{k})
      error('Nelson:validators:mustBeTextScalar', '%s must be a character vector or string scalar.', 'Name');
    end
    name = lower(char(varargin{k}));
    value = varargin{k + 1};
    if strcmp(name, 'attribute')
      attributes = uint16(value(:));
    elseif strcmp(name, 'solidindex')
      solidID = double(value(:));
    else
      error('Nelson:geometry:unsupportedNameValueArgument', 'Unsupported "%s" option.', 'name-value argument');
    end
  end
  if strcmp(fileformat, 'text') && ~isempty(attributes)
    error('Nelson:geometry:attributeParameterSupportedFileFormatBinary', '''Attribute'' parameter is only supported when file format is ''binary''.');
  end
  if strcmp(fileformat, 'binary') && ~isempty(solidID)
    error('Nelson:geometry:solidindexParameterSupportedFileFormatText', '''SolidIndex'' parameter is only supported when file format is ''text''.');
  end
  if strcmp(fileformat, 'binary') && isempty(attributes)
    attributes = uint16(zeros(ntri, 1));
  end
  if strcmp(fileformat, 'text') && isempty(solidID)
    solidID = ones(ntri, 1);
  end
end
%=============================================================================
function tf = localIsFileFormat(value)
  tf = false;
  if localIsTextScalar(value)
    text = lower(char(value));
    tf = strcmp(text, 'binary') || strcmp(text, 'text');
  end
end
%=============================================================================
function tf = localIsName(value)
  tf = false;
  if localIsTextScalar(value)
    text = lower(char(value));
    tf = strcmp(text, 'attribute') || strcmp(text, 'solidindex');
  end
end
%=============================================================================
function tf = localIsTextScalar(value)
  tf = ischar(value) || (isa(value, 'string') && isscalar(value));
end
%=============================================================================
