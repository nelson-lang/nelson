%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = annotation(varargin)
  narginchk(1, 10000);
  nargoutchk(0, 1);

  args = convertStringToCharArgs(varargin);
  [parent, args] = parseAnnotationParent(args);
  if isempty(args) || ~ischar(args{1})
    error('Nelson:annotation:invalidAnnotationType', 'Annotation type must be a string.');
  end

  kind = lower(args{1});
  args = args(2:end);
  if isLineAnnotation(kind)
    [constructorArgs, args] = parseLineAnnotationArgs(args);
  elseif isShapeAnnotation(kind)
    [constructorArgs, args, explicitPosition] = parseShapeAnnotationArgs(args);
    if strcmp(kind, 'textbox') && explicitPosition
      constructorArgs = [constructorArgs, {'FitBoxToText', 'off'}];
    end
  else
    error('Nelson:annotation:unsupportedType', 'Unsupported annotation type.');
  end

  if rem(length(args), 2) ~= 0
    error('Nelson:graphics:nameValueArgumentsExpected', 'Name-value arguments expected.');
  end
  h = __annotation__(kind, parent, constructorArgs{:}, args{:});
  if nargout == 1
    varargout{1} = h;
  end
end
%=============================================================================
function [parent, args] = parseAnnotationParent(args)
  parent = gcf();
  if isempty(args)
    return
  end
  first = args{1};
  if isscalar(first) && isgraphics(first)
    if isgraphics(first, 'figure')
      parent = first;
      args = args(2:end);
    else
      error('Nelson:annotation:invalidAnnotationParent', 'Annotation parent must be a figure.');
    end
  end
end
%=============================================================================
function tf = isLineAnnotation(kind)
  tf = any(strcmp(kind, {'line', 'arrow', 'doublearrow', 'textarrow'}));
end
%=============================================================================
function tf = isShapeAnnotation(kind)
  tf = any(strcmp(kind, {'rectangle', 'ellipse', 'textbox'}));
end
%=============================================================================
function [constructorArgs, args] = parseLineAnnotationArgs(args)
  x = [0.3 0.4];
  y = [0.3 0.4];
  if length(args) >= 2 && isnumeric(args{1}) && isnumeric(args{2})
    x = validateTwoElementVector(args{1}, 'X');
    y = validateTwoElementVector(args{2}, 'Y');
    args = args(3:end);
  elseif ~isempty(args) && ~ischar(args{1})
    error('Nelson:annotation:invalidLineAnnotationCoordinates', 'Line annotation coordinates must be numeric two-element vectors.');
  end
  constructorArgs = {'X', x, 'Y', y};
end
%=============================================================================
function [constructorArgs, args, explicitPosition] = parseShapeAnnotationArgs(args)
  position = [0.3 0.3 0.1 0.1];
  explicitPosition = false;
  if ~isempty(args) && isnumeric(args{1})
    position = validateFourElementVector(args{1}, 'Position');
    args = args(2:end);
    explicitPosition = true;
  elseif ~isempty(args) && ~ischar(args{1})
    error('Nelson:annotation:invalidShapeAnnotationPosition', 'Shape annotation position must be a numeric four-element vector.');
  end
  constructorArgs = {'Position', position};
end
%=============================================================================
function value = validateTwoElementVector(value, name)
  if ~isnumeric(value) || numel(value) ~= 2
    error('Nelson:annotation:mustBeTwoElementVector', '%s must be a two-element numeric vector.', name);
  end
  value = double(reshape(value, 1, 2));
end
%=============================================================================
function value = validateFourElementVector(value, name)
  if ~isnumeric(value) || numel(value) ~= 4
    error('Nelson:annotation:mustBeFourElementVector', '%s must be a four-element numeric vector.', name);
  end
  value = double(reshape(value, 1, 4));
end
%=============================================================================
