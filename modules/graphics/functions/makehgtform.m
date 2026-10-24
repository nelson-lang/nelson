%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function M = makehgtform(varargin)
  % M = makehgtform()
  % M = makehgtform('translate', [tx ty tz])
  % M = makehgtform('scale', s) | ('scale', [sx sy sz])
  % M = makehgtform('xrotate', r) | ('yrotate', r) | ('zrotate', r)
  % Transforms given in sequence are multiplied left to right, so the first
  % listed transform is applied last to a point.
  M = eye(4);
  k = 1;
  while k <= nargin
    name = varargin{k};
    if ~(ischar(name) || isStringScalar(name))
      error('Nelson:makehgtform:invalidTransform', 'Transformation name must be a string.');
    end
    name = lower(char(name));
    if k + 1 > nargin
      error('Nelson:makehgtform:missingValue', ...
        'Missing value for transformation ''%s''.', name);
    end
    value = varargin{k + 1};
    if ~isnumeric(value) || ~isreal(value)
      error('Nelson:makehgtform:invalidValue', ...
        'Transformation value must be real and numeric.');
    end
    T = transformMatrix(name, value);
    M = M * T;
    k = k + 2;
  end
end
%=============================================================================
function T = transformMatrix(name, value)
  switch name
    case 'translate'
      t = translationVector(value);
      T = eye(4);
      T(1:3, 4) = t(:);
    case 'scale'
      s = scaleVector(value);
      T = diag([s(1), s(2), s(3), 1]);
    case 'xrotate'
      T = rotationMatrix(scalarAngle(value), 'x');
    case 'yrotate'
      T = rotationMatrix(scalarAngle(value), 'y');
    case 'zrotate'
      T = rotationMatrix(scalarAngle(value), 'z');
    otherwise
      error('Nelson:makehgtform:unknownTransform', ...
        'Unknown transformation ''%s''.', name);
  end
end
%=============================================================================
function t = translationVector(value)
  if numel(value) ~= 3
    error('Nelson:makehgtform:invalidTranslate', ...
      'Translation must be a three-element vector.');
  end
  t = double(value);
end
%=============================================================================
function s = scaleVector(value)
  if isscalar(value)
    s = double([value, value, value]);
  elseif numel(value) == 3
    s = double(value);
  else
    error('Nelson:makehgtform:invalidScale', ...
      'Scale must be a scalar or a three-element vector.');
  end
end
%=============================================================================
function r = scalarAngle(value)
  if ~isscalar(value)
    error('Nelson:makehgtform:invalidAngle', 'Rotation angle must be a scalar.');
  end
  r = double(value);
end
%=============================================================================
function T = rotationMatrix(angle, axis)
  c = cos(angle);
  s = sin(angle);
  T = eye(4);
  switch axis
    case 'x'
      T(2:3, 2:3) = [c, -s; s, c];
    case 'y'
      T([1, 3], [1, 3]) = [c, s; -s, c];
    case 'z'
      T(1:2, 1:2) = [c, -s; s, c];
  end
end
%=============================================================================
