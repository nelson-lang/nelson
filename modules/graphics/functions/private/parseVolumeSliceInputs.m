%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [parent, x, y, z, V, xs, ys, zs, method] = parseVolumeSliceInputs(args)
  args = convertStringToCharArgs(args);
  [parent, args] = graphicsParseParent(args);
  method = 'linear';
  if ~isempty(args) && ischar(args{end})
    method = args{end};
    args = args(1:end - 1);
  end
  switch numel(args)
    case 4
      V = validateVolume(args{1});
      [x, y, z] = volumeGridVectors(V);
      xs = args{2};
      ys = args{3};
      zs = args{4};
    case 7
      V = validateVolume(args{4});
      [x, y, z] = volumeGridVectors(args{1}, args{2}, args{3}, V);
      xs = args{5};
      ys = args{6};
      zs = args{7};
    otherwise
      if numel(args) < 4
        error('Nelson:notEnoughInputs', 'Not enough input arguments.');
      end
      if numel(args) > 7
        error('Nelson:tooManyInputs', 'Too many input arguments.');
      end
      error('Nelson:wrongNumberOfInputs', 'Wrong number of input arguments.');
  end
  validateSliceLocations(xs, ys, zs);
  if areAxisSlices(xs, ys, zs)
    xs = double(xs(:).');
    ys = double(ys(:).');
    zs = double(zs(:).');
  else
    if ~isequal(size(xs), size(ys)) || ~isequal(size(xs), size(zs))
      error('Nelson:parseVolumeSliceInputs:invalidSliceSurfaceArrays', 'Slice surface arrays must have the same size.');
    end
    xs = double(xs);
    ys = double(ys);
    zs = double(zs);
  end
end
%=============================================================================
function V = validateVolume(V)
  if ~isnumeric(V) || ndims(V) ~= 3
    error('Nelson:graphics:invalidVolumeData', 'Volume data must be a numeric 3-D array.');
  end
  V = double(V);
end
%=============================================================================
function validateSliceLocations(xs, ys, zs)
  if ~isnumeric(xs) || ~isnumeric(ys) || ~isnumeric(zs)
    error('Nelson:validators:mustBeNumeric', '%s must be numeric.', 'Slice locations');
  end
end
%=============================================================================
function tf = areAxisSlices(xs, ys, zs)
  tf = min(size(xs)) <= 1 && min(size(ys)) <= 1 && min(size(zs)) <= 1;
end
%=============================================================================
