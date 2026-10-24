%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [files, digest] = __nelsonc_image_files__(filename, stage, shapes, opaque)
  reader = activate_gateway('graphics_io');
  restoreReader = onCleanup(@() deactivate_gateway(reader));
  resize = activate_gateway('image_processing');
  restoreResize = onCleanup(@() deactivate_gateway(resize));
  digest = sha256(filename, '-file');
  [pixels, map, alpha] = imread(filename);
  if ~strcmp(digest, sha256(filename, '-file'))
    error('Nelson:nelsonc:icon', 'Application image changed during packaging.');
  end
  height = size(pixels, 1);
  width = size(pixels, 2);
  if height < 1 || width < 1 || height * width > 16777216
    error('Nelson:nelsonc:icon:applicationImageTooLarge', 'Application image must contain at most 16 megapixels.');
  end
  if ~isempty(map)
    pixels = reshape(map(double(pixels(:)) + 1, :), [height, width, 3]);
  else
    pixels = normalized(pixels);
    if size(pixels, 3) == 1
      pixels = repmat(pixels, [1, 1, 3]);
    end
  end
  if isempty(alpha)
    alpha = ones(height, width);
  else
    alpha = normalized(alpha);
  end
  % Filter premultiplied colors to avoid dark fringes around transparent edges.
  source = pixels .* repmat(alpha, [1, 1, 3]);
  files = cell(1, size(shapes, 1));
  for k = 1:size(shapes, 1)
    target = shapes(k, :);
    shape = max(1, round([height, width] * min(target(1) / height, target(2) / width)));
    reduced = imresize(source, shape, 'bilinear');
    a = max(0, min(1, imresize(alpha, shape, 'bilinear')));
    rgb = reduced ./ repmat(max(a, eps), [1, 1, 3]);
    rgb = uint8(round(255 * max(0, min(1, rgb))));
    canvas = zeros(target(1), target(2), 3, 'uint8');
    mask = zeros(target(1), target(2), 'uint8');
    rows = floor((target(1) - shape(1)) / 2) + (1:shape(1));
    columns = floor((target(2) - shape(2)) / 2) + (1:shape(2));
    canvas(rows, columns, :) = rgb;
    mask(rows, columns) = uint8(round(255 * a));
    if opaque
      a = repmat(double(mask) / 255, [1, 1, 3]);
      canvas = uint8(round(double(canvas) .* a + 255 * (1 - a)));
      files{k} = fullfile(stage, ['image-', int2str(target(1)), 'x', int2str(target(2)), '.bmp']);
      imwrite(canvas, files{k});
    else
      files{k} = fullfile(stage, ['icon-', int2str(target(1)), '.png']);
      imwrite(canvas, files{k}, 'Alpha', mask);
    end
  end
end

function info = activate_gateway(module)
  library = ['libnls', upper(module(1)), module(2:end), '_builtin', getdynlibext()];
  gateway = fullfile(modulepath('nelson', 'builtin'), library);
  [name, functions, state] = gatewayinfo(gateway);
  info = struct('path', gateway, 'state', state);
  if ~strcmp(state, 'loaded')
    % Minimal sessions do not register every module; use a build-only gateway.
    addgateway(gateway, 'loaded');
  end
end

function deactivate_gateway(info)
  if ~strcmp(info.state, 'loaded')
    removegateway(info.path);
    if strcmp(info.state, 'lazy'); addgateway(info.path, 'auto'); end
  end
end

function value = normalized(value)
  if isinteger(value)
    value = double(value) / double(intmax(class(value)));
  else
    value = double(value);
  end
end
