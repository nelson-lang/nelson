%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function stats = render_compare_images(firstImage, secondImage, artifactFile)
  firstImage = firstImage(:, :, 1:3);
  secondImage = secondImage(:, :, 1:3);
  firstSize = size(firstImage);
  secondSize = size(secondImage);
  if firstSize(1) ~= secondSize(1) || firstSize(2) ~= secondSize(2)
    secondImage = uint8(imresize(secondImage, [firstSize(1), firstSize(2)], 'bilinear'));
    secondSize = size(secondImage);
  end
  asserts.istrue(abs(firstSize(1) - secondSize(1)) <= 2);
  asserts.istrue(abs(firstSize(2) - secondSize(2)) <= 2);
  rowCount = min(firstSize(1), secondSize(1));
  colCount = min(firstSize(2), secondSize(2));
  firstImage = firstImage(1:rowCount, 1:colCount, :);
  secondImage = secondImage(1:rowCount, 1:colCount, :);

  delta = abs(double(secondImage) - double(firstImage));
  stats.maxAbs = max(delta(:));
  stats.meanAbs = mean(delta(:));
  stats.changedRatio = sum(delta(:) > 3) / numel(delta);

  if nargin > 2 && ~isempty(artifactFile)
    diffScaled = delta * 8;
    diffScaled(diffScaled > 255) = 255;
    separator = uint8(255 * ones(size(firstImage, 1), 8, 3));
    comparison = [firstImage, separator, secondImage, separator, uint8(diffScaled)];
    imwrite(comparison, artifactFile);
  end
end
%=============================================================================
