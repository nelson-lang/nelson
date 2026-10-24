%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function bw = ip_activecontour_chanvese(img, bw, iterations, method)
  if iterations == 0 || isempty(bw)
    return
  end
  se = strel('square', 3);
  edgeWeight = ones(size(img));
  if strcmp(method, 'edge')
    gx = imfilter(img, [-1 0 1] / 2, 'replicate');
    gy = imfilter(img, [-1; 0; 1] / 2, 'replicate');
    edgeWeight = 1 ./ (1 + gx .^ 2 + gy .^ 2);
  end
  for k = 1:iterations
    old = bw;
    insideValues = img(bw);
    outsideValues = img(~bw);
    if isempty(insideValues) || isempty(outsideValues)
      break
    end
    insideMean = mean(insideValues(:));
    outsideMean = mean(outsideValues(:));
    insideCost = (img - insideMean) .^ 2;
    outsideCost = (img - outsideMean) .^ 2;
    candidate = insideCost <= outsideCost;
    band = imdilate(bw, se) | old;
    band = band & (imerode(bw, se) ~= imdilate(bw, se));
    bw(band) = candidate(band);
    smoothVotes = imfilter(double(bw), ones(3), 'replicate');
    if strcmp(method, 'edge')
      smoothMask = smoothVotes >= 5;
      bw(band & edgeWeight > 0.5) = smoothMask(band & edgeWeight > 0.5);
    else
      smoothMask = smoothVotes >= 4;
      bw(band) = smoothMask(band);
    end
    bw = imfill(bw, 'holes');
    if isequal(bw, old)
      break
    end
  end
end
%=============================================================================
