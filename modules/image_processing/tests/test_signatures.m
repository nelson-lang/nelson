%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
addpath([fileparts(nfilename('fullpathext'), 'path'), '/private']);
%=============================================================================
vararginFunctions = {'activecontour', 'adaptthresh', 'affine2d', 'affine3d', 'bwareaopen', 'bwboundaries', 'bwconncomp', 'bwmorph', 'bwperim', 'bwselect', ...
  'bwtraceboundary', 'cornermetric', 'detectFASTFeatures', 'detectHarrisFeatures', 'edge', 'fspecial', 'grayconnected', 'im2single', ...
  'im2double', 'im2uint16', 'im2uint8', 'imadjust', 'imbinarize', 'imboxfilt', 'imclearborder', 'imcrop', 'imextendedmax', 'imextendedmin', 'imfilter', 'imhmax', 'imhmin', 'imimposemin', ...
  'imfill', 'imgaussfilt', 'imgaussfilt3', 'imhist', 'imreconstruct', 'imref2d', 'imref3d', 'imregionalmax', 'imregionalmin', 'imregcorr', 'imregister', 'imregtform', 'imresize3', 'imtranslate', 'imwarp', 'medfilt2', 'padarray', 'regionprops', ...
  'intrinsicToWorld', 'projective2d', 'regionprops3', 'strel', 'stretchlim', 'transformPointsForward', 'transformPointsInverse', 'warp', 'watershed', 'worldToIntrinsic', 'worldToSubscript'};
for k = 1:numel(vararginFunctions)
  asserts.isequal(nargin(vararginFunctions{k}), -1);
end
%=============================================================================
fixedOne = {'graythresh', 'hsv2rgb', 'im2gray', 'imcomplement', 'labelmatrix', 'rgb2gray', ...
  'rgb2hsv', 'rgb2ycbcr', 'ycbcr2rgb'};
for k = 1:numel(fixedOne)
  asserts.isequal(nargin(fixedOne{k}), 1);
  if strcmp(fixedOne{k}, 'graythresh')
    asserts.isequal(nargout(fixedOne{k}), 2);
  else
    asserts.isequal(nargout(fixedOne{k}), 1);
  end
end
%=============================================================================
fixedTwo = {'imbothat', 'imclose', 'imdilate', 'imerode', 'imopen', 'imtophat', ...
  'ind2gray', 'ind2rgb', 'sizesMatch'};
for k = 1:numel(fixedTwo)
  asserts.isequal(nargin(fixedTwo{k}), 2);
  asserts.isequal(nargout(fixedTwo{k}), 1);
end
asserts.isequal(nargout('bwlabel'), 2);
asserts.isequal(nargin('fitgeotrans'), 3);
asserts.isequal(nargout('fitgeotrans'), 1);
asserts.isequal(nargin('imregconfig'), 1);
asserts.isequal(nargout('imregconfig'), 2);
asserts.isequal(nargout('imregcorr'), 2);
asserts.isequal(nargout('imregister'), 1);
asserts.isequal(nargout('imregtform'), 1);
asserts.isequal(nargout('imwarp'), 2);
asserts.isequal(nargin('imnoise'), -2);
asserts.isequal(nargout('imnoise'), 1);
asserts.isequal(nargin('rgb2ind'), 3);
asserts.isequal(nargout('rgb2ind'), 2);
asserts.isequal(nargout('transformPointsForward'), -1);
asserts.isequal(nargout('transformPointsInverse'), -1);
%=============================================================================
