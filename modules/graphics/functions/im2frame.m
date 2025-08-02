%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = im2frame(varargin)
  % F = im2frame(RGB)
  % F = im2frame(X, map)
  % F = im2frame(X)
  narginchk(1, 2);
  nargoutchk(0, 1);
  
  inputImage = varargin{1};
  
  % Initialize colormap as empty if not provided
  colormap = [];
  if nargin > 1
    colormap = varargin{2};
  end
  
  % Check dimensions
  if ndims(inputImage) > 4
    error(_('Input image must be a scalar image with at most 4 dimensions.'));
  end
  
  numberOfChannels = size(inputImage, 3);
  
  if numberOfChannels == 3
    % RGB image processing
    if nargin > 1
      warning(_('Colormap ignored for RGB images.'));
    end
    
    % Check if inputImage is a valid RGB image
    if ~isnumeric(inputImage) || ~isreal(inputImage) || ~isreal(inputImage) || issparse(inputImage)
      error(_('RGB image must be a numeric real array.'));
    end
    
  elseif numberOfChannels == 1
    % Indexed or grayscale image processing
    if nargin < 2
      % Handle grayscale image (no colormap provided)
      colormap = gray(256);
    else
      % Check if colormap is a valid colormap
      isValidColormap = isnumeric(colormap) && isreal(colormap) && isfloat(colormap) && ...
      (ndims(colormap) == 2) && (size(colormap, 2) == 3);
      
      if ~isValidColormap
        error(_('A valid colormap expected (Mx3 numeric array).'));
      end
      
      % Check if inputImage is a valid indexed image
      isValidIndexedImage = isnumeric(inputImage) && isreal(inputImage) && ~issparse(inputImage) && ...
      (isfloat(inputImage) && all(inputImage(:) == fix(inputImage(:))) || ...
      (isinteger(inputImage) && intmin(class(inputImage)) == 0));
      
      if ~isValidIndexedImage
        error(_('Indexed image must be an array with integer values.'));
      end
      
      % Handle out-of-range indices for float images
      if isfloat(inputImage)
        invalidIndices = inputImage < 1;
        if any(invalidIndices(:))
          inputImage(invalidIndices) = 1;
        end
      end
      
      % Handle colormap issues
      maxIndexValue = max(inputImage(:));
      isIntegerType = isinteger(inputImage);
      
      if isIntegerType
        if maxIndexValue == intmax(class(inputImage))
          inputImage = single(inputImage);
        end
        inputImage = inputImage + 1;  % Convert 0-based to 1-based indexing
      end
      
      numberOfColors = size(colormap, 1);
      if numberOfColors - isIntegerType < maxIndexValue
        % Extend the colormap or clamp indices
        if numel(inputImage) > maxIndexValue - numberOfColors + isIntegerType
          colormapExtension = repmat(colormap(end, :), maxIndexValue - numberOfColors + isIntegerType, 1);
          colormap(end+(1:size(colormapExtension, 1)), :) = colormapExtension;
        else
          inputImage(inputImage > numberOfColors) = numberOfColors;
        end
      end
    end
  else
    error(_('Input image must be indexed (1 channel) or RGB (3 channels).'));
  end
  
  % Handle 4D images (image sequences)
  if ndims(inputImage) == 4
    inputImage = reshape(num2cell(inputImage, [1 2 3]), 1, size(inputImage, 4));
  end
  
  % Create and return frame structure
  outputFrameStruct = struct('cdata', inputImage, 'colormap', colormap);
  varargout{1} = outputFrameStruct;
end
%=============================================================================
