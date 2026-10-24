%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = im2uint16(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  img = varargin{1};
  indexed = false;
  if nargin == 2
    txt = convertStringsToChars(varargin{2});
    if ~strcmp(txt, 'indexed')
      error('Nelson:invalidText', 'Invalid text argument ''indexed'' expected.');
    end
    indexed = true;
  end
  if indexed
    switch class(img)
      case 'uint16'
        out = img;
      case 'uint8'
        out = uint16(img);
      case 'double'
        ip_assert_integer_valued(img);
        if any(img(:) < 1) || any(img(:) > 65536)
          error('Nelson:image_processing:indexedDoubleImageValuesRange', 'Indexed double image values must be in the range [%d, %d].', 1, 65536);
        end
        out = uint16(img - 1);
      case 'single'
        ip_assert_integer_valued(img);
        if any(img(:) < 1) || any(img(:) > 65536)
          error('Nelson:image_processing:indexedSingleImageValuesRange', 'Indexed single image values must be in the range [%d, %d].', 1, 65536);
        end
        out = uint16(img - 1);
      case 'logical'
        out = uint16(img);
      otherwise
        error('Nelson:image_processing:indexedImageDataTypeUint8Uint16', 'An indexed image must have a data type of ''uint8'', ''uint16'', ''double'', ''single'', or ''logical''.');
    end
  else
    if ~any(strcmp(class(img), {'double', 'logical', 'uint8', 'uint16', 'int16', 'single'}))
      error('Nelson:image_processing:imageDoubleLogicalUint8Uint16Int16', 'Image must be double, logical, uint8, uint16, int16, or single.');
    end
    out = uint16(round(ip_clip01(ip_to_unit_double(img)) * 65535));
  end
end
%=============================================================================
