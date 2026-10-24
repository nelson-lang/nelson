%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function out = im2single(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  if nargin == 2
    txt = convertStringsToChars(varargin{2});
    if ~strcmp(txt, 'indexed')
      error('Nelson:invalidText', 'Invalid text argument ''indexed'' expected.');
    end
    img = varargin{1};
    switch class(img)
      case 'uint8'
        out = single(double(img) + 1);
      case 'uint16'
        out = single(double(img) + 1);
      case {'double', 'single'}
        out = single(img);
      case 'logical'
        out = single(img);
      otherwise
        error('Nelson:image_processing:indexedImageDataTypeNoSingle', 'An indexed image must have a data type of ''uint8'', ''uint16'', ''double'', or ''logical''.');
    end
  else
    out = single(im2double(varargin{1}));
  end
end
%=============================================================================
