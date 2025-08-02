%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = im2double(varargin)
  narginchk(1, 2);
  nargoutchk(0, 1);
  img = varargin{1};
  isIndexed = false;
  if nargin == 2
    txt = varargin{2};
    mustBeTextScalar(txt, 2);
    txt = convertStringsToChars(txt);
    if ~strcmp(txt, 'indexed')
      error('Nelson:invalidText', _('Invalid text argument ''indexed'' expected.'));
    end
    isIndexed = true;
  end
  switch class(img)
    case 'double'
      d = img;
      
    case {'logical', 'single'}
      d = double(img);
      
    case 'uint8'
      if isIndexed
        d = double(img) + 1;
      else
        d = double(img) / 255;
      end
      
    case 'uint16'
      if isIndexed
        d = double(img) + 1;
      else
        d = double(img) / 65535;
      end
      
    otherwise
      if ~isIndexed
        d = (double(img) + 32768) / 65535;
      else
        error(_('An indexed image must have a data type of ''uint8'', ''uint16'', ''double'', ''single'', or ''logical''.'));
      end
    end
    varargout{1} = d;
  end
