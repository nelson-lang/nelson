%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = imagesc(varargin)
  ax = newplot();
  if (length(varargin) == 0)
    im = image();
    im.CDataMapping = 'scaled';
    if (nargout > 0)
      varargout{1} = im;
    end
    return
  end
  lim = [];
  if( (nargin==2) || (nargin==4) )
    if ( (length( varargin{end} ) == 2) && varargin{end}(1)<=varargin{end}(2) )
      lim = varargin{end};
      varargin{end} = [];
      nargin = nargin - 1;
    end
  end
  
  switch(nargin)
    case 1
      handle = image(varargin{1}, 'CDataMapping', 'scaled');
    case 3
      handle = image(varargin{:}, 'CDataMapping', 'scaled');
    otherwise
      error(_('Invalid input arguments'));
    end
    
    if ~isempty(lim),
      clim(lim);
    end
    
    if (nargout > 0)
      varargout{1} = handle;
    end
  end
  %=============================================================================
