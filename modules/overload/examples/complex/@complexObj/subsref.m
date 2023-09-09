%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of the Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = subsref(obj, varargin)
  disp('complexObj_extract:')
  disp('nargin')
  disp(nargin)
  disp('nargout')
  disp(nargout)
  disp(varargin)
  n = numel(varargin);
  switch (n)
    case 1
      s = struct(obj);
      f = varargin{1};
      if ischar(f)
        r = s.(f);
      elseif isnumeric(f)
        s.r = f;
        r = class(s, 'complexObj');
      else
        error(_('not managed.'));
      end
    case 2
      f1 = varargin{1};
      f2 = varargin{2};
      if isnumeric(f1) && isnumeric(f2)
        s = struct(obj);
        s.r = f1;
        s.i = f2;
        r = class(s, 'complexObj')
        disp('ici')
      else
        error(_('not managed.'));
      end
    otherwise
      error(_('not managed.'));
    end
  end
  
