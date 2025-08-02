%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function r = prod(varargin)
  nRhs = length(varargin);
  switch nRhs
    case 0
      error(_('Wrong number of input arguments.'));
    case 1
      X = varargin{1};
      r = sparse(prod(full(X)));
    case 2
      X = varargin{1};
      dim = varargin{2};
      r = sparse(prod(full(X)), dim);
    case 3
      X = varargin{1};
      dim = varargin{2};
      typ = varargin{3};
      r = sparse(prod(full(X)), dim, typ);
    case 4
      X = varargin{1};
      dim = varargin{2};
      typ = varargin{3};
      n = varargin{4};
      r = sparse(prod(full(X)), dim, typ, n);
    otherwise
      error(_('Wrong number of input arguments.'));
    end
  end
  %=============================================================================
