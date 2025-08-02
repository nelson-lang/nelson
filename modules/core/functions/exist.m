%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function res = exist(varargin)
  res = 0;
  narginchk(1, 2)
  varname = varargin{1};
  if nargin == 2
    category = varargin{2};
    supported = {'var', 'file', 'dir', 'builtin'};
    if ~any(strcmp(category, supported))
      error(_('Argument #2 must contain a valid string ''var'', ''builtin'', ''dir'' or ''file'' expected.'));
    end
    switch category
      case 'var'
        if isvar('caller', varname)
          res = 1;
        end
      case 'builtin'
        if isbuiltin(varname)
          res = 5;
        end
      case 'dir'
        if isdir(varname)
          res = 7;
        end
      case 'file'
        if isfile(varname)
          res = 2;
        else
          if ismex(varname)
            res = 3;
          else 
            if ismacro(varname)
              res = 2;
            end
          end
        end
      otherwise
        res = 0;
      end
    else
      res = exist(varname, 'file');
      if res ~= 0
        return;
      end
      res = exist(varname, 'dir');
      if res ~= 0
        return;
      end
      if isvar('caller', varname)
        res = 1;
        return;
      end
      res = exist(varname, 'builtin');
      if res ~= 0
        return;
      end
    end
  end
  %=============================================================================
