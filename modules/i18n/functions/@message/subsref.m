%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = subsref(varargin)
  obj = varargin{1};
  s = varargin{2};
  switch s(1).type
    case '.'
      name = s(1).subs;
      st = struct(obj);
      if length(s) == 1
        switch name
          case 'Identifier'
            res = st.Identifier;
          case 'Arguments'
            res = st.Arguments;
          case 'getString'
            res = getString(obj);
          case 'getUnformattedString'
            res = getUnformattedString(obj);
          case 'string'
            res = string(obj);
          otherwise
            error(sprintf(_('No such property ''%s'' for class ''%s''.'), name, class(obj)));
        end
      elseif length(s) >= 2 && strcmp(s(2).type, '()')
        indices = s(2).subs;
        res = obj.(name)(indices{:});
      elseif length(s) >= 2 && strcmp(s(2).type, '{}')
        indices = s(2).subs;
        res = obj.(name){indices{:}};
      else
        error(message('nelson:runtime:notAValidIndexingExpression'));
      end
    case '()'
      error(message('nelson:runtime:illegalIndexingStructure', "'.'"));
    case '{}'
      error(message('nelson:runtime:illegalIndexingStructure', "'.'"));
    otherwise
      error(message('nelson:runtime:notAValidIndexingExpression'));
  end
  varargout{1} = res;
  %=============================================================================
  